package foresight.eqsat.rewriting

import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.{OperationCanceledException, ParallelMap}
import foresight.eqsat.ReadOnlyEGraph
import foresight.eqsat.mutable
import foresight.eqsat.immutable

/**
 * Represents a rewrite rule as the composition of a [[Searcher]] and an [[Applier]].
 *
 * A rule has two primary components:
 *
 *  1. **Search** – uses a [[Searcher]] to discover all matches of the rule in a given [[EGraph]].
 *  2. **Apply** – for each match, uses an [[Applier]] to produce a [[Command]] that mutates the e-graph
 *     (e.g., unions, insertions). Applications may be parallelized via [[ParallelMap]].
 *
 * After search and apply, a rule performs a **composition step**:
 *  - Command aggregation – all per-match commands are combined into a single, optimized
 *    [[CommandQueue]] so they can be executed as one logical unit.
 *  - This aggregation deduplicates operations, removes no-ops, and preserves an execution order
 *    that is valid but not necessarily identical to the match order.
 *  - The result can be executed immediately (via [[apply]] / [[tryApply]]) or returned for later
 *    execution (via [[delayed]]).
 *
 * # When to use
 * - Use [[apply]] when you want to search-and-apply now, returning the (possibly) updated e-graph.
 * - Use [[tryApply]] when you want to know if *any* change happened, without creating a new object if none did.
 * - Use [[delayed(EGraphT, ParallelMap)]] when you want to stage the work and run it later,
 *   e.g., to batch multiple rules into a single saturation iteration.
 *
 * # Parallelism
 * Both searching and applying matches are parallelizable. The provided [[ParallelMap]] controls
 * concurrency and labeling of sub-tasks:
 *  - [[search]] uses `parallelize.child(s"match $name")`.
 *  - [[delayed(matches:*, *, *)]] uses `parallelize.child(s"apply $name")`.
 *
 * Implementations of [[Applier]] must be safe to execute concurrently over distinct matches of the same e-graph
 * snapshot. While the **order** of applications is not deterministic, rule appliers should be designed so that
 * observable semantics do not depend on application order.
 *
 * # Errors
 * If an error occurs while constructing or executing per-match commands, an
 * [[Rule.ApplicationException]] is thrown with context about the rule and e-graph.
 *
 * @tparam NodeT   Node payload type stored in the e-graph.
 * @tparam MatchT  Match object produced by the [[Searcher]] and consumed by the [[Applier]].
 * @tparam EGraphT Concrete e-graph type this rule targets. Must be both [[EGraphLike]] and [[EGraph]].
 * @param name      Human-readable rule name (used in logs/diagnostics).
 * @param searcher  Component responsible for finding matches (see [[Searcher.search]]).
 * @param applier   Component that turns a match into a [[Command]] acting on the e-graph.
 *
 * @example Defining and running a rule immediately
 * {{{
 * val constantFold: Rule[MyNode, MyMatch, MyEGraph] =
 *   Rule("const-fold", mySearcher, myApplier)
 *
 * // Runs search + apply now; returns updated e-graph (or the same instance if no changes).
 * val updated = constantFold(egraph)
 * }}}
 *
 * @example Staging a rule and applying later (batching with other rules)
 * {{{
 * val r1Cmd = r1.delayed(egraph) // stage first rule
 * val r2Cmd = r2.delayed(egraph) // stage second rule
 *
 * // Merge both into one optimized command
 * val batched = CommandQueue(Seq(r1Cmd, r2Cmd)).optimized
 *
 * // Somewhere later in the saturation loop:
 * val (maybeNewEGraph, _) = batched(egraph, Map.empty, ParallelMap.default)
 * val next = maybeNewEGraph.getOrElse(egraph) // fallback to original if no changes
 * }}}
 */
final case class Rule[NodeT, MatchT, EGraphT <: ReadOnlyEGraph[NodeT]](name: String,
                                                                       searcher: Searcher[NodeT, MatchT, EGraphT],
                                                                       applier: Applier[NodeT, MatchT, EGraphT])
  extends Rewrite[NodeT, MatchT, EGraphT] {

  /**
   * Find all matches of this rule in the given e-graph.
   *
   * @param egraph      E-graph to search.
   * @param parallelize Parallel strategy (used to label/structure match work).
   * @return Sequence of matches (possibly empty).
   */
  override def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Seq[MatchT] = {
    searcher.searchAndCollect(egraph, parallelize.child(s"match $name"))
  }

  /**
   * Build a staged command that, when executed, applies this rule's matches to `egraph`.
   *
   * This does not mutate the e-graph now; instead it returns a [[Command]] that you can:
   *  - enqueue into a [[CommandQueue]] with other rules; and
   *  - execute later as part of a larger saturation step.
   *
   * @param egraph      The e-graph to search for matches. The staged command is intended to be run
   *                    against the same (or equivalent) snapshot.
   * @param parallelize Parallel strategy for both search and later application.
   * @return A single, optimized [[Command]] that applies all current matches of this rule.
   */
  override def delayed(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Command[NodeT] = {
    val pipeline = searcher.andApply(applier)
    aggregateCommands(
      pipeline.searchAndCollect(egraph, parallelize.child(s"match+apply $name")),
      egraph)
  }

  /**
   * Build a staged command from a precomputed set of matches.
   *
   * This overload is useful when you have already performed searching (e.g., to de-duplicate across rules)
   * but still want to construct a single command that applies all matches.
   *
   * @param matches     Matches to apply.
   * @param egraph      Target e-graph from which matches were derived.
   * @param parallelize Parallel strategy used when building per-match commands.
   * @return An optimized [[CommandQueue]] encapsulated as a [[Command]].
   * @throws Rule.ApplicationException
   * if constructing the per-match commands fails.
   */
  override def delayed(matches: Seq[MatchT], egraph: EGraphT, parallelize: ParallelMap): Command[NodeT] = {
    aggregateCommands(
      parallelize.child(s"apply $name")[MatchT, Command[NodeT]](matches, applier.apply(_, egraph)).toSeq,
      egraph)
  }

  private def aggregateCommands(buildCommands: => Seq[Command[NodeT]], egraph: EGraphT): Command[NodeT] = {
    try {
      CommandQueue(buildCommands)
    } catch {
      case e: OperationCanceledException => throw e
      case e: Exception =>
        throw Rule.ApplicationException(this, egraph, e)
    }
  }

  /**
   * Attempt to construct a semantically reversed rule.
   *
   * A rule is reversible only if:
   *  - its [[Searcher]] is a [[ReversibleSearcher]] and can yield a reversed [[Applier]], and
   *  - its [[Applier]] is a [[ReversibleApplier]] and can yield a reversed [[Searcher]].
   *
   * If both components can be reversed, a new [[Rule]] is returned whose name is `"${name} (reversed)"`.
   *
   * @return `Some(reversedRule)` if both components support reversal; otherwise `None`.
   */
  def tryReverse: Option[Rule[NodeT, MatchT, EGraphT]] = {
    val revApplier = searcher match {
      case r: ReversibleSearcher[NodeT, MatchT, EGraphT] => r.tryReverse
      case _ => None
    }

    val revSearcher = applier match {
      case r: ReversibleApplier[NodeT, MatchT, EGraphT] => r.tryReverse
      case _ => None
    }

    (revApplier, revSearcher) match {
      case (Some(ap), Some(sr)) => Some(Rule(s"$name (reversed)", sr, ap))
      case _ => None
    }
  }
}

/**
 * Utilities for [[Rule]].
 */
object Rule {
  /**
   * Thrown when building or executing the application commands for a [[Rule]] fails.
   *
   * This usually wraps an exception raised by the underlying [[Applier]] or by command construction.
   * It carries the failing rule and the target e-graph snapshot to aid debugging.
   *
   * @param rule   Rule being applied when the failure occurred.
   * @param egraph Target e-graph.
   * @param cause  Underlying exception.
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match type.
   * @tparam EGraphT E-graph type.
   */
  final case class ApplicationException[NodeT, MatchT, EGraphT <: ReadOnlyEGraph[NodeT]](rule: Rule[NodeT, MatchT, EGraphT],
                                                                                         egraph: EGraphT,
                                                                                         cause: Throwable)
    extends RuntimeException(s"Error applying rule ${rule.name} to e-graph", cause)
}
