package foresight.eqsat.rewriting

import foresight.eqsat.commands.{CommandSchedule, CommandScheduleBuilder}
import foresight.eqsat.readonly.EGraph

/**
 * Describes how to **turn a match into edits** on an e-graph, without mutating it directly.
 *
 * An [[Applier]] does not update the graph directly. Instead, it builds a [[CommandSchedule]] describing the edits (e.g.,
 * insertions, unions) that, when executed by the command engine, yields a new e-graph.
 *
 * Typically, an [[Applier]] is paired with a [[Searcher]] inside a [[Rule]]:
 *  - the searcher finds `MatchT` values,
 *  - the applier maps each `MatchT` to a `Command[NodeT]`,
 *  - those per-match commands are aggregated/optimized by the rule into a single operation.
 *
 * ## Contract
 * - **Purity**: [[apply]] does not mutate `egraph`. It only describes work via a [[CommandSchedule]].
 * - **Thread-safety**: Appliers may be invoked in parallel across distinct matches of the same snapshot.
 * They must be safe for concurrent use and avoid shared mutable state.
 * - **Idempotence** (recommended): When feasible, produce commands that tolerate duplicates (e.g., union
 * of already-unified classes is a no-op).
 *
 * @tparam NodeT   Node payload type stored in the e-graph.
 * @tparam MatchT  The match type produced by a [[Searcher]] and consumed here.
 * @tparam EGraphT Concrete e-graph type (must be both [[EGraphLike]] and [[EGraph]]).
 */
trait Applier[NodeT, -MatchT, -EGraphT <: EGraph[NodeT]] {

  /**
   * Build a command that applies the effects implied by `m` within `egraph`.
   *
   * The returned command is typically combined with other commands (from other matches) by [[Rule]]
   * into a single optimized batch. It must be safe to execute even if other matches also affect
   * overlapping parts of the graph.
   *
   * @param m      A single match previously found in `egraph`.
   * @param egraph Immutable e-graph snapshot the match was derived from.
   * @return A command representing the intended edits for this match.
   */
  def apply(m: MatchT, egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit
}

/**
 * Constructors, combinators, and utilities for [[Applier]].
 */
object Applier {

  /**
   * A no-op applier: ignore the match and emit an empty command.
   *
   * Useful as a placeholder in reversible pipelines or for rules that are search-only.
   * Reversal returns [[Searcher.empty]] so the pair remains structurally reversible.
   */
  def ignore[NodeT, MatchT, EGraphT <: EGraph[NodeT]]: Applier[NodeT, MatchT, EGraphT] =
    new ReversibleApplier[NodeT, MatchT, EGraphT] {
      override def apply(m: MatchT, egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit = {

      }

      override def tryReverse: Option[Searcher[NodeT, MatchT, EGraphT]] = Some(Searcher.empty)
    }

  /**
   * Conditionally apply: run `applier` only when `filter(match, egraph)` is true; otherwise emit no-op.
   *
   * Preserves reversibility: if the inner applier is [[ReversibleApplier]], reversal yields a
   * [[Searcher.Filter]] with the same predicate.
   *
   * @param applier Inner applier.
   * @param filter  Predicate deciding whether to apply this match.
   * @example Only mutate when the match is "fresh"
   * {{{
   * val guarded = Applier.Filter(base, (m: MyMatch, g: MyEGraph) => !m.isStale(g))
   * }}}
   */
  final case class Filter[
    NodeT, MatchT,
    EGraphT <: EGraph[NodeT]
  ](applier: Applier[NodeT, MatchT, EGraphT],
    filter: (MatchT, EGraphT) => Boolean)
    extends ReversibleApplier[NodeT, MatchT, EGraphT] {

    override def apply(m: MatchT, egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit = {
      if (filter(m, egraph))
        applier.apply(m, egraph, builder)
    }

    override def tryReverse: Option[Searcher[NodeT, MatchT, EGraphT]] = applier match {
      case r: ReversibleApplier[NodeT, MatchT, EGraphT] => r.tryReverse.map(_.filter(filter))
      case _ => None
    }
  }

  /**
   * Pre-map the input match type: transforms `MatchT1` to `MatchT2` and then applies `applier`.
   *
   * Think of this as an input adapter for an existing applier. Handy when you’ve refined or
   * re-shaped matches during search and want to reuse an applier that expects a different match shape.
   *
   * @param applier Downstream applier that consumes `MatchT2`.
   * @param f       Mapping from outer match (`MatchT1`) to inner match (`MatchT2`), with access to `egraph`.
   * @example Adapt a generic "replace at class" applier to a "replace at node" match
   * {{{
   * val nodeToClass: (NodeMatch, MyEGraph) => ClassMatch = ...
   * val adapted: Applier[MyNode, NodeMatch, MyEGraph] =
   *   Applier.Map(classApplier, nodeToClass)
   * }}}
   */
  final case class Map[
    NodeT, MatchT1, MatchT2,
    EGraphT <: EGraph[NodeT]
  ](applier: Applier[NodeT, MatchT2, EGraphT],
    f: (MatchT1, EGraphT) => MatchT2)
    extends Applier[NodeT, MatchT1, EGraphT] {

    override def apply(m: MatchT1, egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit = {
      applier.apply(f(m, egraph), egraph, builder)
    }
  }

  /**
   * Pre-flatMap the input match type: expand one match into **many** then apply and batch them.
   *
   * Useful when a single outer match implies multiple inner edits (e.g., expand an equivalence to
   * a set of updates). The resulting [[CommandQueue]] preserves the order of the produced
   * inner commands (though the engine may later optimize/deduplicate).
   *
   * @param applier Downstream applier that consumes `MatchT2`.
   * @param f       `(MatchT1, EGraphT) => Iterable[MatchT2]` expansion.
   */
  final case class FlatMap[
    NodeT, MatchT1, MatchT2,
    EGraphT <: EGraph[NodeT]
  ](applier: Applier[NodeT, MatchT2, EGraphT],
    f: (MatchT1, EGraphT) => Iterable[MatchT2])
    extends Applier[NodeT, MatchT1, EGraphT] {

    override def apply(m: MatchT1, egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit =
      f(m, egraph).foreach(applier.apply(_, egraph, builder))
  }

  // ---------------------- Syntax sugar for Applier combinators ----------------------

  /**
   * Enrichment methods for composing appliers.
   *
   * @example Guard, then adapt, then fan out
   * {{{
   * val pipelined: Applier[MyNode, Outer, MyEGraph] =
   *   baseApplier
   *     .filter((m: Outer, g) => isWorthIt(m, g))
   *     .map   ((m: Outer, g) => toInner(m, g))
   *     .flatMap((m: Outer, g) => explode(m, g)) // Outer => Iterable[Inner2]
   * }}}
   */
  implicit class ApplierOps[
    NodeT, MatchT,
    EGraphT <: EGraph[NodeT]
  ](private val applier: Applier[NodeT, MatchT, EGraphT]) extends AnyVal {

    /**
     * Conditionally apply this applier; otherwise emit an empty command.
     *
     * @param filter `(match, egraph) => Boolean` predicate.
     * @return An applier that enforces the predicate.
     */
    def filter(filter: (MatchT, EGraphT) => Boolean): Applier[NodeT, MatchT, EGraphT] =
      Filter(applier, filter)

    /**
     * Pre-map the input match type for this applier.
     *
     * Note the direction: you provide a function from **new** input type `MatchT2` to this
     * applier's **existing** input type `MatchT`.
     *
     * @param f `(MatchT2, EGraphT) => MatchT` adapter to the inner applier.
     * @tparam MatchT2 New outer match type.
     * @return An applier that accepts `MatchT2`.
     */
    def map[MatchT2](f: (MatchT2, EGraphT) => MatchT): Applier[NodeT, MatchT2, EGraphT] =
      Map(applier, f)

    /**
     * Pre-flatMap the input match type: expand a single outer match to many inner matches.
     *
     * @param f `(MatchT2, EGraphT) => Iterable[MatchT]` expansion feeding the inner applier.
     * @tparam MatchT2 New outer match type.
     * @return An applier that accepts `MatchT2` and batches inner commands.
     */
    def flatMap[MatchT2](f: (MatchT2, EGraphT) => Iterable[MatchT]): Applier[NodeT, MatchT2, EGraphT] =
      FlatMap(applier, f)
  }
}
