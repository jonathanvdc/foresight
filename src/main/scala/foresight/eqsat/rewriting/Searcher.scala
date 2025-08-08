package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike, Slot}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import foresight.eqsat.saturation.EGraphWithRoot

/**
 * Describes how to find things in an e-graph.
 *
 * A `Searcher` is a pure query over an [[EGraph]]: given an e-graph, it returns some
 * output (often a sequence of matches) without mutating the graph. It is the "Search" half of a
 * [[Rule]], with the "Apply" half implemented by an [[Applier]].
 *
 * Implementations are free to perform internal parallelism, but must be side effect free
 * with respect to the e-graph. Determinism of output order is not guaranteed unless explicitly
 * documented by the concrete implementation.
 *
 * # Phased search
 * Complex searches can be expressed as a pipeline of [[SearcherPhase]] instances. Use:
 *  - [[Searcher.apply]] to build a searcher from a single phase;
 *  - [[chain]] to append a phase that consumes the previous output;
 *  - [[product]] to run two independent searchers and pair their results.
 *
 * # Adapters
 * Use [[requireMetadata]] and [[requireRoot]] when the caller passes decorated e-graphs
 * (e.g., in saturation loops that carry extra context) but your search logic needs only
 * the underlying base e-graph.
 *
 * # Parallelism
 * The `parallelize` parameter allows callers to structure/label work and control parallel map
 * behavior. Implementations should call `parallelize.child("label")` to create scoped tasks.
 *
 * @tparam NodeT   Node payload type stored in the e-graph.
 * @tparam OutputT Result type produced by this searcher (e.g., `Seq[PatternMatch[NodeT]]`).
 * @tparam EGraphT The concrete e-graph type, constrained to be both [[EGraphLike]] and [[EGraph]].
 * @example Single-phase pattern search
 * {{{
 * // Suppose `patternPhase` finds Seq[PatternMatch[MyNode]]
 * val patSearch: Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] =
 *   Searcher(patternPhase)
 *
 * val matches = patSearch.search(egraph)
 * }}}
 * @example Pipelining with `chain` (search -> filter -> transform)
 * {{{
 * val searchA: Searcher[MyNode, Seq[MatchA], MyEGraph] = Searcher(phaseA)
 *
 * // PhaseB consumes A's Seq[MatchA] and returns Seq[MatchB]
 * val refined: Searcher[MyNode, Seq[MatchB], MyEGraph] =
 *   searchA.chain(phaseB)
 *
 * val bs: Seq[MatchB] = refined.search(egraph)
 * }}}
 * @example Running two searches together and pairing the results
 * {{{
 * val defs:  Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] = Searcher(defsPhase)
 * val uses:  Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] = Searcher(usesPhase)
 *
 * val both: Searcher[MyNode, (Seq[PatternMatch[MyNode]], Seq[PatternMatch[MyNode]]), MyEGraph] =
 *   defs.product(uses)
 *
 * val (defMs, useMs) = both.search(egraph)
 * }}}
 * @example Pair with an [[Applier]] to build a [[Rule]]
 * {{{
 * val searcher: Searcher[MyNode, Seq[MyMatch], MyEGraph] = ...
 * val applier:  Applier [MyNode, MyMatch, MyEGraph]      = ...
 * val rule = Rule("my-rule", searcher, applier)
 * val updated = rule(egraph) // search + apply
 * }}}
 */
trait Searcher[NodeT, +OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {

  /**
   * Execute this search over the given e-graph.
   *
   * For structured parallel work, use `parallelize.child("phase-name")`.
   *
   * @param egraph      E-graph to search.
   * @param parallelize Parallel mapping/labeling strategy.
   * @return Search output (often a sequence of matches).
   */
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): OutputT

  /**
   * Append a [[SearcherPhase]] that consumes this searcher's output and produces a new output.
   *
   * This is the primary way to build multi-stage searches while keeping each stage testable.
   *
   * @param phase Phase to run after this searcher.
   * @tparam IntermediateT The input type that `phase` expects from this searcher (must match this `OutputT`).
   * @tparam OutputT2      The new output type after applying `phase`.
   * @return A composed searcher that runs `this` then `phase`.
   */
  final def chain[IntermediateT, OutputT2](phase: SearcherPhase[NodeT, OutputT, IntermediateT, OutputT2, EGraphT]): Searcher[NodeT, OutputT2, EGraphT] = {
    new Searcher[NodeT, OutputT2, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): OutputT2 = {
        phase.search(egraph, Searcher.this.search(egraph, parallelize), parallelize)
      }
    }
  }

  /**
   * Run this searcher **and** another searcher independently over the same e-graph and pair the results.
   *
   * Useful when two match sets are later combined (e.g., via cartesian product or keyed joins).
   *
   * @param other The other searcher to execute alongside this one.
   * @tparam OutputT2 The other searcher's output type.
   * @return A searcher that returns `(thisOutput, otherOutput)`.
   */
  final def product[OutputT2](other: Searcher[NodeT, OutputT2, EGraphT]): Searcher[NodeT, (OutputT, OutputT2), EGraphT] = {
    new Searcher[NodeT, (OutputT, OutputT2), EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): (OutputT, OutputT2) = {
        (Searcher.this.search(egraph, parallelize), other.search(egraph, parallelize))
      }
    }
  }

  /**
   * Adapt this searcher so it accepts an [[foresight.eqsat.metadata.EGraphWithMetadata]] without changing the search logic.
   *
   * The adapter simply unwraps `.egraph` and delegates to this searcher.
   *
   * @return A searcher over [[foresight.eqsat.metadata.EGraphWithMetadata]] that forwards to this searcher.
   */
  final def requireMetadata: Searcher[NodeT, OutputT, EGraphWithMetadata[NodeT, EGraphT]] = {
    new Searcher[NodeT, OutputT, EGraphWithMetadata[NodeT, EGraphT]] {
      override def search(egraph: EGraphWithMetadata[NodeT, EGraphT], parallelize: ParallelMap): OutputT = {
        Searcher.this.search(egraph.egraph, parallelize)
      }
    }
  }

  /**
   * Adapt this searcher so it accepts an [[foresight.eqsat.saturation.EGraphWithRoot]] without changing the search logic.
   *
   * The adapter simply unwraps `.egraph` and delegates to this searcher.
   *
   * @return A searcher over [[foresight.eqsat.saturation.EGraphWithRoot]] that forwards to this searcher.
   */
  final def requireRoot: Searcher[NodeT, OutputT, EGraphWithRoot[NodeT, EGraphT]] = {
    new Searcher[NodeT, OutputT, EGraphWithRoot[NodeT, EGraphT]] {
      override def search(egraph: EGraphWithRoot[NodeT, EGraphT], parallelize: ParallelMap): OutputT = {
        Searcher.this.search(egraph.egraph, parallelize)
      }
    }
  }
}

/**
 * Constructors and combinators for [[Searcher]].
 */
object Searcher {

  /**
   * A no-op searcher that returns an empty sequence of matches but is considered reversible.
   *
   * Reversal yields an [[Applier.ignore]] applier, allowing rules to treat the empty searcher
   * as a structural placeholder in reversible pipelines.
   *
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match element type (e.g., [[PatternMatch]]).
   * @tparam EGraphT E-graph type.
   * @return A [[ReversibleSearcher]] that always returns `Seq.empty`.
   */
  def empty[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: Searcher[NodeT, Seq[MatchT], EGraphT] = {
    new ReversibleSearcher[NodeT, MatchT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = Seq.empty

      override def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]] = Some(Applier.ignore)
    }
  }

  /**
   * Build a `Searcher` from a single [[SearcherPhase]].
   *
   * This is often the entry point for pattern-based searches where the phase's input
   * is `Unit` and the output is a sequence of matches.
   *
   * @param phase First/only phase of the searcher.
   * @tparam NodeT   Node payload type.
   * @tparam OutputT Output produced by `phase`.
   * @tparam EGraphT E-graph type.
   * @tparam T1      Intermediate type generated inside `phase` (if any).
   * @return A searcher whose `search` delegates to `phase.search(egraph, (), parallelize)`.
   * @example
   * {{{
   * val base: Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] =
   *   Searcher(patternPhase)
   * }}}
   */
  def apply[NodeT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], T1](phase: SearcherPhase[NodeT, Unit, T1, OutputT, EGraphT]): Searcher[NodeT, OutputT, EGraphT] = {
    new Searcher[NodeT, OutputT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): OutputT =
        phase.search(egraph, (), parallelize)
    }
  }

  /**
   * Map over the elements produced by an inner sequence-producing searcher.
   *
   * The mapping function receives both the element and the same e-graph snapshot the inner
   * searcher ran on (useful for structural queries or lookups).
   *
   * @param searcher Inner searcher producing `Seq[InputT]`.
   * @param f        Mapping `(InputT, EGraphT) => OutputT`.
   * @tparam NodeT   Node payload type.
   * @tparam InputT  Inner element type.
   * @tparam OutputT Mapped element type.
   * @tparam EGraphT E-graph type.
   */
  final case class Map[NodeT, InputT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](searcher: Searcher[NodeT, Seq[InputT], EGraphT],
                                                                                                         f: (InputT, EGraphT) => OutputT) extends Searcher[NodeT, Seq[OutputT], EGraphT] {

    override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[OutputT] = {
      val matches = searcher.search(egraph, parallelize)
      parallelize(matches, (x: InputT) => f(x, egraph)).toSeq
    }
  }

  /**
   * Filter the sequence output of another searcher using a predicate that can inspect the e-graph.
   *
   * If the inner searcher is [[ReversibleSearcher]], reversal is preserved by wrapping the
   * corresponding [[Applier]] with [[Applier.Filter]].
   *
   * @param searcher  Inner searcher producing `Seq[MatchT]`.
   * @param predicate `(MatchT, EGraphT) => Boolean` deciding retention.
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match element type.
   * @tparam EGraphT E-graph type.
   */
  final case class Filter[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](searcher: Searcher[NodeT, Seq[MatchT], EGraphT],
                                                                                                   predicate: (MatchT, EGraphT) => Boolean) extends ReversibleSearcher[NodeT, MatchT, EGraphT] {

    override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = {
      val matches = searcher.search(egraph, parallelize)
      parallelize(matches, (x: MatchT) => predicate(x, egraph))
        .zip(matches)
        .collect { case (true, m) => m }
        .toSeq
    }

    override def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]] = {
      searcher match {
        case r: ReversibleSearcher[NodeT, MatchT, EGraphT] => r.tryReverse.map(Applier.Filter(_, predicate))
        case _ => None
      }
    }
  }

  /**
   * Flatten a searcher that yields nested sequences, e.g., from a `flatMap`-like phase.
   *
   * @param searcher Inner searcher producing `Seq[Iterable[MatchT]]`.
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match element type.
   * @tparam EGraphT E-graph type.
   */
  final case class Flatten[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](searcher: Searcher[NodeT, Seq[Iterable[MatchT]], EGraphT]) extends Searcher[NodeT, Seq[MatchT], EGraphT] {
    override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] =
      searcher.search(egraph, parallelize).flatten
  }

  // ------------ Extension syntax for sequence-producing searchers ------------

  /**
   * Enrichment for `Searcher[Seq[MatchT]]` providing `filter`/`map`/`flatMap` combinators.
   */
  implicit class SearcherOfSeqOps[
    NodeT,
    MatchT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](private val searcher: Searcher[NodeT, Seq[MatchT], EGraphT]) extends AnyVal {

    /**
     * Keep only elements satisfying `f`.
     *
     * @param f Predicate receiving the element and the e-graph.
     * @return A filtered searcher.
     */
    def filter(f: (MatchT, EGraphT) => Boolean): Searcher[NodeT, Seq[MatchT], EGraphT] =
      Filter(searcher, f)

    /**
     * Transform each element using `f`.
     *
     * @param f Mapping function receiving the element and the e-graph.
     * @tparam OutputT Result element type.
     * @return A mapped searcher.
     */
    def map[OutputT](f: (MatchT, EGraphT) => OutputT): Searcher[NodeT, Seq[OutputT], EGraphT] =
      Map(searcher, f)

    /**
     * Transform each element to a collection and flatten the results.
     *
     * @param f Flat-mapping function receiving the element and the e-graph.
     * @tparam OutputT Result element type.
     * @return A flattened searcher.
     */
    def flatMap[OutputT](f: (MatchT, EGraphT) => Iterable[OutputT]): Searcher[NodeT, Seq[OutputT], EGraphT] =
      map(f).flatten
  }

  /**
   * Enrichment for `Searcher[Seq[Iterable[MatchT]]]` providing `flatten`.
   */
  implicit class SearcherOfSeqOfTraversableOps[
    NodeT,
    MatchT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](private val searcher: Searcher[NodeT, Seq[Iterable[MatchT]], EGraphT]) extends AnyVal {

    /**
     * Creates a new searcher that flattens a sequence of matches.
     * @return A flattened searcher.
     */
    def flatten: Searcher[NodeT, Seq[MatchT], EGraphT] = Flatten(searcher)
  }

  /**
   * Enrichment for `Searcher[Seq[PatternMatch]]` with pattern-specific helpers.
   */
  implicit class SearcherOfPatternMatch[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](private val searcher: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT]) extends AnyVal {

    /**
     * Require that the expression bound to `expr` be independent of the given slots.
     *
     * Independence means the bound expression does not contain any of `slots`.
     * Matches violating this condition are filtered out.
     *
     * @param expr  Pattern variable whose binding is checked.
     * @param slots Slots the binding must not reference.
     * @return A filtered searcher that enforces independence.
     * @example Forbid captures of loop index slots
     * {{{
     * val s: Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] = ...
     * val safe = s.requireIndependent(loopVar, iSlot, jSlot)
     * }}}
     */
    def requireIndependent(expr: Pattern.Var[NodeT], slots: Slot*): Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] = {
      searcher.filter((m, _) => m.isIndependent(expr, slots.toSet))
    }
  }

  /**
   * Enrichment for a searcher that returns a pair of pattern-match lists,
   * providing `merge` to combine them via cartesian product and `PatternMatch.merge`.
   */
  implicit class SearcherOfPatternMatchProductOps[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](private val searcher: Searcher[NodeT, (Seq[PatternMatch[NodeT]], Seq[PatternMatch[NodeT]]), EGraphT]) extends AnyVal {

    /**
     * Merge the two match sets pairwise over their cartesian product using [[PatternMatch.merge]].
     *
     * Note: this may be expensive if both sides are large; consider early filtering.
     *
     * @return A searcher producing the merged matches.
     */
    def merge: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] = {
      new Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[PatternMatch[NodeT]] = {
          val (matches1, matches2) = searcher.search(egraph, parallelize)
          for {
            m1 <- matches1
            m2 <- matches2
          } yield m1.merge(m2)
        }
      }
    }
  }
}
