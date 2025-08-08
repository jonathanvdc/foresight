package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A [[Searcher]] whose results can be **reversed** into an [[Applier]].
 *
 * Reversal turns a search procedure back into an operation that can act upon the matches it finds.
 * This enables round-tripping between:
 *
 * {{{
 * Searcher  (EGraph → Matches)   →   Applier (Matches → EGraph edits)
 * }}}
 *
 * # Contract
 *   - `tryReverse` returns `Some(applier)` if there exists a sound mapping from `MatchT`
 *     back into edit commands for the e-graph.
 *   - Return `None` when the searcher has lost information required for reversal
 *     (e.g., matches were aggregated destructively).
 *
 * @example
 * {{{
 * val forward: ReversibleSearcher[MyNode, MyMatch, MyEGraph] = ...
 * val reverseOpt = forward.tryReverse
 *
 * reverseOpt.foreach { applier =>
 *   val cmds = matchesFromElsewhere.map(applier.apply(_, egraph))
 *   // cmds now represent the inverse or replay of the original search results
 * }
 * }}}
 * @tparam NodeT   Node payload type.
 * @tparam MatchT  The match type emitted by this searcher.
 * @tparam EGraphT Concrete e-graph type (must mix in [[EGraphLike]] and [[EGraph]]).
 */
trait ReversibleSearcher[
  NodeT,
  MatchT,
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
] extends Searcher[NodeT, Seq[MatchT], EGraphT] {

  /**
   * Attempt to reverse this searcher into an [[Applier]] that accepts the same
   * match type it originally produced.
   *
   * @return `Some(applier)` if reversal is possible, `None` otherwise.
   */
  def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]]
}

/**
 * ### Factory and utilities for [[ReversibleSearcher]]
 */
object ReversibleSearcher {

  /**
   * Create a single-phase reversible searcher from a [[ReversibleSearcherPhase]].
   *
   * This is a convenience for the common case where the searcher is built from
   * exactly one reversible phase whose input type is `Unit` (i.e., it does not
   * depend on prior phase results).
   *
   * @example
   * {{{
   * val phase: ReversibleSearcherPhase[MyNode, Unit, _, MyMatch, MyEGraph] = ...
   * val searcher = ReversibleSearcher(phase)
   *
   * val matches = searcher.search(egraph)
   * val reverse  = searcher.tryReverse
   * }}}
   * @param phase The reversible searcher phase to wrap.
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match type produced by the phase.
   * @tparam EGraphT Concrete e-graph type (must mix in [[EGraphLike]] and [[EGraph]]).
   */
  def apply[
    NodeT,
    MatchT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](phase: ReversibleSearcherPhase[NodeT, Unit, _, MatchT, EGraphT]): ReversibleSearcher[NodeT, MatchT, EGraphT] = {

    new ReversibleSearcher[NodeT, MatchT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] =
        phase.search(egraph, (), parallelize)

      override def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]] =
        phase.tryReverse(Applier.ignore)
    }
  }
}
