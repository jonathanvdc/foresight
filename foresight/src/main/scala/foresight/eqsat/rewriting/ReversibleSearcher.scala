package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.ReadOnlyEGraph

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
  EGraphT <: ReadOnlyEGraph[NodeT]
] extends Searcher[NodeT, MatchT, EGraphT] {

  /**
   * Attempt to reverse this searcher into an [[Applier]] that accepts the same
   * match type it originally produced.
   *
   * @return `Some(applier)` if reversal is possible, `None` otherwise.
   */
  def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]]
}
