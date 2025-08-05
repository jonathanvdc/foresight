package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * An applier that can be reversed into a searcher.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MatchT The type of the match.
 * @tparam EGraphT The type of the e-graph that the applier applies the match to.
 */
trait ReversibleApplier[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]
  extends Applier[NodeT, MatchT, EGraphT] {

  /**
   * Tries to reverse the applier, turning it into a searcher.
   * @return If the applier is reversible, a searcher.
   */
  def tryReverse: Option[Searcher[NodeT, Seq[MatchT], EGraphT]]
}
