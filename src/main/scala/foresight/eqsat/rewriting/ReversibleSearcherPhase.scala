package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A phase of a searcher that can be reversed into an applier.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam InputT The type of the input that the searcher consumes from the previous phase.
 * @tparam IntermediateT The type of the output that the searcher produces for a single e-class.
 * @tparam MatchT The type of the output that the searcher produces.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait ReversibleSearcherPhase[NodeT, InputT, IntermediateT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]
  extends SearcherPhase[NodeT, InputT, IntermediateT, Seq[MatchT], EGraphT] {

  /**
   * Tries to reverse the searcher phase, turning it into an applier.
   * @param nextPhase The next phase in the pipeline, derived by reversing the previous searcher phase.
   * @return If the searcher phase is reversible, an applier phase.
   */
  def tryReverse(nextPhase: Applier[NodeT, InputT, EGraphT]): Option[Applier[NodeT, MatchT, EGraphT]]
}
