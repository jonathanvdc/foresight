package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike}

/**
 * A phase of a searcher that searches for matches in an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam InputT The type of the input that the searcher consumes from the previous phase.
 * @tparam IntermediateT The type of the output that the searcher produces for a single e-class.
 * @tparam OutputT The type of the output that the searcher produces for the entire e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait SearcherPhase[NodeT, InputT, IntermediateT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Searches for matches in an e-class.
   * @param call The e-class call to search for matches of.
   * @param egraph The e-graph to search in.
   * @param input An input resulting from a previous searcher phase.
   * @return The matches found in the e-graph.
   */
  def search(call: EClassCall, egraph: EGraphT, input: InputT): IntermediateT

  /**
   * Aggregates the matches found in the e-graph.
   * @param matches The matches found in the e-graph.
   * @return The output of the searcher phase.
   */
  def aggregate(matches: Map[EClassRef, IntermediateT]): OutputT

  /**
   * Erases the input and output types of the searcher phase.
   * @return The searcher phase with the input and output types erased.
   */
  private[rewriting] final def erase: SearcherPhase[NodeT, Any, Any, Any, EGraphT] = {
    new SearcherPhase[NodeT, Any, Any, Any, EGraphT] {
      override def search(call: EClassCall, egraph: EGraphT, input: Any): Any =
        SearcherPhase.this.search(call, egraph, input.asInstanceOf[InputT])

      override def aggregate(matches: Map[EClassRef, Any]): Any =
        SearcherPhase.this.aggregate(matches.asInstanceOf[Map[EClassRef, IntermediateT]]).asInstanceOf[Any]
    }
  }
}
