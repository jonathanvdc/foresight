package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EGraph, EGraphLike}

/**
 * A match that can be updated to be portable to e-graphs derived from the original e-graph in which the match was
 * found.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam This The type of the match.
 */
trait PortableMatch[NodeT, This <: PortableMatch[NodeT, This]] {
  /**
   * Updates the match to an e-graph derived from the original e-graph in which the match was found.
   * @param egraph The e-graph to update the match to be portable to.
   * @return The updated match.
   */
  def port(egraph: EGraph[NodeT]): This
}
