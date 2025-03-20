package fixpoint.eqsat.extraction

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, Tree}

/**
 * An extractor that extract expression trees from e-class calls.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait Extractor[NodeT, Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]] {
  /**
   * Extracts an expression tree from an e-class call.
   * @param call The e-class call to extract a tree from.
   * @param egraph The e-graph containing the e-class call.
   * @return The extracted tree.
   */
  def apply(call: EClassCall, egraph: Repr): Tree[NodeT]
}
