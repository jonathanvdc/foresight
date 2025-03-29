package fixpoint.eqsat.extraction

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Tree}

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

  /**
   * Extracts an expression tree from a mixed tree.
   * @param tree The mixed tree to extract a tree from.
   * @param egraph The e-graph containing the mixed tree.
   * @return The extracted tree.
   */
  final def apply(tree: MixedTree[NodeT, EClassCall], egraph: Repr): Tree[NodeT] = {
    val (call, newGraph) = egraph.add(tree)
    apply(call, newGraph)
  }
}
