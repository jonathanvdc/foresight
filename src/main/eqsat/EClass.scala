package eqsat

/**
 * An e-class in an e-graph. An e-class is a set of equivalent e-nodes.
 * @param ref The reference to the e-class.
 * @param graph The e-graph that the e-class belongs to.
 * @tparam ExprT The type of the expression that the e-class represents.
 */
final case class EClass[ExprT](ref: EClassRef, graph: ImmutableEGraph[ExprT]) {
  /**
   * Gets all nodes in the e-class.
   * @return All nodes in the e-class.
   */
  def nodes: Set[ENode[ExprT]] = graph.nodes(ref)
}
