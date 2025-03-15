package eqsat

/**
 * An e-class in an e-graph. An e-class is a set of equivalent e-nodes.
 * @param ref The reference to the e-class.
 * @param graph The e-graph that the e-class belongs to.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-class.
 */
final case class EClass[NodeT](ref: EClassRef, graph: ImmutableEGraph[NodeT]) {
  /**
   * Gets all nodes in the e-class.
   * @return All nodes in the e-class.
   */
  def nodes: Set[ENode[NodeT]] = graph.nodes(ref)
}
