package fixpoint.eqsat

/**
 * Metadata associated with an e-graph's nodes or classes. This metadata can respond to changes in the e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MetadataT The type of the metadata.
 */
trait Metadata[NodeT, MetadataT] {
  /**
   * Updates the metadata when an e-node is added to the e-graph.
   * @param node The e-node that was added.
   * @param ref The reference to the e-class that was created for the e-node.
   * @param graph The e-graph that the e-node was added to.
   * @return The update metadata.
   */
  def onAdd(node: ENode[NodeT], ref: AppliedRef, graph: EGraph[NodeT]): Metadata[NodeT, MetadataT]

  /**
   * Updates the metadata when e-classes are unioned in the e-graph.
   * @param equivalences The equivalences that were created by the union.
   * @param graph The e-graph that the e-classes were unioned in.
   * @return The update metadata.
   */
  def onUnionMany(equivalences: Set[Set[AppliedRef]], graph: EGraph[NodeT]): Metadata[NodeT, MetadataT]
}
