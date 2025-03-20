package fixpoint.eqsat.metadata

import fixpoint.eqsat.{EClassCall, EGraph, ENode}

/**
 * Metadata associated with an e-graph's nodes or classes. This metadata can respond to changes in the e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MetadataT The type of the metadata.
 */
trait Metadata[NodeT, MetadataT] {
  /**
   * Updates the metadata when an e-node is added to the e-graph.
   * @param node The e-node that was added.
   * @param ref The e-class that was created for the e-node, instantiated with an identity slot mapping.
   * @param after The e-graph that the e-node was added to.
   * @return The update metadata.
   */
  def onAdd(node: ENode[NodeT], ref: EClassCall, after: EGraph[NodeT]): Metadata[NodeT, MetadataT]

  /**
   * Updates the metadata when e-classes are unioned in the e-graph.
   * @param equivalences The equivalences that were created by the union.
   * @param after The e-graph that the e-classes were unioned in.
   * @return The update metadata.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): Metadata[NodeT, MetadataT]
}
