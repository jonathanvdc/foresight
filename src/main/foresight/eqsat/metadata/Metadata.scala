package foresight.eqsat.metadata

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, ENode}

/**
 * Metadata associated with an e-graph's nodes or classes. This metadata can respond to changes in the e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MetadataT The type of the metadata.
 */
trait Metadata[NodeT, MetadataT] {
  /**
   * Updates the metadata when an e-node is added to the e-graph.
   * @param added The set of e-nodes that were added to the e-graph, along with their corresponding e-class calls.
   * @param after The e-graph that the e-node was added to.
   * @param parallelize The parallelization strategy to use.
   * @return The updated metadata.
   */
  def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                after: EGraph[NodeT],
                parallelize: ParallelMap): Metadata[NodeT, MetadataT]

  /**
   * Updates the metadata when e-classes are unioned in the e-graph.
   * @param equivalences The equivalences that were created by the union.
   * @param after The e-graph that the e-classes were unioned in.
   * @return The update metadata.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): Metadata[NodeT, MetadataT]
}
