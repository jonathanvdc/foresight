package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, ENode, immutable, readonly}

/**
 * A metadata manager that wraps an underlying immutable metadata instance,
 * updating it in place as the e-graph is modified.
 *
 * @param _metadata The initial immutable metadata instance.
 * @tparam NodeT     The type of the nodes in the e-graph.
 * @tparam MetadataT The internal payload type of the underlying immutable metadata.
 */
private final class UpdatingImmutableMetadata[NodeT, MetadataT](private var _metadata: immutable.Metadata[NodeT, MetadataT])
  extends FreezableMetadata[NodeT, MetadataT] {

  /** The current underlying immutable metadata. */
  def metadata: immutable.Metadata[NodeT, MetadataT] = _metadata

  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)], after: readonly.EGraph[NodeT], parallelize: ParallelMap): Unit = {
    _metadata = _metadata.onAddMany(added, after, parallelize)
  }

  override def onUnionMany(equivalences: Set[Set[EClassCall]], after: readonly.EGraph[NodeT]): Unit = {
    _metadata = _metadata.onUnionMany(equivalences, after)
  }

  override def emptied: Metadata[NodeT] = new UpdatingImmutableMetadata[NodeT, MetadataT](_metadata.emptied)

  override def freeze(): immutable.Metadata[NodeT, MetadataT] = _metadata
}
