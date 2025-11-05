package foresight.eqsat.mutable

import foresight.eqsat.immutable

/**
 * A metadata manager that can be frozen into an immutable snapshot of its current state.
 *
 * @tparam NodeT     The type of the nodes in the e-graph.
 * @tparam MetadataT The internal payload type of the underlying immutable metadata.
 */
trait FreezableMetadata[NodeT, MetadataT] extends Metadata[NodeT] {
  /** Freezes the metadata, returning an immutable snapshot of its current state. */
  def freeze(): immutable.Metadata[NodeT, MetadataT]
}

/**
 * A companion object for the [[FreezableMetadata]] trait that provides a way to create a
 * [[FreezableMetadata]] from an existing immutable metadata manager.
 */
object FreezableMetadata {
  /**
   * Wraps an existing immutable metadata manager in a mutable interface that supports in-place updates.
   *
   * @param metadata The immutable metadata manager to wrap.
   * @tparam NodeT     The type of the nodes in the e-graph.
   * @tparam MetadataT The internal payload type of the underlying immutable metadata.
   * @return A [[FreezableMetadata]] that wraps the provided immutable metadata manager.
   */
  def apply[NodeT, MetadataT](metadata: immutable.Metadata[NodeT, MetadataT]): FreezableMetadata[NodeT, MetadataT] = {
    new UpdatingImmutableMetadata(metadata)
  }
}
