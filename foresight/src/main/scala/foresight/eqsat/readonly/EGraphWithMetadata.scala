package foresight.eqsat.readonly

import foresight.eqsat.metadata.Metadata

/**
 * An e-graph decorator that supports metadata management.
 *
 * This trait extends [[EGraphDecorator]] to provide access to registered metadata managers.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam Repr  The concrete type of the underlying e-graph representation.
 */
trait EGraphWithMetadata[NodeT, +Repr <: ReadOnlyEGraph[NodeT]] extends EGraphDecorator[NodeT, Repr] {
  /**
   * Retrieve a registered metadata manager by name.
   *
   * This performs a downcast; callers must provide an accurate type argument.
   *
   * @param name The registration name.
   * @tparam MetadataManagerT The expected metadata manager type.
   * @return The registered manager cast to `MetadataManagerT`.
   * @throws NoSuchElementException if no registration exists under `name`.
   * @throws ClassCastException if the stored manager has a different type.
   */
  def getMetadata[MetadataManagerT <: Metadata[NodeT, _]](name: String): MetadataManagerT
}
