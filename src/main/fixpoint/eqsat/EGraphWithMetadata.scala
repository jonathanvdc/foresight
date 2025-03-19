package fixpoint.eqsat

/**
 * An e-graph with associated metadata. Metadata is kept in sync with the e-graph upon every change.
 * @param egraph The e-graph.
 * @param metadata The metadata associated with the e-graph.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 * @tparam Repr The type of the underlying e-graph.
 */
final case class EGraphWithMetadata[NodeT, +Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]] private(egraph: Repr,
                                                                                                        private val metadata: Map[String, Metadata[NodeT, _]])
  extends EGraphLike[NodeT, EGraphWithMetadata[NodeT, Repr]] with EGraph[NodeT] {

  /**
   * Registers metadata with the e-graph.
   * @param name The name of the metadata.
   * @param metadata The metadata to add.
   * @tparam MetadataT The type of the metadata.
   * @return The e-graph with the added metadata.
   */
  def addMetadata[MetadataT](name: String, metadata: Metadata[NodeT, MetadataT]): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, this.metadata + (name -> metadata))
  }

  /**
   * Unregisters metadata from the e-graph.
   * @param name The name of the metadata.
   * @return The e-graph with the removed metadata.
   */
  def removeMetadata(name: String): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, metadata - name)
  }

  /**
   * Gets metadata from the e-graph.
   * @param name The name of the metadata.
   * @tparam MetadataManagerT The type of the metadata.
   * @return The metadata.
   */
  def getMetadata[MetadataManagerT <: Metadata[NodeT, _]](name: String): MetadataManagerT = {
    metadata(name).asInstanceOf[MetadataManagerT]
  }

  override def tryCanonicalize(ref: EClassRef): Option[AppliedRef] = egraph.tryCanonicalize(ref)
  override def canonicalize(node: ENode[NodeT]): AppliedENode[NodeT] = egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(app: AppliedRef): Set[ENode[NodeT]] = egraph.nodes(app)
  override def users(ref: EClassRef): Set[ENode[NodeT]] = egraph.users(ref)
  override def find(node: ENode[NodeT]): Option[AppliedRef] = egraph.find(node)

  override def add(node: ENode[NodeT]): (AppliedRef, EGraphWithMetadata[NodeT, Repr]) = {
    val (ref, newEgraph) = egraph.add(node)
    (ref, EGraphWithMetadata(newEgraph, metadata.mapValues(_.onAdd(node, ref, newEgraph))))
  }

  override def unionMany(pairs: Seq[(AppliedRef, AppliedRef)]): (Set[Set[AppliedRef]], EGraphWithMetadata[NodeT, Repr]) = {
    val (equivalences, newEgraph) = egraph.unionMany(pairs)
    (equivalences, EGraphWithMetadata(newEgraph, metadata.mapValues(_.onUnionMany(equivalences, newEgraph))))
  }
}

/**
 * Companion object for e-graphs with metadata.
 */
object EGraphWithMetadata {
  /**
   * Creates an e-graph that manages metadata.
   * @param egraph The e-graph.
   * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
   * @tparam Repr The type of the underlying e-graph.
   * @return The e-graph with metadata.
   */
  def apply[NodeT, Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, Map.empty)
  }
}
