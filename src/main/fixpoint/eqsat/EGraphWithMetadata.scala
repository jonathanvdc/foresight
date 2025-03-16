package fixpoint.eqsat

/**
 * An e-graph with associated metadata. Metadata is kept in sync with the e-graph upon every change.
 * @param egraph The e-graph.
 * @param metadata The metadata associated with the e-graph.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 */
final case class EGraphWithMetadata[NodeT](egraph: EGraph[NodeT], metadata: Map[String, Metadata[NodeT, _]])
  extends EGraphLike[NodeT, EGraphWithMetadata[NodeT]] with EGraph[NodeT] {

  /**
   * Registers metadata with the e-graph.
   * @param name The name of the metadata.
   * @param metadata The metadata to add.
   * @tparam MetadataT The type of the metadata.
   * @return The e-graph with the added metadata.
   */
  def addMetadata[MetadataT](name: String, metadata: Metadata[NodeT, MetadataT]): EGraphWithMetadata[NodeT] = {
    EGraphWithMetadata(egraph, this.metadata + (name -> metadata))
  }

  /**
   * Unregisters metadata from the e-graph.
   * @param name The name of the metadata.
   * @return The e-graph with the removed metadata.
   */
  def removeMetadata(name: String): EGraphWithMetadata[NodeT] = {
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

  override def tryCanonicalize(ref: EClassRef): Option[EClassRef] = egraph.tryCanonicalize(ref)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(ref: EClassRef): Set[ENode[NodeT]] = egraph.nodes(ref)
  override def parents(ref: EClassRef): Set[EClassRef] = egraph.parents(ref)
  override def find(node: ENode[NodeT]): Option[EClassRef] = egraph.find(node)

  override def add(node: ENode[NodeT]): (EClassRef, EGraphWithMetadata[NodeT]) = {
    val (ref, newEgraph) = egraph.add(node)
    (ref, EGraphWithMetadata(newEgraph, metadata.mapValues(_.onAdd(node, ref, newEgraph))))
  }

  override def unionMany(pairs: Seq[(EClassRef, EClassRef)]): (Set[Set[EClassRef]], EGraphWithMetadata[NodeT]) = {
    val (equivalences, newEgraph) = egraph.unionMany(pairs)
    (equivalences, EGraphWithMetadata(newEgraph, metadata.mapValues(_.onUnionMany(equivalences, newEgraph))))
  }
}
