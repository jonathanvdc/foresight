package foresight.eqsat.metadata

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, EGraph, EGraphLike, ENode, ShapeCall}

/**
 * An e-graph with associated metadata. Metadata is kept in sync with the e-graph upon every change.
 *
 * @param egraph The e-graph.
 * @param metadata The metadata associated with the e-graph.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-graph.
 * @tparam Repr The type of the underlying e-graph.
 */
final case class EGraphWithMetadata[NodeT, +Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]] private(egraph: Repr,
                                                                                                        private val metadata: Map[String, Metadata[NodeT, _]])
  extends EGraphLike[NodeT, EGraphWithMetadata[NodeT, Repr]] with EGraph[NodeT] {

  /**
   * Creates a new e-graph with the same metadata but an underlying e-graph of a different type. The underlying e-graph
   * must contain the same nodes and classes as the current e-graph.
   * @param newRepr The new underlying e-graph.
   * @return A new e-graph with the same metadata.
   */
  def migrateTo[Repr2 <: EGraphLike[NodeT, Repr2] with EGraph[NodeT]](newRepr: Repr2): EGraphWithMetadata[NodeT, Repr2] = {
    EGraphWithMetadata(newRepr, metadata)
  }

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
   * Adds an analysis to the e-graph.
   * @param analysis The analysis to add.
   * @tparam A The type of the analysis result.
   * @return The e-graph with the added analysis.
   */
  def addAnalysis[A](analysis: Analysis[NodeT, A]): EGraphWithMetadata[NodeT, Repr] = {
    addMetadata(analysis.name, analysis(egraph))
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

  override def tryCanonicalize(ref: EClassRef): Option[EClassCall] = egraph.tryCanonicalize(ref)
  override def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(call: EClassCall): Set[ENode[NodeT]] = egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[NodeT]] = egraph.users(ref)
  override def find(node: ENode[NodeT]): Option[EClassCall] = egraph.find(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = egraph.areSame(first, second)

  override def tryAddMany(nodes: Seq[ENode[NodeT]],
                          parallelize: ParallelMap): (Seq[AddNodeResult], EGraphWithMetadata[NodeT, Repr]) = {
    val (results, newEgraph) = egraph.tryAddMany(nodes, parallelize)
    val newNodes = nodes.zip(results).collect {
      case (node, AddNodeResult.Added(call)) =>
        (node, call)
    }

    val p = parallelize.child("metadata for new nodes")
    val newMetadata = p[(String, Metadata[NodeT, _]), (String, Metadata[NodeT, _])](metadata, {
      case (key, metadata) => key -> metadata.onAddMany(newNodes, newEgraph, p.child(s"metadata for new nodes - $key"))
    }).toMap
    (results, EGraphWithMetadata(newEgraph, newMetadata))
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)],
                         parallelize: ParallelMap): (Set[Set[EClassCall]], EGraphWithMetadata[NodeT, Repr]) = {
    val (equivalences, newEgraph) = egraph.unionMany(pairs, parallelize)
    val p = parallelize.child("metadata unification")
    val newEGraph = EGraphWithMetadata(
      newEgraph,
      p[(String, Metadata[NodeT, _]), (String, Metadata[NodeT, _])](metadata, {
        case (key, metadata) => key -> p.child(s"metadata unification - $key").run(metadata.onUnionMany(equivalences, newEgraph))
      }).toMap)
    (equivalences, newEGraph)
  }

  override def emptied: EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph.emptied, metadata.mapValues(_.emptied).view.force)
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
