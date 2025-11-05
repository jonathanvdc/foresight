package foresight.eqsat.mutable

import foresight.eqsat.metadata.Analysis
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, ENode, readonly}

import scala.collection.mutable

/**
 * A mutable e-graph that supports adding e-nodes and merging e-classes, along with
 * metadata managers that track auxiliary information about the e-graph's contents.
 *
 * Metadata managers are registered under string names, and are updated automatically
 * when the e-graph is modified. See [[Metadata]] for details on implementing custom
 * metadata managers.
 *
 * @param egraph   The underlying e-graph representation.
 * @param metadata Initial metadata managers, keyed by name.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam Repr  The concrete type of the underlying e-graph representation.
 */
final class EGraphWithMetadata[
  NodeT,
  +Repr <: EGraph[NodeT]
](
  override val egraph: Repr,
  private val metadata: mutable.Map[String, Metadata[NodeT]]
 )
  extends readonly.EGraphWithMetadata[NodeT, Repr] with EGraph[NodeT] {

  /**
   * Retrieve a registered metadata manager by name.
   *
   * This performs a downcast; callers must provide an accurate type argument.
   *
   * @param name The registration name.
   * @tparam MetadataManagerT The expected metadata manager type.
   * @return The registered manager cast to `MetadataManagerT`.
   * @throws NoSuchElementException if no registration exists under `name`.
   * @throws ClassCastException     if the stored manager has a different type.
   */
  override def getMetadata[MetadataManagerT](name: String): MetadataManagerT = {
    metadata(name).asInstanceOf[MetadataManagerT]
  }

  /**
   * Register (or replace) a metadata manager under a name.
   *
   * If the name already exists it will be overwritten.
   *
   * @param name     Unique name for this metadata manager.
   * @param metadata The manager to register.
   * @tparam MetadataT The manager's internal payload type.
   */
  def addMetadata[MetadataT](name: String, metadata: Metadata[NodeT]): Unit = {
    this.metadata(name) = metadata
  }

  /**
   * Build and register an [[Analysis]] result in one step.
   *
   * Invokes the analysis on the current `egraph` and stores the resulting metadata under `analysis.name`.
   *
   * @param analysis The analysis to execute and register.
   * @tparam A Analysis result payload type.
   */
  def addAnalysis[A](analysis: Analysis[NodeT, A]): Unit = {
    addMetadata(analysis.name, analysis(egraph))
  }

  /**
   * Unregister a metadata manager by name. No other state is modified.
   *
   * @param name The registration name to remove.
   * @return `true` if a registration was removed, `false` if none existed.
   */
  def removeMetadata(name: String): Boolean = {
    metadata.remove(name).isDefined
  }

  override def tryAddMany(nodes: Seq[ENode[NodeT]], parallelize: ParallelMap): Seq[AddNodeResult] = {
    val results = egraph.tryAddMany(nodes, parallelize)
    val newNodes = nodes.zip(results).collect {
      case (node, AddNodeResult.Added(call)) =>
        (node, call)
    }

    val p = parallelize.child("metadata for new nodes")
    p[(String, Metadata[NodeT]), Unit](metadata, {
      case (key, metadata) => metadata.onAddMany(newNodes, egraph, p.child(s"metadata for new nodes - $key"))
    })
    results
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): Set[Set[EClassCall]] = {
    val equivalences = egraph.unionMany(pairs, parallelize)

    val p = parallelize.child("metadata unification")
    p[(String, Metadata[NodeT]), Unit](metadata, {
      case (key, metadata) => p.child(s"metadata unification - $key").run(metadata.onUnionMany(equivalences, egraph))
    })
    equivalences
  }

  override def emptied: this.type = new EGraphWithMetadata(
    egraph.emptied,
    mutable.Map[String, Metadata[NodeT]](metadata.toSeq.map { case (k, v) => k -> v.emptied }:_*)
  ).asInstanceOf[this.type]
}

/**
 * A companion object for the [[EGraphWithMetadata]] class.
 */
object EGraphWithMetadata {
  /**
   * Creates a new mutable e-graph with metadata support.
   *
   * @param egraph   The underlying e-graph representation.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam Repr  The concrete type of the underlying e-graph representation.
   * @return A new mutable e-graph with metadata support.
   */
  def apply[
    NodeT,
    Repr <: EGraph[NodeT]
  ](
    egraph: Repr
   ): EGraphWithMetadata[NodeT, Repr] = {
    new EGraphWithMetadata(egraph, mutable.Map.empty[String, Metadata[NodeT]])
  }
}
