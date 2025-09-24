package foresight.eqsat.metadata

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat._
import foresight.util.collections.StrictMapOps.toStrictMapOps

/**
 * Wrapper that couples an [[EGraph]] with a set of registered [[Metadata]] managers
 * and keeps them *in sync* on every change to the underlying e-graph.
 *
 * The wrapper is functional: all mutating operations return a new
 * [[EGraphWithMetadata]] and leave the receiver usable.
 *
 * ## Responsibilities
 * - Forward core e-graph queries/operations to the underlying `egraph`.
 * - On structural changes:
 *   - After node insertions → call `onAddMany` on each registered metadata.
 *   - After unions → call `onUnionMany` on each registered metadata.
 * - Maintain a name-indexed registry of metadata managers.
 *
 * ## Consistency model
 * - `tryAddMany` and `unionMany` first update the underlying e-graph, then update
 *   all metadata against the post-update graph.
 * - Metadata updates are *batched* and *order-insensitive*; implementations should
 *   treat their inputs as sets.
 *
 * ## Concurrency
 * - A child [[ParallelMap]] is provided for each batch update so metadata can parallelize
 *   per-item work if desired. Correctness must not depend on parallelism.
 *
 * @param egraph
 *   The underlying e-graph.
 * @param metadata
 *   Name-indexed metadata managers kept consistent with `egraph`.
 * @tparam NodeT
 *   The IR node type carried by e-nodes in the graph.
 * @tparam Repr
 *   Concrete representation type of the underlying e-graph.
 */
final case class EGraphWithMetadata[NodeT, +Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]] private(
                                                                                                         egraph: Repr,
                                                                                                         private val metadata: Map[String, Metadata[NodeT, _]]
                                                                                                       ) extends EGraphLike[NodeT, EGraphWithMetadata[NodeT, Repr]] with EGraph[NodeT] {

  /**
   * Re-wrap the same set of metadata around a different e-graph representation.
   *
   * The caller is responsible for ensuring `newRepr` is state-equivalent to the current
   * `egraph` (same classes, nodes, and canonicalization). No metadata recomputation is performed.
   *
   * @param newRepr The new e-graph representation to wrap.
   * @tparam Repr2 The target e-graph representation type.
   * @return A wrapper around `newRepr` that reuses the current metadata instances.
   */
  def migrateTo[Repr2 <: EGraphLike[NodeT, Repr2] with EGraph[NodeT]](newRepr: Repr2): EGraphWithMetadata[NodeT, Repr2] = {
    EGraphWithMetadata(newRepr, metadata)
  }

  /**
   * Register (or replace) a metadata manager under a name.
   *
   * If the name already exists it will be overwritten.
   *
   * @param name Unique name for this metadata manager.
   * @param metadata The manager to register.
   * @tparam MetadataT The manager's internal payload type.
   * @return A new wrapper with the additional registration.
   */
  def addMetadata[MetadataT](name: String, metadata: Metadata[NodeT, MetadataT]): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, this.metadata + (name -> metadata))
  }

  /**
   * Build and register an [[Analysis]] result in one step.
   *
   * Invokes the analysis on the current `egraph` and stores the resulting metadata under `analysis.name`.
   *
   * @param analysis The analysis to execute and register.
   * @tparam A Analysis result payload type.
   * @return A new wrapper with the analysis registered.
   */
  def addAnalysis[A](analysis: Analysis[NodeT, A]): EGraphWithMetadata[NodeT, Repr] = {
    addMetadata(analysis.name, analysis(egraph))
  }

  /**
   * Unregister a metadata manager by name. No other state is modified.
   *
   * @param name The registration name to remove.
   * @return A new wrapper without the given registration.
   */
  def removeMetadata(name: String): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, metadata - name)
  }

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
  def getMetadata[MetadataManagerT <: Metadata[NodeT, _]](name: String): MetadataManagerT = {
    metadata(name).asInstanceOf[MetadataManagerT]
  }

  // === EGraph/EGraphLike delegation ===
  override def canonicalizeOrNull(ref: EClassRef): EClassCall = egraph.canonicalizeOrNull(ref)
  override def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = egraph.classes
  override def nodes(call: EClassCall): Iterable[ENode[NodeT]] = egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[NodeT]] = egraph.users(ref)
  override def findOrNull(node: ENode[NodeT]): EClassCall = egraph.findOrNull(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = egraph.areSame(first, second)

  /**
   * Batch add nodes to the e-graph and update all registered metadata.
   *
   * Steps:
   *   1. Delegate to `egraph.tryAddMany`, producing `results` and `newEgraph`.
   *   2. Extract the subset of `(node, class)` pairs that were actually **added**.
   *   3. For each metadata manager, call `onAddMany(added, newEgraph, childParallel)` and collect results.
   *
   * Parallelism:
   *   - Uses a child parallel context `"metadata for new nodes"`.
   *   - Each metadata gets its own child `"metadata for new nodes - <key>"`.
   *
   * @param nodes Candidate nodes to insert.
   * @param parallelize Parallel strategy to use for the batch and metadata updates.
   * @return
   *   - The per-node results from the underlying e-graph.
   *   - A new wrapper with the updated e-graph and metadata.
   */
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

  /**
   * Batch union e-classes in the e-graph and update all registered metadata.
   *
   * Steps:
   *   1. Delegate to `egraph.unionMany`, producing the set of *equivalence groups* and `newEgraph`.
   *   2. For each metadata manager, call `onUnionMany(equivalences, newEgraph)` in a child parallel context.
   *
   * Notes:
   *   - `equivalences` summarizes the unions as disjoint groups of classes that became one canonical class.
   *   - Metadata implementations must not rely on any particular representative; consult `newEgraph`.
   *
   * @param pairs Pairs of classes requested to be unified.
   * @param parallelize Parallel strategy for the batch and metadata updates.
   * @return
   *   - The disjoint equivalence groups that resulted from the unions.
   *   - A new wrapper with the updated e-graph and metadata.
   */
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

  /**
   * Reset the wrapper to an empty e-graph and corresponding empty metadata.
   *
   * Calls `egraph.emptied` and `metadata.emptied` for each registered manager.
   *
   * @return A wrapper over an empty e-graph with fresh empty metadata instances.
   */
  override def emptied: EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph.emptied, metadata.mapValuesStrict(_.emptied))
  }
}

/**
 * Factory for [[EGraphWithMetadata]].
 */
object EGraphWithMetadata {
  /**
   * Create a wrapper around an e-graph with no metadata registered yet.
   *
   * @param egraph The e-graph to manage.
   * @tparam NodeT IR node type carried by e-nodes.
   * @tparam Repr Concrete e-graph representation type.
   * @return An [[EGraphWithMetadata]] with an empty registry.
   */
  def apply[NodeT, Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr): EGraphWithMetadata[NodeT, Repr] = {
    EGraphWithMetadata(egraph, Map.empty)
  }
}
