package foresight.eqsat.metadata

import foresight.eqsat.collections.SlotMap
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat._

/**
 * Incremental, queryable results of running an [[Analysis]] over an e-graph.
 *
 * This is a concrete [[Metadata]] implementation that:
 *   - Stores per-[[EClassRef]] results in `results`.
 *   - Answers analysis queries for canonicalized e-class applications and mixed trees.
 *   - Stays in sync by reacting to batched node insertions ([[onAddMany]]) and unions ([[onUnionMany]]).
 *
 * Instances are functional: updates return a new [[AnalysisMetadata]] and leave the receiver usable.
 *
 * @param analysis The analysis whose results are stored here.
 * @param results  Map from canonical e-class to its analysis result.
 * @tparam NodeT   IR node type carried by the e-graph.
 * @tparam A       Analysis result type.
 */
final case class AnalysisMetadata[NodeT, A](analysis: Analysis[NodeT, A], results: Map[EClassRef, A])
  extends Metadata[NodeT, AnalysisMetadata[NodeT, A]] {

  /**
   * Compute the analysis result for an e-class application.
   *
   * Canonicalizes `call` against `egraph` and then delegates to [[applyPrecanonicalized]].
   *
   * @param call   The (possibly non-canonical) e-class application.
   * @param egraph The e-graph to canonicalize within.
   * @return The analysis result for `call`.
   */
  def apply(call: EClassCall, egraph: EGraph[NodeT]): A = applyPrecanonicalized(egraph.canonicalize(call))

  /**
   * Evaluate a mixed tree whose leaves are either e-class applications or concrete nodes.
   *
   * For a call leaf, this delegates to [[apply(EClassCall,EGraph)]]. For a node leaf, it first
   * computes the results of all argument subtrees and then invokes the analysis transfer function
   * [[Analysis.make]] using the node’s definitions and uses provided by the tree.
   *
   * @param tree   The mixed tree to evaluate.
   * @param egraph The e-graph to resolve calls and canonicalization.
   * @return The analysis result for the whole tree.
   */
  def apply(tree: MixedTree[NodeT, EClassCall],
            egraph: EGraph[NodeT]): A = {
    tree match {
      case MixedTree.Atom(call: EClassCall) => apply(call, egraph)

      case MixedTree.Node(node, defs, uses, args) =>
        val argsResults = args.map(apply(_, egraph))
        analysis.make(node, defs, uses, argsResults)
    }
  }

  /**
   * Compute the analysis result for an already-canonicalized e-class application.
   *
   * The stored class result is parameterized over *generic* slots; we specialize it to this call
   * by alpha-renaming according to the call’s argument mapping.
   *
   * @param call Canonical e-class application.
   * @return The specialized analysis result.
   */
  private def applyPrecanonicalized(call: EClassCall): A =
    analysis.rename(results(call.ref), call.args)

  /**
   * Update results after a batch of newly-added e-nodes.
   *
   * For each `(node, call)` that was actually inserted:
   *   1. Canonicalize the node in `after`.
   *   2. Build a renaming that maps:
   *      - Call argument slots → the node’s argument slots (`call.args.inverse`), then
   *      - The node’s definition slots → a fresh, generic slot bijection
   *        ([[SlotMap.bijectionFromSetToFresh]]) so results are stored generically.
   *   3. Rename the canonical node by that mapping and evaluate its arguments via [[applyPrecanonicalized]].
   *   4. Compute the node’s local result with [[Analysis.make]] and associate it with `call.ref`.
   *
   * The resulting per-class updates are merged into `results`. Work may run in parallel via `parallelize`.
   *
   * @param added       Newly added nodes with their destination e-class applications (already successful inserts).
   * @param after       The post-insertion e-graph.
   * @param parallelize Parallelization strategy for per-item work.
   * @return A new [[AnalysisMetadata]] reflecting the added nodes.
   */
  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                         after: EGraph[NodeT],
                         parallelize: ParallelMap): Metadata[NodeT, AnalysisMetadata[NodeT, A]] = {

    val resultsPerNode = parallelize[(ENode[NodeT], EClassCall), (EClassRef, A)](added, {
      case (node, call) =>
        val canonicalizedNode = after.canonicalize(node)
        // Build specialization: args map to the call's slots; definitions get fresh generic slots.
        val renaming = call.args.inverse.concat(SlotMap.bijectionFromSetToFresh(canonicalizedNode.definitions.toSet))
        val genericNode = canonicalizedNode.rename(renaming).asNode
        val args = genericNode.args.map(applyPrecanonicalized)
        call.ref -> analysis.make(genericNode, args)
    })
    AnalysisMetadata(analysis, results ++ resultsPerNode)
  }

  /**
   * Update results after a batch of e-class unions.
   *
   * For each disjoint equivalence group in `equivalences`:
   *   1. Choose the representative class (as determined by `after`).
   *   2. Merge results from the other classes into the representative using [[Analysis.join]].
   *   3. Drop results for non-representative classes from the map.
   *   4. Enqueue affected nodes/classes for reprocessing.
   *
   * Then, propagate to a fixpoint using an [[AnalysisUpdater]] worklist so that any nodes whose
   * inputs changed are recomputed consistently.
   *
   * @param equivalences Disjoint groups of classes that became canonical equivalents.
   * @param after        The post-union e-graph.
   * @return A new [[AnalysisMetadata]] consistent with the unified state.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val updater = new AnalysisUpdater(analysis, after, results)

    // First, merge class-level results within each equivalence group.
    for (equiv <- equivalences) {
      val representative = after.canonicalize(equiv.head.ref).ref
      val others = equiv.filterNot(_.ref == representative)
      val result = others.foldLeft(updater.results(representative))((acc, call) => analysis.join(acc, updater.apply(call)))

      // Drop merged classes and write back the representative.
      updater.results --= others.map(_.ref)
      updater.update(representative, result)
    }

    // Propagate changes to a fixpoint.
    updater.processPending()

    AnalysisMetadata(analysis, updater.results)
  }

  /**
   * Create an empty metadata instance for an empty e-graph.
   *
   * The same [[analysis]] instance is retained, but all class results are cleared.
   */
  override def emptied: Metadata[NodeT, AnalysisMetadata[NodeT, A]] = {
    AnalysisMetadata(analysis, Map.empty)
  }
}
