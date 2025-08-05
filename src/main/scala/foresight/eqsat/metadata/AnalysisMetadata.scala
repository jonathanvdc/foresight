package foresight.eqsat.metadata

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassRef, EGraph, ENode, MixedTree, SlotMap}

/**
 * Analysis results for an e-graph's classes. This metadata can respond to changes in the e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis results.
 */
final case class AnalysisMetadata[NodeT, A](analysis: Analysis[NodeT, A], results: Map[EClassRef, A])
  extends Metadata[NodeT, AnalysisMetadata[NodeT, A]] {

  /**
   * Computes the analysis result for an e-class application.
   * @param call The e-class application.
   * @param egraph The e-graph that the e-class application is in.
   * @return The analysis result for the e-class application.
   */
  def apply(call: EClassCall, egraph: EGraph[NodeT]): A = applyPrecanonicalized(egraph.canonicalize(call))

  /**
   * Computes the analysis result for a mixed tree of e-class applications and nodes.
   * @param tree The mixed tree of e-class applications and nodes.
   * @param egraph The e-graph that the mixed tree is in.
   * @return The analysis result for the mixed tree.
   */
  def apply(tree: MixedTree[NodeT, EClassCall],
            egraph: EGraph[NodeT]): A = {
    tree match {
      case MixedTree.Call(call: EClassCall) => apply(call, egraph)

      case MixedTree.Node(node, defs, uses, args) =>
        val argsResults = args.map(apply(_, egraph))
        analysis.make(node, defs, uses, argsResults)
    }
  }

  /**
   * Computes the analysis result for an e-class application, assuming that the e-class application has already been
   * canonicalized.
   * @param call The e-class application.
   * @return The analysis result for the e-class application.
   */
  private def applyPrecanonicalized(call: EClassCall): A =
    analysis.rename(results(call.ref), call.args)

  override def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                         after: EGraph[NodeT],
                         parallelize: ParallelMap): Metadata[NodeT, AnalysisMetadata[NodeT, A]] = {

    val resultsPerNode = parallelize[(ENode[NodeT], EClassCall), (EClassRef, A)](added, {
      case (node, call) =>
        val canonicalizedNode = after.canonicalize(node)
        val renaming = SlotMap(call.args.inverse.map ++ SlotMap.bijectionFromSetToFresh(canonicalizedNode.definitions.toSet).map)
        val genericNode = canonicalizedNode.rename(renaming).asNode
        val args = genericNode.args.map(applyPrecanonicalized)
        call.ref -> analysis.make(genericNode, args)
    })
    AnalysisMetadata(analysis, results ++ resultsPerNode)
  }

  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val updater = new AnalysisUpdater(analysis, after, results)

    // First apply the analysis to all the e-class applications that were involved in the union.
    for (equiv <- equivalences) {
      val representative = after.canonicalize(equiv.head.ref).ref
      val others = equiv.filterNot(_.ref == representative)
      val result = others.foldLeft(updater.results(representative))((acc, call) => analysis.join(acc, updater.apply(call)))

      // Drop the results of the other e-class applications.
      updater.results --= others.map(_.ref)
      updater.update(representative, result)
    }

    // Process the worklist by applying the analysis to potentially updated e-nodes.
    updater.processPending()

    AnalysisMetadata(analysis, updater.results)
  }

  override def emptied: Metadata[NodeT, AnalysisMetadata[NodeT, A]] = {
    AnalysisMetadata(analysis, Map.empty)
  }
}
