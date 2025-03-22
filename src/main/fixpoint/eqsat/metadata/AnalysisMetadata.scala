package fixpoint.eqsat.metadata

import fixpoint.eqsat.{EClassCall, EClassRef, EGraph, ENode}

/**
 * Analysis results for an e-graph's classes. This metadata can respond to changes in the e-graph.
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
   * Computes the analysis result for an e-class application, assuming that the e-class application has already been
   * canonicalized.
   * @param call The e-class application.
   * @return The analysis result for the e-class application.
   */
  private def applyPrecanonicalized(call: EClassCall): A = analysis.rename(results(call.ref), call.args)

  def onAdd(node: ENode[NodeT], call: EClassCall, after: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val genericNode = node.rename(call.args.inverse)
    val args = genericNode.args.map(applyPrecanonicalized)
    val result = analysis.make(genericNode, args)
    AnalysisMetadata(analysis, results + (call.ref -> result))
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
}
