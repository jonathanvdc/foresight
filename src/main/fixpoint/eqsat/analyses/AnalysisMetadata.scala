package fixpoint.eqsat.analyses

import fixpoint.eqsat.{EClassCall, EClassRef, EGraph, ENode, Metadata}

import scala.collection.mutable

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
   * @return The analysis result for the e-class application.
   */
  def apply(call: EClassCall): A = analysis.rename(results(call.ref), call.args)

  def onAdd(node: ENode[NodeT], call: EClassCall, after: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val args = node.args.map(apply)
    val result = analysis.make(node, args)
    AnalysisMetadata(analysis, results + (call.ref -> result))
  }

  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): AnalysisMetadata[NodeT, A] = {
    val worklist = mutable.Set.empty[ENode[NodeT]]
    var results = this.results

    def update(ref: EClassRef, result: A): Unit = {
      if (result != results(ref)) {
        results += (ref -> result)
        worklist ++= after.users(ref)
      }
    }

    // First apply the analysis to all the e-class applications that were involved in the union.
    for (equiv <- equivalences) {
      val representative = after.canonicalize(equiv.head.ref).ref
      val others = equiv.filterNot(_.ref == representative)
      val result = others.foldLeft(results(representative))((acc, call) => analysis.join(acc, apply(call)))

      // Drop the results of the other e-class applications.
      results --= others.map(_.ref)

      update(representative, result)
    }

    // Process the worklist by applying the analysis to potentially updated e-nodes.
    while (worklist.nonEmpty) {
      // Group the worklist by the e-class of the e-node.
      val worklistPerClass = worklist.groupBy(n => after.find(n).get.ref)

      // Clear the worklist so it can accept further updates.
      worklist.clear()

      // Apply the analysis to the updates e-nodes in each e-class.
      for ((ref, nodes) <- worklistPerClass) {
        val result = nodes.foldLeft(results(ref))((acc, node) => {
          val args = node.args.map(apply)
          analysis.join(acc, analysis.make(node, args))
        })

        update(ref, result)
      }
    }

    AnalysisMetadata(analysis, results)
  }
}
