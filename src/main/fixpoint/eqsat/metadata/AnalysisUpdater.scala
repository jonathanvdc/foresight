package fixpoint.eqsat.metadata

import fixpoint.eqsat.{EClassCall, EClassRef, EGraph, ENode}

import scala.collection.mutable

/**
 * An analysis updater that helps compute the analysis results for an e-graph.
 * @param analysis The analysis to update.
 * @param egraph The e-graph to update the analysis for.
 * @param results The current analysis results.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis results.
 */
private[metadata] class AnalysisUpdater[NodeT, A](analysis: Analysis[NodeT, A],
                                                  egraph: EGraph[NodeT],
                                                  var results: Map[EClassRef, A]) {

  private val worklist = mutable.Set.empty[ENode[NodeT]]

  /**
   * Computes the analysis result for an e-class application.
   * @param call The e-class application
   * @return The analysis result for the e-class application
   */
  def apply(call: EClassCall): A = analysis.rename(results(call.ref), call.args)

  /**
   * Updates the analysis result for an e-class.
   * @param ref The e-class to update the analysis result for.
   * @param result The new analysis result.
   */
  def update(ref: EClassRef, result: A): Unit = {
    results.get(ref) match {
      case Some(oldResult) if oldResult == result => ()
      case _ =>
        results += (ref -> result)
        worklist ++= egraph.users(ref)
    }
  }

  /**
   * Processes the worklist by applying the analysis to potentially updated e-nodes.
   */
  def processPending(): Unit = {
    while (worklist.nonEmpty) {
      // Group the worklist by the e-class of the e-node.
      val worklistPerClass = worklist.groupBy(n => egraph.find(n).get.ref)

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
  }
}
