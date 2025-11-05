
package foresight.eqsat.readonly

import foresight.eqsat.metadata.Analysis
import foresight.eqsat.{EClassCall, EClassRef, ENode}

import scala.collection.mutable

/**
 * An analysis updater that helps compute the analysis results for an e-graph.
 * @param analysis The analysis to update.
 * @param egraph The e-graph to update the analysis for.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis results.
 */
private[eqsat] abstract class AnalysisUpdater[NodeT, A](analysis: Analysis[NodeT, A],
                                                        egraph: EGraph[NodeT]) {

  private val worklist = mutable.Set.empty[ENode[NodeT]]

  /**
   * Computes the analysis result for an e-class reference.
   * @param ref The e-class reference
   * @return The analysis result for the e-class reference
   */
  def apply(ref: EClassRef): A

  /**
   * Gets the stored analysis result for the given e-class reference.
   * @param ref The e-class reference to get the analysis result for.
   * @return The analysis result for the given e-class reference, or None if not present.
   */
  def get(ref: EClassRef): Option[A]

  /**
   * Checks if the analysis contains a result for the given e-class reference.
   * @param ref The e-class reference to check.
   * @return True if the analysis contains a result for the given e-class reference, false otherwise.
   */
  def contains(ref: EClassRef): Boolean

  /**
   * Sets the analysis result for an e-class.
   *
   * @param ref    The e-class to update the analysis result for.
   * @param result The new analysis result.
   */
  def add(ref: EClassRef, result: A): Unit

  /**
   * Updates the analysis result for an e-class.
   *
   * @param ref    The e-class to update the analysis result for.
   * @param result The new analysis result.
   */
  final def update(ref: EClassRef, result: A): Unit = {
    get(ref) match {
      case Some(oldResult) if oldResult == result => ()
      case _ =>
        add(ref, result)
        worklist ++= egraph.users(ref)
    }
  }

  /**
   * Computes the analysis result for an e-class application.
   * @param call The e-class application
   * @return The analysis result for the e-class application
   */
  final def apply(call: EClassCall): A = analysis.rename(apply(call.ref), call.args)

  /**
   * Processes the worklist by applying the analysis to potentially updated e-nodes.
   * @param initialized Whether the analysis has been initialized for the e-graph. Initialization means that each
   *                    e-class has an analysis result.
   */
  final def processPending(initialized: Boolean = true): Unit = {
    while (worklist.nonEmpty) {
      // Group the worklist by the e-class of the e-node.
      val worklistPerClass = worklist.groupBy(n => egraph.find(n).get.ref)

      // Clear the worklist so it can accept further updates.
      worklist.clear()

      // Apply the analysis to the updates e-nodes in each e-class.
      for ((ref, nodes) <- worklistPerClass) {
        val init = get(ref)
        if (initialized) {
          assert(init.isDefined, s"Analysis not initialized for $ref")
        }

        val result = nodes.foldLeft(init)((acc, node) => {
          if (node.args.forall(arg => contains(arg.ref))) {
            val args = node.args.map(apply)
            val nodeResult = analysis.make(node, args)
            acc match {
              case None => Some(nodeResult)
              case Some(result) => Some(analysis.join(result, nodeResult))
            }
          } else {
            acc
          }
        })

        result.foreach(update(ref, _))
      }
    }
  }
}
