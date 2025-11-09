
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

  // Readiness-driven worklist: only nodes whose argument classes already have results are queued.
  private val nodeQueue = new mutable.Queue[ENode[NodeT]]()
  // Tracks nodes currently pending in the queue to avoid duplicate enqueues while in flight.
  private val enqueued  = new mutable.HashSet[ENode[NodeT]]()

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

  /** Enqueue a node if all of its argument classes currently have results. */
  private def enqueueIfReady(node: ENode[NodeT]): Unit = {
    if (!enqueued.contains(node)) {
      // Node is ready iff all arg classes are present.
      var ready = true
      val as = node.args
      var i = 0
      while (ready && i < as.length) {
        ready = contains(as(i).ref)
        i += 1
      }
      if (ready) {
        enqueued += node
        nodeQueue.enqueue(node)
      }
    }
  }

  /** After a class improves, some of its user nodes may now be ready; enqueue those. */
  private def onClassImproved(ref: EClassRef): Unit = {
    val it = egraph.users(ref).iterator
    while (it.hasNext) enqueueIfReady(it.next())
  }

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
        onClassImproved(ref)
    }
  }

  /**
   * Computes the analysis result for an e-class application.
   * @param call The e-class application
   * @return The analysis result for the e-class application
   */
  final def apply(call: EClassCall): A = analysis.rename(apply(call.ref), call.args)

  /**
   * Processes the readiness-driven worklist. Nodes are enqueued exactly when their last dependency becomes available.
   */
  final def processPending(initialized: Boolean = true): Unit = {
    // Process nodes whose arguments are already available; new class improvements will enqueue more.
    while (nodeQueue.nonEmpty) {
      val node = nodeQueue.dequeue()
      // Allow future re-enqueues if upstream classes improve again.
      enqueued -= node

      // By construction, all args have results.
      val args = node.args.map(apply)

      val nodeResult = analysis.make(node, args)
      val ref = egraph.find(node).get.ref

      get(ref) match {
        case Some(old) =>
          val joined = analysis.join(old, nodeResult)
          if ((joined.asInstanceOf[AnyRef] ne old.asInstanceOf[AnyRef]) && joined != old) {
            add(ref, joined)
            onClassImproved(ref)
          }
        case None =>
          add(ref, nodeResult)
          onClassImproved(ref)
      }
    }
  }
}
