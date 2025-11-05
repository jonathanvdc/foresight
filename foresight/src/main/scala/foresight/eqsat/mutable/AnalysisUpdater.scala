package foresight.eqsat.mutable

import foresight.eqsat.metadata.Analysis
import foresight.eqsat.readonly
import foresight.eqsat.EClassRef

import scala.collection.mutable.HashMap

/**
 * An analysis updater that helps compute the analysis results for an e-graph.
 * @param analysis The analysis to update.
 * @param egraph The e-graph to update the analysis for.
 * @param results The current analysis results.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis results.
 */
private[mutable] class AnalysisUpdater[NodeT, A](analysis: Analysis[NodeT, A],
                                                 egraph: readonly.EGraph[NodeT],
                                                 var results: HashMap[EClassRef, A])
  extends readonly.AnalysisUpdater[NodeT, A](analysis, egraph) {

  override def add(ref: EClassRef, result: A): Unit = {
    results.update(ref, result)
  }

  override def apply(ref: EClassRef): A = results(ref)
  override def contains(ref: EClassRef): Boolean = results.contains(ref)
  override def get(ref: EClassRef): Option[A] = results.get(ref)
}
