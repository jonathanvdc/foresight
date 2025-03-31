package foresight.eqsat.rewriting

import foresight.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap

/**
 * A phase of a searcher that searches for matches in an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam InputT The type of the input that the searcher consumes from the previous phase.
 * @tparam IntermediateT The type of the output that the searcher produces for a single e-class.
 * @tparam OutputT The type of the output that the searcher produces for the entire e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait SearcherPhase[NodeT, -InputT, IntermediateT, +OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Searches for matches in an e-class.
   * @param call The e-class call to search for matches of.
   * @param egraph The e-graph to search in.
   * @param input An input resulting from a previous searcher phase.
   * @return The matches found in the e-graph.
   */
  def search(call: EClassCall, egraph: EGraphT, input: InputT): IntermediateT

  /**
   * Aggregates the matches found in the e-graph.
   * @param matches The matches found in the e-graph.
   * @return The output of the searcher phase.
   */
  def aggregate(matches: Map[EClassRef, IntermediateT]): OutputT

  /**
   * Searches for matches in an e-graph.
   * @param egraph The e-graph to search in.
   * @param input The input to the searcher phase.
   * @param parallelize The parallelization strategy to use.
   * @return The output of the searcher phase.
   */
  final def search(egraph: EGraphT, input: InputT, parallelize: ParallelMap = ParallelMap.parallel): OutputT = {
    val classes = egraph.classes
    val searchClass = (c: EClassRef) => c -> search(egraph.canonicalize(c), egraph, input)
    val matches = parallelize(classes, searchClass).toMap
    aggregate(matches)
  }
}
