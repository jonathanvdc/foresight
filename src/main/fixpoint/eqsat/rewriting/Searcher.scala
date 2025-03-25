package fixpoint.eqsat.rewriting

import fixpoint.eqsat.parallel.ParallelMap
import fixpoint.eqsat.{EGraph, EGraphLike}

/**
 * A searcher that searches for matches in an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam OutputT The type of the output that the searcher produces.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait Searcher[NodeT, +OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Searches for matches in an e-graph.
   * @param egraph The e-graph to search in.
   * @param parallelize The parallelization strategy to use.
   * @return The output of the searcher.
   */
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.parallel): OutputT

  /**
   * Chains this searcher with another searcher phase.
   *
   * @param phase The phase to chain with.
   * @tparam IntermediateT The type of the intermediate output of the phase.
   * @tparam OutputT2 The type of the output of the phase.
   * @return The chained searcher.
   */
  final def chain[IntermediateT, OutputT2](phase: SearcherPhase[NodeT, OutputT, IntermediateT, OutputT2, EGraphT]): Searcher[NodeT, OutputT2, EGraphT] = {
    new Searcher[NodeT, OutputT2, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): OutputT2 = {
        phase.search(egraph, Searcher.this.search(egraph, parallelize), parallelize)
      }
    }
  }

  /**
   * Creates a new searcher that applies both this searcher and another searcher, and returns a pair of the results.
   *
   * @param other The other searcher to apply.
   * @tparam OutputT2 The type of the output of the other searcher.
   * @return A searcher that applies both this searcher and the other searcher.
   */
  final def product[OutputT2](other: Searcher[NodeT, OutputT2, EGraphT]): Searcher[NodeT, (OutputT, OutputT2), EGraphT] = {
    new Searcher[NodeT, (OutputT, OutputT2), EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): (OutputT, OutputT2) = {
        (Searcher.this.search(egraph, parallelize), other.search(egraph, parallelize))
      }
    }
  }
}

/**
 * A companion object for Searcher.
 */
object Searcher {
  /**
   * Creates a searcher from a single phase.
   *
   * @param phase The phase of the searcher.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam OutputT The type of the output that the searcher produces.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @tparam T1 The type of the intermediate output of the first phase.
   * @return A searcher with a single phase.
   */
  def apply[NodeT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], T1](phase: SearcherPhase[NodeT, Unit, T1, OutputT, EGraphT]): Searcher[NodeT, OutputT, EGraphT] = {
    new Searcher[NodeT, OutputT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): OutputT = phase.search(egraph, (), parallelize)
    }
  }
}
