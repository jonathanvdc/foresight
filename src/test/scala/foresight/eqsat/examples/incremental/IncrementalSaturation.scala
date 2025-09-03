package foresight.eqsat.examples.incremental

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.Strategy
import foresight.eqsat.{EGraph, EGraphLike, Tree}

final case class IncrementalSaturation[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], InnerData](strategy: Strategy[NodeT, EGraphT, InnerData])
  extends Strategy[NodeT, EGraphT, (InnerData, Seq[Tree[NodeT]])] {

  /**
   * The initial data for the strategy's first iteration.
   */
  def initialData: (InnerData, Seq[Tree[NodeT]]) = {
    (strategy.initialData, Seq.empty)
  }

  /**
   * Applies a single iteration of the strategy.
   * @param egraph The e-graph to saturate.
   * @param data Data that is carried forward from one iteration to the next.
   * @param parallelize The parallelization strategy to use.
   * @return The new e-graph and data. The new e-graph is `None` if the strategy did not make any changes to the
   *         e-graph.
   */
  def apply(egraph: EGraphT, data: (InnerData, Seq[Tree[NodeT]]), parallelize: ParallelMap): (Option[EGraphT], (InnerData, Seq[Tree[NodeT]])) = {
    ???
  }
}
