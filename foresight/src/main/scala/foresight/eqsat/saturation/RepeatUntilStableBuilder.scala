package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A builder for creating a strategy that repeatedly applies a given `body` strategy until it reaches a stable state,
 * executing a `betweenIterations` strategy between each iteration of `body`.
 *
 * The `body` strategy is applied at least once. After each application of `body`, the `betweenIterations`
 * strategy is executed. If `body` makes no changes to the e-graph (i.e., it returns `None`), the
 * repetition stops, and the final e-graph and combined data from all iterations are returned.
 *
 * @param body The main strategy to be applied repeatedly.
 * @param betweenIterations The strategy to be applied between iterations of `body`.
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the strategies operate on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
 * @tparam DataT The type of data produced by the `body` strategy.
 * @tparam BetweenItersDataT The type of data produced by the `betweenIterations` strategy.
 */
final case class RepeatUntilStableBuilder[
  NodeT,
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
  DataT,
  BetweenItersDataT
]
  (
    body: Strategy[NodeT, EGraphT, DataT],
    betweenIterations: Strategy[NodeT, EGraphT, BetweenItersDataT]
  ) {

  /**
   * Creates a strategy that repeatedly applies the `body` strategy until it reaches a stable state,
   * executing the `betweenIterations` strategy between each iteration of `body`.
   *
   * The `body` strategy is applied at least once. After each application of `body`, the `betweenIterations`
   * strategy is executed. If `body` makes no changes to the e-graph (i.e., it returns `None`), the
   * repetition stops, and the final e-graph and combined data from all iterations are returned.
   *
   * @return A strategy that applies `body` repeatedly until stable, with state carried between iterations.
   */
  def repeatUntilStableWithState: Strategy[NodeT, EGraphT, (DataT, BetweenItersDataT)] =
    RepeatUntilStableWithState(body, betweenIterations)

  /**
   * Creates a strategy that repeatedly applies the `body` strategy until it reaches a stable state,
   * executing the `betweenIterations` strategy between each iteration of `body`.
   *
   * The `body` strategy is applied at least once. After each application of `body`, the `betweenIterations`
   * strategy is executed. If `body` makes no changes to the e-graph (i.e., it returns `None`), the
   * repetition stops, and the final e-graph is returned.
   *
   * @return A strategy that applies `body` repeatedly until stable.
   */
  def repeatUntilStable: Strategy[NodeT, EGraphT, Unit] =
    repeatUntilStableWithState.dropData
}
