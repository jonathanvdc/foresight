package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A strategy that repeatedly applies a body strategy until it produces no new e-graph,
 * applying a between-iterations strategy after each successful application of the body.
 *
 * @param body The main strategy to apply repeatedly.
 * @param betweenIterations A strategy to apply between iterations of the body.
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph, which must implement both [[EGraphLike]] and [[EGraph]].
 * @tparam DataT The type of data maintained by the body strategy.
 * @tparam BetweenItersDataT The type of data maintained by the between-iterations strategy.
 */
private final case class RepeatUntilStableWithState[
  NodeT,
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
  DataT,
  BetweenItersDataT]
  (
    body: Strategy[NodeT, EGraphT, DataT],
    betweenIterations: Strategy[NodeT, EGraphT, BetweenItersDataT]
  ) extends Strategy[NodeT, EGraphT, (DataT, BetweenItersDataT)] {

  override def initialData: (DataT, BetweenItersDataT) = (body.initialData, betweenIterations.initialData)

  override def apply(egraph: EGraphT, data: (DataT, BetweenItersDataT), parallelize: ParallelMap): (Option[EGraphT], (DataT, BetweenItersDataT)) = {
    // currentEGraph starts as the input egraph, and is updated if body or betweenIterations return a new egraph
    var currentEGraph: Option[EGraphT] = None

    // lastEGraphFromBody keeps track of the last non-empty egraph returned by body, but not by betweenIterations
    var lastEGraphFromBody: Option[EGraphT] = None

    // currentBodyData and currentBetweenItersData hold the current state data for body and betweenIterations respectively
    var (currentBodyData, currentBetweenItersData) = data

    // resultBetweenItersData keeps track of the last data returned by betweenIterations after body produced a new egraph.
    // This is used to ensure that the final returned data drops the data from the last betweenIterations call if body
    // did not produce a new egraph. It is kept in sync with lastEGraphFromBody.
    var lastBetweenItersDataFromBody = currentBetweenItersData

    var changed = true
    while (changed) {
      // Apply the body strategy to the current egraph and body data
      body(currentEGraph.getOrElse(egraph), currentBodyData, parallelize) match {
        case (Some(newEGraph), newData) =>
          // If body returned a new egraph, update lastEGraphFromBody, lastBetweenItersDataFromBody, and currentBodyData
          lastEGraphFromBody = Some(newEGraph)
          lastBetweenItersDataFromBody = currentBetweenItersData
          currentBodyData = newData

          // Apply the betweenIterations strategy to the new egraph and current between-iterations data
          // Update currentEGraph and currentBetweenItersData based on the result
          val (betweenEgraph, betweenData) = betweenIterations(newEGraph, currentBetweenItersData, parallelize)
          currentEGraph = betweenEgraph.orElse(Some(newEGraph))
          currentBetweenItersData = betweenData

        case (None, newData) =>
          // If body did not return a new egraph, we are done
          currentBodyData = newData
          changed = false
      }
    }

    // Return the last egraph produced by body (if any) and the final data
    (lastEGraphFromBody, (currentBodyData, lastBetweenItersDataFromBody))
  }
}
