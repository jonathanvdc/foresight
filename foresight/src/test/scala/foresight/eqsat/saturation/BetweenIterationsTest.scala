package foresight.eqsat.saturation

import foresight.eqsat.immutable.EGraph
import foresight.eqsat.parallel.ParallelMap
import org.junit.Test

class BetweenIterationsTest {
  type NodeT = Int
  type EGraphT = EGraph[NodeT]


  private object CountApplications extends Strategy[NodeT, EGraphT, Int] {
    override def initialData: Int = 0

    override def apply(egraph: EGraphT, data: Int, parallelize: ParallelMap): (Option[EGraphT], Int) = {
      (Some(egraph), data + 1)
    }
  }

  @Test
  def betweenZeroIterations(): Unit = {
    val strat = CountApplications
      .withIterationLimit(0)
      .betweenIterations(CountApplications)
      .repeatUntilStableWithState

    strat(EGraph.empty, strat.initialData, ParallelMap.sequential) match {
      case (None, ((bodyData, _), betweenItersData)) =>
        assert(bodyData == 0, s"Expected body to be applied 0 times, got $bodyData")
        assert(betweenItersData == 0, s"Expected betweenIterations to be applied 0 times, got $betweenItersData")
      case _ =>
        throw new AssertionError("Expected the strategy to return no e-graph.")
    }
  }

  @Test
  def betweenManyIterations(): Unit = {
    for (i <- 1 to 5) {
      val strat = CountApplications
        .withIterationLimit(i)
        .betweenIterations(CountApplications)
        .repeatUntilStableWithState

      strat(EGraph.empty, strat.initialData, ParallelMap.sequential) match {
        case (Some(_), ((bodyData, _), betweenItersData)) =>
          assert(bodyData == i, s"Expected body to be applied $i times, got $bodyData")
          assert(betweenItersData == i - 1, s"Expected betweenIterations to be applied ${i - 1} times, got $betweenItersData")
        case _ =>
          throw new AssertionError("Expected the strategy to return an e-graph.")
      }
    }
  }
}
