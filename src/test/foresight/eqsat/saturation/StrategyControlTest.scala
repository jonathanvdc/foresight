package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EGraph, EGraphLike}
import org.junit.Test

/**
 * Unit tests for the control methods defined in Strategy.
 */
class StrategyControlTest {
  /**
   * A test strategy that counts the number of times it is applied to an e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  private final class ApplicationCountingStrategy[EGraphT <: EGraphLike[_, EGraphT] with EGraph[_]]
    extends Strategy[EGraphT, Unit] {

    var applicationCount: Int = 0

    override def initialData: Unit = ()

    override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
      applicationCount += 1
      (Some(egraph), ())
    }
  }

  @Test
  def simpleApplication(): Unit = {
    val strategy = new ApplicationCountingStrategy[EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")
    strategy(EGraph.empty)
    assert(strategy.applicationCount == 1, "Strategy is applied once.")
  }

  @Test
  def repeatedApplicationWithIterationLimit(): Unit = {
    val strategy = new ApplicationCountingStrategy[EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")

    // Apply the strategy with an iteration limit of 3
    strategy
      .withIterationLimit(3)
      .repeatUntilStableWithState
      .apply(EGraph.empty)

    assert(strategy.applicationCount == 3, "Strategy is applied three times due to iteration limit.")
  }

  @Test
  def doubleRepeatedApplication(): Unit = {
    val strategy = new ApplicationCountingStrategy[EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")

    // Apply the strategy with an iteration limit of 3, running twice with repeatUntilStable
    strategy
      .withIterationLimit(3)
      .repeatUntilStable
      .withIterationLimit(3)
      .repeatUntilStable
      .apply(EGraph.empty)

    assert(strategy.applicationCount == 9, "Strategy is applied nine times due to double application.")
  }

  @Test
  def doubleRepeatedApplicationWithState(): Unit = {
    val strategy = new ApplicationCountingStrategy[EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")

    // Apply the strategy with an iteration limit of 3, running twice with repeatUntilStableWithState
    strategy
      .withIterationLimit(3)
      .repeatUntilStableWithState
      .withIterationLimit(3)
      .repeatUntilStableWithState
      .apply(EGraph.empty)

    assert(strategy.applicationCount == 3, "Strategy is applied three times due to retained state.")
  }
}
