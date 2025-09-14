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
  private final class ApplicationCountingStrategy[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]
    extends Strategy[NodeT, EGraphT, Unit] {

    var applicationCount: Int = 0

    override def initialData: Unit = ()

    override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
      applicationCount += 1
      (Some(egraph), ())
    }
  }

  @Test
  def simpleApplication(): Unit = {
    val strategy = new ApplicationCountingStrategy[Nothing, EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")
    strategy(EGraph.empty)
    assert(strategy.applicationCount == 1, "Strategy is applied once.")
  }

  @Test
  def repeatedApplicationWithIterationLimit(): Unit = {
    val strategy = new ApplicationCountingStrategy[Nothing, EGraph[Nothing]]
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
    val strategy = new ApplicationCountingStrategy[Nothing, EGraph[Nothing]]
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
    val strategy = new ApplicationCountingStrategy[Nothing, EGraph[Nothing]]
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

  @Test
  def changeLoggerFiresEveryIteration(): Unit = {
    val strategy = new ApplicationCountingStrategy[Nothing, EGraph[Nothing]]
    assert(strategy.applicationCount == 0, "Strategy is not applied yet.")

    var counter = 0

    // Apply the strategy with an iteration limit of 3, using change logging
    strategy
      .withChangeLogger((_, _) => counter += 1)
      .withIterationLimit(3)
      .repeatUntilStableWithState
      .apply(EGraph.empty)

    assert(strategy.applicationCount == 3, "Strategy is applied three times due to iteration limit.")
    assert(counter == 3, "Change logger fires three times, once for each iteration.")
  }
}
