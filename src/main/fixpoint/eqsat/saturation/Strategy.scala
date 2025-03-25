package fixpoint.eqsat.saturation

import fixpoint.eqsat.parallel.{CancellationToken, OperationCanceledException, ParallelMap}
import fixpoint.eqsat.{EGraph, EGraphLike}

import scala.concurrent.duration.Duration

/**
 * A strategy for saturating an e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
trait Strategy[EGraphT <: EGraphLike[_, EGraphT] with EGraph[_], Data] {
  /**
   * The initial data for the strategy's first iteration.
   */
  def initialData: Data

  /**
   * Applies a single iteration of the strategy.
   * @param egraph The e-graph to saturate.
   * @param data Data that is carried forward from one iteration to the next.
   * @param parallelize The parallelization strategy to use.
   * @return The new e-graph and data. The new e-graph is `None` if the strategy did not make any changes to the
   *         e-graph.
   */
  def apply(egraph: EGraphT, data: Data, parallelize: ParallelMap): (Option[EGraphT], Data)

  /**
   * Applies the strategy to an e-graph with the initial data.
   * @param egraph The e-graph to saturate.
   * @param parallelize The parallelization strategy to use.
   * @return The new e-graph. The new e-graph is `None` if the strategy did not make any changes to the e-graph.
   */
  final def apply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.parallel): Option[EGraphT] = {
    apply(egraph, initialData, parallelize)._1
  }

  /**
   * Chains this strategy with another strategy.
   * @param other The other strategy to chain with this strategy.
   * @tparam Data2 The type of the data carried by the other strategy.
   * @return A new strategy that applies this strategy followed by the other strategy.
   */
  final def chain[Data2](other: Strategy[EGraphT, Data2]): Strategy[EGraphT, (Data, Data2)] = {
    new Strategy[EGraphT, (Data, Data2)] {
      override def initialData: (Data, Data2) = (Strategy.this.initialData, other.initialData)
      override def apply(egraph: EGraphT, data: (Data, Data2), parallelize: ParallelMap): (Option[EGraphT], (Data, Data2)) = {
        val (newEgraph, newData) = Strategy.this(egraph, data._1, parallelize)
        val (newEgraph2, newData2) = other(newEgraph.getOrElse(egraph), data._2, parallelize)
        (newEgraph2.orElse(newEgraph), (newData, newData2))
      }
    }
  }

  /**
   * Creates a strategy that repeatedly applies iterations of this strategy until a fixpoint is reached.
   * @return A new strategy that applies this strategy until a fixpoint is reached.
   */
  final def untilFixpoint: Strategy[EGraphT, Data] = {
    new Strategy[EGraphT, Data] {
      override def initialData: Data = Strategy.this.initialData
      override def apply(egraph: EGraphT, data: Data, parallelize: ParallelMap): (Option[EGraphT], Data) = {
        var currentEgraph: Option[EGraphT] = None
        var currentData = data
        var changed = true
        while (changed) {
          val (newEgraph, newData) = Strategy.this(currentEgraph.getOrElse(egraph), currentData, parallelize)
          changed = newEgraph.isDefined
          currentEgraph = newEgraph.orElse(currentEgraph)
          currentData = newData
        }
        (currentEgraph, currentData)
      }
    }
  }

  /**
   * Creates a strategy that runs this strategy with a timeout.
   * @param timeout The timeout for the strategy. If the strategy does not complete within the timeout, it is canceled.
   *                The timeout is a time budget that is shared across all iterations of the strategy.
   * @return A new strategy that applies this strategy with a timeout.
   */
  final def withTimeout(timeout: Duration): Strategy[EGraphT, (Data, Duration)] = {
    new Strategy[EGraphT, (Data, Duration)] {
      override def initialData: (Data, Duration) = (Strategy.this.initialData, timeout)
      override def apply(egraph: EGraphT,
                         data: (Data, Duration),
                         parallelize: ParallelMap): (Option[EGraphT], (Data, Duration)) = {
        val (innerData, remainingTime) = data

        // If the remaining time is zero, return immediately.
        if (remainingTime.toNanos <= 0) {
          return (None, data)
        }

        // Set up a cancellation token that will cancel the operation once we are out of time.
        val token = new CancellationToken
        token.cancelAfter(remainingTime)

        // Apply the strategy and catch the cancellation exception if the cancellation request comes in while the
        // operation is in progress.
        try {
          val timeBefore = System.nanoTime()
          val (newEgraph, newData) = Strategy.this(egraph, innerData, parallelize.cancelable(token))
          val timeAfter = System.nanoTime()
          val elapsedNanos = timeAfter - timeBefore
          if (elapsedNanos >= remainingTime.toNanos) {
            (newEgraph, (newData, Duration.Zero))
          } else {
            (newEgraph, (newData, remainingTime - Duration.fromNanos(elapsedNanos)))
          }
        } catch {
          case OperationCanceledException =>
            (None, (innerData, Duration.Zero))
        }
      }
    }
  }
}
