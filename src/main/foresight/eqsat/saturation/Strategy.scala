package foresight.eqsat.saturation

import foresight.eqsat.metadata.{Analysis, EGraphWithMetadata}
import foresight.eqsat.{EClassCall, EGraph, EGraphLike}
import foresight.eqsat.parallel.{CancellationToken, OperationCanceledException, ParallelMap}
import foresight.eqsat.rewriting.{PortableMatch, Searcher}

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
  final def apply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Option[EGraphT] = {
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
   * Creates a strategy that runs this strategy with a limit on the number of iterations. The limit is the maximum
   * number of iterations that the strategy will run. Once the limit is reached, the strategy will make no further
   * changes to the e-graph.
   * @param limit The maximum number of iterations to run the strategy. If the limit is zero, the strategy will never
   *              change the e-graph.
   * @return A new strategy that applies this strategy with a limit on the number of iterations.
   */
  final def withIterationLimit(limit: Int): Strategy[EGraphT, (Data, Int)] = {
    new Strategy[EGraphT, (Data, Int)] {
      override def initialData: (Data, Int) = (Strategy.this.initialData, limit)
      override def apply(egraph: EGraphT,
                         data: (Data, Int),
                         parallelize: ParallelMap): (Option[EGraphT], (Data, Int)) = {
        val (innerData, remainingIterations) = data

        // If the remaining iterations is zero, return immediately.
        if (remainingIterations <= 0) {
          return (None, data)
        }

        // Apply the strategy.
        val (newEgraph, newData) = Strategy.this(egraph, innerData, parallelize)
        (newEgraph, (newData, remainingIterations - 1))
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

  /**
   * Creates a strategy that runs this strategy with an optional timeout.
   * @param timeout An optional timeout for the strategy. If the timeout is defined, the strategy will run with the specified
   *                timeout. If the timeout is not defined, the strategy will run without a timeout.
   * @return A new strategy that applies this strategy with an optional timeout.
   */
  final def withTimeout(timeout: Option[Duration]): Strategy[EGraphT, (Data, Duration)] = {
    timeout match {
      case Some(t) => withTimeout(t)
      case None => new Strategy[EGraphT, (Data, Duration)] {
        override def initialData: (Data, Duration) = (Strategy.this.initialData, Duration.Zero)
        override def apply(egraph: EGraphT,
                           data: (Data, Duration),
                           parallelize: ParallelMap): (Option[EGraphT], (Data, Duration)) = {
          val (innerData, duration) = data
          val newEgraph = Strategy.this(egraph, innerData, parallelize)
          (newEgraph._1, (newEgraph._2, duration))
        }
      }
    }
  }

  /**
   * Creates a strategy that drops the data carried by this strategy, running each iteration with the initial data.
   * @return A new strategy that applies this strategy without carrying any data.
   */
  final def dropData: Strategy[EGraphT, Unit] = {
    new Strategy[EGraphT, Unit] {
      override def initialData: Unit = ()
      override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
        val (newEGraph, _) = Strategy.this(egraph, Strategy.this.initialData, parallelize)
        (newEGraph, ())
      }
    }
  }
}

/**
 * The companion object for the [[Strategy]] class.
 */
object Strategy {
  implicit class WithMetadata[NodeT,
                              EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                              Data](private val strategy: Strategy[EGraphWithMetadata[NodeT, EGraphT], Data]) extends AnyVal {

    /**
     * Adds an analysis to the e-graph metadata. The analysis is carried over from one iteration to the next.
     * @param analysis The analysis to add to the e-graph.
     * @tparam A The type of the analysis.
     * @return A new strategy that enhances the e-graph with an analysis.
     */
    def addAnalysis[A](analysis: Analysis[NodeT, A]): Strategy[EGraphWithMetadata[NodeT, EGraphT], (Data, Boolean)] = {
      new Strategy[EGraphWithMetadata[NodeT, EGraphT], (Data, Boolean)] {
        override def initialData: (Data, Boolean) = (strategy.initialData, false)

        override def apply(egraph: EGraphWithMetadata[NodeT, EGraphT],
                           data: (Data, Boolean),
                           parallelize: ParallelMap): (Option[EGraphWithMetadata[NodeT, EGraphT]], (Data, Boolean)) = {
          val (innerData, hasAddedAnalysis) = data
          val updatedGraph = if (hasAddedAnalysis) egraph else egraph.addAnalysis(analysis)
          val (newEGraph, newData) = strategy(updatedGraph, innerData, parallelize)
          (newEGraph, (newData, hasAddedAnalysis || newEGraph.isDefined))
        }
      }
    }

    /**
     * Creates a strategy that adds metadata to the e-graph within each iteration of the strategy. Metadata does not
     * escape individual iterations of this strategy.
     * @return A new strategy that enhances the e-graph with metadata.
     */
    def closeMetadata: Strategy[EGraphT, Data] = {
      new Strategy[EGraphT, Data] {
        override def initialData: Data = strategy.initialData

        override def apply(egraph: EGraphT,
                           data: Data,
                           parallelize: ParallelMap): (Option[EGraphT], Data) = {
          val (newEGraph, newData) = strategy(EGraphWithMetadata(egraph), data, parallelize)
          (newEGraph.map(_.egraph), newData)
        }
      }
    }
  }

  implicit class WithRecordedApplications[NodeT,
                                          EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                          Match <: PortableMatch[NodeT, Match],
                                          Data](private val strategy: Strategy[EGraphWithRecordedApplications[NodeT, EGraphT, Match], Data]) extends AnyVal {

    /**
     * Creates a strategy that records applied matches within each iteration of the strategy. Recorded match
     * applications do not escape individual iterations of this strategy.
     * @return A new strategy that records applied matches.
     */
    def closeRecording: Strategy[EGraphT, Data] = {
      new Strategy[EGraphT, Data] {
        override def initialData: Data = strategy.initialData

        override def apply(egraph: EGraphT,
                           data: Data,
                           parallelize: ParallelMap): (Option[EGraphT], Data) = {
          val (newEGraph, newData) = strategy(EGraphWithRecordedApplications(egraph), data, parallelize)
          (newEGraph.map(_.egraph), newData)
        }
      }
    }
  }

  /**
   * An implicit class that adds operations to the [[Strategy]] trait for strategies that operate on an e-graph with a root.
   * @param strategy The strategy to add operations to.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Data The type of the data carried by the strategy.
   */
  implicit class WithRoot[NodeT,
                          EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                          Data](private val strategy: Strategy[EGraphWithRoot[NodeT, EGraphT], Data]) extends AnyVal {

    /**
     * Creates a strategy that operates on an e-graph with a root. The root is typically used to extract trees from the
     * e-graph. Roots are introduced at the beginning of the strategy and are closed at the end of the strategy.
     * @param findRoot A function that finds the root of the e-graph.
     * @return A new strategy that operates on an [[EGraphWithRoot]].
     */
    def closeRoot(findRoot: EGraphT => EClassCall): Strategy[EGraphT, Data] = {
      new Strategy[EGraphT, Data] {
        override def initialData: Data = strategy.initialData

        override def apply(egraph: EGraphT,
                           data: Data,
                           parallelize: ParallelMap): (Option[EGraphT], Data) = {
          val (newEGraph, newData) = strategy(EGraphWithRoot(egraph, Some(findRoot(egraph))), data, parallelize)
          (newEGraph.map(_.graph), newData)
        }
      }
    }
  }
}
