package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.metadata.Analysis
import foresight.eqsat.parallel.{CancellationToken, OperationCanceledException, ParallelMap}
import foresight.eqsat.rewriting.PortableMatch
import foresight.eqsat.{EClassCall, Tree}
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithMetadata, EGraphWithRecordedApplications, EGraphWithRoot}

import scala.concurrent.duration.Duration

/**
 * A generic, composable strategy for saturating an e-graph using rewrite rules.
 *
 * Strategies define the logic for applying one or more transformation steps to an [[foresight.eqsat.EGraph]],
 * potentially over multiple iterations. They are responsible for how rewrites are applied,
 * how progress is tracked, and how iteration is controlled (e.g., timeouts, fixpoints, or iteration limits).
 *
 * ## Design philosophy
 *
 * Strategies are:
 *   - **Modular** – They can be composed using `thenApply`, repeated with `repeatUntilStable`, or extended with
 *   features like logging, timeout, or metadata.
 *   - **Stateful** – Each strategy carries a custom `Data` payload that can track iteration-specific state across
 *   calls.
 *   - **Flexible** – They work with different e-graph wrappers like [[foresight.eqsat.metadata.EGraphWithMetadata]],
 *   [[EGraphWithRoot]], and [[EGraphWithRecordedApplications]] via extension methods.
 *
 * ## Key operations
 *
 *   - `apply` performs a single iteration of the strategy.
 *   - `initialData` provides the initial state for the strategy.
 *   - `thenApply`, `repeatUntilStable`, `withIterationLimit`, and `withTimeout` allow control-flow composition.
 *   - `withChangeLogger` allows inspection/debugging after each iteration.
 *
 * ## Usage example
 * {{{
 * val strategy: Strategy[Node, MyEGraph, MyState] =
 *   myRuleApplicationStrategy
 *     .withIterationLimit(10)
 *     .withTimeout(1.second)
 *     .repeatUntilStable
 * }}}
 *
 * ## Integration with e-graph wrappers
 *
 * The companion object defines implicit classes that lift strategies to:
 *   - e-graphs with metadata (`addAnalyses`, `closeMetadata`)
 *   - e-graphs with roots (`withRoot`, `closeRoot`)
 *   - strategies that rebase after each iteration (`thenRebase`)
 *   - strategies that record match applications (`closeRecording`)
 *
 * These allow higher-level behavior without modifying the core strategy.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph being transformed (e.g., plain, rooted, or metadata-enriched).
 * @tparam Data The type of state carried by the strategy across iterations.
 */
trait Strategy[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data] {
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
   * Chains another strategy to this strategy. The resulting strategy applies this strategy first, and then applies
   * the other strategy to the result of this strategy.
   * @param other The other strategy to chain with this strategy.
   * @tparam Data2 The type of the data carried by the other strategy.
   * @return A new strategy that applies this strategy followed by the other strategy.
   */
  final def thenApply[Data2](other: Strategy[NodeT, EGraphT, Data2]): Strategy[NodeT, EGraphT, (Data, Data2)] = {
    new Strategy[NodeT, EGraphT, (Data, Data2)] {
      override def initialData: (Data, Data2) = (Strategy.this.initialData, other.initialData)
      override def apply(egraph: EGraphT, data: (Data, Data2), parallelize: ParallelMap): (Option[EGraphT], (Data, Data2)) = {
        val (newEgraph, newData) = Strategy.this(egraph, data._1, parallelize)
        val (newEgraph2, newData2) = other(newEgraph.getOrElse(egraph), data._2, parallelize)
        (newEgraph2.orElse(newEgraph), (newData, newData2))
      }
    }
  }

  /**
   * Creates a strategy that repeatedly applies iterations of this strategy until a fixpoint is reached. State is
   * carried forward from one iteration to the next, allowing the strategy to accumulate changes from one repetition to
   * the next.
   * @return A new strategy that applies this strategy until a fixpoint is reached.
   */
  final def repeatUntilStableWithState: Strategy[NodeT, EGraphT, Data] = {
    new Strategy[NodeT, EGraphT, Data] {
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
   * Creates a strategy that repeatedly applies iterations of this strategy until a fixpoint is reached. The data
   * carried by the strategy is initialized at the start of the repetition, carried forward through each iteration, and
   * dropped at the end of the repetition.
   * @return A new strategy that applies this strategy until a fixpoint is reached.
   */
  final def repeatUntilStable: Strategy[NodeT, EGraphT, Unit] = repeatUntilStableWithState.dropData

  /**
   * Registers a strategy to be executed between each iteration of this strategy when using
   * `repeatUntilStableWithState` or `repeatUntilStable`.
   * @param betweenIterations The strategy to execute between each iteration of this strategy.
   * @tparam BetweenItersDataT The type of the data carried by the `betweenIterations` strategy.
   * @return A builder for creating a strategy that applies this strategy until a fixpoint is reached, with the
   *         `betweenIterations` strategy executed between each iteration.
   */
  final def betweenIterations[BetweenItersDataT](betweenIterations: Strategy[NodeT, EGraphT, BetweenItersDataT]): RepeatUntilStableBuilder[NodeT, EGraphT, Data, BetweenItersDataT] =
    RepeatUntilStableBuilder(this, betweenIterations)

  /**
   * Creates a strategy that places a limit on the number of iterations. The limit is the maximum number of iterations
   * that the strategy will run. Once the limit is reached, additional applications of the strategy will make no further
   * changes to the e-graph.
   * @param limit The maximum number of iterations to run the strategy. If the limit is zero, the strategy will never
   *              change the e-graph.
   * @return A new strategy that applies this strategy with a limit on the number of iterations.
   */
  final def withIterationLimit(limit: Int): Strategy[NodeT, EGraphT, (Data, Int)] = {
    new Strategy[NodeT, EGraphT, (Data, Int)] {
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
   * Creates a strategy that runs this strategy with an optional limit on the number of iterations.
   * @param limit An optional limit on the number of iterations. If the limit is defined, the strategy will run with
   *              the specified limit. If the limit is not defined, the strategy will run without a limit.
   * @return A new strategy that applies this strategy with an optional limit on the number of iterations.
   */
  final def withIterationLimit(limit: Option[Int]): Strategy[NodeT, EGraphT, (Data, Int)] = {
    limit match {
      case Some(l) => withIterationLimit(l)
      case None => extendData(0)
    }
  }

  /**
   * Creates a strategy that runs this strategy with a timeout.
   * @param timeout The timeout for the strategy. If the strategy does not complete within the timeout, it is canceled.
   *                The timeout is a time budget that is shared across all iterations of the strategy.
   * @return A new strategy that applies this strategy with a timeout.
   */
  final def withTimeout(timeout: Duration): Strategy[NodeT, EGraphT, (Data, Duration)] = {
    new Strategy[NodeT, EGraphT, (Data, Duration)] {
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
          case OperationCanceledException(exceptionToken) if exceptionToken == token =>
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
  final def withTimeout(timeout: Option[Duration]): Strategy[NodeT, EGraphT, (Data, Duration)] = {
    timeout match {
      case Some(t) => withTimeout(t)
      case None => extendData(Duration.Zero)
    }
  }

  /**
   * Creates a strategy that drops the data carried by this strategy, running each iteration with the initial data.
   * @return A new strategy that applies this strategy without carrying any data.
   */
  final def dropData: Strategy[NodeT, EGraphT, Unit] = {
    new Strategy[NodeT, EGraphT, Unit] {
      override def initialData: Unit = ()
      override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
        val (newEGraph, _) = Strategy.this(egraph, Strategy.this.initialData, parallelize)
        (newEGraph, ())
      }
    }
  }

  /**
   * Creates a strategy that extends the data carried by this strategy with additional data.
   * @param data2 The additional data to extend the data carried by this strategy.
   * @tparam Data2 The type of the additional data.
   * @return A new strategy that extends the data carried by this strategy with the additional data.
   */
  private final def extendData[Data2](data2: Data2): Strategy[NodeT, EGraphT, (Data, Data2)] = {
    new Strategy[NodeT, EGraphT, (Data, Data2)] {
      override def initialData: (Data, Data2) = (Strategy.this.initialData, data2)

      override def apply(egraph: EGraphT,
                         data: (Data, Data2),
                         parallelize: ParallelMap): (Option[EGraphT], (Data, Data2)) = {
        val (newEGraph, newData) = Strategy.this(egraph, data._1, parallelize)
        (newEGraph, (newData, data._2))
      }
    }
  }

  /**
   * Lifts this strategy to operate on an e-graph with a root. The root is typically used to extract trees from the
   * e-graph.
   * @return A new strategy that operates on an [[EGraphWithRoot]].
   */
  final def withRoot: Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] = {
    new Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] {
      override def initialData: Data = Strategy.this.initialData

      override def apply(egraph: EGraphWithRoot[NodeT, EGraphT],
                         data: Data,
                         parallelize: ParallelMap): (Option[EGraphWithRoot[NodeT, EGraphT]], Data) = {
        val (newEGraph, newData) = Strategy.this(egraph.egraph, data, parallelize)
        (newEGraph.map(egraph.migrateTo), newData)
      }
    }
  }

  /**
   * Wraps this strategy with a logger that observes and logs changes to the e-graph after each iteration.
   *
   * This is useful for debugging or analysis purposes, as it provides visibility into the differences between
   * successive versions of the e-graph produced by the strategy. If the strategy does not modify the e-graph
   * during an iteration, the logger is not invoked.
   *
   * @param logChange A function that takes the previous and updated e-graphs and performs logging or inspection.
   * @return A new strategy that behaves identically to this one, but logs changes whenever they occur.
   */
  final def withChangeLogger(logChange: (EGraphT, EGraphT) => Unit): Strategy[NodeT, EGraphT, Data] = {
    new Strategy[NodeT, EGraphT, Data] {
      override def initialData: Data = Strategy.this.initialData

      override def apply(egraph: EGraphT,
                         data: Data,
                         parallelize: ParallelMap): (Option[EGraphT], Data) = {
        val (newEGraph, newData) = Strategy.this(egraph, data, parallelize)
        newEGraph.foreach { newEgraph =>
          logChange(egraph, newEgraph)
        }
        (newEGraph, newData)
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
                              Data](private val strategy: Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphT], Data]) extends AnyVal {

    /**
     * Adds analyses to the e-graph within each iteration of the strategy. Analyses are added to the e-graph during the
     * strategy's first iteration and are carried forward through each iteration.
     * @param analyses The analyses to add to the e-graph. Each analysis computes some lattice-valued fact about each
     *                 e-class.
     * @return A new strategy that enhances the e-graph with analyses.
     */
    def addAnalyses(analyses: Analysis[NodeT, _]*): Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphT], (Data, Boolean)] = {
      new Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphT], (Data, Boolean)] {
        override def initialData: (Data, Boolean) = (strategy.initialData, false)

        override def apply(egraph: EGraphWithMetadata[NodeT, EGraphT],
                           data: (Data, Boolean),
                           parallelize: ParallelMap): (Option[EGraphWithMetadata[NodeT, EGraphT]], (Data, Boolean)) = {
          val (innerData, hasAddedAnalysis) = data
          val updatedGraph =
            if (hasAddedAnalysis) egraph
            else analyses.foldLeft(egraph)((g, a) => g.addAnalysis(a))
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
    def closeMetadata: Strategy[NodeT, EGraphT, Data] = {
      new Strategy[NodeT, EGraphT, Data] {
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
                                          Data](private val strategy: Strategy[NodeT, EGraphWithRecordedApplications[NodeT, EGraphT, Match], Data]) extends AnyVal {

    /**
     * Creates a strategy that records applied matches within each iteration of the strategy. Recorded match
     * applications do not escape individual iterations of this strategy.
     * @return A new strategy that records applied matches.
     */
    def closeRecording: Strategy[NodeT, EGraphT, Data] = {
      new Strategy[NodeT, EGraphT, Data] {
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
                          Data](private val strategy: Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], Data]) extends AnyVal {

    /**
     * Creates a strategy that operates on an e-graph with a root. The root is typically used to extract trees from the
     * e-graph. Roots are introduced at the beginning of the strategy and are closed at the end of the strategy.
     * @param findRoot A function that finds the root of the e-graph.
     * @return A new strategy that operates on an [[EGraphWithRoot]].
     */
    def closeRoot(findRoot: EGraphT => EClassCall): Strategy[NodeT, EGraphT, Data] = {
      new Strategy[NodeT, EGraphT, Data] {
        override def initialData: Data = strategy.initialData

        override def apply(egraph: EGraphT,
                           data: Data,
                           parallelize: ParallelMap): (Option[EGraphT], Data) = {
          val (newEGraph, newData) = strategy(EGraphWithRoot(egraph, Some(findRoot(egraph))), data, parallelize)
          (newEGraph.map(_.egraph), newData)
        }
      }
    }

    /**
     * Chains a rebasing operation to this strategy.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * Concretely, each iteration does the following:
     *  1. Applies the underlying strategy.
     *  2. If no change occurs, returns the original graph.
     *  3. If a change occurs, extracts a tree from the current root.
     *  4. If the extracted tree is equivalent to the previous tree, returns the modified graph as-is.
     *  5. Otherwise, creates a new empty e-graph and inserts the new tree as the new root.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A new strategy that performs rebasing after each transformation when the result tree changes.
     */
    def thenRebase(extractor: Extractor[NodeT, EGraphT],
                   areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], (Data, Option[Tree[NodeT]])] = {
      TransformAndRebase(strategy, extractor, areEquivalent)
    }

    /**
     * Creates a builder for a strategy that repeatedly applies iterations of this strategy until a fixpoint is
     * reached, with a rebasing operation executed between each successful iteration.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree after each iteration,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A builder for creating a strategy that applies this strategy until a fixpoint is reached, with rebasing
     *         executed between each iteration.
     */
    def rebaseBetweenIterations(extractor: Extractor[NodeT, EGraphT],
                                areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): RepeatUntilStableBuilder[NodeT, EGraphWithRoot[NodeT, EGraphT], Data, Option[Tree[NodeT]]] = {
      strategy.betweenIterations(Rebase(extractor, areEquivalent))
    }
  }

  /**
   * An implicit class that adds operations to the [[Strategy]] trait for strategies that operate on an e-graph with
   * metadata and a root.
   * @param strategy The strategy to add operations to.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Data The type of the data carried by the strategy.
   */
  implicit class WithMetadataAndRoot[NodeT,
                                     EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                     Data](private val strategy: Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data]) extends AnyVal {

    /**
     * Chains a rebasing operation to this strategy.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * Concretely, each iteration does the following:
     *  1. Applies the underlying strategy.
     *  2. If no change occurs, returns the original graph.
     *  3. If a change occurs, extracts a tree from the current root.
     *  4. If the extracted tree is equivalent to the previous tree, returns the modified graph as-is.
     *  5. Otherwise, creates a new empty e-graph and inserts the new tree as the new root.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A new strategy that performs rebasing after each transformation when the result tree changes.
     */
    def thenRebase(extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                   areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], (Data, Option[Tree[NodeT]])] = {
      TransformAndRebase.withMetadata(strategy, extractor, areEquivalent)
    }

    /**
     * Creates a builder for a strategy that repeatedly applies iterations of this strategy until a fixpoint is
     * reached, with a rebasing operation executed between each successful iteration.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree after each iteration,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A builder for creating a strategy that applies this strategy until a fixpoint is reached, with rebasing
     *         executed between each iteration.
     */
    def rebaseBetweenIterations(extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                                areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): RepeatUntilStableBuilder[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data, Option[Tree[NodeT]]] = {
      strategy.betweenIterations(Rebase.withMetadata(extractor, areEquivalent))
    }
  }

  /**
   * An implicit class that adds operations to the [[Strategy]] trait for strategies that operate on an e-graph with
   * recorded applications, metadata, and a root.
   * @param strategy The strategy to add operations to.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Match The type of the matches produced by the strategy.
   * @tparam Data The type of the data carried by the strategy.
   */
  implicit class WithRecordingMetadataAndRoot[NodeT,
                                              EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                              Match <: PortableMatch[NodeT, Match],
                                              Data](private val strategy: Strategy[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], Data]) extends AnyVal {

    /**
     * Chains a rebasing operation to this strategy.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * Concretely, each iteration does the following:
     *  1. Applies the underlying strategy.
     *  2. If no change occurs, returns the original graph.
     *  3. If a change occurs, extracts a tree from the current root.
     *  4. If the extracted tree is equivalent to the previous tree, returns the modified graph as-is.
     *  5. Otherwise, creates a new empty e-graph and inserts the new tree as the new root.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A new strategy that performs rebasing after each transformation when the result tree changes.
     */
    def thenRebase(extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                   areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): Strategy[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], (Data, Option[Tree[NodeT]])] = {
      TransformAndRebase.withRecording(strategy, extractor, areEquivalent)
    }

    /**
     * Creates a builder for a strategy that repeatedly applies iterations of this strategy until a fixpoint is
     * reached, with a rebasing operation executed between each successful iteration.
     *
     * This allows the strategy to restart from a clean e-graph rooted at a newly extracted tree after each iteration,
     * but only when a change occurs **and** the extracted tree differs from the previous one.
     *
     * Rewriting strategies often accumulate redundant or irrelevant information in the e-graph;
     * rebasing helps keep the search focused by collapsing the current best result into a fresh
     * starting point.
     *
     * @param extractor Extracts a tree from the e-graph starting at a root node.
     * @param areEquivalent An optional function to compare trees for equivalence. Defaults to structural equality (`==`).
     * @return A builder for creating a strategy that applies this strategy until a fixpoint is reached, with rebasing
     *         executed between each iteration.
     */
    def rebaseBetweenIterations(extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                                areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): RepeatUntilStableBuilder[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], Data, Option[Tree[NodeT]]] = {
      strategy.betweenIterations(Rebase.withRecording(extractor, areEquivalent))
    }
  }
}
