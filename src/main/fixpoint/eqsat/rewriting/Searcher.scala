package fixpoint.eqsat.rewriting

import fixpoint.eqsat.parallel.ParallelMap
import fixpoint.eqsat.rewriting.patterns.PatternMatch
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

  /**
   * Searcher extension methods for searchers that produce sequences of matches.
   * @param searcher The searcher to extend.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the matches.
   * @tparam EGraphT The type of the e-graph.
   */
  implicit class SearcherOfSeqOps[NodeT,
                                  MatchT,
                                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](val searcher: Searcher[NodeT, Seq[MatchT], EGraphT]) extends AnyVal {

    /**
     * Applies a mapping to each element of the searcher's output, using the e-graph as an argument.
     * @param f The function to apply to each output.
     * @tparam OutputT The type of the output.
     * @return A searcher that applies the function to each output.
     */
    def mapWithEGraph[OutputT](f: (MatchT, EGraphT) => OutputT): Searcher[NodeT, Seq[OutputT], EGraphT] = {
      new Searcher[NodeT, Seq[OutputT], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[OutputT] = {
          parallelize(searcher.search(egraph, parallelize), (x: MatchT) => f(x, egraph)).toSeq
        }
      }
    }

    /**
     * Applies a mapping to each element of the searcher's output.
     * @param f The function to apply to each output.
     * @tparam OutputT The type of the output.
     * @return A searcher that applies the function to each output.
     */
    def map[OutputT](f: MatchT => OutputT): Searcher[NodeT, Seq[OutputT], EGraphT] = {
      new Searcher[NodeT, Seq[OutputT], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[OutputT] = {
          parallelize(searcher.search(egraph, parallelize), f).toSeq
        }
      }
    }

    /**
     * Filters the output of the searcher, retaining only the elements that satisfy the predicate. The predicate
     * receives the e-graph as an argument.
     * @param f The predicate to filter the output.
     * @return A searcher that filters the output.
     */
    def filterWithEGraph(f: (MatchT, EGraphT) => Boolean): Searcher[NodeT, Seq[MatchT], EGraphT] = {
      new Searcher[NodeT, Seq[MatchT], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = {
          val matches = searcher.search(egraph, parallelize)
          parallelize(matches, (x: MatchT) => f(x, egraph)).zip(matches).collect { case (true, m) => m }.toSeq
        }
      }
    }

    /**
     * Filters the output of the searcher, retaining only the elements that satisfy the predicate.
     * @param f The predicate to filter the output.
     * @return A searcher that filters the output.
     */
    def filter(f: MatchT => Boolean): Searcher[NodeT, Seq[MatchT], EGraphT] = {
      new Searcher[NodeT, Seq[MatchT], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = {
          val matches = searcher.search(egraph, parallelize)
          parallelize(matches, f).zip(matches).collect { case (true, m) => m }.toSeq
        }
      }
    }
  }

  /**
   * Searcher extension methods for searchers that produce pairs of [[PatternMatch]] sequences.
   * @param searcher The searcher to extend.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  implicit class SearcherOfPatternMatchProductOps[NodeT,
                                                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](val searcher: Searcher[NodeT, (Seq[PatternMatch[NodeT]], Seq[PatternMatch[NodeT]]), EGraphT]) extends AnyVal {
    /**
     * Merges the two pattern matches produced by the searcher.
     * @return A searcher that merges the two pattern matches.
     */
    def merge: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] = {
      new Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] {
        override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[PatternMatch[NodeT]] = {
          // Find the individual matches.
          val (matches1, matches2) = searcher.search(egraph, parallelize)

          // Merge their cartesian product.
          for {
            match1 <- matches1
            match2 <- matches2
          } yield match1.merge(match2)
        }
      }
    }
  }
}
