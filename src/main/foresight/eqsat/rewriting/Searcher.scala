package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike, MixedTree, Slot}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.patterns.{CompiledPattern, MachineSearcherPhase, Pattern, PatternMatch}

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
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): OutputT

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

  /**
   * Creates a new searcher that takes an e-graph with metadata as input and runs this searcher on that e-graph.
   * @return A searcher that takes an e-graph with metadata as input.
   */
  final def requireMetadata: Searcher[NodeT, OutputT, EGraphWithMetadata[NodeT, EGraphT]] = {
    new Searcher[NodeT, OutputT, EGraphWithMetadata[NodeT, EGraphT]] {
      override def search(egraph: EGraphWithMetadata[NodeT, EGraphT], parallelize: ParallelMap): OutputT = {
        Searcher.this.search(egraph.egraph, parallelize)
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
   * A searcher that applies a function to the outputs of another searcher.
   * @param searcher The searcher to apply the function to.
   * @param f The mapping function to apply to the outputs of the searcher.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam InputT The type of inner searcher's matches.
   * @tparam OutputT The mapping function's output type.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   */
  final case class Map[NodeT, InputT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](
    searcher: Searcher[NodeT, Seq[InputT], EGraphT],
    f: (InputT, EGraphT) => OutputT) extends Searcher[NodeT, Seq[OutputT], EGraphT] {

    override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[OutputT] = {
      val matches = searcher.search(egraph, parallelize)
      parallelize(matches, (x: InputT) => f(x, egraph)).toSeq
    }
  }

  /**
   * A searcher that filters the output of another searcher based on a predicate.
   * @param searcher The searcher to filter.
   * @param predicate The predicate to filter the output.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the matches.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   */
  final case class Filter[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](
    searcher: Searcher[NodeT, Seq[MatchT], EGraphT],
    predicate: (MatchT, EGraphT) => Boolean) extends Searcher[NodeT, Seq[MatchT], EGraphT] {

    override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = {
      val matches = searcher.search(egraph, parallelize)
      parallelize(matches, (x: MatchT) => predicate(x, egraph)).zip(matches).collect { case (true, m) => m }.toSeq
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
                                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](private val searcher: Searcher[NodeT, Seq[MatchT], EGraphT]) extends AnyVal {

    /**
     * Applies a mapping to each element of the searcher's output, using the e-graph as an argument.
     *
     * @param f The function to apply to each output.
     * @tparam OutputT The type of the output.
     * @return A searcher that applies the function to each output.
     */
    def map[OutputT](f: (MatchT, EGraphT) => OutputT): Searcher[NodeT, Seq[OutputT], EGraphT] = Map(searcher, f)

    /**
     * Filters the output of the searcher, retaining only the elements that satisfy the predicate. The predicate
     * receives the e-graph as an argument.
     *
     * @param f The predicate to filter the output.
     * @return A searcher that filters the output.
     */
    def filter(f: (MatchT, EGraphT) => Boolean): Searcher[NodeT, Seq[MatchT], EGraphT] = Filter(searcher, f)
  }

  implicit class SearcherOfPatternMatch[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](private val searcher: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT]) extends AnyVal {
    /**
     * Requires that the expression bound to a variable is independent of a set of slots. That is, the expression may
     * not contain any of the slots in the set. Potential matches that do not satisfy this condition are filtered out.
     * @param expr The variable to check.
     * @param slots The slots to check.
     * @return A searcher that filters out matches that do not satisfy the condition.
     */
    def requireIndependent(expr: Pattern.Var[NodeT], slots: Slot*): Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraphT] = {
      searcher.filter((m, _) => {
        // Check if the expression is independent of the slots.
        m.isIndependent(expr, slots.toSet)
      })
    }
  }

  /**
   * Searcher extension methods for searchers that produce pairs of [[PatternMatch]] sequences.
   * @param searcher The searcher to extend.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   */
  implicit class SearcherOfPatternMatchProductOps[NodeT,
                                                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](private val searcher: Searcher[NodeT, (Seq[PatternMatch[NodeT]], Seq[PatternMatch[NodeT]]), EGraphT]) extends AnyVal {
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
