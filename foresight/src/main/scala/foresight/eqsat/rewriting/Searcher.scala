package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.{EGraph, EGraphLike, ReadOnlyEGraph}

import java.util.concurrent.ConcurrentLinkedQueue

/**
 * Describes how to find things in an e-graph.
 *
 * A `Searcher` is a pure query over an [[EGraph]]: given an e-graph, it returns some
 * output (often a sequence of matches) without mutating the graph. It is the "Search" half of a
 * [[Rule]], with the "Apply" half implemented by an [[Applier]].
 *
 * Implementations are free to perform internal parallelism, but must be side effect free
 * with respect to the e-graph. Determinism of output order is not guaranteed unless explicitly
 * documented by the concrete implementation.
 *
 * The `parallelize` parameter allows callers to structure/label work and control parallel map
 * behavior.
 *
 * @tparam NodeT   Node payload type stored in the e-graph.
 * @tparam MatchT Result type produced by this searcher (e.g., `PatternMatch[NodeT]`).
 * @tparam EGraphT The concrete e-graph type, constrained to be both [[EGraphLike]] and [[EGraph]].
 * @example Pair with an [[Applier]] to build a [[Rule]]
 * {{{
 * val searcher: Searcher[MyNode, Seq[MyMatch], MyEGraph] = ...
 * val applier:  Applier [MyNode, MyMatch, MyEGraph]      = ...
 * val rule = Rule("my-rule", searcher, applier)
 * val updated = rule(egraph) // search + apply
 * }}}
 */
trait Searcher[NodeT, MatchT, EGraphT <: ReadOnlyEGraph[NodeT]]
  extends SearcherLike[NodeT, MatchT, EGraphT, Searcher[NodeT, MatchT, EGraphT]] {

  /**
   * Execute this search over the given e-graph. Invokes the searcher's continuation
   * for each match until all matches are processed or the continuation returns `false`.
   *
   * For structured parallel work, use `parallelize.child("phase-name")`.
   *
   * @param egraph      E-graph to search.
   * @param parallelize Parallel mapping/labeling strategy.
   */
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Unit

  /**
   * Run this searcher and collect all matches into a sequence.
   *
   * Note: this eagerly allocates a sequence of all matches. For large match sets,
   * consider using the continuation-based `search` method to process matches
   * incrementally or short-circuit early.
   *
   * @param egraph      E-graph to search.
   * @param parallelize Parallel mapping/labeling strategy.
   * @return A sequence of all matches found by this searcher.
   */
  final def searchAndCollect(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Seq[MatchT] = {
    parallelize.collectFrom[MatchT] { add: (MatchT => Unit) =>
      this.andThen(new ContinuationBuilder {
        def apply(downstream: Continuation): Continuation = (m: MatchT, egraph: EGraphT) => {
          if (downstream(m, egraph)) {
            add(m)
            true
          } else {
            false
          }
        }
      }).search(egraph, parallelize)
    }
  }

  /**
   * Run this searcher **and** another searcher independently over the same e-graph and generate
   * the cartesian product of their results.
   *
   * @param other The other searcher to execute alongside this one.
   * @tparam MatchT2 The other searcher's match type.
   * @return A searcher that returns `(thisOutput, otherOutput)`.
   */
  final def product[MatchT2](other: Searcher[NodeT, MatchT2, EGraphT]): Searcher[NodeT, (MatchT, MatchT2), EGraphT] = {
    final case class ProductSearcher(buildContinuation: SearcherContinuation.ContinuationBuilder[NodeT, (MatchT, MatchT2), EGraphT])
      extends Searcher[NodeT, (MatchT, MatchT2), EGraphT] with SearcherLike[NodeT, (MatchT, MatchT2), EGraphT, ProductSearcher] {

      override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {
        val leftMatches = Searcher.this.searchAndCollect(egraph, parallelize)
        val rightMatches = other.searchAndCollect(egraph, parallelize)
        val cont = continuation
        for {
          m1 <- leftMatches
          m2 <- rightMatches
        } {
          if (!cont((m1, m2), egraph)) return
        }
      }

      override def withContinuationBuilder(continuation: SearcherContinuation.ContinuationBuilder[NodeT, (MatchT, MatchT2), EGraphT]): ProductSearcher = {
        ProductSearcher(continuation)
      }
    }

    ProductSearcher(SearcherContinuation.identityBuilder)
  }
}

/**
 * Constructors and combinators for [[Searcher]].
 */
object Searcher {

  /**
   * A no-op searcher that returns an empty sequence of matches and is considered reversible.
   *
   * Reversal yields [[Applier.ignore]], allowing rules to treat the empty searcher
   * as a structural placeholder in reversible pipelines.
   *
   * @tparam NodeT   Node payload type.
   * @tparam MatchT  Match element type (e.g., [[patterns.PatternMatch]]).
   * @tparam EGraphT E-graph type.
   * @return A [[ReversibleSearcher]] that always returns `Seq.empty`.
   */
  def empty[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: Searcher[NodeT, MatchT, EGraphT] = {
    new ReversibleSearcher[NodeT, MatchT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {}
      override def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]] = Some(Applier.ignore)
      override def buildContinuation: ContinuationBuilder = SearcherContinuation.identityBuilder
      override def withContinuationBuilder(continuation: ContinuationBuilder): Searcher[NodeT, MatchT, EGraphT] = this
    }
  }

  private final case class MergedSearcher[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
  ](searcher: Searcher[NodeT, (PatternMatch[NodeT], PatternMatch[NodeT]), EGraphT],
    buildContinuation: SearcherContinuation.ContinuationBuilder[NodeT, PatternMatch[NodeT], EGraphT])

    extends Searcher[NodeT, PatternMatch[NodeT], EGraphT]
      with SearcherLike[NodeT, PatternMatch[NodeT], EGraphT, MergedSearcher[NodeT, EGraphT]] {

    override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {
      val cont = this.continuation

      object MergeContinuation extends SearcherContinuation.ContinuationBuilder[NodeT, (PatternMatch[NodeT], PatternMatch[NodeT]), EGraphT] {
        def apply(downstream: SearcherContinuation.Continuation[NodeT, (PatternMatch[NodeT], PatternMatch[NodeT]), EGraphT]): SearcherContinuation.Continuation[NodeT, (PatternMatch[NodeT], PatternMatch[NodeT]), EGraphT] = {
          (m: (PatternMatch[NodeT], PatternMatch[NodeT]), e: EGraphT) =>
            if (downstream(m, e)) {
              cont(m._1.merge(m._2), e)
            } else {
              false
            }
        }
      }

      searcher.andThen(MergeContinuation).search(egraph, parallelize)
    }

    override def withContinuationBuilder(continuation: SearcherContinuation.ContinuationBuilder[NodeT, PatternMatch[NodeT], EGraphT]): MergedSearcher[NodeT, EGraphT] = {
      MergedSearcher(searcher, continuation)
    }
  }

  /**
   * Enhances a searcher that produces pairs of `PatternMatch` results
   * with a `merge` method to combine each pair into a single `PatternMatch`.
   *
   * @param self         The searcher that works on the e-graph.
   * @tparam NodeT       Node payload type.
   * @tparam EGraphT     Base e-graph type.
   */
  implicit class SearcherOfPatternMatchPairsOps[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](val self: Searcher[NodeT, (PatternMatch[NodeT], PatternMatch[NodeT]), EGraphT]) extends AnyVal {
    type MatchT = (PatternMatch[NodeT], PatternMatch[NodeT])
    type Continuation = self.Continuation
    type ContinuationBuilder = self.ContinuationBuilder

    /**
     * Merges pairs of `PatternMatch` results into single `PatternMatch` results by calling
     * `PatternMatch.merge` on each pair.
     *
     * The resulting searcher produces `PatternMatch` results instead of pairs.
     *
     * @return A new searcher that merges each pair of matches into a single match.
     */
    def merge: Searcher[NodeT, PatternMatch[NodeT], EGraphT] = {
      MergedSearcher(self, SearcherContinuation.identityBuilder)
    }
  }
}
