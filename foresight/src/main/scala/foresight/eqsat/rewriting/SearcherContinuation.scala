package foresight.eqsat.rewriting

import foresight.eqsat.readonly.ReadOnlyEGraph

/**
 * Defines common continuations for searchers.
 */
object SearcherContinuation {
  /**
   * A continuation function that processes matches found by a searcher.
   *
   * This type alias represents a function that takes a match of type `Match`
   * and returns a `Boolean` indicating whether to continue processing further matches.
   */
  type Continuation[Node, -Match, -EGraphT <: ReadOnlyEGraph[Node]] = (Match, EGraphT) => Boolean

  /**
   * A continuation builder function that takes a downstream continuation and returns a new continuation.
   *
   * This type alias represents a function that takes an existing `Continuation`
   * and returns a new `Continuation`, allowing for the construction of complex
   * continuations by chaining simpler ones together.
   */
  trait ContinuationBuilder[Node, Match, EGraphT <: ReadOnlyEGraph[Node]] {
    /**
     * Given a downstream continuation, returns a new continuation that incorporates
     * additional processing logic.
     *
     * @param downstream The downstream continuation to chain after this one.
     * @return A new continuation that processes matches and then invokes the downstream continuation.
     */
    def apply(downstream: Continuation[Node, Match, EGraphT]): Continuation[Node, Match, EGraphT]

    /**
     * Attempt to reverse this continuation builder into an [[Applier]] that accepts the same
     * match type it originally processed.
     *
     * By default, this method returns `None`, indicating that reversal is not supported.
     * Subclasses can override this method to provide specific reversal logic.
     *
     * @param nextPhase The next phase applier to chain after this one.
     * @return `Some(applier)` if reversal is possible, `None` otherwise.
     */
    def tryReverse(nextPhase: Applier[Node, Match, EGraphT]): Option[Applier[Node, Match, EGraphT]] = None
  }

  /**
   * A continuation that ignores all matches and always returns `true`.
   *
   * This continuation effectively does nothing and allows all matches to be processed.
   */
  def ignore[Node, Match, EGraphT <: ReadOnlyEGraph[Node]]: Continuation[Node, Match, EGraphT] =
    (_: Match, _: EGraphT) => true

  /**
   * An identity continuation builder that returns the downstream continuation unchanged.
   *
   * This builder effectively does nothing and preserves the original continuation.
   *
   * @tparam Match The type of matches processed by the continuation.
   * @return A continuation builder that acts as the identity function.
   */
  def identityBuilder[Node, Match, EGraphT <: ReadOnlyEGraph[Node]]: ContinuationBuilder[Node, Match, EGraphT] =
    new ContinuationBuilder[Node, Match, EGraphT] {
      override def apply(downstream: Continuation[Node, Match, EGraphT]): Continuation[Node, Match, EGraphT] = downstream

      override def tryReverse(nextPhase: Applier[Node, Match, EGraphT]): Option[Applier[Node, Match, EGraphT]] = {
        Some(nextPhase)
    }
  }

  private final case class FilterContinuationBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](p: (Match, EGraphT) => Boolean)
    extends ContinuationBuilder[Node, Match, EGraphT] {

    override def apply(continuation: Continuation[Node, Match, EGraphT]): Continuation[Node, Match, EGraphT] = {
      (m: Match, egraph: EGraphT) => {
        if (p(m, egraph)) continuation(m, egraph) else true
      }
    }

    override def tryReverse(nextPhase: Applier[Node, Match, EGraphT]): Option[Applier[Node, Match, EGraphT]] = {
      Some(Applier.Filter(nextPhase, p))
    }
  }

  /**
   * Creates a continuation builder that filters matches based on a predicate.
   *
   * The resulting continuation will only invoke the downstream continuation
   * if the predicate returns `true` for the given match and e-graph.
   *
   * @param p Predicate function to filter matches.
   * @tparam Match The type of matches processed by the continuation.
   * @return A continuation builder that filters matches using the provided predicate.
   */
  def filterBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](p: (Match, EGraphT) => Boolean): ContinuationBuilder[Node, Match, EGraphT] = {
    FilterContinuationBuilder(p)
  }

  private final case class MapContinuationBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](f: (Match, EGraphT) => Match)
    extends ContinuationBuilder[Node, Match, EGraphT] {

    override def apply(continuation: Continuation[Node, Match, EGraphT]): Continuation[Node, Match, EGraphT] = {
      (m: Match, egraph: EGraphT) => continuation(f(m, egraph), egraph)
    }
  }

  /**
   * Creates a continuation builder that transforms matches using a provided function.
   *
   * The resulting continuation will apply the transformation function to each match
   * before invoking the downstream continuation.
   *
   * @param f Function to transform a match.
   * @tparam Match The type of matches processed by the continuation.
   * @return A continuation builder that maps matches using the provided function.
   */
  def mapBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](f: (Match, EGraphT) => Match): ContinuationBuilder[Node, Match, EGraphT] = {
    MapContinuationBuilder(f)
  }

  private final case class FlatMapContinuationBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](f: (Match, EGraphT) => Iterable[Match])
    extends ContinuationBuilder[Node, Match, EGraphT] {

    override def apply(continuation: Continuation[Node, Match, EGraphT]): Continuation[Node, Match, EGraphT] = {
      new Continuation[Node, Match, EGraphT] {
        override def apply(m: Match, egraph: EGraphT): Boolean = {
          val nextMatches = f(m, egraph)
          for (n <- nextMatches) {
            if (!continuation(n, egraph)) return false
          }
          true
        }
      }
    }
  }

  /**
   * Creates a continuation builder that transforms each match into zero or more matches.
   *
   * The resulting continuation will apply the transformation function to each match,
   * producing an iterable of matches, and will invoke the downstream continuation
   * for each of these matches.
   *
   * @param f Function to transform a match into an iterable of matches.
   * @tparam Match The type of matches processed by the continuation.
   * @return A continuation builder that flat-maps matches using the provided function.
   */
  def flatMapBuilder[
    Node,
    Match,
    EGraphT <: ReadOnlyEGraph[Node]
  ](f: (Match, EGraphT) => Iterable[Match]): ContinuationBuilder[Node, Match, EGraphT] = {
    FlatMapContinuationBuilder(f)
  }
}
