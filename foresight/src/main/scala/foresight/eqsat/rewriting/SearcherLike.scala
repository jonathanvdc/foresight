package foresight.eqsat.rewriting

import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import foresight.eqsat.{ReadOnlyEGraph, Slot}

/**
 * A trait representing a searcher-like object that can be extended with continuation *builders*.
 *
 * This trait now stores a continuation builder in CPS form: a function
 * `(Match => Boolean) => (Match => Boolean)` which composes left-to-right
 * (so `filter.andThen(flatMap)` runs `filter` then `flatMap`).
 *
 * @tparam Match The type of matches produced by the searcher-like object.
 * @tparam This  The concrete type of the searcher-like object, used for fluent method chaining.
 */
trait SearcherLike[Node, Match, EGraphT <: ReadOnlyEGraph[Node], +This <: SearcherLike[Node, Match, EGraphT, This]] {
  /**
   * A continuation function that processes matches found by a searcher.
   *
   * This type alias represents a function that takes a match of type `Match`
   * and returns a `Boolean` indicating whether to continue processing further matches.
   */
  type Continuation = SearcherContinuation.Continuation[Node, Match, EGraphT]

  /**
   * A continuation builder function that takes a downstream continuation and returns a new continuation.
   *
   * This type alias represents a function that takes an existing `Continuation`
   * and returns a new `Continuation`, allowing for the construction of complex
   * continuations by chaining simpler ones together.
   */
  type ContinuationBuilder = SearcherContinuation.ContinuationBuilder[Node, Match, EGraphT]

  /**
   * A continuation *builder* in CPS form. Given a downstream continuation, returns
   * the combined continuation. Composing builders with `andThen` preserves
   * left-to-right user order: `filter.andThen(flatMap)` runs `filter` then `flatMap`.
   */
  def buildContinuation: ContinuationBuilder

  /**
   * Returns a new instance of this searcher-like object with the specified continuation *builder*.
   *
   * @param continuation The new continuation builder to use.
   * @return A new instance of this searcher-like object with the updated continuation builder.
   */
  def withContinuationBuilder(continuation: ContinuationBuilder): This

  /**
   * Chains a new continuation builder to the existing one, creating a new instance
   * of this searcher-like object with the combined builder. Builders are composed left-to-right.
   *
   * @param buildNextContinuation A builder to chain after the current builder.
   * @return A new instance of this searcher-like object with the combined continuation builder.
   */
  final def andThen(buildNextContinuation: ContinuationBuilder): This = {
    object CombinedContinuationBuilder extends ContinuationBuilder {
      override def apply(continuation: Continuation): Continuation = {
        buildContinuation(buildNextContinuation(continuation))
      }

      override def tryReverse(nextPhase: Applier[Node, Match, EGraphT]): Option[Applier[Node, Match, EGraphT]] = {
        // To reverse a chain of builders A.andThen(B), we reverse B first, then A.
        buildNextContinuation.tryReverse(nextPhase).flatMap { midPhase =>
          buildContinuation.tryReverse(midPhase)
        }
      }
    }

    withContinuationBuilder(CombinedContinuationBuilder)
  }

  /**
   * Builds the final continuation by applying the stored continuation builder
   * to a default no-op continuation.
   *
   * @return The final `SearcherContinuation` that processes matches.
   */
  final def continuation: Continuation = buildContinuation(SearcherContinuation.ignore)

  /**
   * Filters matches based on a predicate, creating a new instance of the searcher-like object.
   * @param p Predicate function to filter matches.
   * @return A new instance of the searcher-like object that only processes matches satisfying the predicate.
   */
  final def filter(p: (Match, EGraphT) => Boolean): This = {
    andThen(SearcherContinuation.filterBuilder(p))
  }

  /**
   * Transforms each match into another match, creating a new instance of the searcher-like object.
   * @param f Function to transform a match.
   * @return A new instance of the searcher-like object that processes the transformed matches.
   */
  final def map(f: (Match, EGraphT) => Match): This = {
    andThen(SearcherContinuation.mapBuilder(f))
  }

  /**
   * Transforms each match into zero or more matches, creating a new instance of the searcher-like object.
   * @param f Function to transform a match into an iterable of matches.
   * @return A new instance of the searcher-like object that processes the transformed matches.
   */
  final def flatMap(f: (Match, EGraphT) => Iterable[Match]): This = {
    andThen(SearcherContinuation.flatMapBuilder(f))
  }
}

/**
 * Extension methods for [[SearcherLike]] to provide additional functionality.
 */
object SearcherLike {
  /**
   * Enrichment for searchers with pattern-specific helpers.
   */
  implicit class SearcherLikeOfPatternMatch[
    Node,
    EGraphT <: ReadOnlyEGraph[Node],
    +This <: SearcherLike[Node, PatternMatch[Node], EGraphT, This]
  ](private val self: SearcherLike[Node, PatternMatch[Node], EGraphT, This]) extends AnyVal {

    /**
     * Require that the expression bound to `expr` be independent of the given slots.
     *
     * Independence means the bound expression does not contain any of `slots`.
     * Matches violating this condition are filtered out.
     *
     * @param expr  Pattern variable whose binding is checked.
     * @param slots Slots the binding must not reference.
     * @return A filtered searcher that enforces independence.
     * @example Forbid captures of loop index slots
     * {{{
     * val s: Searcher[MyNode, Seq[PatternMatch[MyNode]], MyEGraph] = ...
     * val safe = s.requireIndependent(loopVar, iSlot, jSlot)
     * }}}
     */
    def requireIndependent(expr: Pattern.Var, slots: Slot*): This = {
      self.filter((m: PatternMatch[Node], _) => m.isIndependent(expr, slots.toSet))
    }
  }
}
