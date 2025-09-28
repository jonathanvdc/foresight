package foresight.eqsat.rewriting

import foresight.eqsat.readonly.ReadOnlyEGraph

/**
 * An [[Applier]] that can be **reversed** into a [[Searcher]] producing the same kind of matches.
 *
 * Reversal enables round-tripping between the "apply" and "search" halves of a [[Rule]]:
 * if both components are reversible, [[Rule.tryReverse]] can produce a valid reversed rule.
 * This is useful for bidirectional rewrites (e.g., associativity/commutativity variants),
 * normalization ↔ denormalization passes, or equivalence proofs.
 *
 * # Contract
 *   - `tryReverse` returns a searcher that, on any compatible e-graph snapshot, enumerates
 *     the `MatchT` instances that this applier consumes.
 *   - Reversal can be **partial**: return `None` when an inverse query is not well-defined
 *     (e.g., lossy edits, reliance on external nondeterminism).
 *   - Determinism matters: for caching and equality, the reversed searcher uses only graph
 *     information available at query time.
 *
 * # Composition
 *   - Works with [[Applier.Filter]] and [[Searcher.Filter]]: guard predicates can be mirrored.
 *   - Compatible with `Rule.tryReverse`: reversal succeeds only when **both** the rule’s
 *     applier and searcher expose inverses.
 *
 * @tparam NodeT   Node payload type stored in the e-graph.
 * @tparam MatchT  Match type consumed by this applier and produced by the reversed searcher.
 * @tparam EGraphT Concrete e-graph type (both [[EGraphLike]] and [[EGraph]]).
 * @example Reversible constant folding applier
 * {{{
 * object FoldApplier extends ReversibleApplier[MyNode, FoldMatch, MyEGraph] {
 *   def apply(m: FoldMatch, g: MyEGraph): Command[MyNode] = buildFoldCommand(m, g)
 *
 *   def tryReverse: Option[Searcher[MyNode, Seq[FoldMatch], MyEGraph]] =
 *     Some(Searcher(patternPhaseForUnfolding)) // finds places where folding would apply
 * }
 * }}}
 */
trait ReversibleApplier[
  NodeT,
  MatchT,
  EGraphT <: ReadOnlyEGraph[NodeT]
] extends Applier[NodeT, MatchT, EGraphT] {

  /**
   * Produce a [[Searcher]] that finds the matches this applier would act on, or `None`
   * when no meaningful inverse exists.
   *
   * @return A reversed searcher for `MatchT`, or `None` if reversal is not available.
   */
  def tryReverse: Option[Searcher[NodeT, MatchT, EGraphT]]
}
