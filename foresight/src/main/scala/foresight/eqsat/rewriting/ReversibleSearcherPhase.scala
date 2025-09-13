package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A searcher phase that supports **reversal** into an [[Applier]].
 *
 * In a multiphase search pipeline, reversal proceeds **backwards**: the last
 * reversible search phase turns into an applier on its match type, the previous
 * phase turns into an applier on its own match type that *feeds into* the next
 * applier, and so on. This trait exposes a [[tryReverse]] hook that receives the
 * already-reversed "next" applier and returns an applier for this phase.
 *
 * # Reversal model
 * Consider a search pipeline:
 * {{{
 * phase1: SearcherPhase[Node, In1, I1, Out1, G]
 * phase2: ReversibleSearcherPhase[Node, Out1, I2, Match, G]
 * // Searcher = Searcher(phase1).chain(phase2)
 * }}}
 * Reversal of `phase2` yields an `Applier[Node, Match, G]`. If `phase1` is also
 * reversible (as a `ReversibleSearcherPhase`), it can use `tryReverse(nextApplier)`
 * to produce an `Applier[Node, In1, G]` that feeds matches forward into `nextApplier`.
 *
 * # Contract
 *   - `tryReverse(nextPhase)` returns an applier that, when applied to a `MatchT`, produces
 *     commands equivalent to the edits this phase implies, then delegates to `nextPhase`.
 *   - Partial reversal is valid: return `None` when an inverse is not meaningful (e.g., lossy mapping).
 *   - Determinism and portability matter for caching: base the generated commands only on data available
 *     in the provided e-graph snapshot.
 *
 * # Composition
 *   - Works hand-in-hand with [[ReversibleApplier]] / [[ReversibleSearcher]] to enable full
 *     [[Rule.tryReverse]].
 *   - Pairs naturally with filtering/mapping wrappers (e.g., [[Searcher.Filter]], [[Applier.Filter]]).
 *
 * @tparam NodeT          Node payload type.
 * @tparam InputT         Input carried from the previous phase (the phase you would reverse next).
 * @tparam IntermediateT  Per-class result type for this phase (same role as in [[SearcherPhase]]).
 * @tparam MatchT         Match element type this phase ultimately contributes (the search output of this phase).
 * @tparam EGraphT        Concrete e-graph type (both [[EGraphLike]] and [[EGraph]]).
 */
trait ReversibleSearcherPhase[
  NodeT,
  InputT,
  IntermediateT,
  MatchT,
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]
] extends SearcherPhase[NodeT, InputT, IntermediateT, Seq[MatchT], EGraphT] {

  /**
   * Reverse this phase into an [[Applier]] over this phase's match type, optionally
   * composing with the already-reversed applier of the **next** phase in the pipeline.
   *
   * @param nextPhase  Applier produced by reversing the subsequent search phase.
   *                   Use it to forward reconstructed inputs downstream. For the
   *                   last phase in a pipeline, pass an identity/no-op applier.
   * @return           An applier for `MatchT`, or `None` if reversal is unavailable.
   */
  def tryReverse(nextPhase: Applier[NodeT, InputT, EGraphT]): Option[Applier[NodeT, MatchT, EGraphT]]
}
