package foresight.eqsat.rewriting

import foresight.eqsat.EGraph

/**
 * A match value that can be **re-targeted** to a later e-graph snapshot derived from the one
 * where it was originally computed.
 *
 * In Foresight, e-graphs are **immutable** and many strategies use **delayed/batched execution**.
 * A [[Searcher]] may compute matches on snapshot `G0`, while the corresponding [[Applier]] or
 * [[Rule]] eventually executes commands against a **derived** snapshot `Gk` (after unions, rebuilds,
 * or other changes). A `PortableMatch` supplies a `port` operation that updates any snapshot-sensitive
 * references (e.g., e-class IDs, node handles) so the match remains valid on the newer graph.
 *
 * # When to implement `PortableMatch`?
 * Implement `PortableMatch` when you want your match type to remain meaningful across snapshots.
 * This includes both:
 *
 *   1. **ID-bearing matches** — the match stores e-graph identifiers that change across snapshots
 *   (e.g., e-class refs/IDs, node IDs). `port` converts these into their equivalents in the new graph.
 *
 *   2. **Structural-only matches** — the match stores only stable, structural data (e.g., constants,
 *   shapes). In this case, `port` can simply return `this`. Even so, implementing `PortableMatch`
 *   is valuable because the caching and recording infrastructure requires all matches to be portable:
 *     - [[foresight.eqsat.saturation.EGraphWithRecordedApplications]] records applied matches per rule
 *      and re-ports them after unions.
 *     - [[foresight.eqsat.saturation.SearchAndApply$.withCaching]] filters out already-applied matches; portability
 *      keeps equality and hashing consistent as the graph evolves.
 *
 * # What counts as a "derived" e-graph?
 * A derived e-graph is one obtained by applying commands to (or otherwise evolving from) the original
 * snapshot where the match was found (e.g., `G0 -> G1 -> ... -> Gk`). It must share lineage; porting to
 * an unrelated e-graph is undefined behavior.
 *
 * # Contract
 * - `port(egraph)` returns an equivalent match relative to `egraph`.
 * - **Idempotence** on a fixed snapshot: `m.port(G) == m.port(G)` and `m.port(G).port(G) == m.port(G)`.
 * - **Determinism**: only data available in `egraph` is used when re-targeting to preserve equality and hashing.
 *
 * @tparam NodeT Node payload type stored in the e-graph.
 * @tparam This  Concrete self type so `port` returns the same match shape (F-bounded).
 * @example E-class–based match (IDs canonicalized across snapshots)
 * {{{
 * final case class ClassMatch[NodeT](cls: EClassRef)
 *   extends PortableMatch[NodeT, ClassMatch[NodeT]] {
 *   def port(egraph: EGraph[NodeT]): ClassMatch[NodeT] =
 *     copy(cls = egraph.canonicalize(cls))
 * }
 * }}}
 * @example Structural-only match (no IDs) — `port` is a no-op yet enables caching and recording
 * {{{
 * final case class ConstShapeMatch[NodeT](shape: ShapeDescriptor)
 *   extends PortableMatch[NodeT, ConstShapeMatch[NodeT]] {
 *   def port(egraph: EGraph[NodeT]): ConstShapeMatch[NodeT] = this
 * }
 * }}}
 * @see [[foresight.eqsat.saturation.EGraphWithRecordedApplications]]
 *      for how applied matches are recorded and re-ported after unions; and
 *      [[foresight.eqsat.saturation.SearchAndApply$.withCaching]]
 *      for how cached applications are filtered using `PortableMatch`.
 */
trait PortableMatch[NodeT, This <: PortableMatch[NodeT, This]] {

  /**
   * Re-target this match to a **derived** e-graph snapshot by updating any snapshot-sensitive
   * references it carries (e.g., e-class IDs, node IDs). For structural-only matches, this can
   * be a no-op that returns `this`, which still enables uniform caching and recording.
   *
   * @param egraph A later snapshot derived from the original e-graph where this match was found.
   * @return A match equivalent to this one but valid on `egraph`.
   */
  def port(egraph: EGraph[NodeT]): This
}
