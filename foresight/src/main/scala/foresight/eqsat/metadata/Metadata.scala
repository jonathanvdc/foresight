package foresight.eqsat.metadata

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.{EClassCall, ENode}

/**
 * Metadata that is kept in sync with an [[EGraph]].
 *
 * Implementations are expected to be functional (no in-place mutation of the receiver):
 * each update returns a new [[Metadata]] value reflecting the post-update [[EGraph]] state.
 *
 * ## Lifecycle
 * A typical iteration over an e-graph:
 *   1. Start with an empty or previously built metadata instance.
 *   2. Apply a batch of node insertions → call [[onAddMany]].
 *   3. Apply a batch of class unions (congruence closure, rebuild, etc.) → call [[onUnionMany]].
 *   4. Repeat as the e-graph evolves.
 *
 * Implementations assume batches are sets of effects: ordering is not guaranteed,
 * and duplicates must not change the result (idempotence).
 *
 * ## Concurrency
 * The `parallelize` strategy passed to [[onAddMany]] can be used to parallelize
 * per-element work. Implementations treat it as an optional accelerator:
 * correctness must not depend on parallel execution.
 *
 * @tparam NodeT
 *   The e-graph node payload type (your IR node type).
 * @tparam MetadataT
 *   The internal payload/type carried by this metadata instance (e.g., summaries, indices).
 */
trait Metadata[NodeT, MetadataT] {

  /**
   * Incorporate a batch of newly-added e-nodes into the metadata.
   *
   * Each element of `added` is the concrete node together with the destination e-class (an [[EClassCall]]).
   * The `after` graph is the *post-insertion* e-graph where all `added` nodes already exist.
   *
   * ### Expectations
   * - **Batch semantics**: treat `added` as a set; do not rely on order.
   * - **Pure update**: return a new metadata instance; the receiver remains usable.
   * - **Performance**: use `parallelize` for per-item work when beneficial. Must remain correct if executed serially.
   *
   * @param added
   *   Newly created e-nodes and their target classes, all belonging to `after`.
   * @param after
   *   The e-graph *after* the addition of all nodes in `added`.
   * @param parallelize
   *   Parallelization strategy for mapping over `added`.
   * @return
   *   A metadata instance consistent with `after` and all `added` nodes.
   */
  def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                after: EGraph[NodeT],
                parallelize: ParallelMap): Metadata[NodeT, MetadataT]

  /**
   * Incorporate a batch of e-class unions into the metadata.
   *
   * `equivalences` is a set of *equivalence groups*, where each inner set represents
   * the e-classes that were merged together during this step (i.e., they now refer to a single
   * canonical class in `after`). Groups are disjoint; taken together they summarize this union batch.
   *
   * The `after` graph is the *post-union* e-graph: all classes inside each group are already merged.
   *
   * ### Expectations
   * - **Batch semantics**: treat groups and their elements as sets; ordering must not affect results.
   * - **Canonicalization**: do not assume which representative was chosen; consult `after` if needed.
   * - **Pure update**: return a new metadata instance; the receiver remains usable.
   *
   * @param equivalences
   *   Disjoint groups of e-classes that have been unified in this step.
   * @param after
   *   The e-graph *after* applying all unions in `equivalences`.
   * @return
   *   A metadata instance consistent with the unified state of `after`.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: EGraph[NodeT]): Metadata[NodeT, MetadataT]

  /**
   * Produce a fresh, empty metadata instance suitable for an empty e-graph.
   *
   * This is equivalent to the "zero" state before any nodes or unions have been applied.
   * It must not alias or share mutable state with the receiver.
   *
   * @return
   *   An empty metadata instance.
   */
  def emptied: Metadata[NodeT, MetadataT]
}
