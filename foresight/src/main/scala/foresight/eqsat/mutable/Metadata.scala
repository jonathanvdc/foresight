package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.readonly.{EGraph => ReadOnlyEGraph}
import foresight.eqsat.{EClassCall, ENode}

/**
 * Mutable metadata that is kept in sync with a mutable [[EGraph]].
 *
 * This trait represents mutable metadata: implementations perform
 * in-place updates to the receiver, rather than returning new metadata instances.
 * Each update mutates the receiver to reflect the post-update [[EGraph]] state.
 *
 * ## Lifecycle
 * A typical iteration over a mutable e-graph:
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
 */
trait Metadata[NodeT] {

  /**
   * Incorporate a batch of newly-added e-nodes into the metadata, performing an in-place update.
   *
   * Each element of `added` is the concrete node together with the destination e-class (an [[EClassCall]]).
   * The `after` graph is the *post-insertion* e-graph where all `added` nodes already exist.
   *
   * ### Expectations
   * - **Batch semantics**: treat `added` as a set; do not rely on order.
   * - **In-place update**: this method mutates the receiver so that it is consistent with the new graph state after the addition.
   * - **Performance**: use `parallelize` for per-item work when beneficial. Must remain correct if executed serially.
   *
   * @param added
   *   Newly created e-nodes and their target classes, all belonging to `after`.
   * @param after
   *   The e-graph *after* the addition of all nodes in `added`.
   * @param parallelize
   *   Parallelization strategy for mapping over `added`.
   * @return
   *   Unit. The receiver is mutated in-place to reflect the updated state.
   */
  def onAddMany(added: Seq[(ENode[NodeT], EClassCall)],
                after: ReadOnlyEGraph[NodeT],
                parallelize: ParallelMap): Unit

  /**
   * Incorporate a batch of e-class unions into the metadata, performing an in-place update.
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
   * - **In-place update**: this method mutates the receiver so that it is consistent with the unified state of `after`.
   *
   * @param equivalences
   *   Disjoint groups of e-classes that have been unified in this step.
   * @param after
   *   The e-graph *after* applying all unions in `equivalences`.
   * @return
   *   Unit. The receiver is mutated in-place to reflect the unified state.
   */
  def onUnionMany(equivalences: Set[Set[EClassCall]], after: ReadOnlyEGraph[NodeT]): Unit

  /**
   * Produce a fresh, empty metadata instance suitable for an empty e-graph.
   *
   * This is equivalent to the "zero" state before any nodes or unions have been applied.
   * The returned instance must not alias or share mutable state with the receiver.
   *
   * @return
   *   A new, empty metadata instance, with no shared mutable state with the receiver.
   */
  def emptied: Metadata[NodeT]
}
