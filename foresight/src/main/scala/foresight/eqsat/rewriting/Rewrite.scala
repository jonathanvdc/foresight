package foresight.eqsat.rewriting

import foresight.eqsat.commands.Command
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.immutable
import foresight.eqsat.mutable
import foresight.eqsat.readonly.EGraph

import scala.collection.mutable.HashMap

/**
 * A rewrite rule encapsulates a search-and-replace operation on an e-graph.
 *
 * A rule is parameterized by:
 *  - `NodeT`: The node type for expressions represented by the e-graph.
 *  - `MatchT`: The type of matches produced by this rule's search phase.
 *  - `EGraphT`: The type of e-graph this rule can be applied to. This must be a subtype of [[EGraph]].
 *
 * Rules support three main operations:
 *  - [[search]]: Find all matches of this rule in a given e-graph.
 *  - [[delayed]]: Build a staged command that applies all current matches when executed later.
 *  - [[apply]]/[[applyImmutable]]/[[tryApply]]: Search and apply all matches immediately, returning
 *    either the updated e-graph or a flag indicating whether any change occurred.
 *
 * The staged command produced by [[delayed]] can be enqueued into a [[CommandQueue]] along with
 * other rules, allowing callers to batch multiple rewrites into a single saturation step.
 *
 * @tparam NodeT   Node type for expressions represented by the e-graph.
 * @tparam MatchT  Type of matches produced by this rule's search phase.
 * @tparam EGraphT Type of e-graph this rule can be applied to. Must be a subtype of
 *                 [[EGraph]].
 */
trait Rewrite[NodeT, MatchT, -EGraphT <: EGraph[NodeT]] {
  /**
   * The name of this rewrite. Rewrite names must be unique within a saturation pipeline.
   */
  def name: String

  /**
   * Find all matches of this rule in the given e-graph.
   *
   * @param egraph      E-graph to search.
   * @param parallelize Parallel strategy (used to label/structure match work).
   * @return Sequence of matches (possibly empty).
   */
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Seq[MatchT]

  /**
   * Build a staged command from a precomputed set of matches.
   *
   * This overload is useful when you have already performed searching (e.g., to de-duplicate across rules)
   * but still want to construct a single command that applies all matches.
   *
   * @param matches     Matches to apply.
   * @param egraph      Target e-graph from which matches were derived.
   * @param parallelize Parallel strategy used when building per-match commands.
   * @return An optimized [[CommandQueue]] encapsulated as a [[Command]].
   * @throws Rule.ApplicationException
   * if constructing the per-match commands fails.
   */
  def delayed(matches: Seq[MatchT], egraph: EGraphT, parallelize: ParallelMap): Command[NodeT]

  /**
   * Build a staged command that, when executed, applies this rule's matches to `egraph`.
   *
   * This does not mutate the e-graph now; instead it returns a [[Command]] that you can:
   *  - enqueue into a [[CommandQueue]] with other rules; and
   *  - execute later as part of a larger saturation step.
   *
   * @param egraph      The e-graph to search for matches. The staged command is intended to be run
   *                    against the same (or equivalent) snapshot.
   * @param parallelize Parallel strategy for both search and later application.
   * @return A single, optimized [[Command]] that applies all current matches of this rule.
   */
  def delayed(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Command[NodeT]

  /**
   * Search the e-graph for matches and apply them immediately, if any.
   *
   * Executes the same staged command produced by [[delayed(EGraphT, ParallelMap)]], but runs it right away.
   * Returns `Some(updatedEGraph)` iff at least one application produced an effective change; otherwise `None`.
   *
   * This is useful in saturation loops that want to detect quiescence without rebuilding when nothing changed.
   *
   * @param egraph      Target e-graph.
   * @param parallelize Parallel strategy used for both search and apply.
   * @return `Some(updatedEGraph)` if any change occurred, `None` if the rule was a no-op.
   */
  def tryApply[
    MutEGraphT <: EGraphT with immutable.EGraph[NodeT] with immutable.EGraphLike[NodeT, MutEGraphT]
  ](
     egraph: MutEGraphT,
     parallelize: ParallelMap = ParallelMap.default
   ): Option[MutEGraphT] = {
    val mutGraph = mutable.FreezableEGraph[NodeT, MutEGraphT](egraph)
    val anyChanges = delayed(egraph, parallelize)(mutGraph, HashMap.empty, parallelize)
    if (anyChanges) {
      Some(mutGraph.freeze())
    } else {
      None
    }
  }

  /**
   * Search and apply all matches immediately, returning the resulting e-graph.
   *
   * If the rule makes no changes, the original `egraph` is returned unchanged.
   * For change-detection, prefer [[tryApply]].
   *
   * @param egraph      Target e-graph.
   * @param parallelize Parallel strategy used for both search and apply.
   * @return The updated e-graph if changes occurred; otherwise the original `egraph`.
   */
  def applyImmutable[
    Repr <: EGraphT with immutable.EGraphLike[NodeT, Repr] with immutable.EGraph[NodeT]
  ](
     egraph: Repr,
     parallelize: ParallelMap = ParallelMap.default
   ): Repr = {
    tryApply(egraph, parallelize).getOrElse(egraph)
  }

  /**
   * Search and apply all matches immediately.
   *
   * If the rule makes any changes, `true` is returned without mutating the e-graph.
   * If no changes occur, `false` is returned.
   *
   * @param egraph      Target e-graph.
   * @param parallelize Parallel strategy used for both search and apply.
   * @return The updated e-graph if changes occurred; otherwise the original `egraph`.
   */
  def apply[
    MutEGraphT <: EGraphT with mutable.EGraph[NodeT]
  ](
     egraph: MutEGraphT,
     parallelize: ParallelMap = ParallelMap.default
   ): Boolean = {
    delayed(egraph, parallelize)(egraph, HashMap.empty, parallelize)
  }
}
