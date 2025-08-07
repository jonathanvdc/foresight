package foresight.eqsat

import foresight.eqsat.parallel.ParallelMap

/**
 * A lightweight wrapper around an e-graph that defers the application of union operations.
 *
 * Normally, applying a union in an e-graph requires invoking [[EGraph.unionMany(Seq[(EClassCall, EClassCall)], ParallelMap)]], which
 * merges the e-classes and triggers a rebuild. When many unions are expected—such as during
 * a rewrite pass—it is more efficient to defer them and apply them all at once.
 *
 * `EGraphWithPendingUnions` collects such deferred unions and lets you apply them in a batch
 * by calling [[rebuilt]] or [[rebuild]]. Internally, these methods delegate to [[EGraph.unionMany(Seq[(EClassCall, EClassCall)], ParallelMap)]],
 * ensuring that all unions are applied and the e-graph is fully rebuilt.
 *
 * You rarely need to construct this wrapper directly. Instead, you can call [[EGraph.union]],
 * which returns an `EGraphWithPendingUnions`, allowing fluent chaining:
 *
 * {{{
 * val updated = egraph
 *   .union(a, b)    // returns EGraphWithPendingUnions
 *   .union(c, d)    // chains further unions
 *   .rebuilt        // applies all pending unions and returns a rebuilt EGraph
 * }}}
 *
 * This is particularly useful in rewrite systems and equality saturation loops, where batching
 * union operations avoids excessive intermediate rebuilds and improves performance.
 *
 * @param egraph The underlying e-graph.
 * @param pending A list of deferred unions (pairs of e-class references).
 * @tparam NodeT The type of e-nodes stored in the e-graph.
 * @tparam Repr The concrete type of the underlying e-graph, which must support union and rebuild.
 */
final case class EGraphWithPendingUnions[NodeT, +Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                                                     pending: List[(EClassCall, EClassCall)]) {
  /**
   * Checks whether the e-graph has any deferred unions pending application.
   *
   * @return `true` if there are pending unions and the e-graph should be rebuilt;
   *         `false` if the e-graph is already up to date.
   */
  def requiresRebuild: Boolean = pending.nonEmpty

  /**
   * Defers a union between two e-classes.
   *
   * The union is not applied immediately but instead recorded in the list of pending unions.
   * Use [[rebuild]] or [[rebuilt]] to apply all deferred unions and obtain a fully updated e-graph.
   *
   * If the two e-classes already belong to the same class, this method is a no-op.
   *
   * @param left The first e-class reference.
   * @param right The second e-class reference.
   * @return A new [[EGraphWithPendingUnions]] with the union operation added to the pending list.
   */
  def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[NodeT, Repr] = {
    if (egraph.areSame(left, right)) {
      this
    } else {
      EGraphWithPendingUnions(egraph, (left, right) :: pending)
    }
  }

  /**
   * Applies all pending unions and rebuilds the underlying e-graph using the given parallelization strategy.
   *
   * @param parallelize The strategy used to parallelize the rebuild (e.g., thread pool, sequential fallback).
   * @return The rebuilt e-graph with all pending unions applied.
   */
  def rebuild(parallelize: ParallelMap): Repr = {
    if (pending.isEmpty) {
      egraph
    } else {
      egraph.unionMany(pending, parallelize)._2
    }
  }

  /**
   * Applies all pending unions and returns the rebuilt e-graph using the default parallelization strategy.
   *
   * This is a convenience alias for `rebuild(ParallelMap.default)`.
   *
   * @return The rebuilt e-graph with all pending unions applied.
   */
  def rebuilt: Repr = rebuild(ParallelMap.default)
}

/**
 * A companion object for [[EGraphWithPendingUnions]].
 */
object EGraphWithPendingUnions {
  /**
   * Wraps an e-graph with no pending unions.
   *
   * This constructor is mostly used internally; most users should instead use [[EGraph.union]],
   * which returns an [[EGraphWithPendingUnions]] and enables fluent chaining.
   *
   * @param egraph The e-graph to wrap.
   * @tparam NodeT The type of e-nodes in the e-graph.
   * @tparam Repr  The concrete type of the e-graph.
   * @return A wrapper around the e-graph with no unions pending.
   */
  def apply[NodeT, Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr): EGraphWithPendingUnions[NodeT, Repr] =
    EGraphWithPendingUnions(egraph, Nil)
}
