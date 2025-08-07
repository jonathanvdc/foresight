package foresight.eqsat

import foresight.eqsat.parallel.ParallelMap

/**
 * A lightweight wrapper around an e-graph that defers the application of union operations.
 *
 * Instead of immediately applying a union (which may trigger expensive rebuilds),
 * this class lets you collect multiple unions and apply them in a batch using [[rebuilt]] or [[rebuild]].
 *
 * Most users will not need to construct this class directly. Instead, you can call [[EGraph.union]]
 * on any `EGraph`, which returns an `EGraphWithPendingUnions`, allowing fluent chaining of unions:
 *
 * {{{
 * val updated = egraph
 *   .union(a, b)    // returns EGraphWithPendingUnions
 *   .union(c, d)    // chains further unions
 *   .rebuilt        // applies all unions and returns a rebuilt EGraph
 * }}}
 *
 * This is particularly useful in rewrite systems and equality saturation loops, where many unions
 * are computed but it's more efficient to apply them all at once.
 *
 * @param egraph The underlying e-graph.
 * @param pending A list of deferred unions (pairs of e-class references).
 * @tparam NodeT The type of e-nodes stored in the e-graph.
 * @tparam Repr The concrete type of the underlying e-graph.
 */
final case class EGraphWithPendingUnions[NodeT, +Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                                                     pending: List[(EClassCall, EClassCall)]) {
  /**
   * Determines whether the e-graph requires a rebuild.
   * @return True if the e-graph requires a rebuild, otherwise false.
   */
  def requiresRebuild: Boolean = pending.nonEmpty

  /**
   * Unions two e-classes in this e-graph. The resulting e-class contains all e-nodes from both e-classes.
   * The effects of this operation are deferred until the e-graph is rebuilt.
   *
   * @param left The reference to the first e-class to union.
   * @param right The reference to the second e-class to union.
   * @return The e-class reference of the resulting e-class, and the new e-graph with the e-classes unioned.
   */
  def union(left: EClassCall, right: EClassCall): EGraphWithPendingUnions[NodeT, Repr] = {
    if (egraph.areSame(left, right)) {
      this
    } else {
      EGraphWithPendingUnions(egraph, (left, right) :: pending)
    }
  }

  /**
   * Rebuilds the e-graph, applying all pending unions.
   * @param parallelize The parallelization strategy to use.
   * @return The new e-graph with the e-graph rebuilt.
   */
  def rebuild(parallelize: ParallelMap): Repr = {
    if (pending.isEmpty) {
      egraph
    } else {
      egraph.unionMany(pending, parallelize)._2
    }
  }

  /**
   * Rebuilds the e-graph, applying all pending unions.
   * @return The new e-graph with the e-graph rebuilt.
   */
  def rebuilt: Repr = rebuild(ParallelMap.default)
}

/**
 * A companion object for the e-graph with pending unions.
 */
object EGraphWithPendingUnions {
  /**
   * Creates a new e-graph with pending unions.
   * @param egraph The e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam Repr The type of the e-graph.
   * @return An e-graph with pending unions.
   */
  def apply[NodeT, Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr): EGraphWithPendingUnions[NodeT, Repr] =
    EGraphWithPendingUnions(egraph, Nil)
}
