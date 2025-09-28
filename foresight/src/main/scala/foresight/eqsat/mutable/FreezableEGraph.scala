package foresight.eqsat.mutable

import foresight.eqsat.immutable

/**
 * An e-graph that can be frozen into an immutable snapshot of its current state.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 */
trait FreezableEGraph[
  NodeT,
  +EGraphT <: immutable.EGraph[NodeT] with immutable.EGraphLike[NodeT, EGraphT]
] extends EGraph[NodeT] {
  /** Freezes the e-graph, returning an immutable snapshot of its current state. */
  def freeze(): EGraphT
}

/**
 * A companion object for the [[FreezableEGraph]] trait that provides a convenient way to create a
 * [[FreezableEGraph]] from an existing immutable e-graph.
 */
object FreezableEGraph {
  /**
   * Wraps an existing immutable e-graph in a mutable interface that supports in-place updates.
   *
   * @param egraph The immutable e-graph to wrap.
   * @tparam NodeT  The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both
   *                 [[immutable.EGraphLike]] and [[immutable.EGraph]].
   * @return A [[FreezableEGraph]] that wraps the provided immutable e-graph.
   */
  def apply[
    NodeT,
    EGraphT <: immutable.EGraphLike[NodeT, EGraphT] with immutable.EGraph[NodeT]
  ](egraph: EGraphT): FreezableEGraph[NodeT, EGraphT] = {
    new UpdatingImmutableEGraph(egraph)
  }
}
