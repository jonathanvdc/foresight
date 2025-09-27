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
