package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, ENode, ShapeCall, immutable}

/**
 * A mutable wrapper around an immutable e-graph that updates the internal reference on mutations.
 *
 * @param _egraph The underlying immutable e-graph.
 * @tparam NodeT   The type of the nodes in the e-graph.
 * @tparam EGraphT The concrete type of the underlying immutable e-graph.
 */
final class UpdatingImmutableEGraph[
  NodeT,
  EGraphT <: immutable.EGraph[NodeT] with immutable.EGraphLike[NodeT, EGraphT]
](private var _egraph: EGraphT) extends EGraph[NodeT] {
  /** The current underlying immutable e-graph. */
  def egraph: EGraphT = _egraph

  private def update[A](tuple: (A, EGraphT)): A = {
    val (result, newEgraph) = tuple
    _egraph = newEgraph
    result
  }

  override def tryAddMany(nodes: Seq[ENode[NodeT]], parallelize: ParallelMap): Seq[AddNodeResult] = update {
    _egraph.tryAddMany(nodes, parallelize)
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)], parallelize: ParallelMap): Set[Set[EClassCall]] = update {
    _egraph.unionMany(pairs, parallelize)
  }

  override def emptied: EGraph[NodeT] = new UpdatingImmutableEGraph(_egraph.emptied)

  override def canonicalizeOrNull(ref: EClassRef): EClassCall = _egraph.canonicalizeOrNull(ref)
  override def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = _egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = _egraph.classes
  override def nodes(call: EClassCall): Iterable[ENode[NodeT]] = _egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[NodeT]] = _egraph.users(ref)
  override def findOrNull(node: ENode[NodeT]): EClassCall = _egraph.findOrNull(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = _egraph.areSame(first, second)
}
