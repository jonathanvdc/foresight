package foresight.eqsat.mutable

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, ENode, ShapeCall, immutable}

private final class UpdatingImmutableEGraph[
  NodeT,
  EGraphT <: immutable.EGraph[NodeT] with immutable.EGraphLike[NodeT, EGraphT]
](private var _egraph: EGraphT) extends FreezableEGraph[NodeT, EGraphT] {
  /** The current underlying immutable e-graph. */
  override def freeze(): EGraphT = _egraph

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

  override def emptied: this.type = new UpdatingImmutableEGraph(_egraph.emptied).asInstanceOf[this.type]

  override def canonicalizeOrNull(ref: EClassRef): EClassCall = _egraph.canonicalizeOrNull(ref)
  override def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = _egraph.canonicalize(node)
  override def classes: Iterable[EClassRef] = _egraph.classes
  override def nodes(call: EClassCall): Iterable[ENode[NodeT]] = _egraph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[NodeT]] = _egraph.users(ref)
  override def findOrNull(node: ENode[NodeT]): EClassCall = _egraph.findOrNull(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = _egraph.areSame(first, second)
}
