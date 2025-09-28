package foresight.eqsat.readonly

import foresight.eqsat.{EClassCall, EClassRef, ENode, ReadOnlyEGraph, ShapeCall}

/**
 * A decorator for e-graphs that forwards all calls to an underlying e-graph representation.
 *
 * This can be used as a base trait for implementing wrappers that add functionality without
 * modifying the underlying e-graph.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam Repr  The concrete type of the underlying e-graph representation.
 */
trait EGraphDecorator[NodeT, +Repr <: ReadOnlyEGraph[NodeT]] extends ReadOnlyEGraph[NodeT] {
  /**
   * The underlying e-graph representation.
   */
  def egraph: Repr

  // === EGraph/EGraphLike delegation ===
  final override def canonicalizeOrNull(ref: EClassRef): EClassCall = egraph.canonicalizeOrNull(ref)
  final override def canonicalize(node: ENode[NodeT]): ShapeCall[NodeT] = egraph.canonicalize(node)
  final override def classes: Iterable[EClassRef] = egraph.classes
  final override def nodes(call: EClassCall): Iterable[ENode[NodeT]] = egraph.nodes(call)
  final override def nodes(call: EClassCall, nodeType: NodeT): Iterable[ENode[NodeT]] = egraph.nodes(call, nodeType)
  final override def users(ref: EClassRef): Set[ENode[NodeT]] = egraph.users(ref)
  final override def findOrNull(node: ENode[NodeT]): EClassCall = egraph.findOrNull(node)
  final override def areSame(first: EClassCall, second: EClassCall): Boolean = egraph.areSame(first, second)
}
