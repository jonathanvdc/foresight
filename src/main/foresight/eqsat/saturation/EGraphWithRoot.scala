package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EClassRef, EGraph, EGraphLike, ENode, ShapeCall}

/**
 * An e-graph that has a root e-class.
 * @param graph The underlying e-graph that contains the nodes and classes.
 * @param root An optional root e-class call that represents the root of the e-graph.
 * @tparam Node The type of the nodes described by the e-nodes in the e-graph.
 * @tparam Repr The type of the underlying e-graph that implements the [[EGraphLike]] and [[EGraph]] traits.
 */
final case class EGraphWithRoot[Node, Repr <: EGraphLike[Node, Repr] with EGraph[Node]](graph: Repr,
                                                                                        root: Option[EClassCall])
  extends EGraphLike[Node, EGraphWithRoot[Node, Repr]] with EGraph[Node] {

  /**
   * Creates a new [[EGraphWithRoot]] that retains the current graph but sets a new root.
   * @param root The new root of the e-graph.
   * @return A new [[EGraphWithRoot]] with the specified root.
   */
  def withRoot(root: EClassCall): EGraphWithRoot[Node, Repr] = {
    EGraphWithRoot(graph, Some(root))
  }

  override def tryCanonicalize(ref: EClassRef): Option[EClassCall] = graph.tryCanonicalize(ref)
  override def canonicalize(node: ENode[Node]): ShapeCall[Node] = graph.canonicalize(node)
  override def classes: Iterable[EClassRef] = graph.classes
  override def nodes(call: EClassCall): Set[ENode[Node]] = graph.nodes(call)
  override def users(ref: EClassRef): Set[ENode[Node]] = graph.users(ref)
  override def find(node: ENode[Node]): Option[EClassCall] = graph.find(node)
  override def areSame(first: EClassCall, second: EClassCall): Boolean = graph.areSame(first, second)
  override def tryAddMany(nodes: Seq[ENode[Node]],
                          parallelize: ParallelMap): (Seq[AddNodeResult], EGraphWithRoot[Node, Repr]) = {
    graph.tryAddMany(nodes, parallelize) match {
      case (results, newGraph) => (results, EGraphWithRoot(newGraph, root))
    }
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)],
                         parallelize: ParallelMap): (Set[Set[EClassCall]], EGraphWithRoot[Node, Repr]) = {
    graph.unionMany(pairs, parallelize) match {
      case (newClasses, newGraph) =>
        (newClasses, EGraphWithRoot(newGraph, root))
    }
  }

  override def emptied: EGraphWithRoot[Node, Repr] = {
    EGraphWithRoot(graph.emptied, None)
  }
}
