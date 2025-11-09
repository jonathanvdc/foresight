package foresight.eqsat.immutable

import foresight.eqsat.*
import foresight.eqsat.parallel.ParallelMap

import scala.collection.compat.immutable.ArraySeq

/**
 * An e-graph that has a root e-class.
 * @param egraph The underlying e-graph that contains the nodes and classes.
 * @param root An optional root e-class call that represents the root of the e-graph.
 * @tparam Node The type of the nodes described by the e-nodes in the e-graph.
 * @tparam Repr The type of the underlying e-graph that implements the [[EGraphLike]] and [[EGraph]] traits.
 */
final case class EGraphWithRoot[
  Node,
  Repr <: EGraphLike[Node, Repr] with EGraph[Node]
](egraph: Repr,
  root: Option[EClassCall])
  extends EGraphLike[Node, EGraphWithRoot[Node, Repr]]
    with EGraph[Node]
    with readonly.EGraphDecorator[Node, Repr] {

  assert(
    root.forall(r => egraph.contains(r.ref)),
    "The root must be an e-class call in the underlying e-graph."
  )

  /**
   * Creates a new [[EGraphWithRoot]] that retains the current graph but sets a new root.
   * @param root The new root of the e-graph.
   * @return A new [[EGraphWithRoot]] with the specified root.
   */
  def withRoot(root: EClassCall): EGraphWithRoot[Node, Repr] = {
    EGraphWithRoot(egraph, Some(root))
  }

  /**
   * Creates a new [[EGraphWithRoot]] that retains the current root but uses a new underlying e-graph.
   * @param newGraph The new underlying e-graph.
   * @return A new [[EGraphWithRoot]] with the specified e-graph and the current root.
   */
  def migrateTo(newGraph: Repr): EGraphWithRoot[Node, Repr] = {
    EGraphWithRoot(newGraph, root)
  }

  override def tryAddMany(nodes: ArraySeq[ENode[Node]],
                          parallelize: ParallelMap): (ArraySeq[AddNodeResult], EGraphWithRoot[Node, Repr]) = {
    egraph.tryAddMany(nodes, parallelize) match {
      case (results, newGraph) => (results, EGraphWithRoot(newGraph, root))
    }
  }

  override def unionMany(pairs: Seq[(EClassCall, EClassCall)],
                         parallelize: ParallelMap): (Set[Set[EClassCall]], EGraphWithRoot[Node, Repr]) = {
    egraph.unionMany(pairs, parallelize) match {
      case (newClasses, newGraph) =>
        (newClasses, EGraphWithRoot(newGraph, root))
    }
  }

  override def emptied: EGraphWithRoot[Node, Repr] = {
    EGraphWithRoot(egraph.emptied, None)
  }
}

/**
 * A companion object for the [[EGraphWithRoot]] that provides a convenient way to create an e-graph with a root from a
 * mixed tree of e-class applications and nodes.
 */
object EGraphWithRoot {
  /**
   * Creates an [[EGraphWithRoot]] from a mixed tree of e-class applications and nodes.
   * @param tree The mixed tree of e-class applications and nodes to be added to the e-graph.
   * @tparam NodeT The type of the nodes in the mixed tree.
   * @return A tuple containing the root e-class call and a new [[EGraphWithRoot]] with the tree added.
   */
  def from[NodeT](tree: MixedTree[NodeT, Nothing]): (EClassCall, EGraphWithRoot[NodeT, EGraph[NodeT]]) = {
    val (root, newGraph) = EGraph.from(tree)
    (root, EGraphWithRoot(newGraph, Some(root)))
  }
}
