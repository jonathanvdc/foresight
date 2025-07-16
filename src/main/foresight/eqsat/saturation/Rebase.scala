package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, Tree}

/**
 * A strategy that rebases the e-graph by extracting a tree from the e-graph, subsequently adding that tree to a new
 * e-graph.
 *
 * @param extractor The extractor to use for extracting the tree.
 * @param findRoot A function to find the root of the e-graph.
 * @param withRoot A function to create a new e-graph with the specified root.
 * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the strategy operates on.
 */
final case class Rebase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](extractor: Extractor[NodeT, EGraphT],
                                                                                         findRoot: EGraphT => EClassCall,
                                                                                         withRoot: (EGraphT, EClassCall) => EGraphT,
                                                                                         areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean) extends Strategy[EGraphT, Option[Tree[NodeT]]] {
  override def initialData: Option[Tree[NodeT]] = None

  override def apply(egraph: EGraphT, data: Option[Tree[NodeT]], parallelize: ParallelMap): (Option[EGraphT], Option[Tree[NodeT]]) = {
    val oldRoot = findRoot(egraph)

    val tree = extractor(oldRoot, egraph)

    if (data.isDefined && areEquivalent(data.get, tree)) {
      // If the tree is equivalent to the previously extracted tree, we can skip rebasing.
      return (None, data)
    }

    val emptyGraph = egraph.emptied
    val (newRoot, newGraph) = emptyGraph.add(tree)

    (Some(withRoot(newGraph, newRoot)), Some(tree))
  }
}

/**
 * A companion object for the [[Rebase]] strategy that provides a convenient way to create a [[Rebase]] instance
 * operating on an [[EGraphWithRoot]].
 */
object Rebase {
  /**
   * Creates a [[Rebase]] strategy that operates on an [[EGraphWithRoot]] using the provided extractor.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @return A [[Rebase]] strategy that operates on an [[EGraphWithRoot]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](extractor: Extractor[NodeT, EGraphT],
                                                                             areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): Rebase[NodeT, EGraphWithRoot[NodeT, EGraphT]] = {
    new Rebase(
      new Extractor[NodeT, EGraphWithRoot[NodeT, EGraphT]] {
        override def apply(call: EClassCall, egraph: EGraphWithRoot[NodeT, EGraphT]): Tree[NodeT] = {
          extractor(call, egraph.graph)
        }
      },
      (egraph: EGraphWithRoot[NodeT, EGraphT]) =>
        egraph.root.getOrElse(throw new IllegalStateException("Root is not set in EGraphWithRoot")),
      (egraph: EGraphWithRoot[NodeT, EGraphT], root: EClassCall) => egraph.withRoot(root),
      areEquivalent)
  }
}
