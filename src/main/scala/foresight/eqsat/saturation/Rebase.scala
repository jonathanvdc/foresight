package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, Tree}

/**
 * A strategy that rebases the e-graph by extracting a tree from the e-graph, subsequently adding that tree to a new
 * e-graph.
 *
 * @param extractor The extractor to use for extracting the tree.
 * @param getRoot A function to find the root of the e-graph.
 * @param setRoot A function to create a new e-graph with the specified root.
 * @param areEquivalent A function to determine if two trees are equivalent. If a newly extracted tree is equivalent
 *                      to the previously extracted tree, no rebasing is performed.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the strategy operates on.
 */
final case class Rebase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](extractor: Extractor[NodeT, EGraphT],
                                                                                         getRoot: EGraphT => EClassCall,
                                                                                         setRoot: (EGraphT, EClassCall) => EGraphT,
                                                                                         areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean)
  extends Strategy[NodeT, EGraphT, Option[Tree[NodeT]]] {
  
  override def initialData: Option[Tree[NodeT]] = None

  override def apply(egraph: EGraphT, data: Option[Tree[NodeT]], parallelize: ParallelMap): (Option[EGraphT], Option[Tree[NodeT]]) = {
    val oldRoot = getRoot(egraph)

    val newTree = extractor(oldRoot, egraph)
    if (data.exists(areEquivalent(_, newTree))) {
      // No change in the extracted tree, so no need to rebase
      return (None, data)
    }

    val emptyGraph = egraph.emptied
    val (newRoot, newGraph) = emptyGraph.add(newTree)

    (Some(setRoot(newGraph, newRoot)), Some(newTree))
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
   * @param areEquivalent A function to determine if two trees are equivalent. If a newly extracted tree is equivalent
   *                      to the previously extracted tree, no rebasing is performed. Defaults to always returning false.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @return A [[Rebase]] strategy that operates on an [[EGraphWithRoot]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](extractor: Extractor[NodeT, EGraphT],
                                                                             areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (_: Tree[NodeT], _: Tree[NodeT]) => false): Rebase[NodeT, EGraphWithRoot[NodeT, EGraphT]] = {
    new Rebase(
      new Extractor[NodeT, EGraphWithRoot[NodeT, EGraphT]] {
        override def apply(call: EClassCall, egraph: EGraphWithRoot[NodeT, EGraphT]): Tree[NodeT] = {
          extractor(call, egraph.egraph)
        }
      },
      (egraph: EGraphWithRoot[NodeT, EGraphT]) =>
        egraph.root.getOrElse(throw new IllegalStateException("Root is not set in EGraphWithRoot")),
      (egraph: EGraphWithRoot[NodeT, EGraphT], root: EClassCall) => egraph.withRoot(root),
      areEquivalent)
  }
}
