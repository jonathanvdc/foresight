package foresight.eqsat.saturation

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, Tree}
import foresight.eqsat.extraction.Extractor
import foresight.eqsat.parallel.ParallelMap

/**
 * A strategy that first transforms the e-graph using a given strategy and then rebases the e-graph by extracting a
 * tree from the e-graph, subsequently adding that tree to a new e-graph. If the transformation does not change the
 * e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree and checks if it is
 * equivalent to the previously extracted tree. If it is, the e-graph is left unchanged and not rebased; otherwise, it
 * adds the new tree to an empty e-graph and returns the new e-graph with the new root.
 *
 * @param transform The strategy to apply to the e-graph before rebasing.
 * @param extractor The extractor to use for extracting the tree.
 * @param getRoot A function to find the root of the e-graph.
 * @param setRoot A function to create a new e-graph with the specified root.
 * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the strategy operates on.
 */
final case class TransformAndRebase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphT, Data],
                                                                                                           extractor: Extractor[NodeT, EGraphT],
                                                                                                           getRoot: EGraphT => EClassCall,
                                                                                                           setRoot: (EGraphT, EClassCall) => EGraphT,
                                                                                                           areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean) extends Strategy[EGraphT, (Data, Option[Tree[NodeT]])] {
  override def initialData: (Data, Option[Tree[NodeT]]) = (transform.initialData, None)

  override def apply(egraph: EGraphT, data: (Data, Option[Tree[NodeT]]), parallelize: ParallelMap): (Option[EGraphT], (Data, Option[Tree[NodeT]])) = {
    val (newEGraph, newInnerData) = transform.apply(egraph, data._1, parallelize)

    newEGraph match {
      case None =>
        // If the transformation did not change the e-graph, we can skip rebasing.
        (None, (newInnerData, data._2))

      case Some(newEGraph) =>
        // If the transformation changed the e-graph, we need to rebase. First, we determine the old root and the old
        // tree. Then, we extract the new tree from the new e-graph. If the new tree is equivalent to the old tree,
        // we can skip rebasing and determine that the e-graph has not changed. Otherwise, we add the new tree to an
        // empty e-graph and return the new e-graph with the new root.
        val oldRoot = getRoot(egraph)
        val oldTree = data._2
        val newTree = extractor(oldRoot, newEGraph)

        if (oldTree.isDefined && areEquivalent(oldTree.get, newTree)) {
          // If the tree is equivalent to the previously extracted tree, we can skip rebasing.
          return (None, (newInnerData, oldTree))
        }

        val emptyGraph = newEGraph.emptied
        val (newRoot, newGraph) = emptyGraph.add(newTree)
        (Some(setRoot(newGraph, newRoot)), (newInnerData, Some(newTree)))
    }
  }
}

/**
 * A companion object for the [[TransformAndRebase]] strategy that provides a convenient way to create a
 * [[TransformAndRebase]] instance operating on an [[EGraphWithRoot]].
 */
object TransformAndRebase {
  /**
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]] using the provided extractor.
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphWithRoot[NodeT, EGraphT], Data],
                                                                                   extractor: Extractor[NodeT, EGraphT],
                                                                                   areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): TransformAndRebase[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] = {
    new TransformAndRebase(
      transform,
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
