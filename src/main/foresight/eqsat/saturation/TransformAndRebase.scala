package foresight.eqsat.saturation

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, Tree}
import foresight.eqsat.extraction.Extractor
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.PortableMatch

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
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]].
   * A transformation strategy is applied to the e-graph, and then the e-graph is rebased by extracting a tree
   * from the e-graph and adding it to a new e-graph with a new root. If the transformation does not change
   * the e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree
   * and checks if it is equivalent to the previously extracted tree. If it is, the e-graph is left
   * unchanged and not rebased; otherwise, it adds the new tree to an empty e-graph and returns the new
   * e-graph with the new root.
   *
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @param areEquivalent A function to check if two trees are equivalent.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Data The type of data that the transformation strategy operates on. This can be used to carry additional
   *              information during the transformation process.
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphWithRoot[NodeT, EGraphT], Data],
                                                                                   extractor: Extractor[NodeT, EGraphT],
                                                                                   areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean): TransformAndRebase[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] = {
    new TransformAndRebase(
      transform,
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

  /**
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]].
   * A transformation strategy is applied to the e-graph, and then the e-graph is rebased by extracting a tree
   * from the e-graph and adding it to a new e-graph with a new root. If the transformation does not change
   * the e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree
   * and checks if it is equivalent to the previously extracted tree. If it is, the e-graph is left
   * unchanged and not rebased; otherwise, it adds the new tree to an empty e-graph and returns the new
   * e-graph with the new root.
   *
   * This version uses structural equality to check if two trees are equivalent.
   *
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithRoot]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphWithRoot[NodeT, EGraphT], Data],
                                                                                   extractor: Extractor[NodeT, EGraphT]): TransformAndRebase[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] = {
    apply(transform, extractor, (x: Tree[NodeT], y: Tree[NodeT]) => x == y)
  }

  /**
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   * A transformation strategy is applied to the e-graph, and then the e-graph is rebased by extracting a tree
   * from the e-graph and adding it to a new e-graph with a new root. If the transformation does not change
   * the e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree
   * and checks if it is equivalent to the previously extracted tree. If it is, the e-graph is left
   * unchanged and not rebased; otherwise, it adds the new tree to an empty e-graph and returns the new
   * e-graph with the new root.
   *
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Data The type of data that the transformation strategy operates on. This can be used to carry additional
   *              information during the transformation process.
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   */
  def withMetadata[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data],
                                                                                          extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                                                                                          areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean): TransformAndRebase[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data] = {
      new TransformAndRebase(
        transform,
        extractor,
        (egraph: EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]) =>
          egraph.egraph.root.getOrElse(throw new IllegalStateException("Root is not set in EGraphWithRoot")),
        (egraph: EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], root: EClassCall) => egraph.migrateTo(egraph.egraph.withRoot(root)),
        areEquivalent)
  }

  /**
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   * A transformation strategy is applied to the e-graph, and then the e-graph is rebased by extracting a tree
   * from the e-graph and adding it to a new e-graph with a new root. If the transformation does not change
   * the e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree
   * and checks if it is equivalent to the previously extracted tree. If it is, the e-graph is left
   * unchanged and not rebased; otherwise, it adds the new tree to an empty e-graph and returns the new
   * e-graph with the new root.
   *
   * This version uses structural equality to check if two trees are equivalent.
   *
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   */
  def withMetadata[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data],
                                                                                          extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]]): TransformAndRebase[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data] = {
    withMetadata(transform, extractor, (x: Tree[NodeT], y: Tree[NodeT]) => x == y)
  }

  /**
   * Creates a [[TransformAndRebase]] strategy that operates on an [[EGraphWithRecordedApplications]] of an
   * [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   * A transformation strategy is applied to the e-graph, and then the e-graph is rebased by extracting a tree
   * from the e-graph and adding it to a new e-graph with a new root. If the transformation does not change
   * the e-graph, it skips rebasing. If the transformation changes the e-graph, it extracts a new tree
   * and checks if it is equivalent to the previously extracted tree. If it is, the e-graph is left
   * unchanged and not rebased; otherwise, it adds the new tree to an empty e-graph and returns the new
   * e-graph with the new root.
   *
   * @param transform The strategy to apply to the e-graph before rebasing.
   * @param extractor The extractor to use for extracting trees from e-class calls in the e-graph.
   * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the strategy operates on, which must be a subtype of both [[EGraphLike]] and [[EGraph]].
   * @tparam Match The type of matches produced by the transformation strategy.
   * @tparam Data The type of data that the transformation strategy operates on. This can be used to carry additional
   *              information during the transformation process.
   * @return A [[TransformAndRebase]] strategy that operates on an [[EGraphWithRecordedApplications]] of an
   *         [[EGraphWithMetadata]] of an [[EGraphWithRoot]].
   */
  def withRecording[NodeT,
                    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                    Match <: PortableMatch[NodeT, Match],
                    Data](transform: Strategy[EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], Data],
                          extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]],
                          areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean): TransformAndRebase[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], Data] = {
    new TransformAndRebase(
      transform,
      new Extractor[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match]] {
        override def apply(call: EClassCall, egraph: EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match]): Tree[NodeT] = {
          extractor(call, egraph.egraph)
        }
      },
      (egraph: EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match]) =>
        egraph.egraph.egraph.root.getOrElse(throw new IllegalStateException("Root is not set in EGraphWithRoot")),
      (egraph: EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], root: EClassCall) =>
        egraph.migrateTo(egraph.egraph.migrateTo(egraph.egraph.egraph.withRoot(root))),
      areEquivalent)
  }
}
