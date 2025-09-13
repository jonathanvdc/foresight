package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.PortableMatch
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, Tree}

/**
 * A compound strategy that transforms an e-graph and optionally rebases it by extracting and reinserting a tree.
 *
 * This strategy wraps a transformation strategy (`transform`) and augments it with **tree rebasing semantics**:
 *
 *   1. The inner strategy is first applied to the e-graph.
 *   2. If it produces no change, the strategy halts and rebasing is skipped.
 *   3. If it does change the e-graph, the new tree rooted at a given [[foresight.eqsat.EClassCall]] is extracted.
 *   4. If the newly extracted tree is equivalent (according to `areEquivalent`) to the previous one, rebasing is skipped.
 *   5. Otherwise, the new tree is added to an empty e-graph, producing a new root and a freshly rooted e-graph.
 *
 * This approach is especially useful in fixpoint-style optimization loops, where you want to:
 *   - Track convergence at the level of extracted trees, not just internal graph structure.
 *   - Avoid redundant re-extractions and expansions.
 *   - Restart from a canonical or minimal representative tree when changes occur.
 *
 * @param transform The core transformation strategy to apply before rebasing.
 * @param extractor A mechanism for extracting a [[foresight.eqsat.Tree]] from an e-graph at a given root.
 * @param getRoot A function that extracts the [[foresight.eqsat.EClassCall]] representing the current root of the e-graph.
 * @param setRoot A function that inserts a new tree root into an e-graph.
 * @param areEquivalent A predicate for determining whether two trees are considered the same.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph, usually a wrapper like [[EGraphWithRoot]] or
 *                 [[foresight.eqsat.metadata.EGraphWithMetadata]].
 * @tparam Data The internal state type carried by the transformation strategy.
 */
final case class TransformAndRebase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[NodeT, EGraphT, Data],
                                                                                                           extractor: Extractor[NodeT, EGraphT],
                                                                                                           getRoot: EGraphT => EClassCall,
                                                                                                           setRoot: (EGraphT, EClassCall) => EGraphT,
                                                                                                           areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean) extends Strategy[NodeT, EGraphT, (Data, Option[Tree[NodeT]])] {
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
 * Factory methods for constructing [[TransformAndRebase]] strategies with different e-graph wrappers.
 *
 * These helpers simplify creation of rebasing strategies over common e-graph variants, including:
 *   - [[EGraphWithRoot]]
 *   - [[foresight.eqsat.metadata.EGraphWithMetadata]]
 *   - [[EGraphWithRecordedApplications]]
 *   - combinations of the above
 *
 * In all cases, the behavior is:
 *   1. Apply a strategy to the e-graph.
 *   2. If changed, extract a tree and rebase to an empty e-graph if the new tree differs from the old.
 *   3. Return a new rooted e-graph or skip rebasing as needed.
 */
object TransformAndRebase {

  /**
   * Creates a [[TransformAndRebase]] strategy for an [[EGraphWithRoot]].
   *
   * @param transform A strategy operating on [[EGraphWithRoot]].
   * @param extractor A tree extractor operating on the inner [[foresight.eqsat.EGraph]].
   * @param areEquivalent Optional function for tree equivalence. Defaults to `==`.
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], Data],
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
   * Creates a [[TransformAndRebase]] strategy for an [[EGraphWithRoot]].
   *
   * @param transform A strategy operating on [[EGraphWithRoot]].
   * @param extractor A tree extractor operating on the inner [[foresight.eqsat.EGraph]].
   */
  def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[NodeT, EGraphWithRoot[NodeT, EGraphT], Data],
                                                                                   extractor: Extractor[NodeT, EGraphT]): TransformAndRebase[NodeT, EGraphWithRoot[NodeT, EGraphT], Data] = {
    apply(transform, extractor, (x: Tree[NodeT], y: Tree[NodeT]) => x == y)
  }

  /**
   * Creates a [[TransformAndRebase]] strategy over [[foresight.eqsat.metadata.EGraphWithMetadata]] containing an
   * [[EGraphWithRoot]].
   *
   * This overload adds support for e-graph metadata and allows rebasing while preserving analyses.
   *
   * @param transform Strategy operating on metadata-enriched rooted e-graphs.
   * @param extractor Tree extractor operating on the same enriched graph.
   * @param areEquivalent Optional function for tree equivalence. Defaults to `==`.
   */
  def withMetadata[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data],
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
   * Creates a [[TransformAndRebase]] strategy over [[foresight.eqsat.metadata.EGraphWithMetadata]] containing an
   * [[EGraphWithRoot]].
   *
   * This overload adds support for e-graph metadata and allows rebasing while preserving analyses.
   *
   * @param transform Strategy operating on metadata-enriched rooted e-graphs.
   * @param extractor Tree extractor operating on the same enriched graph.
   */
  def withMetadata[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](transform: Strategy[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data],
                                                                                          extractor: Extractor[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]]]): TransformAndRebase[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Data] = {
    withMetadata(transform, extractor, (x: Tree[NodeT], y: Tree[NodeT]) => x == y)
  }

  /**
   * Creates a [[TransformAndRebase]] strategy over [[EGraphWithRecordedApplications]] containing
   * an [[foresight.eqsat.metadata.EGraphWithMetadata]] and [[EGraphWithRoot]].
   *
   * This version records match applications, supports metadata, and operates on rooted graphs.
   *
   * @param transform Strategy operating on a deeply wrapped e-graph.
   * @param extractor Tree extractor over metadata-rooted e-graphs.
   * @param areEquivalent Optional function for tree equivalence. Defaults to `==`.
   */
  def withRecording[NodeT,
                    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                    Match <: PortableMatch[NodeT, Match],
                    Data](transform: Strategy[NodeT, EGraphWithRecordedApplications[NodeT, EGraphWithMetadata[NodeT, EGraphWithRoot[NodeT, EGraphT]], Match], Data],
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
