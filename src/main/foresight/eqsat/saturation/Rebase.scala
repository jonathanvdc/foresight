package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A strategy that rebases the e-graph by extracting a tree from the e-graph, subsequently adding that tree to a new
 * e-graph.
 *
 * @param extractor The extractor to use for extracting the tree.
 * @param findRoot A function to find the root of the e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the strategy operates on.
 */
final case class Rebase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](extractor: Extractor[NodeT, EGraphT],
                                                                                         findRoot: EGraphT => EClassCall) extends Strategy[EGraphT, Option[EClassCall]] {
  /**
   * The initial data for the strategy's first iteration.
   */
  override def initialData: Option[EClassCall] = None

  override def apply(egraph: EGraphT, data: Option[EClassCall], parallelize: ParallelMap): (Option[EGraphT], Option[EClassCall]) = {
    val oldRoot = data match {
      case Some(root) if egraph.contains(root.ref) => root
      case None => findRoot(egraph)
    }

    val tree = extractor(oldRoot, egraph)

    val emptyGraph = egraph.emptied
    val (newRoot, newGraph) = emptyGraph.add(tree)

    Some(newGraph) -> Some(newRoot)
  }
}
