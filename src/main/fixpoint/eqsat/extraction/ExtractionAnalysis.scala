package fixpoint.eqsat.extraction

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, ENode, Slot, SlotMap, Tree}
import fixpoint.eqsat.metadata.{Analysis, EGraphWithMetadata}

/**
 * An analysis that produces extraction trees with minimal cost.
 * @param name The name of the analysis.
 * @param cost The cost function for the analysis.
 * @param costOrdering The ordering for the cost.
 * @param nodeOrdering An ordering for the nodes.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam C The type of the cost.
 */
final case class ExtractionAnalysis[NodeT, C](name: String,
                                              cost: CostFunction[NodeT, C])
                                             (implicit costOrdering: Ordering[C],
                                              nodeOrdering: Ordering[NodeT])
  extends Analysis[NodeT, ExtractionTreeCall[NodeT, C]] {

  /**
   * The extractor corresponding to this extraction analysis.
   * @tparam Repr The type of the underlying e-graph.
   * @return The extractor for the analysis.
   */
  def extractor[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]]: Extractor[NodeT, EGraphWithMetadata[NodeT, Repr]] = {
    new Extractor[NodeT, EGraphWithMetadata[NodeT, Repr]] {
      override def apply(call: EClassCall, egraph: EGraphWithMetadata[NodeT, Repr]): Tree[NodeT] = {
        val extractionTree = get(egraph)(call, egraph)
        extractionTree.applied.toTree
      }
    }
  }

  /**
   * Renames the slots in an analysis result.
   *
   * @param result   The analysis result to rename.
   * @param renaming The renaming to apply to the slots. The keys of the map are the slots as they appear in the result,
   *                 and the values are the slots to which they are renamed.
   * @return The analysis result with the slots renamed.
   */
  override def rename(result: ExtractionTreeCall[NodeT, C], renaming: SlotMap): ExtractionTreeCall[NodeT, C] = {
    ExtractionTreeCall(result.tree, result.renaming.composePartial(renaming))
  }

  /**
   * Makes an analysis result for a node.
   *
   * @param node The node to make the analysis result for.
   * @param args The analysis results for the arguments to the node.
   * @return The analysis result for the node.
   */
  override def make(node: ENode[NodeT], args: Seq[ExtractionTreeCall[NodeT, C]]): ExtractionTreeCall[NodeT, C] = {
    val treeCost = cost(node.nodeType, node.definitions, node.uses, args)
    ExtractionTreeCall(
      ExtractionTree(treeCost, node.nodeType, node.definitions, node.uses, args),
      SlotMap.identity(node.slots.toSet -- node.definitions))
  }

  /**
   * Joins two analysis results.
   *
   * @param left  The left analysis result.
   * @param right The right analysis result.
   * @return The joined analysis result.
   */
  override def join(left: ExtractionTreeCall[NodeT, C],
                    right: ExtractionTreeCall[NodeT, C]): ExtractionTreeCall[NodeT, C] = {
    ExtractionTreeOrdering[NodeT, C]().callOrdering.min(left, right)
  }
}

/**
 * Companion object for the extraction analysis.
 */
object ExtractionAnalysis {
  /**
   * An extraction analysis that extracts the smallest expression tree for each e-class.
   *
   * @param nodeOrdering An ordering for the nodes.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return The extraction analysis.
   */
  def smallest[NodeT](implicit nodeOrdering: Ordering[NodeT]): ExtractionAnalysis[NodeT, Int] = {
    ExtractionAnalysis(
      "SmallestExtractionAnalysis",
      new CostFunction[NodeT, Int] {
        override def apply(nodeType: NodeT,
                           definitions: Seq[Slot],
                           uses: Seq[Slot],
                           args: Seq[ExtractionTreeCall[NodeT, Int]]): Int = {
          args.map(_.cost).sum + 1
        }
      })
  }

  /**
   * An extraction analysis that extracts the shallowest expression tree for each e-class.
   *
   * @param nodeOrdering An ordering for the nodes.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return The extraction analysis.
   */
  def shallowest[NodeT](implicit nodeOrdering: Ordering[NodeT]): ExtractionAnalysis[NodeT, Int] = {
    ExtractionAnalysis(
      "ShallowestExtractionAnalysis",
      new CostFunction[NodeT, Int] {
        override def apply(nodeType: NodeT,
                           definitions: Seq[Slot],
                           uses: Seq[Slot],
                           args: Seq[ExtractionTreeCall[NodeT, Int]]): Int = {
          args.map(_.cost).max + 1
        }
      })
  }
}
