package fixpoint.eqsat.extraction

import fixpoint.eqsat.{ENode, SlotMap}
import fixpoint.eqsat.metadata.Analysis

/**
 * An analysis that produces extraction trees with minimal cost.
 * @param cost The cost function for the analysis.
 * @param costOrdering The ordering for the cost.
 * @param nodeOrdering An ordering for the nodes.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam C The type of the cost.
 */
final case class ExtractionAnalysis[NodeT, C](cost: CostFunction[NodeT, C])
                                             (implicit costOrdering: Ordering[C],
                                              nodeOrdering: Ordering[NodeT]) extends Analysis[NodeT, ExtractionTreeCall[NodeT, C]] {

  /**
   * Renames the slots in an analysis result.
   *
   * @param result   The analysis result to rename.
   * @param renaming The renaming to apply to the slots. The keys of the map are the slots as they appear in the result,
   *                 and the values are the slots to which they are renamed.
   * @return The analysis result with the slots renamed.
   */
  override def rename(result: ExtractionTreeCall[NodeT, C], renaming: SlotMap): ExtractionTreeCall[NodeT, C] = {
    ExtractionTreeCall(result.tree, result.renaming.compose(renaming))
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
      SlotMap.identity(node.slots.toSet))
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
