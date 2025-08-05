package foresight.eqsat.extraction

import foresight.eqsat.{Slot, SlotMap, Tree}

/**
 * A cost function for extraction analyses.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam C The type of the cost.
 */
trait CostFunction[NodeT, C] {
  /**
   * Applies the cost function to a tree.
   * @param nodeType The type of the node.
   * @param definitions The slots of the node that are defined by the node itself.
   * @param uses The slots of the node that are used by the node and are defined elsewhere.
   * @param args The children of the node.
   * @return The cost of the tree.
   */
  def apply(nodeType: NodeT,
            definitions: Seq[Slot],
            uses: Seq[Slot],
            args: Seq[ExtractionTreeCall[NodeT, C]]): C

  /**
   * Applies the cost function to a tree.
   * @param tree The tree to which the cost function is applied.
   * @return The cost of the tree.
   */
  final def apply(tree: Tree[NodeT]): C = {
    toExtractionTree(tree).cost
  }

  private final def toExtractionTreeCall(tree: Tree[NodeT]): ExtractionTreeCall[NodeT, C] = {
    val extractionTree = toExtractionTree(tree)
    ExtractionTreeCall(
      extractionTree,
      SlotMap.identity(extractionTree.slotSet))
  }

  private final def toExtractionTree(tree: Tree[NodeT]): ExtractionTree[NodeT, C] = {
    val args = tree.args.map(toExtractionTreeCall)
    ExtractionTree(
      apply(tree.nodeType, tree.definitions, tree.uses, args),
      tree.nodeType,
      tree.definitions,
      tree.uses,
      args)
  }
}
