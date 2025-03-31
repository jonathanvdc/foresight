package foresight.eqsat.extraction

import foresight.eqsat.Slot

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
}
