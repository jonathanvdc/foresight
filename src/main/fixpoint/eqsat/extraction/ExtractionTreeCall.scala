package fixpoint.eqsat.extraction

import fixpoint.eqsat.SlotMap

/**
 * A renamed extraction tree.
 * @param tree The extraction tree that is renamed.
 * @param renaming The renaming of the slots in the tree.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam C The type of the cost.
 */
final case class ExtractionTreeCall[+NodeT, C](tree: ExtractionTree[NodeT, C], renaming: SlotMap) {
  /**
   * Gets the cost of the tree.
   * @return The cost of the tree.
   */
  def cost: C = tree.cost

  /**
   * Gets the size of the tree.
   * @return The size of the tree.
   */
  def size: Int = tree.size

  /**
   * Gets the depth of the tree.
   * @return The depth of the tree.
   */
  def depth: Int = tree.depth
}
