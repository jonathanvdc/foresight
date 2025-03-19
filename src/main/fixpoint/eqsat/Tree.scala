package fixpoint.eqsat

import fixpoint.eqsat.slots.Slot

/**
 * A tree data structure.
 *
 * @param nodeType The type of the node.
 * @param slots The slots of the node.
 * @param args The children of the node.
 * @tparam NodeT The type of the node.
 */
final case class Tree[NodeT](nodeType: NodeT, slots: Seq[Slot], args: Seq[Tree[NodeT]])

/**
 * A companion object for the tree data structure.
 */
object Tree {
  /**
   * Creates a new tree with no slots.
   * @param nodeType The type of the node.
   * @param args The children of the node.
   * @tparam NodeT The type of the node.
   * @return A new tree with no slots.
   */
  def unslotted[NodeT](nodeType: NodeT, args: Seq[Tree[NodeT]]): Tree[NodeT] = Tree(nodeType, Seq.empty, args)
}
