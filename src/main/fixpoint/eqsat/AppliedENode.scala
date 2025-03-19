package fixpoint.eqsat

import fixpoint.eqsat.slots.{Slot, SlotMap}

/**
 * An e-node whose slots have been renamed.
 * @param renaming The renaming of the slots. The keys of the map are the slots as they appear in node, and the values
 *                 are the slots to which they are renamed.
 * @param node The e-node.
 * @tparam NodeT The type of the e-node.
 */
final case class AppliedENode[+NodeT](renaming: SlotMap, node: ENode[NodeT]) {
  /**
   * Gets the node type of the e-node.
   * @return The node type of the e-node.
   */
  def nodeType: NodeT = node.nodeType

  /**
   * Gets the slots used by the e-node after renaming.
   * @return The slots used by the e-node.
   */
  def slots: Seq[Slot] = applied.publicSlots

  /**
   * Gets the arguments of the e-node after renaming.
   * @return The arguments of the e-node.
   */
  def args: Seq[AppliedRef] = applied.args

  /**
   * Gets the e-node with the slots renamed according to the renaming.
   * @return The e-node with the slots renamed.
   */
  def applied: ENode[NodeT] = node.rename(renaming)

  /**
   * Composes the renaming with another renaming.
   *
   * @param renaming The new renaming with which to compose the original renaming.
   * @return The e-node application that results from composing the renaming with another renaming.
   */
  def rename(renaming: SlotMap): AppliedENode[NodeT] = {
    val newRenaming = this.renaming.compose(renaming)
    AppliedENode(newRenaming, node)
  }
}
