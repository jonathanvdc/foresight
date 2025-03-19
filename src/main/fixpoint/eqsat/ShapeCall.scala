package fixpoint.eqsat

import fixpoint.eqsat.slots.{Slot, SlotMap}

/**
 * An e-node that has been decomposed into a shape and a renaming map that assigns argument slots to the shape's
 * numeric slots.
 *
 * @param renaming The renaming of the slots. The keys of the map are the slots as they appear in node, and the values
 *                 are the slots to which they are renamed.
 * @param shape The e-node.
 * @tparam NodeT The type of the e-node.
 */
final case class ShapeCall[+NodeT](shape: ENode[NodeT], renaming: SlotMap) {
  /**
   * Gets the node type of the e-node.
   * @return The node type of the e-node.
   */
  def nodeType: NodeT = shape.nodeType

  /**
   * Gets the e-node's slot definitions after renaming.
   * @return The e-node's slot definitions.
   */
  def definitions: Seq[Slot] = asNode.definitions

  /**
   * Gets the e-node's slot uses after renaming.
   * @return The e-node's slot uses.
   */
  def uses: Seq[Slot] = asNode.uses

  /**
   * Gets the arguments of the e-node after renaming.
   * @return The arguments of the e-node.
   */
  def args: Seq[EClassCall] = asNode.args

  /**
   * Gets the e-node with the slots renamed according to the renaming.
   * @return The e-node with the slots renamed.
   */
  def asNode: ENode[NodeT] = shape.rename(renaming)

  /**
   * Composes the renaming with another renaming.
   *
   * @param renaming The new renaming with which to compose the original renaming.
   * @return The e-node application that results from composing the renaming with another renaming.
   */
  def rename(renaming: SlotMap): ShapeCall[NodeT] = {
    val newRenaming = this.renaming.compose(renaming)
    ShapeCall(shape, newRenaming)
  }
}
