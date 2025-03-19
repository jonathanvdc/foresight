package fixpoint.eqsat

import fixpoint.eqsat.slots.{Slot, SlotMap}

/**
 * An e-node in an e-graph. E-nodes are used to represent expressions in an e-graph. Each e-node has a node type and a
 * sequence of e-class references that represent the arguments of the e-node.
 *
 * @param nodeType The node type of the e-node.
 * @param privateSlots The slots used by the e-node that are not visible outside the e-node. These slots are redundant
 *                     by construction. Their purpose is to allow e-nodes to introduce fresh variable bindings, such as
 *                     in let expressions or lambda abstractions.
 * @param publicSlots The slots used by the e-node that are visible outside the e-node. These slots refer to variables
 *                    defined elsewhere.
 * @param args The arguments of the e-node.
 * @tparam NodeT The type of the node that the e-node represents. Node types contain all information required to form
 *               an expression aside from its slots and arguments.
 */
final case class ENode[+NodeT](nodeType: NodeT, privateSlots: Seq[Slot], publicSlots: Seq[Slot], args: Seq[AppliedRef]) {
  /**
   * Gets all slots used by the e-node, including the slots used by its arguments.
   * @return The set of slots used by the e-node.
   */
  def allSlots: Seq[Slot] = privateSlots ++ publicSlots ++ args.flatMap(_.args.values)

  /**
   * Gets all distinct slots used by the e-node, including the slots used by its arguments. Distinct slots appear in
   * the list in the same order as they appear in the slots and args.
   * @return The set of distinct slots used by the e-node.
   */
  def distinctSlots: Seq[Slot] = allSlots.distinct

  /**
   * Renames the slots of the e-node according to a renaming.
   * @param renaming The renaming of the slots. The keys of the map are the slots as they appear in the e-node, and the
   *                 values are the slots to which they are renamed.
   * @return The e-node with the slots renamed.
   */
  def rename(renaming: SlotMap): ENode[NodeT] = {
    val newPrivateSlots = privateSlots.map(renaming.apply)
    val newPublicSlots = publicSlots.map(renaming.apply)
    val newArgs = args.map(ref => ref.copy(args = ref.args.compose(renaming)))
    copy(privateSlots = newPrivateSlots, publicSlots = newPublicSlots, args = newArgs)
  }

  /**
   * Gets the e-node with the slots renamed to numeric slots in lexicographical order. The shape is augmented with a
   * slot map that maps the renamed slots to the original slots.
   *
   * @return The e-node with the slots renamed.
   */
  def shape: AppliedENode[NodeT] = {
    val renamedSlots = SlotMap(distinctSlots.zipWithIndex.map(p => p._1 -> Slot.numeric(p._2)).toMap)
    AppliedENode(renamedSlots.inverse, rename(renamedSlots))
  }

  /**
   * Determines whether the e-node is a shape.
   * @return True if the e-node is a shape; otherwise, false.
   */
  def isShape: Boolean = this == shape.node
}

/**
 * The companion object of the e-node class.
 */
object ENode {
  /**
   * Creates an e-node with no slots.
   * @param nodeType The node type of the e-node.
   * @param args The arguments of the e-node.
   * @tparam NodeT The type of the node that the e-node represents.
   * @return The e-node with the given node type and arguments.
   */
  def unslotted[NodeT](nodeType: NodeT, args: Seq[AppliedRef]): ENode[NodeT] = {
    ENode(nodeType, Seq.empty, Seq.empty, args)
  }
}
