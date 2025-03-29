package fixpoint.eqsat

/**
 * An e-node in an e-graph. E-nodes are used to represent expressions in an e-graph. Each e-node has a node type and a
 * sequence of e-class references that represent the arguments of the e-node.
 *
 * @param nodeType The node type of the e-node.
 * @param definitions The slots used by the e-node that are not visible outside the e-node. These slots are
 *                    redundant by construction. Their purpose is to allow e-nodes to introduce variable bindings, such
 *                    as in let expressions or lambda abstractions.
 * @param uses The slots used by the e-node that are visible outside the e-node. These slots refer to variables
 *             defined elsewhere.
 * @param args The arguments of the e-node.
 * @tparam NodeT The type of the node that the e-node represents. Node types contain all information required to form
 *               an expression aside from its slots and arguments.
 */
final case class ENode[+NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[EClassCall]) {
  /**
   * Gets all slots used by the e-node: definition slots, used slots and slots from arguments.
   * @return The set of slots used by the e-node.
   */
  def slots: Seq[Slot] = definitions ++ uses ++ args.flatMap(_.args.values)

  /**
   * Renames the slots of the e-node according to a renaming.
   * @param renaming The renaming of the slots. The keys of the map are the slots as they appear in the e-node, and the
   *                 values are the slots to which they are renamed.
   * @return The e-node with the slots renamed.
   */
  def rename(renaming: SlotMap): ENode[NodeT] = {
    assert({
      val allSlots = slots.toSet
      renaming.keySet.forall(allSlots.contains)
    })

    val newDefinitions = definitions.map(renaming.apply)
    val newUses = uses.map(renaming.apply)
    val newArgs = args.map(call => call.copy(args = call.args.composeRetain(renaming)))
    copy(definitions = newDefinitions, uses = newUses, args = newArgs)
  }

  /**
   * Gets the e-node with the slots renamed to numeric slots in lexicographical order. The shape is augmented with a
   * slot map that maps the renamed slots to the original slots.
   *
   * @return The e-node with the slots renamed.
   */
  def asShapeCall: ShapeCall[NodeT] = {
    val renamedSlots = SlotMap(slots.distinct.zipWithIndex.map(p => p._1 -> Slot.numeric(p._2)).toMap)
    ShapeCall(rename(renamedSlots), renamedSlots.inverse)
  }

  /**
   * Determines whether the e-node is a shape.
   * @return True if the e-node is a shape; otherwise, false.
   */
  def isShape: Boolean = this == asShapeCall.shape
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
  def unslotted[NodeT](nodeType: NodeT, args: Seq[EClassCall]): ENode[NodeT] = {
    ENode(nodeType, Seq.empty, Seq.empty, args)
  }
}
