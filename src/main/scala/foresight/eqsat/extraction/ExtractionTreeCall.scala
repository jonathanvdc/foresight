package foresight.eqsat.extraction

import foresight.eqsat.{Slot, SlotMap}

/**
 * A renamed extraction tree.
 *
 * @param tree The extraction tree that is renamed.
 * @param renaming The renaming of the slots in the tree.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam C The type of the cost.
 */
final case class ExtractionTreeCall[+NodeT, C](tree: ExtractionTree[NodeT, C], renaming: SlotMap) {
  assert(tree.slotSet.forall(renaming.contains))

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

  /**
   * The slots of the tree, in the order in which they appear, after applying the renaming.
   * @return The slots of the tree.
   */
  def slots: Seq[Slot] = tree.slots.map(renaming(_))

  /**
   * The set of slots of the tree after applying the renaming.
   * @return The slots of the tree.
   */
  def slotSet: Set[Slot] = tree.slotSet.map(renaming(_))

  /**
   * Renames the slots in the tree.
   * @param renaming The renaming of the slots. The keys of the map are the slots as they appear in the tree, and the
   *                 values are the slots to which they are renamed.
   * @return The tree with the slots renamed.
   */
  def rename(renaming: SlotMap): ExtractionTreeCall[NodeT, C] = {
    assert(this.renaming.valueSet.forall(renaming.contains))
    ExtractionTreeCall(tree, this.renaming.composePartial(renaming))
  }

  /**
   * The tree with the renaming applied.
   * @return The tree with the renaming applied.
   */
  def applied: ExtractionTree[NodeT, C] = {
    val newDefinitions = tree.definitions.map(renaming.apply)
    val newUses = tree.uses.map(renaming.apply)
    val newArgs = tree.args.map(_.rename(renaming))
    ExtractionTree(tree.cost, tree.nodeType, newDefinitions, newUses, newArgs)
  }
}
