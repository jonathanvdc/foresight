package foresight.eqsat

/**
 * An application of an e-class reference to arguments. These arguments come in the form of a slot map, where the keys
 * are the parameter slots of the e-class and the values are the arguments.
 *
 * @param ref The reference to the e-class.
 * @param args The arguments applied to the e-class, described as a slot map. The slot map's keys are the e-class's
 *             parameter slots and the values are the arguments.
 */
final case class EClassCall(ref: EClassRef, args: SlotMap) {
  /**
   * Gets the slots used by the applied reference. These are the slots used by the applied reference's arguments, not
   * parameter slots of the e-class reference.
   * @return The slots used by the applied reference.
   */
  def slots: Seq[Slot] = args.values

  /**
   * Gets the argument slots of the applied reference.
   * @return The argument slots of the applied reference.
   */
  def slotSet: Set[Slot] = args.valueSet

  /**
   * Renames the argument slots of the applied reference.
   * @param renaming The renaming of the argument slots.
   * @return The applied reference with the arguments renamed.
   */
  def rename(renaming: SlotMap): EClassCall = {
    assert(args.valueSet.subsetOf(renaming.keySet), "Argument slots must be in the renaming.")
    EClassCall(ref, args.composePartial(renaming))
  }

  /**
   * Renames the argument slots of the e-class application. Argument slots that are not in the renaming are dropped.
   * @param renaming The renaming of the argument slots.
   * @return The applied e-class with the arguments renamed.
   */
  def renamePartial(renaming: SlotMap): EClassCall = {
    EClassCall(ref, args.composePartial(renaming))
  }

  /**
   * Checks if the applied e-class is well-formed in the given e-graph. An applied e-class is well-formed if all
   * the class's slots appear as keys in the argument slot map.
   * @param egraph The e-graph in which to check the e-class application.
   * @return True if the application is well-formed in the e-graph; false otherwise.
   */
  def isWellFormed(egraph: EGraph[_]): Boolean = {
    val slots = egraph.canonicalize(ref).args.valueSet
    slots.subsetOf(args.keySet)
  }
}
