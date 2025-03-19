package fixpoint.eqsat

import fixpoint.eqsat.slots.{Slot, SlotMap}

/**
 * A reference to an e-class with applied arguments.
 *
 * @param ref The reference to the e-class.
 * @param args The arguments applied to the e-class, described as a slot map. The slot map's keys are the e-class's
 *             parameter slots and the values are the arguments.
 */
final case class AppliedRef(ref: EClassRef, args: SlotMap) {
  /**
   * Gets the slots used by the applied reference. These are the slots used by the applied reference's arguments, not
   * parameter slots of the e-class reference.
   * @return The slots used by the applied reference.
   */
  def slots: Set[Slot] = args.values

  /**
   * Renames the argument slots of the applied reference.
   * @param renaming The renaming of the argument slots.
   * @return The applied reference with the arguments renamed.
   */
  def rename(renaming: SlotMap): AppliedRef = {
    assert(args.values.subsetOf(renaming.keys), "Argument slots must be in the renaming.")
    AppliedRef(ref, args.composePartial(renaming))
  }
}
