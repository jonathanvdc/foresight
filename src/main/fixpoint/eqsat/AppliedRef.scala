package fixpoint.eqsat

import fixpoint.eqsat.slots.SlotMap

/**
 * A reference to an e-class with applied arguments.
 *
 * @param ref The reference to the e-class.
 * @param args The arguments applied to the e-class, described as a slot map.
 */
final case class AppliedRef(ref: EClassRef, args: SlotMap)
