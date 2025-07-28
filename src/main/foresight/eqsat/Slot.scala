package foresight.eqsat

/**
 * A slot in an e-graph. Slots are unique identifiers for variables in the e-graph.
 */
sealed trait Slot extends Ordered[Slot] {
  /**
   * Tests whether this slot is a numbered slot.
   * Numbered slots are equivalent if they have the same number.
   * Unique slots are always distinct.
   * @return True if this slot is a numbered slot, false otherwise.
   */
  final def isNumbered: Boolean = this match {
    case Slot.NumberedSlot(_) => true
    case _: Slot.UniqueSlot => false
  }
}

/**
 * Companion object for the Slot trait.
 */
object Slot {
  /**
   * A numbered slot. All slots with the same number are considered equivalent.
   * @param n The number of the slot.
   */
  final case class NumberedSlot(n: Int) extends Slot {
    override def compare(that: Slot): Int = that match {
      case NumberedSlot(m) => n compare m
      case _: UniqueSlot => -1
    }
  }

  /**
   * A unique slot. Each unique slot is considered distinct from all other slots.
   */
  final class UniqueSlot extends Slot {
    override def compare(that: Slot): Int = that match {
      case _: NumberedSlot => 1
      case _: UniqueSlot => hashCode() compare that.hashCode()
    }
  }

  /**
   * Creates a new unique slot.
   * @return A new unique slot.
   */
  def fresh(): Slot = new UniqueSlot

  /**
   * Creates a new numbered slot.
   * @param n The number of the slot.
   * @return A new numbered slot.
   */
  def numeric(n: Int): Slot = NumberedSlot(n)
}
