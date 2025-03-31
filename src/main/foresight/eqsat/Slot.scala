package foresight.eqsat

/**
 * A slot in an e-graph. Slots are unique identifiers for variables in the e-graph.
 */
sealed trait Slot extends Ordered[Slot]

/**
 * Companion object for the Slot trait.
 */
object Slot {
  /**
   * A numbered slot. All slots with the same number are considered equivalent.
   * @param n The number of the slot.
   */
  private final case class NumberedSlot(n: Int) extends Slot {
    override def compare(that: Slot): Int = that match {
      case NumberedSlot(m) => n compare m
      case _: UniqueSlot => -1
    }
  }

  /**
   * A unique slot. Each unique slot is considered distinct from all other slots.
   */
  private final class UniqueSlot extends Slot {
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
