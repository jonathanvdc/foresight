package fixpoint.eqsat.rewriting.patterns

/**
 * A slot in a pattern.
 */
final class SlotVar

/**
 * A companion object for slots.
 */
object SlotVar {
  /**
   * Creates a fresh slot variable.
   * @return The fresh slot variable.
   */
  def fresh(): SlotVar = new SlotVar
}
