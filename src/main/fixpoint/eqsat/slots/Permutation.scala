package fixpoint.eqsat.slots

/**
 * A permutation of slots.
 * @tparam Repr The type of the permutation.
 */
trait Permutation[Repr <: Permutation[Repr]] {
  /**
   * Composes this permutation with another permutation.
   * @param other The other permutation.
   * @return The composition of the two permutations.
   */
  def compose(other: Repr): Repr

  /**
   * Gets the inverse of this permutation.
   * @return The inverse of this permutation.
   */
  def inverse: Repr

  /**
   * Applies the permutation to a slot.
   * @param slot The slot to apply the permutation to.
   * @return The slot after the permutation is applied.
   */
  def apply(slot: Slot): Slot

  /**
   * Iterates over the slots in the permutation.
   * @return An iterator over the slots in the permutation.
   */
  def iterator: Iterator[(Slot, Slot)]
}
