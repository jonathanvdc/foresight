package foresight.util.ordering

/**
 * Provides lexicographical ordering for sequences.
 *
 * This object contains an implicit method to create an `Ordering[Seq[A]]` given an `Ordering[A]`, allowing sequences
 * to be compared elementwise.
 */
object SeqOrdering {
  /**
   * Returns a lexicographical ordering for sequences of type `A`.
   *
   * @param ord the ordering for elements of type `A`
   * @tparam A the element type of the sequence
   * @return an ordering for `Seq[A]` based on the provided element ordering
   */
  implicit def lexOrdering[A](implicit ord: Ordering[A]): Ordering[Seq[A]] = {
    new Ordering[Seq[A]] {
      override def compare(x: Seq[A], y: Seq[A]): Int = {
        Ordering.Iterable(ord).compare(x, y)
      }
    }
  }
}
