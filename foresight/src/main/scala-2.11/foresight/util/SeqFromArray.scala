package foresight.util

/**
 * Utility object for converting arrays to sequences.
 */
object SeqFromArray {
  /** A type alias for the result of wrapping an array in a sequence. */
  type Seq[+T] = scala.collection.Seq[T]

  /**
   * Converts an array to a sequence.
   *
   * @param arr The array to convert.
   * @tparam T The type of the elements in the array.
   * @return A sequence containing the elements of the array.
   */
  def apply[T](arr: Array[T]): Seq[T] = {
    // Scala 2.12 and earlier don't have ArraySeq, so we use the implicit conversion
    // from Array to Seq.
    arr
  }
}
