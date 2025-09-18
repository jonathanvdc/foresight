package foresight.util

import scala.collection.immutable.ArraySeq

/**
 * Utility object for converting arrays to sequences.
 */
object SeqFromArray {
  /** A type alias for the result of wrapping an array in a sequence. */
  type Seq[+T] = ArraySeq[T]

  /**
   * Converts an array to a sequence.
   *
   * @param arr The array to convert.
   * @tparam T The type of the elements in the array.
   * @return A sequence containing the elements of the array.
   */
  def apply[T](arr: Array[T]): Seq[T] = ArraySeq.unsafeWrapArray(arr)
}
