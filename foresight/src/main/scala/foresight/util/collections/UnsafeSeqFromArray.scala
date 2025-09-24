package foresight.util.collections

import scala.collection.compat._
import scala.reflect.ClassTag

/**
 * Utility object for converting arrays to sequences.
 */
private[foresight] object UnsafeSeqFromArray {
  /**
   * Converts an array to a sequence.
   *
   * @param arr The array to convert.
   * @tparam T The type of the elements in the array.
   * @return A sequence containing the elements of the array.
   */
  def apply[T](arr: Array[T]): immutable.ArraySeq[T] = {
    immutable.ArraySeq.unsafeWrapArray(arr)
  }

  /**
   * Converts a sequence to an immutable ArraySeq.
   *
   * @param seq The sequence to convert.
   * @tparam T The type of the elements in the sequence.
   * @return An immutable ArraySeq containing the elements of the sequence.
   */
  def apply[T: ClassTag](seq: Seq[T]): immutable.ArraySeq[T] = {
    immutable.ArraySeq.unsafeWrapArray(seq.toArray)
  }
}
