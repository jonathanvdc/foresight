package foresight.eqsat.collections

import foresight.eqsat.Slot
import foresight.util.collections.AbstractArraySeq

import scala.collection.compat.immutable

/**
 * A sequence of slots backed by an array.
 *
 * This class provides an efficient implementation of a sequence of slots using an underlying array.
 * It extends `AbstractArraySeq` to leverage common sequence operations while providing specific
 * implementations for creating new arrays and instances of `SlotSeq`.
 *
 * @param array The underlying array storing the slots.
 * @param length The number of valid slots in the array.
 */
final class SlotSeq private (array: Array[Slot], length: Int)
  extends AbstractArraySeq[Slot, SlotSeq](array, length) {

  /**
   * Provides access to the underlying array.
   * This method is unsafe because it exposes the internal representation,
   * allowing for potential modification of the array outside of this class.
   *
   * @return The underlying array of slots.
   */
  private[foresight] def unsafeArray: Array[Slot] = array

  /**
   * Creates a new array of the specified size.
   */
  override protected def newArray(size: Int): Array[Slot] = new Array[Slot](size)

  /**
   * Creates a new instance of This with the given array and length.
   */
  override protected def create(newArray: Array[Slot], newLength: Int): SlotSeq = new SlotSeq(newArray, newLength)
}

object SlotSeq {
  /**
   * An empty `SlotSeq`.
   */
  val empty: SlotSeq = unsafeWrapArray(Array.empty)

  /**
   * Creates a new `SlotSeq` from the given array and length.
   * This method is unsafe because it does not copy the array,
   * so the caller must ensure that the array is not modified after being passed in.
   *
   * @param array The underlying array storing the slots.
   * @param length The number of valid slots in the array.
   * @return A new `SlotSeq` instance.
   */
  private[foresight] def unsafeWrapArray(array: Array[Slot], length: Int): SlotSeq = new SlotSeq(array, length)

  /**
   * Creates a new `SlotSeq` from the given array, using the entire array length.
   * This method is unsafe because it does not copy the array,
   * so the caller must ensure that the array is not modified after being passed in.
   * @param array The underlying array storing the slots.
   * @return A new `SlotSeq` instance.
   */
  private[foresight] def unsafeWrapArray(array: Array[Slot]): SlotSeq = new SlotSeq(array, array.length)

  /**
   * Creates a new `SlotSeq` from the given slots.
   * This method copies the slots into a new array.
   * @param slots The slots to include in the sequence.
   * @return A new `SlotSeq` instance.
   */
  def apply(slots: Slot*): SlotSeq = {
    from(slots)
  }

  /**
   * Creates a new `SlotSeq` from the given iterable of slots.
   * This method copies the slots into a new array.
   * @param slots The slots to include in the sequence.
   * @return A new `SlotSeq` instance.
   */
  def from(slots: Iterable[Slot]): SlotSeq = {
    slots match {
      case s: SlotSeq => s
      case as: immutable.ArraySeq[Slot] if as.unsafeArray.isInstanceOf[Array[Slot]] =>
        unsafeWrapArray(as.unsafeArray.asInstanceOf[Array[Slot]])
      case _ if slots.isEmpty => empty
      case _ => unsafeWrapArray(slots.toArray)
    }
  }
}
