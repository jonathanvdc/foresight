package foresight.eqsat.collections

import foresight.eqsat.Slot
import scala.annotation.tailrec

/**
 * A small immutable set of slots.
 *
 * The set is implemented as a sorted array of slots.
 * This allows for efficient equality checks, comparison and set operations.
 */
final class SlotSet private (private val _slots: Array[Slot]) extends Set[Slot] {
  /**
   * Returns the underlying array of slots.
   * This method is unsafe because it exposes the internal representation of the set.
   * The caller must not modify the array.
   *
   * @return The underlying array of slots.
   */
  private[eqsat] def unsafeArray: Array[Slot] = _slots

  /**
   * Maps the slots in the set using the given function.
   *
   * @param f The function to map the slots.
   * @return A new slot set containing the mapped slots.
   */
  def map(f: Slot => Slot): SlotSet = {
    if (isEmpty) this
    else SlotSet.from(_slots.iterator.map(f))
  }

  override def size: Int = _slots.length
  override def isEmpty: Boolean = _slots.isEmpty
  override def empty: SlotSet = SlotSet.empty

  override def incl(elem: Slot): Set[Slot] = {
    val out = SlotSetArrayOps.insert(_slots, elem)
    if (out eq _slots) this else new SlotSet(out)
  }

  override def excl(elem: Slot): Set[Slot] = {
    val out = SlotSetArrayOps.removeElem(_slots, elem)
    if (out eq _slots) this else new SlotSet(out)
  }

  override def concat(that: IterableOnce[Slot]): SlotSet = that match {
    case s: SlotSet =>
      if ((this eq s) || s.size == 0) this
      else if (this.size == 0) s
      else new SlotSet(SlotSetArrayOps.union(this._slots, s._slots))
    case _ =>
      val other = SlotSet.from(that)
      if (other.isEmpty) this else new SlotSet(SlotSetArrayOps.union(this._slots, other._slots))
  }

  private def removedAllImpl(s: SlotSet): SlotSet = {
    if ((this eq s) || this.size == 0) SlotSet.empty
    else if (s.size == 0) this
    else {
      val out = SlotSetArrayOps.diff(this._slots, s._slots)
      if (out.asInstanceOf[AnyRef] eq this._slots.asInstanceOf[AnyRef]) this else new SlotSet(out)
    }
  }

  override def removedAll(that: IterableOnce[Slot]): SlotSet = that match {
    case s: SlotSet => removedAllImpl(s)
    case _ => removedAllImpl(SlotSet.from(that))
  }

  override def diff(that: collection.Set[Slot]): SlotSet = {
    if (that.isEmpty) this else removedAll(that)
  }

  private def intersectImpl(s: SlotSet): SlotSet = {
    val out = SlotSetArrayOps.intersect(this._slots, s._slots)
    if (out.isEmpty) SlotSet.empty else new SlotSet(out)
  }

  override def intersect(that: collection.Set[Slot]): SlotSet = that match {
    case s: SlotSet => intersectImpl(s)
    case _          => intersectImpl(SlotSet.from(that))
  }

  override def contains(elem: Slot): Boolean = SlotSetArrayOps.contains(_slots, elem)
  override def iterator: Iterator[Slot] = _slots.iterator

  override def subsetOf(that: collection.Set[Slot]): Boolean = that match {
    case s: SlotSet => SlotSetArrayOps.isSubset(this._slots, s._slots)
    case _          => super.subsetOf(that)
  }

  private def equalsOtherSet(set: SlotSet): Boolean = SlotSetArrayOps.arraysEqual(this._slots, set._slots)

  override def equals(that: Any): Boolean = that match {
    case set: SlotSet => equalsOtherSet(set)
    case _            => super.equals(that)
  }

  override def hashCode(): Int = SlotSetArrayOps.fnv1a(_slots)
}

/**
 * Companion object for [[SlotSet]].
 */
object SlotSet {
  /**
   * An empty slot set.
   */
  val empty: SlotSet = new SlotSet(Array.empty)

  /**
   * Creates a slot set from the given array of slots.
   * The array is used directly without copying, so it must not be modified after calling this method.
   * The array must be sorted and contain no duplicates.
   * This method is unsafe because it does not check the preconditions.
   *
   * @param arr The array of slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  private[eqsat] def fromArrayUnsafe(arr: Array[Slot]): SlotSet = new SlotSet(arr)

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  def from(slots: IterableOnce[Slot]): SlotSet = {
    val arr = SlotSetArrayOps.fromIterator(slots.iterator)
    if (arr.isEmpty) empty else new SlotSet(arr)
  }

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  def apply(slots: Slot*): SlotSet = from(slots)
}
