package foresight.eqsat.collections

import foresight.eqsat.Slot
import scala.collection.immutable.{Set => ISet}
import scala.collection.{GenSet, GenTraversableOnce, SetLike}

/**
 * A small immutable set of slots.
 *
 * The set is implemented as a sorted array of slots.
 * This allows for efficient equality checks, comparison and set operations.
 */
final class SlotSet private (private val _slots: Array[Slot])
  extends ISet[Slot]
    with SetLike[Slot, SlotSet] {

  /**
   * Returns the underlying array of slots.
   * This method is unsafe because it exposes the internal representation of the set.
   * The caller must not modify the array.
   *
   * @return The underlying array of slots.
   */
  def unsafeArray: Array[Slot] = _slots

  /**
   * Maps the slots in the set using the given function.
   *
   * @param f The function to map the slots.
   * @return A new slot set containing the mapped slots.
   */
  def map(f: Slot => Slot): SlotSet = {
    if (isEmpty) this
    else SlotSet.fromUnsortedMutableArrayUnsafe(_slots.map(f))
  }

  override def empty: SlotSet = SlotSet.empty
  override def size: Int = _slots.length
  override def isEmpty: Boolean = _slots.length == 0
  override def iterator: Iterator[Slot] = _slots.iterator
  override def contains(elem: Slot): Boolean = SlotSetArrayOps.contains(_slots, elem)

  def +(elem: Slot): SlotSet = {
    val out = SlotSetArrayOps.insert(_slots, elem)
    if (out eq _slots) this else new SlotSet(out)
  }
  def -(elem: Slot): SlotSet = {
    val out = SlotSetArrayOps.removeElem(_slots, elem)
    if (out eq _slots) this else new SlotSet(out)
  }

  // Optimized ++ and -- for SlotSet
  override def ++(that: GenTraversableOnce[Slot]): SlotSet = that match {
    case s: SlotSet =>
      if ((this eq s) || s.size == 0) this
      else if (this.size == 0) s
      else new SlotSet(SlotSetArrayOps.union(this._slots, s._slots))
    case _ =>
      val otherArr = SlotSetArrayOps.fromIterator(that.toIterator)
      if (otherArr.isEmpty) this else new SlotSet(SlotSetArrayOps.union(this._slots, otherArr))
  }

  override def --(that: GenTraversableOnce[Slot]): SlotSet = that match {
    case s: SlotSet =>
      if ((this eq s) || this.size == 0) SlotSet.empty
      else if (s.size == 0) this
      else { val out = SlotSetArrayOps.diff(this._slots, s._slots); if (out.asInstanceOf[AnyRef] eq this._slots) this else new SlotSet(out) }
    case _ =>
      val otherArr = SlotSetArrayOps.fromIterator(that.toIterator)
      if (otherArr.isEmpty) this else { val out = SlotSetArrayOps.diff(this._slots, otherArr); if (out.asInstanceOf[AnyRef] eq this._slots) this else new SlotSet(out) }
  }

  override def intersect(that: GenSet[Slot]): SlotSet = that match {
    case s: SlotSet =>
      val out = SlotSetArrayOps.intersect(this._slots, s._slots)
      if (out.isEmpty) SlotSet.empty else new SlotSet(out)
    case _ =>
      val otherArr = SlotSetArrayOps.fromIterator(that.iterator)
      val out = SlotSetArrayOps.intersect(this._slots, otherArr)
      if (out.isEmpty) SlotSet.empty else new SlotSet(out)
  }

  // Optimize subsetOf when other is SlotSet
  override def subsetOf(that: GenSet[Slot]): Boolean = that match {
    case s: SlotSet => SlotSetArrayOps.isSubset(this._slots, s._slots)
    case _          => super.subsetOf(that)
  }

  override def equals(other: Any): Boolean = other match {
    case s: SlotSet => SlotSetArrayOps.arraysEqual(this._slots, s._slots)
    case _          => super.equals(other)
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
  private[eqsat] def fromSortedArrayUnsafe(arr: Array[Slot]): SlotSet = new SlotSet(arr)

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  private[eqsat] def fromUnsortedArrayUnsafe(slots: Array[Slot]): SlotSet = {
    if (slots.isEmpty) empty
    else if (slots.length == 1) new SlotSet(slots)
    else from(slots)
  }

  /**
   * Creates a slot set from the given array of slots.
   * Duplicates are removed and the slots are sorted.
   * The array is used directly without copying, so it must not
   * be used or modified after calling this method.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  private[eqsat] def fromUnsortedMutableArrayUnsafe(slots: Array[Slot]): SlotSet = {
    val arr = SlotSetArrayOps.fromUnsortedMutableArray(slots)
    if (arr.isEmpty) empty else new SlotSet(arr)
  }

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  def from(slots: TraversableOnce[Slot]): SlotSet = {
    val arr = SlotSetArrayOps.fromIterator(slots.toIterator)
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
