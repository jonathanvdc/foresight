package foresight.eqsat.collections

import foresight.eqsat.Slot

/**
 * A small immutable set of slots.
 *
 * The set is implemented as a sorted array of slots.
 * This allows for efficient equality checks, comparison and set operations.
 */
final class SlotSet private(private val _slots: Array[Slot]) extends Set[Slot] {
  /**
   * Returns the underlying array of slots.
   * This method is unsafe because it exposes the internal representation of the set.
   * The caller must not modify the array.
   *
   * @return The underlying array of slots.
   */
  private[eqsat] def unsafeArray: Array[Slot] = _slots

  override def size: Int = _slots.length
  override def isEmpty: Boolean = _slots.isEmpty

  override def empty: SlotSet = SlotSet.empty

  /**
   * Maps the slots in the set using the given function.
   * @param f The function to map the slots.
   * @return A new slot set containing the mapped slots.
   */
  def map(f: Slot => Slot): SlotSet = {
    SlotSet.from(_slots.iterator.map(f))
  }

  /**
   * Checks if the array `small` is a subset of the array `big`.
   * @param small The potential subset array (must be sorted, no duplicates).
   * @param big The potential superset array (must be sorted, no duplicates).
   * @return True if `small` is a subset of `big`, false otherwise.
   */
  @inline
  private def isSubset(small: Array[Slot], big: Array[Slot]): Boolean = {
    var i = 0 // idx in big
    var j = 0 // idx in small
    val n = big.length
    val m = small.length
    while (i < n && j < m) {
      val a = big(i)
      val b = small(j)
      if (a == b) { i += 1; j += 1 }
      else if (a < b) { i += 1 }
      else return false // a > b -> small has an element not present in big
    }
    j == m
  }

  /**
   * Checks if the array `big` is a superset of the array `small`.
   * @param big The potential superset array (must be sorted, no duplicates).
   * @param small The potential subset array (must be sorted, no duplicates).
   * @return True if `big` is a superset of `small`, false otherwise.
   */
  @inline
  private def isSuperset(big: Array[Slot], small: Array[Slot]): Boolean =
    isSubset(small, big)

  override def incl(elem: Slot): Set[Slot] = {
    val arr = _slots
    val n = arr.length

    // Binary search for existing position; if found, return this
    var lo = 0
    var hi = n - 1
    while (lo <= hi) {
      val mid = (lo + hi) >>> 1
      val v = arr(mid)
      if (v == elem) {
        return this
      } else if (v < elem) {
        lo = mid + 1
      } else {
        hi = mid - 1
      }
    }

    // Not found: insert at index `lo` (the insertion point)
    val newArr = new Array[Slot](n + 1)
    if (lo > 0) System.arraycopy(arr, 0, newArr, 0, lo)
    newArr(lo) = elem
    if (lo < n) System.arraycopy(arr, lo, newArr, lo + 1, n - lo)
    new SlotSet(newArr)
  }

  override def excl(elem: Slot): Set[Slot] = {
    if (!contains(elem)) this
    else new SlotSet(_slots.filterNot(_ == elem))
  }

  override def concat(that: IterableOnce[Slot]): SlotSet = {
    that match {
      case s: SlotSet =>
        // Fast paths
        if ((this eq s) || s.size == 0) return this
        if (this.size == 0) return s

        val a = this._slots
        val b = s._slots
        val n = a.length
        val m = b.length

        // Early check: is b a subset of a? (then return this)
        if (isSubset(b, a)) return this
        if (isSubset(a, b)) return s

        // Merge with COW: allocate once to n + m, fill, then trim if needed
        val merged = new Array[Slot](n + m)
        var i = 0
        var j = 0
        var k = 0
        while (i < n && j < m) {
          val av = a(i)
          val bv = b(j)
          if (av == bv) {
            merged(k) = av; k += 1
            i += 1; j += 1
          } else if (av < bv) {
            merged(k) = av; k += 1
            i += 1
          } else { // av > bv
            merged(k) = bv; k += 1
            j += 1
          }
        }
        // Copy any remainder (only one of these loops will run)
        while (i < n) { merged(k) = a(i); k += 1; i += 1 }
        while (j < m) { merged(k) = b(j); k += 1; j += 1 }

        // Trim if duplicates reduced size, otherwise reuse merged as-is
        if (k == merged.length) SlotSet.fromArrayUnsafe(merged)
        else {
          val trimmed = new Array[Slot](k)
          System.arraycopy(merged, 0, trimmed, 0, k)
          SlotSet.fromArrayUnsafe(trimmed)
        }

      case _ =>
        // Generic path: fall back to constructing via iterator; keeps correctness.
        val it = that.iterator
        if (!it.hasNext) this else SlotSet.from(_slots ++ it)
    }
  }

  private def removedAll(s: SlotSet): SlotSet = {
    // Fast paths
    if ((this eq s) || this.size == 0) return SlotSet.empty
    if (s.size == 0) return this

    val a = this._slots
    val b = s._slots
    val n = a.length
    val m = b.length

    // If all of `a` is contained in `b`, result is empty.
    if (isSubset(a, b)) return SlotSet.empty

    // Check for overlap without allocating; if disjoint, return `this`.
    var i = 0
    var j = 0
    var foundOverlap = false
    while (i < n && j < m && !foundOverlap) {
      val av = a(i)
      val bv = b(j)
      if (av == bv) foundOverlap = true
      else if (av < bv) i += 1
      else j += 1
    }
    if (!foundOverlap) return this

    // There is at least one element to drop: perform copy-on-write diff.
    // Allocate an array as large as `a` (max possible after removals),
    // copy prefix before first overlap, then continue merging.
    val out = new Array[Slot](n)
    // `i` and `j` currently point to the first overlap or its vicinity.
    // Rewind to copy the prefix before the first potential match.
    // We know the prefix is [0, i) if we stopped because av == bv; if we
    // stopped because one pointer reached end, we'd have returned above.
    // Ensure `i` points to the first candidate in `a` for merging.
    // If the loop ended on av == bv, `i` is correct; otherwise no overlap.
    val prefixLen = i
    if (prefixLen > 0) System.arraycopy(a, 0, out, 0, prefixLen)
    var k = prefixLen

    // Resume from current (i, j); drop matches, keep `a` elements < `b` head.
    while (i < n && j < m) {
      val av = a(i)
      val bv = b(j)
      if (av == bv) {
        // drop this element from result
        i += 1;
        j += 1
      } else if (av < bv) {
        out(k) = av;
        k += 1;
        i += 1
      } else {
        j += 1
      }
    }
    // Copy any remaining `a` tail (all > last `b` seen)
    while (i < n) {
      out(k) = a(i); k += 1; i += 1
    }

    // Trim if needed, otherwise reuse `out` as-is
    if (k == out.length) SlotSet.fromArrayUnsafe(out)
    else {
      val trimmed = new Array[Slot](k)
      System.arraycopy(out, 0, trimmed, 0, k)
      SlotSet.fromArrayUnsafe(trimmed)
    }
  }

  override def removedAll(that: IterableOnce[Slot]): SlotSet = that match {
    case s: SlotSet => removedAll(s)
    case _ => removedAll(SlotSet.from(that))
  }

  override def diff(that: collection.Set[Slot]): SlotSet = {
    if (that.isEmpty) this
    else removedAll(that)
  }

  private def intersect(s: SlotSet): SlotSet = {
    val a = this._slots
    val b = s._slots
    val n = a.length
    val m = b.length

    // Fast paths
    if (n == 0 || m == 0) return SlotSet.empty
    // If one is a subset of the other, return the smaller one directly
    if (isSubset(a, b)) return this
    if (isSubset(b, a)) return s

    // Check quickly if there is any overlap; if not, return empty without allocating
    var i = 0
    var j = 0
    var hasOverlap = false
    while (i < n && j < m && !hasOverlap) {
      val av = a(i)
      val bv = b(j)
      if (av == bv) hasOverlap = true
      else if (av < bv) i += 1
      else j += 1
    }
    if (!hasOverlap) return SlotSet.empty

    // There is at least one common element. Build the intersection using COW.
    // Maximum possible size is min(n, m).
    val outCap = if (n < m) n else m
    val out = new Array[Slot](outCap)
    var k = 0

    // Continue from current i, j (either at the first overlap or its vicinity).
    // If we exited on equality, ensure we record that element first.
    if (i < n && j < m && a(i) == b(j)) {
      out(k) = a(i);
      k += 1
      i += 1;
      j += 1
    }

    while (i < n && j < m) {
      val av = a(i)
      val bv = b(j)
      if (av == bv) {
        out(k) = av;
        k += 1
        i += 1;
        j += 1
      } else if (av < bv) {
        i += 1
      } else {
        j += 1
      }
    }

    if (k == 0) SlotSet.empty
    else if (k == out.length) SlotSet.fromArrayUnsafe(out)
    else {
      val trimmed = new Array[Slot](k)
      System.arraycopy(out, 0, trimmed, 0, k)
      SlotSet.fromArrayUnsafe(trimmed)
    }
  }

  override def intersect(that: collection.Set[Slot]): SlotSet = that match {
    case s: SlotSet => intersect(s)
    case _ => this.intersect(SlotSet.from(that))
  }

  override def contains(elem: Slot): Boolean = {
    _slots.contains(elem)
  }

  override def iterator: Iterator[Slot] = _slots.iterator

  override def subsetOf(that: collection.Set[Slot]): Boolean = that match {
    case s: SlotSet => isSubset(this._slots, s._slots)
    case _          => super.subsetOf(that)
  }

  private def equalsOtherSet(set: SlotSet): Boolean = {
    if (this eq set) return true
    if (this.size != set.size) return false

    var i = 0
    while (i < _slots.length) {
      if (_slots(i) != set._slots(i)) return false
      i += 1
    }
    true
  }

  override def equals(that: Any): Boolean = that match {
    case set: SlotSet => equalsOtherSet(set)
    case _            => super.equals(that)
  }

  override def hashCode(): Int = {
    // FNV-1a hash
    var hash = 0x811c9dc5
    for (slot <- _slots) {
      hash ^= slot.hashCode()
      hash *= 0x01000193
    }
    hash
  }
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
   * @param arr The array of slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  private[eqsat] def fromArrayUnsafe(arr: Array[Slot]): SlotSet = {
    new SlotSet(arr)
  }

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   *
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  def from(slots: IterableOnce[Slot]): SlotSet = {
    val uniqueSorted = slots.iterator.toArray.sorted.distinct
    if (uniqueSorted.isEmpty) return empty
    new SlotSet(uniqueSorted)
  }

  /**
   * Creates a slot set from the given slots.
   * Duplicates are removed and the slots are sorted.
   * @param slots The slots to include in the set.
   * @return A new slot set containing the given slots.
   */
  def apply(slots: Slot*): SlotSet = from(slots)
}
