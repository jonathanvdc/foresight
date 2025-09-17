package foresight.eqsat.collections

import foresight.eqsat.Slot

/**
 * Version-independent array operations for SlotSet.
 * Keep this logic free of Scala collection-version APIs.
 */
private[collections] object SlotSetArrayOps {
  // Unsafe wrapper constructor delegate – the concrete SlotSet classes
  // will wrap arrays into instances. We keep array-level ops only here.

  /** Binary search for elem in sorted unique array. */
  def contains(a: Array[Slot], elem: Slot): Boolean = {
    var lo = 0
    var hi = a.length - 1
    while (lo <= hi) {
      val mid = (lo + hi) >>> 1
      val v = a(mid)
      if (v == elem) return true
      else if (v < elem) lo = mid + 1
      else hi = mid - 1
    }
    false
  }

  /** Insert `elem` into sorted-unique array `a` (no mutation of input). */
  def insert(a: Array[Slot], elem: Slot): Array[Slot] = {
    var lo = 0
    var hi = a.length - 1
    while (lo <= hi) {
      val mid = (lo + hi) >>> 1
      val v = a(mid)
      if (v == elem) return a // already present – reuse
      else if (v < elem) lo = mid + 1 else hi = mid - 1
    }
    val n = a.length
    val out = new Array[Slot](n + 1)
    if (lo > 0) System.arraycopy(a, 0, out, 0, lo)
    out(lo) = elem
    if (lo < n) System.arraycopy(a, lo, out, lo + 1, n - lo)
    out
  }

  /** Remove `elem` from sorted-unique array `a` if present. */
  def removeElem(a: Array[Slot], elem: Slot): Array[Slot] = {
    // locate
    var lo = 0
    var hi = a.length - 1
    while (lo <= hi) {
      val mid = (lo + hi) >>> 1
      val v = a(mid)
      if (v == elem) {
        val n = a.length
        val out = new Array[Slot](n - 1)
        if (mid > 0) System.arraycopy(a, 0, out, 0, mid)
        if (mid + 1 < n) System.arraycopy(a, mid + 1, out, mid, n - (mid + 1))
        return out
      } else if (v < elem) lo = mid + 1 else hi = mid - 1
    }
    a // not present
  }

  /** small ⊆ big  (both sorted unique). */
  @inline def isSubset(small: Array[Slot], big: Array[Slot]): Boolean = {
    var i = 0 // idx in big
    var j = 0 // idx in small
    val n = big.length
    val m = small.length
    while (i < n && j < m) {
      val a = big(i)
      val b = small(j)
      if (a == b) { i += 1; j += 1 }
      else if (a < b) { i += 1 }
      else return false // a > b
    }
    j == m
  }
  @inline def isSuperset(big: Array[Slot], small: Array[Slot]): Boolean = isSubset(small, big)

  /** a ∪ b -> new sorted unique array. */
  def union(a: Array[Slot], b: Array[Slot]): Array[Slot] = {
    if (a.length == 0) return b
    if (b.length == 0) return a
    if (isSubset(a, b)) return b
    if (isSubset(b, a)) return a

    val n = a.length; val m = b.length
    val merged = new Array[Slot](n + m)
    var i = 0; var j = 0; var k = 0
    while (i < n && j < m) {
      val av = a(i); val bv = b(j)
      if (av == bv) { merged(k) = av; k += 1; i += 1; j += 1 }
      else if (av < bv) { merged(k) = av; k += 1; i += 1 }
      else { merged(k) = bv; k += 1; j += 1 }
    }
    while (i < n) { merged(k) = a(i); k += 1; i += 1 }
    while (j < m) { merged(k) = b(j); k += 1; j += 1 }

    if (k == merged.length) merged
    else {
      val trimmed = new Array[Slot](k)
      System.arraycopy(merged, 0, trimmed, 0, k)
      trimmed
    }
  }

  /** a − b (set difference) -> new sorted unique array. */
  def diff(a: Array[Slot], b: Array[Slot]): Array[Slot] = {
    if (a.length == 0) return a
    if (b.length == 0) return a
    if (isSubset(a, b)) return Array.empty[Slot]

    val n = a.length; val m = b.length
    var i = 0; var j = 0
    // quick probe to skip allocation if disjoint
    var overlap = false
    while (i < n && j < m && !overlap) {
      val av = a(i); val bv = b(j)
      if (av == bv) overlap = true
      else if (av < bv) i += 1 else j += 1
    }
    if (!overlap) return a

    val out = new Array[Slot](n)
    val prefixLen = i
    if (prefixLen > 0) System.arraycopy(a, 0, out, 0, prefixLen)
    var k = prefixLen
    while (i < n && j < m) {
      val av = a(i); val bv = b(j)
      if (av == bv) { i += 1; j += 1 }
      else if (av < bv) { out(k) = av; k += 1; i += 1 }
      else j += 1
    }
    while (i < n) { out(k) = a(i); k += 1; i += 1 }

    if (k == out.length) out
    else {
      val trimmed = new Array[Slot](k)
      System.arraycopy(out, 0, trimmed, 0, k)
      trimmed
    }
  }

  /** a ∩ b -> new sorted unique array. */
  def intersect(a: Array[Slot], b: Array[Slot]): Array[Slot] = {
    if (a.length == 0 || b.length == 0) return Array.empty[Slot]
    if (isSubset(a, b)) return a
    if (isSubset(b, a)) return b

    val n = a.length; val m = b.length
    var i = 0; var j = 0
    var overlap = false
    while (i < n && j < m && !overlap) {
      val av = a(i); val bv = b(j)
      if (av == bv) overlap = true
      else if (av < bv) i += 1 else j += 1
    }
    if (!overlap) return Array.empty[Slot]

    val outCap = if (n < m) n else m
    val out = new Array[Slot](outCap)
    var k = 0
    if (i < n && j < m && a(i) == b(j)) { out(k) = a(i); k += 1; i += 1; j += 1 }
    while (i < n && j < m) {
      val av = a(i); val bv = b(j)
      if (av == bv) { out(k) = av; k += 1; i += 1; j += 1 }
      else if (av < bv) i += 1 else j += 1
    }
    if (k == 0) Array.empty[Slot]
    else if (k == out.length) out
    else { val trimmed = new Array[Slot](k); System.arraycopy(out, 0, trimmed, 0, k); trimmed }
  }

  /** Build a sorted-unique array from an iterator (order-insensitive input). */
  def fromIterator(it: Iterator[Slot]): Array[Slot] = {
    // Collect to array buffer
    val arr = it.toArray
    if (arr.isEmpty) return arr
    scala.util.Sorting.quickSort(arr)
    // unique in-place into a new array of same size, then trim
    var k = 1
    var last = arr(0)
    var i = 1
    while (i < arr.length) {
      val v = arr(i)
      if (v != last) { arr(k) = v; k += 1; last = v }
      i += 1
    }
    if (k == arr.length) arr
    else {
      val trimmed = new Array[Slot](k)
      System.arraycopy(arr, 0, trimmed, 0, k)
      trimmed
    }
  }

  /** Array structural equality assuming sorted-unique canonical form. */
  def arraysEqual(a: Array[Slot], b: Array[Slot]): Boolean = {
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) return true
    if (a.length != b.length) return false
    var i = 0
    while (i < a.length) { if (a(i) != b(i)) return false; i += 1 }
    true
  }

  /** FNV-1a hash over slots. */
  def fnv1a(a: Array[Slot]): Int = {
    var hash = 0x811c9dc5
    var i = 0
    while (i < a.length) {
      hash ^= a(i).hashCode()
      hash *= 0x01000193
      i += 1
    }
    hash
  }
}
