package foresight.eqsat.collections

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassCall, Permutation, Slot}
import foresight.util.SeqFromArray
import foresight.util.collections.StrictMapOps.toStrictMapOps
import foresight.util.ordering.SeqOrdering

import scala.collection.mutable

/**
 * A total map from parameter slots (keys) to argument slots (values).
 *
 * In slotted e-graphs, `SlotMap` carries the binding from an e-class's parameter slots to the caller's
 * argument slots (e.g., inside [[EClassCall]]), and is also used for renaming during canonicalization.
 *
 * Conventions and terminology:
 *   - Bijection: every key maps to a unique value (no two keys map to the same value).
 *   - Permutation: a bijection whose key set equals its value set (i.e., a re-labeling of the same set).
 *
 * Ordering and iteration:
 *   - Iteration (`iterator`, `keys`, `values`) is key-sorted (`Ordered[Slot]`), making listings deterministic.
 *   - `Ordered[SlotMap]` provides lexicographic order by sorted keys, then by corresponding values.
 *
 * Composition model:
 *   - Viewing slot maps as functions on slots, `this.compose(other)` corresponds to `other ∘ this`.
 *     That is, apply `this` first, then `other`.
 */
final class SlotMap private(private val _keys: Array[Slot],
                            private val _values: Array[Slot]) extends Permutation[SlotMap] with Ordered[SlotMap] {
  // Implementation details: store as two parallel arrays for memory efficiency and fast iteration.
  // Invariant: keys are unique (no duplicates) and are sorted in ascending order.
  // This is enforced by the companion object constructors.

  /**
   * The number of entries in this map.
   */
  def size: Int = _keys.length

  /**
   * True if the map has no entries.
   */
  def isEmpty: Boolean = _keys.isEmpty

  /**
   * True if `k` is present as a key.
   */
  def contains(k: Slot): Boolean = _keys.contains(k)

  /**
   * Returns the value bound to `k`, if any.
   */
  def get(k: Slot): Option[Slot] = getOrElse(k, null) match {
    case null => None
    case v => Some(v)
  }

  /**
   * Returns the value bound to `k`, or `default` if `k` is not present.
   */
  def getOrElse(k: Slot, default: => Slot): Slot = {
    val idx = _keys.indexOf(k)
    if (idx >= 0) _values(idx) else default
  }

  /**
   * Entries in ascending key order.
   */
  def iterator: Iterator[(Slot, Slot)] = _keys.iterator.zip(_values.iterator)

  /**
   * Keys in ascending order.
   */
  def keys: SeqFromArray.Seq[Slot] = SeqFromArray(_keys)

  /**
   * Values in the order of their sorted keys.
   */
  def values: SeqFromArray.Seq[Slot] = SeqFromArray(_values)

  /**
   * The set of keys.
   */
  def keySet: SlotSet = SlotSet.fromArrayUnsafe(_keys)

  /**
   * The set of values (distinct).
   */
  def valueSet: SlotSet = SlotSet.from(_values)

  /**
   * Inverts the mapping by swapping keys and values.
   *
   * Only meaningful when this map is a bijection. If not, throws an exception.
   *
   * @return The inverted map (`v -> k` for each `k -> v`).
   * @throws IllegalStateException If this map is not a bijection.
   */
  override def inverse: SlotMap = {
    // Fast path for empty map
    if (isEmpty) return this
    else if (size == 1) return SlotMap(_values, _keys)

    // Prepare arrays for the inverted map (values become keys, keys become values)
    val sortedKeys = java.util.Arrays.copyOf(_values, size)
    val sortedValues = java.util.Arrays.copyOf(_keys, size)

    // In-place insertion sort on (sortedKeys, sortedValues) by sortedKeys
    var i = 1
    while (i < sortedKeys.length) {
      val k = sortedKeys(i)
      val v = sortedValues(i)
      var j = i - 1
      // Shift larger keys one position to the right
      while (j >= 0 && (k < sortedKeys(j))) {
        sortedKeys(j + 1) = sortedKeys(j)
        sortedValues(j + 1) = sortedValues(j)
        j -= 1
      }

      // Insert the saved pair
      sortedKeys(j + 1) = k
      sortedValues(j + 1) = v
      i += 1
    }

    // Verify bijection (no duplicate values in original, i.e., no duplicate keys now)
    var d = 1
    while (d < sortedKeys.length) {
      if (sortedKeys(d - 1) == sortedKeys(d))
        throw new IllegalStateException("Cannot invert SlotMap: duplicate values found.")
      d += 1
    }

    SlotMap(sortedKeys, sortedValues)
  }


  /**
   * True if each key maps to a unique value (no collisions in `values`).
   */
  def isBijection: Boolean = {
    valueSet.size == _values.length
  }

  /**
   * True if this is a bijection and `keySet == valueSet`.
   */
  def isPermutation: Boolean = isBijection && keySet == valueSet

  /**
   * Concatenation (union) of two slot maps.
   *
   * Later entries overwrite earlier ones with the same key.
   *
   * @param other The other slot map to concatenate.
   * @return A new `SlotMap` containing all entries from `this` and `other`,
   *         with `other`'s entries taking precedence on key collisions.
   */
  def concat(other: SlotMap): SlotMap = {
    // Fast paths
    if (this.isEmpty) return other
    if (other.isEmpty) return this

    val aK = _keys
    val aV = _values
    val bK = other._keys
    val bV = other._values

    // Upper bound on result size is the sum of input sizes
    val total = aK.length + bK.length
    val keysBuf = new Array[Slot](total)
    val valuesBuf = new Array[Slot](total)

    var i = 0  // index in aK/aV (this)
    var j = 0  // index in bK/bV (other)
    var k = 0  // size of merged output so far

    // Merge two sorted key arrays in linear time
    while (i < aK.length && j < bK.length) {
      val ka = aK(i)
      val kb = bK(j)
      if (ka < kb) {
        keysBuf(k) = ka
        valuesBuf(k) = aV(i)
        i += 1; k += 1
      } else if (kb < ka) {
        keysBuf(k) = kb
        valuesBuf(k) = bV(j)
        j += 1; k += 1
      } else { // ka == kb -> overwrite with other's value
        keysBuf(k) = kb
        valuesBuf(k) = bV(j)
        i += 1; j += 1; k += 1
      }
    }

    // Copy any tail from either side
    if (i < aK.length) {
      System.arraycopy(aK, i, keysBuf,   k, aK.length - i)
      System.arraycopy(aV, i, valuesBuf, k, aV.length - i)
      k += (aK.length - i)
    }
    if (j < bK.length) {
      System.arraycopy(bK, j, keysBuf,   k, bK.length - j)
      System.arraycopy(bV, j, valuesBuf, k, bV.length - j)
      k += (bK.length - j)
    }

    // Trim to the exact size if we over‑allocated
    if (k == total) SlotMap(keysBuf, valuesBuf)
    else SlotMap(java.util.Arrays.copyOf(keysBuf, k),
                 java.util.Arrays.copyOf(valuesBuf, k))
  }

  /**
   * Composition `other ∘ this` with a totality check.
   *
   * Requires that every value produced by `this` appears as a key in `other` (`valueSet == other.keySet`).
   * Result maps `a -> c` when `a -> b` in `this` and `b -> c` in `other`.
   *
   * @throws IllegalArgumentException if `valueSet != other.keySet`.
   */
  def compose(other: SlotMap): SlotMap = {
    var i = 0
    while (i < _values.length) {
      val v = _values(i)
      if (!other.contains(v))
        throw new IllegalArgumentException(s"Cannot compose SlotMaps: intermediate value $v is not a key in other.")

      i += 1
    }

    composePartial(other)
  }

  /**
   * Partial composition `other ∘ this`.
   *
   * Like `compose`, but drops entries whose intermediate value is not a key in `other`.
   * Useful for projecting a mapping into a smaller codomain.
   */
  def composePartial(other: SlotMap): SlotMap = {
    // Lazily allocate (copy-on-write) buffers.
    // - keysBuffer is allocated only if we *drop* at least one entry.
    // - valuesBuffer is allocated only if we either drop any entry (we need a compacted array)
    //   or if a mapped value differs from the original at a kept position.
    var keysBuffer: Array[Slot] = null
    var valuesBuffer: Array[Slot] = null
    var i = 0
    var j = 0
    while (i < size) {
      val v = _values(i)
      other.getOrElse(v, null) match {
        case null =>
          // Drop this entry.
          if (keysBuffer == null) {
            // First drop: allocate keys buffer and copy over keys kept so far.
            keysBuffer = new Array[Slot](size)
            Array.copy(_keys, 0, keysBuffer, 0, j)
          }
          if (valuesBuffer == null) {
            // First drop also requires compaction of values; copy over kept values so far.
            valuesBuffer = new Array[Slot](size)
            Array.copy(_values, 0, valuesBuffer, 0, j)
          }
        // Note: do not advance j (we dropped this entry).

        case w =>
          // Keep this entry (possibly with a remapped value).
          if (keysBuffer != null) {
            // We're in "dropping" mode; we must compact keys into keysBuffer.
            keysBuffer(j) = _keys(i)
          }

          // Copy-on-write for values:
          // Allocate valuesBuffer only if (a) we've started dropping (needing compaction), or
          // (b) the mapped value differs from the original at this kept position.
          if ((valuesBuffer != null) || (w != _values(i))) {
            if (valuesBuffer == null) {
              valuesBuffer = new Array[Slot](size)
              // Copy previously identical values for kept entries [0, j).
              Array.copy(_values, 0, valuesBuffer, 0, j)
            }
            valuesBuffer(j) = w
          }
          j += 1
      }
      i += 1
    }

    if (keysBuffer == null) {
      // No drops. If no value changed, we can return `this` unchanged.
      if (valuesBuffer == null) this
      else SlotMap(_keys, valuesBuffer)
    } else {
      // Some entries were dropped; both buffers must be allocated.
      // keysBuffer and valuesBuffer are guaranteed non-null by the first-drop logic above.
      SlotMap(keysBuffer.take(j), valuesBuffer.take(j))
    }
  }

  private def composeWithDefault(other: SlotMap, default: Slot => Slot): SlotMap = {
    if (isEmpty) return this

    // Allocate values buffer lazily if we need to change anything
    val len = _keys.length
    var valuesArr: Array[Slot] = null
    var i = 0
    while (i < len) {
      val v = _values(i)
      val mapped = other.getOrElse(v, null) match {
        case null => default(v)
        case w => w
      }
      if (valuesArr == null && mapped != v) {
        valuesArr = new Array[Slot](size)
        // Copy previous unchanged values
        Array.copy(_values, 0, valuesArr, 0, i)
      }
      if (valuesArr != null) valuesArr(i) = mapped
      i += 1
    }

    if (valuesArr == null) this
    else SlotMap(_keys, valuesArr)
  }

  /**
   * Retaining composition `other ∘ this` with fallback.
   *
   * Like `compose`, but if an intermediate value `b` is not a key in `other`, keeps the original `a -> b`.
   * Useful when you want to apply a renaming where defined, but leave other bindings unchanged.
   */
  def composeRetain(other: SlotMap): SlotMap = {
    if (other.isEmpty) return this

    composeWithDefault(other, v => v)
  }

  /**
   * Freshening composition `other ∘ this`.
   *
   * Like `compose`, but if an intermediate value `b` is not a key in `other`, replaces it with a fresh slot.
   * Useful for isolating unmapped bindings so they cannot alias existing slots.
   */
  def composeFresh(other: SlotMap): SlotMap = {
    composeWithDefault(other, _ => Slot.fresh())
  }

  /**
   * Renames this mapping by applying `renaming` to both keys and values.
   *
   * Only entries whose key and value both appear in `renaming` are kept; others are dropped.
   * Useful for retargeting a mapping to a fresh slot space.
   *
   * @param renaming Mapping from old to new slots.
   * @return A `SlotMap` with keys and values renamed according to `renaming`.
   */
  def rename(renaming: SlotMap): SlotMap = {
    // TODO: implement more efficiently without intermediate collections
    SlotMap.fromPairs(iterator.collect {
      case (k, v) if renaming.contains(k) && renaming.contains(v) => (renaming(k), renaming(v))
    }.toSeq)
  }

  /**
   * Internal sanity check to detect duplicate keys.
   */
  private[eqsat] def check(): Unit = {
    val found = mutable.HashSet[Slot]()
    keys.foreach { x =>
      assert(!found.contains(x))
      found.add(x)
    }
  }

  /**
   * Applies this mapping to a slot, returning the image or the slot itself if unmapped.
   */
  override def apply(slot: Slot): Slot = {
    val idx = _keys.indexOf(slot)
    if (idx >= 0) _values(idx) else slot
  }

  /**
   * Lexicographic order by sorted keys, then by the values at those keys.
   */
  override def compare(that: SlotMap): Int = {
    val ordering = SeqOrdering.lexOrdering(Ordering.by(identity[Slot]))

    val keyComparison = ordering.compare(keys, that.keys)
    if (keyComparison != 0) {
      return keyComparison
    }

    ordering.compare(values, that.values)
  }

  /**
   * Keeps only entries whose key satisfies `p`.
   */
  def filterKeys(p: Slot => Boolean): SlotMap = {
    if (isEmpty) return this

    // Copy-on-write: allocate only if we actually drop at least one key.
    val n = _keys.length
    var keysBuffer: Array[Slot] = null
    var valuesBuffer: Array[Slot] = null
    var i = 0
    var j = 0
    while (i < n) {
      val k = _keys(i)
      if (p(k)) {
        // Keep the pair; if we've started dropping, write into compacted buffers.
        if (keysBuffer != null) {
          keysBuffer(j) = k
          valuesBuffer(j) = _values(i)
        }
        j += 1
      } else {
        // First time we drop an entry, allocate buffers and copy previously kept prefix.
        if (keysBuffer == null) {
          keysBuffer = new Array[Slot](n)
          valuesBuffer = new Array[Slot](n)
          if (j > 0) {
            System.arraycopy(_keys, 0, keysBuffer, 0, j)
            System.arraycopy(_values, 0, valuesBuffer, 0, j)
          }
        }
        // Do not advance j (we dropped this entry).
      }
      i += 1
    }

    // If no drops occurred, return `this` unchanged; otherwise trim buffers.
    if (keysBuffer == null) this
    else SlotMap(java.util.Arrays.copyOf(keysBuffer, j),
                 java.util.Arrays.copyOf(valuesBuffer, j))
  }

  /**
   * Structural equality: two SlotMaps are equal iff they contain the exact
   * same (key -> value) bindings in the same sorted-key order.
   *
   * Since the invariant is that keys are unique and sorted ascending, this
   * is equivalent to element-wise equality of the parallel arrays.
   */
  override def equals(other: Any): Boolean = other match {
    case that: SlotMap =>
      (this eq that) || (arraysEqual(this._keys, that._keys) && arraysEqual(this._values, that._values))
    case _ => false
  }

  private def arraysEqual(a: Array[Slot], b: Array[Slot]): Boolean = {
    if (a.length != b.length) return false
    var i = 0
    while (i < a.length) {
      if (a(i) != b(i)) return false
      i += 1
    }
    true
  }

  /**
   * Structural hash: mixes keys and values pairwise to be consistent with equals.
   */
  override def hashCode(): Int = {
    var h = 1
    var i = 0
    while (i < _keys.length) {
      // Use Scala's universal hash (##) to be safe for value classes, nulls, etc.
      h = 31 * h + _keys(i).##
      h = 31 * h + _values(i).##
      i += 1
    }
    h
  }
}

/**
 * Constructors and utilities for building [[SlotMap]]s.
 */
object SlotMap {
  /**
   * Internal constructor: expects `keys` to be unique and sorted ascending.
   * Call public builders like fromPairs/identity/bijectionFromSetToFresh to enforce invariants.
   */
  private[eqsat] def apply(keys: Array[Slot], values: Array[Slot]): SlotMap = {
    require(keys.length == values.length, "Mismatched key/value array lengths")
    new SlotMap(keys, values)
  }

  /**
   * The empty mapping.
   */
  val empty: SlotMap = SlotMap(Array.empty, Array.empty)

  /**
   * Builds a map from `(key, value)` pairs. Later pairs overwrite earlier ones with the same key.
   */
  def from(pairs: (Slot, Slot)*): SlotMap = fromPairs(pairs)

  /**
   * Builds a map from `(key, value)` pairs. Later pairs overwrite earlier ones with the same key.
   */
  def fromPairs(pairs: Iterable[(Slot, Slot)]): SlotMap = {
    // TODO: implement more efficiently without intermediate collections
    val map = mutable.LinkedHashMap.empty[Slot, Slot]
    for ((k, v) <- pairs) map(k) = v
    val keysArr = map.keys.toArray.sorted
    val valuesArr = keysArr.map(map)
    SlotMap(keysArr, valuesArr)
  }

  /**
   * Internal constructor that skips all checks, assuming `keys` are unique and sorted ascending.
   *
   * Useful for internal use when you know the arrays are already valid.
   *
   * @param keys   Array of keys (must be unique and sorted ascending).
   * @param values Array of values (same length as keys).
   * @return A new SlotMap instance.
   */
  private[eqsat] def fromArraysUnsafe(keys: Array[Slot], values: Array[Slot]): SlotMap = {
    // Unsafe: does not check for duplicate keys or sort them.
    // Useful for internal use when you know the arrays are already valid.
    new SlotMap(keys, values)
  }

  /**
   * Identity mapping on the given set (`s -> s` for all `s`).
   */
  def identity(set: SlotSet): SlotMap = {
    val keys = set.unsafeArray
    SlotMap(keys, keys)
  }

  /**
   * Builds a bijection from each key in `set` to a fresh unique slot.
   *
   * Useful for capturing without aliasing or for introducing fresh binders.
   */
  def bijectionFromSetToFresh(set: Set[Slot]): SlotMap = {
    val keys = set.toArray.sorted
    val values = Array.fill(keys.length)(Slot.fresh())
    SlotMap(keys, values)
  }
}
