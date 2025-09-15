package foresight.eqsat

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
  def keys: Seq[Slot] = _keys

  /**
   * Values in the order of their sorted keys.
   */
  def values: Seq[Slot] = _values

  /**
   * The set of keys.
   */
  def keySet: Set[Slot] = _keys.toSet

  /**
   * The set of values (distinct).
   */
  def valueSet: Set[Slot] = _values.toSet

  /**
   * Inverts the mapping by swapping keys and values.
   *
   * Only meaningful when this map is a bijection. If multiple keys map to the same value,
   * later entries overwrite earlier ones in the result.
   *
   * @return The inverted map (`v -> k` for each `k -> v`).
   */
  override def inverse: SlotMap = {
    // Create an array of indices for the _values array
    val idx = _values.indices.toArray
    // Sort the indices based on the corresponding values in _values, preserving order for equal elements
    scala.util.Sorting.stableSort(idx, (a: Int, b: Int) => _values(a) < _values(b))

    // Prepare arrays to hold the sorted keys and values for the inverse map
    val sortedKeys = new Array[Slot](size)
    val sortedValues = new Array[Slot](size)
    var j = 0
    var i = 0
    while (i < idx.length) {
      val v = _values(idx(i))
      // Only add unique values (skip duplicates)
      if (j == 0 || sortedKeys(j - 1) != v) {
        sortedKeys(j) = v                // Inverse: value becomes key
        sortedValues(j) = _keys(idx(i))  // Inverse: key becomes value
        j += 1
      }
      i += 1
    }

    // Return a new SlotMap with the inverted mapping
    if (i == j) {
      // All values were unique
      SlotMap(sortedKeys, sortedValues)
    } else {
      throw new IllegalStateException("Cannot invert SlotMap: duplicate values found.")
    }
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
   */
  def concat(other: SlotMap): SlotMap = {
    // Merge two sorted arrays of (key, value) pairs, with later entries overwriting earlier ones
    val merged = mutable.LinkedHashMap.empty[Slot, Slot]
    // Add all entries from this, then overwrite with entries from other
    for (i <- _keys.indices) merged(_keys(i)) = _values(i)
    for (i <- other._keys.indices) merged(other._keys(i)) = other._values(i)
    // Extract keys and values in sorted-key order, aligning values accordingly
    val keysArr = merged.keys.toArray.sorted
    val valuesArr = keysArr.map(merged)
    SlotMap(keysArr, valuesArr)
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
    require(valueSet == other.keySet, "Slot maps are not composable")
    composePartial(other)
  }

  /**
   * Partial composition `other ∘ this`.
   *
   * Like `compose`, but drops entries whose intermediate value is not a key in `other`.
   * Useful for projecting a mapping into a smaller codomain.
   */
  def composePartial(other: SlotMap): SlotMap = {
    // Fast paths for empty maps
    if (isEmpty) return this
    if (other.isEmpty) return other

    // Allocate values buffer eagerly, but keys buffer lazily if we need to drop entries
    var keysBuffer: Array[Slot] = null
    val valuesBuffer = new Array[Slot](size)
    var i = 0
    var j = 0
    while (i < size) {
      val v = _values(i)
      other.getOrElse(v, null) match {
        case null =>
          // Drop this entry; allocate buffers if needed
          if (keysBuffer == null) {
            // First drop; allocate buffers and copy kept entries so far
            keysBuffer = new Array[Slot](size)
            Array.copy(_keys, 0, keysBuffer, 0, j)
          }
        case w =>
          if (keysBuffer != null) {
            // We are dropping some entries; copy kept ones to keys buffer
            keysBuffer(j) = _keys(i)
          }
          valuesBuffer(j) = w
          j += 1
      }
      i += 1
    }

    if (i == j) {
      // All entries were kept
      SlotMap(_keys, valuesBuffer)
    } else {
      // Some entries were dropped; return a smaller array
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
    val newKeys = mutable.ArrayBuffer.empty[Slot]
    val newValues = mutable.ArrayBuffer.empty[Slot]
    for (i <- _keys.indices) {
      val k = _keys(i)
      if (p(k)) {
        newKeys += k
        newValues += _values(i)
      }
    }
    SlotMap(newKeys.toArray, newValues.toArray)
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
   * Identity mapping on the given set (`s -> s` for all `s`).
   */
  def identity(set: Set[Slot]): SlotMap = {
    val keys = set.toArray.sorted
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
