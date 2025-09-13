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
 *
 * @param map The underlying immutable mapping from parameter to argument slots.
 */
final case class SlotMap(map: Map[Slot, Slot]) extends Permutation[SlotMap] with Ordered[SlotMap] {
  /**
   * The number of entries in this map.
   */
  def size: Int = map.size

  /**
   * True if the map has no entries.
   */
  def isEmpty: Boolean = map.isEmpty

  /**
   * True if `k` is present as a key.
   */
  def contains(k: Slot): Boolean = map.contains(k)

  /**
   * Returns a copy with an extra mapping `k -> v`.
   */
  def insert(k: Slot, v: Slot): SlotMap = {
    SlotMap(map + (k -> v))
  }

  private def sortedByKeys = map.toSeq.sortBy(_._1)

  /**
   * Returns the value bound to `k`, if any.
   */
  def get(k: Slot): Option[Slot] = map.get(k)

  /**
   * Entries in ascending key order.
   */
  def iterator: Iterator[(Slot, Slot)] = sortedByKeys.iterator

  /**
   * Keys in ascending order.
   */
  def keys: Seq[Slot] = sortedByKeys.map(_._1)

  /**
   * Values in the order of their sorted keys.
   */
  def values: Seq[Slot] = sortedByKeys.map(_._2)

  /**
   * The set of keys.
   */
  def keySet: Set[Slot] = map.keySet

  /**
   * The set of values (distinct).
   */
  def valueSet: Set[Slot] = map.values.toSet

  /**
   * Inverts the mapping by swapping keys and values.
   *
   * Only meaningful when this map is a bijection. If multiple keys map to the same value,
   * later entries overwrite earlier ones in the result.
   *
   * @return The inverted map (`v -> k` for each `k -> v`).
   */
  override def inverse: SlotMap = {
    val newMap = map.map(_.swap)
    SlotMap(newMap)
  }

  /**
   * True if each key maps to a unique value (no collisions in `values`).
   */
  def isBijection: Boolean = {
    val valuesSet = map.values.toSet
    valuesSet.size == map.size
  }

  /**
   * True if this is a bijection and `keySet == valueSet`.
   */
  def isPermutation: Boolean = isBijection && keySet == valueSet

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
    val newMap = map.flatMap { case (k, v) =>
      other.get(v).map(k -> _)
    }
    SlotMap(newMap)
  }

  /**
   * Retaining composition `other ∘ this` with fallback.
   *
   * Like `compose`, but if an intermediate value `b` is not a key in `other`, keeps the original `a -> b`.
   * Useful when you want to apply a renaming where defined, but leave other bindings unchanged.
   */
  def composeRetain(other: SlotMap): SlotMap = {
    val newMap = map.map { case (k, v) =>
      k -> other.get(v).getOrElse(v)
    }
    SlotMap(newMap)
  }

  /**
   * Freshening composition `other ∘ this`.
   *
   * Like `compose`, but if an intermediate value `b` is not a key in `other`, replaces it with a fresh slot.
   * Useful for isolating unmapped bindings so they cannot alias existing slots.
   */
  def composeFresh(other: SlotMap): SlotMap = {
    val newMap = map.map { case (k, v) =>
      k -> other.map.getOrElse(v, Slot.fresh())
    }
    SlotMap(newMap)
  }

  /**
   * Internal sanity check to detect duplicate keys.
   */
  private[eqsat] def check(): Unit = {
    val found = mutable.HashSet[Slot]()
    map.keys.foreach { x =>
      assert(!found.contains(x))
      found.add(x)
    }
  }

  /**
   * Applies this mapping to a slot, returning the image or the slot itself if unmapped.
   */
  override def apply(slot: Slot): Slot = map.getOrElse(slot, slot)

  /**
   * Lexicographic order by sorted keys, then by the values at those keys.
   */
  override def compare(that: SlotMap): Int = {
    val leftSortedKeys = keys
    val rightSortedKeys = that.keys

    val ordering = SeqOrdering.lexOrdering(Ordering.by(identity[Slot]))

    val keyComparison = ordering.compare(leftSortedKeys, rightSortedKeys)
    if (keyComparison != 0) {
      return keyComparison
    }

    val leftSortedValues = leftSortedKeys.map(map)
    val rightSortedValues = rightSortedKeys.map(that.map)

    ordering.compare(leftSortedValues, rightSortedValues)
  }

  /**
   * Keeps only entries whose key satisfies `p`.
   */
  def filterKeys(p: Slot => Boolean): SlotMap = {
    SlotMap(map.filterKeysStrict(p))
  }
}

/**
 * Constructors and utilities for building [[SlotMap]]s.
 */
object SlotMap {
  /**
   * The empty mapping.
   */
  val empty: SlotMap = SlotMap(Map.empty)

  /**
   * Builds a map from `(key, value)` pairs. Later pairs overwrite earlier ones with the same key.
   */
  def fromPairs(pairs: Iterable[(Slot, Slot)]): SlotMap = {
    SlotMap(pairs.toMap)
  }

  /**
   * Identity mapping on the given set (`s -> s` for all `s`).
   */
  def identity(set: Set[Slot]): SlotMap = {
    val newMap = set.map(x => x -> x).toMap
    SlotMap(newMap)
  }

  /**
   * Builds a bijection from each key in `set` to a fresh unique slot.
   *
   * Useful for capturing without aliasing or for introducing fresh binders.
   */
  def bijectionFromSetToFresh(set: Set[Slot]): SlotMap = {
    val newMap = set.map(x => x -> Slot.fresh()).toMap
    SlotMap(newMap)
  }
}
