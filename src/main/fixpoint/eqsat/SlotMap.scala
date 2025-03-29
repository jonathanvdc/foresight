package fixpoint.eqsat

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqDerivedOrdering

/**
 * A mapping of parameter slots to argument slots.
 *
 * @param map The mapping of slots to slots.
 */
final case class SlotMap(map: Map[Slot, Slot]) extends Permutation[SlotMap] with Ordered[SlotMap] {
  /**
   * The number of slots in the slot map.
   */
  def size: Int = map.size

  /**
   * Check if the slot map is empty.
   */
  def isEmpty: Boolean = map.isEmpty

  /**
   * Check if the slot map contains a slot.
   *
   * @param k The slot to check for.
   * @return True if the slot map contains the slot.
   */
  def contains(k: Slot): Boolean = map.contains(k)

  /**
   * Insert a slot mapping into the slot map.
   *
   * @param k The key.
   * @param v The value.
   * @return A new slot map with the mapping inserted.
   */
  def insert(k: Slot, v: Slot): SlotMap = {
    SlotMap(map + (k -> v))
  }

  private def sortedByKeys = map.toSeq.sortBy(_._1)

  /**
   * Get the value to which a key slot maps.
   *
   * @param k The key.
   * @return The value, if it exists.
   */
  def get(k: Slot): Option[Slot] = map.get(k)

  def iterator: Iterator[(Slot, Slot)] = sortedByKeys.iterator

  def keys: Seq[Slot] = sortedByKeys.map(_._1)

  def values: Seq[Slot] = sortedByKeys.map(_._2)

  def keySet: Set[Slot] = map.keySet

  def valueSet: Set[Slot] = map.values.toSet

  /**
   * Invert the slot map. For a, b in the slot map, the result will contain b -> a.
   * @return The inverted slot map.
   */
  override def inverse: SlotMap = {
    val newMap = map.map(_.swap)
    SlotMap(newMap)
  }

  /**
   * Check if the slot map is a bijection. A bijection is a mapping where each key maps to a unique value.
   *
   * @return True if the slot map is a bijection.
   */
  def isBijection: Boolean = {
    val valuesSet = map.values.toSet
    valuesSet.size == map.size
  }

  /**
   * Check if the slot map is a permutation. A permutation is a bijection where the keys and values are the same.
   *
   * @return True if the slot map is a permutation.
   */
  def isPermutation: Boolean = isBijection && keySet == valueSet

  /**
   * Compose with another slot map. For a, b, c such that a -> b in this slot map and b -> c in the other slot map,
   * the result will contain a -> c. Throws an exception if the slot maps are not composable.
   *
   * @param other The other slot map.
   * @return A new slot map.
   */
  def compose(other: SlotMap): SlotMap = {
    if (valueSet != other.keySet) {
      throw new IllegalArgumentException("Slot maps are not composable")
    }
    composePartial(other)
  }

  /**
   * Compose with another slot map. For a, b, c such that a -> b in this slot map and b -> c in the other slot map,
   * the result will contain a -> c. Discards any slots that are not in the other slot map.
   *
   * @param other The other slot map.
   * @return A new slot map.
   */
  def composePartial(other: SlotMap): SlotMap = {
    val newMap = map.flatMap { case (k, v) =>
      other.get(v).map(k -> _)
    }
    SlotMap(newMap)
  }

  /**
   * Compose with another slot map. For a, b, c such that a -> b in this slot map and b -> c in the other slot map,
   * the result will contain a -> c. Retains the original mapping a -> b if b is not in the other slot map.
   *
   * @param other The other slot map.
   * @return A new slot map.
   */
  def composeRetain(other: SlotMap): SlotMap = {
    val newMap = map.map { case (k, v) =>
      k -> other.get(v).getOrElse(v)
    }
    SlotMap(newMap)
  }

  /**
   * Compose with another slot map. For a, b, c such that a -> b in this slot map and b -> c in the other slot map,
   * the result will contain a -> c. If b is not in the other slot map, it is replaced with a fresh slot.
   *
   * @param other The other slot map.
   * @return A new slot map with fresh slots.
   */
  def composeFresh(other: SlotMap): SlotMap = {
    val newMap = map.map { case (k, v) =>
      k -> other.map.getOrElse(v, Slot.fresh())
    }
    SlotMap(newMap)
  }

  private[eqsat] def check(): Unit = {
    val found = mutable.HashSet[Slot]()
    map.keys.foreach { x =>
      assert(!found.contains(x))
      found.add(x)
    }
  }

  /**
   * Apply the slot map to a slot.
   *
   * @param slot The slot to apply the slot map to.
   * @return The result of applying the slot map.
   */
  override def apply(slot: Slot): Slot = map.getOrElse(slot, slot)

  /**
   * Compare two slot maps.
   *
   * @param that The other slot map.
   * @return -1 if this slot map is less than the other, 0 if they are equal, and 1 if this slot map is greater.
   */
  override def compare(that: SlotMap): Int = {
    val leftSortedKeys = keys
    val rightSortedKeys = that.keys

    val keyComparison = leftSortedKeys.compare(rightSortedKeys)
    if (keyComparison != 0) {
      return keyComparison
    }

    val leftSortedValues = leftSortedKeys.map(map)
    val rightSortedValues = rightSortedKeys.map(that.map)

    leftSortedValues.compare(rightSortedValues)
  }

  /**
   * Filters the slot map by a predicate on the keys.
   * @param p The predicate to filter the keys by.
   * @return A new slot map with only the keys that satisfy the predicate.
   */
  def filterKeys(p: Slot => Boolean): SlotMap = {
    SlotMap(map.filterKeys(p))
  }
}

/**
 * A companion object for the slot map data structure.
 */
object SlotMap {
  /**
   * An empty slot map.
   */
  def empty: SlotMap = SlotMap(Map.empty)

  /**
   * Create a slot map from a sequence of slot pairs.
   *
   * @param pairs The slot pairs.
   * @return A new slot map.
   */
  def fromPairs(pairs: Iterable[(Slot, Slot)]): SlotMap = {
    SlotMap(pairs.toMap)
  }

  /**
   * Create an identity slot map for a set of slots. The identity slot map maps each slot to itself.
   *
   * @param set The slots to create an identity slot map for.
   * @return A new slot map.
   */
  def identity(set: Set[Slot]): SlotMap = {
    val newMap = set.map(x => x -> x).toMap
    SlotMap(newMap)
  }

  /**
   * Create a bijection from a set of slots to fresh slots.
   *
   * @param set The set of slots that serve as keys in the bijection.
   * @return A new slot map.
   */
  def bijectionFromSetToFresh(set: Set[Slot]): SlotMap = {
    val newMap = set.map(x => x -> Slot.fresh()).toMap
    SlotMap(newMap)
  }
}
