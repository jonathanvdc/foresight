package foresight.util.collections

import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{AbstractMap, Iterable, ListMap, SeqMap, StrictOptimizedMapOps}
import scala.collection.{MapFactory, MapFactoryDefaults, immutable, mutable}

/**
 * A small immutable map implemented using two arrays for keys and values.
 *
 * This implementation is optimized for small maps (a few dozen entries or less).
 * It provides efficient lookups, insertions, and deletions while maintaining
 * insertion order of keys.
 *
 * @param _keys   The array of keys.
 * @param _values The array of values.
 * @param size    The number of valid entries in the arrays.
 * @tparam K The type of keys.
 * @tparam V The type of values.
 */
final class ArrayMap[K, +V] private[collections] (
                                                   private val _keys: ArrayMapArrays.ArrRef,
                                                   private val _values: ArrayMapArrays.ArrRef,
                                                   override val size: Int
                                                 )
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, ArrayMap, ArrayMap[K, V]]
    with MapFactoryDefaults[K, V, ArrayMap, Iterable]
    with DefaultSerializable {

  import foresight.util.collections.ArrayMapArrays._

  private[foresight] def unsafeKeysArray: ArrayMapArrays.ArrRef = _keys
  private[foresight] def unsafeValuesArray: ArrayMapArrays.ArrRef = _values

  override def mapFactory: MapFactory[ArrayMap] = ArrayMap

  override def get(key: K): Option[V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx >= 0) Some(valueAt[V](_values, idx)) else None
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx >= 0) valueAt[V](_values, idx) else default
  }

  override def contains(key: K): Boolean = indexOf(_keys, key.asInstanceOf[AnyRef], size) >= 0

  override def iterator: Iterator[(K, V)] = ArrayMapArrays.iterator[K, V](_keys, _values, size)

  override def removed(key: K): ArrayMap[K, V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx < 0) this
    else if (size == 1) ArrayMap.empty
    else {
      val (ks, vs, n) = copyRemoveAt(_keys, _values, size, idx)
      new ArrayMap[K, V](ks, vs, n)
    }
  }

  override def updated[V1 >: V](key: K, value: V1): ArrayMap[K, V1] = {
    val rawK = key.asInstanceOf[AnyRef]
    val rawV = value.asInstanceOf[AnyRef]
    val idx  = indexOf(_keys, rawK, size)
    if (idx >= 0) {
      val (ks, vs, n) = copyUpdateAt(_keys, _values, size, idx, rawV)
      new ArrayMap[K, V1](ks, vs, n)
    } else {
      val (ks, vs, n) = copyAppend(_keys, _values, size, rawK, rawV)
      new ArrayMap[K, V1](ks, vs, n)
    }
  }

  /**
   * Maps the values of the ArrayMap strictly, i.e., it does not allow for lazy evaluation.
   * This means that the function `f` is applied to each value immediately, and the
   * resulting ArrayMap is created without any lazy evaluation.
   *
   * @param f The function to apply to each value.
   * @tparam W The type of the resulting values.
   * @return A new ArrayMap with the same keys and the values transformed by `f`.
   */
  def mapValuesStrict[W](f: V => W): ArrayMap[K, W] = {
    var i = 0
    while (i < this.size) {
      val oldRef = _values(i)
      val mapped = f(oldRef.asInstanceOf[V]).asInstanceOf[AnyRef]
      if (mapped ne oldRef) {
        // First difference found: allocate once and finish mapping
        val newValues = new Array[AnyRef](this.size)
        // values for [0, i) are unchanged; copy existing refs
        java.lang.System.arraycopy(_values, 0, newValues, 0, i)
        newValues(i) = mapped
        i += 1
        while (i < this.size) {
          newValues(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
          i += 1
        }
        return new ArrayMap[K, W](_keys, newValues, this.size)
      }
      i += 1
    }
    // No changes produced by f; reuse this instance (safe due to AnyRef storage)
    this.asInstanceOf[ArrayMap[K, W]]
  }
}

object ArrayMap extends MapFactory[ArrayMap] {
  private val _empty: ArrayMap[Any, Any] = new ArrayMap[Any, Any](new Array[AnyRef](0), new Array[AnyRef](0), 0)

  def empty[K, V]: ArrayMap[K, V] = _empty.asInstanceOf[ArrayMap[K, V]]

  def from[K, V](it: IterableOnce[(K, V)]): ArrayMap[K, V] = it match {
    case am: ArrayMap[K, V] => am
    case _ =>
      val builder = newBuilder[K, V]
      builder.sizeHint(it.size)
      builder ++= it
      builder.result()
  }

  /**
   * Wraps the given arrays as an ArrayMap without copying.
   * This method is unsafe because it exposes the internal representation of the map.
   * The caller must not modify the arrays after calling this method.
   *
   * @param keys   The array of keys.
   * @param values The array of values.
   * @param size   The number of valid entries in the arrays.
   * @return An ArrayMap wrapping the given arrays.
   */
  private[foresight] def wrapArraysUnsafe[K, V](keys: Array[AnyRef], values: Array[AnyRef], size: Int): ArrayMap[K, V] = {
    require(keys.length >= size, "Keys array length must be at least size")
    require(values.length >= size, "Values array length must be at least size")
    new ArrayMap[K, V](keys, values, size)
  }

  override def newBuilder[K, V]: ArrayMapBuilder[K, V] = new ArrayMapBuilder[K, V]()
}
