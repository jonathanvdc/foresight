package foresight.util.collections

import scala.collection.immutable

final class ArrayMap[K, +V] private[collections] (
                                                   private val _keys: ArrayMapArrays.ArrRef,
                                                   private val _values: ArrayMapArrays.ArrRef,
                                                   override val size: Int
                                                 ) extends immutable.Map[K, V] with Serializable {
  import foresight.util.collections.ArrayMapArrays._

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

  override def removed(key: K): immutable.Map[K, V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx < 0) this
    else if (size == 1) ArrayMap.empty
    else {
      val (ks, vs, n) = copyRemoveAt(_keys, _values, size, idx)
      new ArrayMap[K, V](ks, vs, n)
    }
  }

  override def updated[V1 >: V](key: K, value: V1): immutable.Map[K, V1] = {
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
}

object ArrayMap {
  private val _empty: ArrayMap[Any, Any] = new ArrayMap[Any, Any](new Array[AnyRef](0), new Array[AnyRef](0), 0)

  def empty[K, V]: ArrayMap[K, V] = _empty.asInstanceOf[ArrayMap[K, V]]

  def apply[K, V](kvs: (K, V)*): ArrayMap[K, V] = from(kvs)
  def from[K, V](it: IterableOnce[(K, V)]): ArrayMap[K, V] = it match {
    case am: ArrayMap[K, V] => am
    case _ =>
      var res = empty[K, V]
      val iter = it.iterator
      while (iter.hasNext) {
        val (k, v) = iter.next()
        res = res.updated(k, v).asInstanceOf[ArrayMap[K, V]]
      }
      res
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
}
