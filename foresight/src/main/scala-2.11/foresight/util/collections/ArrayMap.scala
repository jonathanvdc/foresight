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

  override def getOrElse[B1 >: V](key: K, default: => B1): B1 = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx >= 0) valueAt[V](_values, idx) else default
  }

  override def iterator: Iterator[(K, V)] = ArrayMapArrays.iterator[K, V](_keys, _values, size)

  override def +[V1 >: V](kv: (K, V1)): immutable.Map[K, V1] = {
    val (k, v) = kv
    val rawK = k.asInstanceOf[AnyRef]
    val rawV = v.asInstanceOf[AnyRef]
    val idx  = indexOf(_keys, rawK, size)
    if (idx >= 0) {
      val (ks, vs, n) = copyUpdateAt(_keys, _values, size, idx, rawV)
      new ArrayMap[K, V1](ks, vs, n)
    } else {
      val (ks, vs, n) = copyAppend(_keys, _values, size, rawK, rawV)
      new ArrayMap[K, V1](ks, vs, n)
    }
  }

  override def -(key: K): immutable.Map[K, V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx < 0) this
    else if (size == 1) ArrayMap.empty
    else {
      val (ks, vs, n) = copyRemoveAt(_keys, _values, size, idx)
      new ArrayMap[K, V](ks, vs, n)
    }
  }
}

object ArrayMap {
  def empty[K, V]: ArrayMap[K, V] = new ArrayMap[K, V](new Array[AnyRef](0), new Array[AnyRef](0), 0)
  def apply[K, V](kvs: (K, V)*): ArrayMap[K, V] = from(kvs)
  def from[K, V](it: TraversableOnce[(K, V)]): ArrayMap[K, V] = it match {
    case am: ArrayMap[K, V] => am
    case _ =>
      var res = empty[K, V]
      val iter = it.toIterator
      while (iter.hasNext) {
        val (k, v) = iter.next()
        res = (res + (k -> v)).asInstanceOf[ArrayMap[K, V]]
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
