package foresight.util.collections

import scala.collection.generic.ImmutableMapFactory
import scala.collection.{GenTraversableOnce, immutable, mutable}

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
  extends immutable.AbstractMap[K, V]
    with immutable.MapLike[K, V, ArrayMap[K, V]]
    with Serializable {

  import foresight.util.collections.ArrayMapArrays._

  private[foresight] def unsafeKeysArray: ArrayMapArrays.ArrRef = _keys
  private[foresight] def unsafeValuesArray: ArrayMapArrays.ArrRef = _values

  override def empty: ArrayMap[K, V] = ArrayMap.empty

  override def get(key: K): Option[V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx >= 0) Some(valueAt[V](_values, idx)) else None
  }

  override def getOrElse[B1 >: V](key: K, default: => B1): B1 = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx >= 0) valueAt[V](_values, idx) else default
  }

  override def iterator: Iterator[(K, V)] = ArrayMapArrays.iterator[K, V](_keys, _values, size)

  override def updated[V1 >: V](k: K, v: V1): ArrayMap[K, V1] = {
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

  override def +[V1 >: V](kv: (K, V1)): ArrayMap[K, V1] = updated(kv._1, kv._2)

  override def -(key: K): ArrayMap[K, V] = {
    val idx = indexOf(_keys, key.asInstanceOf[AnyRef], size)
    if (idx < 0) this
    else if (size == 1) ArrayMap.empty
    else {
      val (ks, vs, n) = copyRemoveAt(_keys, _values, size, idx)
      new ArrayMap[K, V](ks, vs, n)
    }
  }

  override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): ArrayMap[K, V1] = {
    val builder = ArrayMap.newBuilder[K, V1]
    builder.sizeHint(this.size + xs.size)
    builder ++= this
    xs.foreach(builder.addOne)
    builder.result()
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

object ArrayMap extends ImmutableMapFactory[ArrayMap] {
  type ArrRef = Array[AnyRef]

  def empty[K, V]: ArrayMap[K, V] = new ArrayMap[K, V](new Array[AnyRef](0), new Array[AnyRef](0), 0)
  def from[K, V](it: TraversableOnce[(K, V)]): ArrayMap[K, V] = it match {
    case am: ArrayMap[K, V] => am
    case _ =>
      val builder = newBuilder[K, V]
      builder.sizeHint(it.size)
      builder ++= it
      builder.result()
  }

  override def newBuilder[K, V]: ArrayMapBuilder[K, V] = new ArrayMapBuilder[K, V]()

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
  private[foresight] def unsafeWrapArrays[K, V](keys: Array[_], values: Array[_], size: Int): ArrayMap[K, V] = {
    require(keys.length >= size, "Keys array length must be at least size")
    require(values.length >= size, "Values array length must be at least size")
    new ArrayMap[K, V](keys.asInstanceOf[ArrayMapArrays.ArrRef], values.asInstanceOf[ArrayMapArrays.ArrRef], size)
  }
}
