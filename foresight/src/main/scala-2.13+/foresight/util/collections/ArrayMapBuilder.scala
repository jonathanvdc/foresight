package foresight.util.collections

import scala.collection.mutable

class ArrayMapBuilder[K, V] extends mutable.Builder[(K, V), ArrayMap[K, V]] {
  import foresight.util.collections.ArrayMapArrays._

  // Growable buffers; we only allocate once up front and then grow geometrically.
  private var keys: Array[AnyRef] = new Array[AnyRef](4)
  private var values: Array[AnyRef] = new Array[AnyRef](4)
  private var n: Int = 0

  private def grow(): Unit = {
    val cap = math.max(4, keys.length * 2)
    val newK = new Array[AnyRef](cap)
    val newV = new Array[AnyRef](cap)
    java.lang.System.arraycopy(keys, 0, newK, 0, n)
    java.lang.System.arraycopy(values, 0, newV, 0, n)
    keys = newK
    values = newV
  }

  override def addOne(elem: (K, V)): this.type = {
    val kRef = elem._1.asInstanceOf[AnyRef]
    val vRef = elem._2.asInstanceOf[AnyRef]
    val idx = indexOf(keys, kRef, n)
    if (idx >= 0) {
      // overwrite in place; preserves first-in insertion order
      values(idx) = vRef
    } else {
      if (n == keys.length) grow()
      keys(n) = kRef
      values(n) = vRef
      n += 1
    }
    this
  }

  override def clear(): Unit = {
    // Keep capacity to be frugal; just reset the logical size.
    n = 0
  }

  override def result(): ArrayMap[K, V] = {
    ArrayMap.wrapArraysUnsafe[K, V](keys, values, n)
  }
}
