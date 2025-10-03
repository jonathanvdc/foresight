package foresight.util.collections

import scala.collection.AbstractIterator

private[collections] object ArrayMapArrays {
  type ArrRef = ArrayMap.ArrRef

  @inline def keyAt[K](keys: ArrRef, i: Int): K = keys(i).asInstanceOf[K]
  @inline def valueAt[V](values: ArrRef, i: Int): V = values(i).asInstanceOf[V]

  @inline def indexOf(keys: ArrRef, key: AnyRef, size: Int): Int = {
    var i = 0
    while (i < size) {
      if (keys(i) == key) return i
      i += 1
    }
    -1
  }

  @inline def copyRemoveAt(keys: ArrRef, values: ArrRef, size: Int, idx: Int): (ArrRef, ArrRef, Int) = {
    val n  = size - 1
    val ks = new Array[AnyRef](n)
    val vs = new Array[AnyRef](n)
    // prefix
    System.arraycopy(keys, 0, ks, 0, idx)
    System.arraycopy(values, 0, vs, 0, idx)
    // suffix
    val tail = size - idx - 1
    System.arraycopy(keys, idx + 1, ks, idx, tail)
    System.arraycopy(values, idx + 1, vs, idx, tail)
    (ks, vs, n)
  }

  @inline def copyUpdateAt(keys: ArrRef, values: ArrRef, size: Int, idx: Int, newVal: AnyRef): (ArrRef, ArrRef, Int) = {
    val vs = new Array[AnyRef](size)
    System.arraycopy(values, 0, vs, 0, size)
    vs(idx) = newVal
    (keys, vs, size)
  }

  @inline def copyAppend(keys: ArrRef, values: ArrRef, size: Int, k: AnyRef, v: AnyRef): (ArrRef, ArrRef, Int) = {
    val n  = size + 1
    val ks = new Array[AnyRef](n)
    val vs = new Array[AnyRef](n)
    System.arraycopy(keys, 0, ks, 0, size)
    System.arraycopy(values, 0, vs, 0, size)
    ks(size) = k
    vs(size) = v
    (ks, vs, n)
  }

  @inline def iterator[K, V](keys: ArrRef, values: ArrRef, sz: Int): Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private var i = 0
    override def hasNext: Boolean = i < sz
    override def next(): (K, V) = {
      val k = keyAt[K](keys, i)
      val v = valueAt[V](values, i)
      i += 1
      (k, v)
    }
  }
}
