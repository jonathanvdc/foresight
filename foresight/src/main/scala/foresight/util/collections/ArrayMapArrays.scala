package foresight.util.collections

import scala.annotation.tailrec
import scala.collection.AbstractIterator

private[collections] object ArrayMapArrays {
  type ArrRef = Array[AnyRef]

  @inline def keyAt[K](keys: ArrRef, i: Int): K = keys(i).asInstanceOf[K]
  @inline def valueAt[V](values: ArrRef, i: Int): V = values(i).asInstanceOf[V]

  @tailrec def indexOf(keys: ArrRef, key: AnyRef, size: Int, i: Int = 0): Int =
    if (i >= size) -1
    else if (keys(i) == key) i
    else indexOf(keys, key, size, i + 1)

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
    val vs = java.util.Arrays.copyOf(values, size)
    vs(idx) = newVal
    (keys, vs, size)
  }

  @inline def copyAppend(keys: ArrRef, values: ArrRef, size: Int, k: AnyRef, v: AnyRef): (ArrRef, ArrRef, Int) = {
    val n  = size + 1
    val ks = java.util.Arrays.copyOf(keys, n)
    val vs = java.util.Arrays.copyOf(values, n)
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
