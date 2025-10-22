package foresight.eqsat.hashCons.mutable

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassCall, EClassRef}
import foresight.eqsat.hashCons.AbstractMutableSlottedUnionFind

import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * A mutable union-find data structure that uses an array to store parent pointers.
  *
  * Write operations to this implementation is not thread-safe.
  */
private[hashCons] final class SlottedUnionFind extends AbstractMutableSlottedUnionFind {
  private var parents: AtomicReferenceArray[EClassCall] =
    new AtomicReferenceArray[EClassCall](16)

  private var currentSize: Int = 0

  override def update(key: EClassRef, value: EClassCall): Unit = {
    parents.lazySet(key.id, value)
  }

  override protected def getParentOrNull(ref: EClassRef): EClassCall = {
    if (ref.id >= parents.length) null
    else parents.get(ref.id)
  }

  override def size: Int = currentSize

  override def add(slots: SlotSet): EClassRef = {
    val key = EClassRef(currentSize)
    if (key.id >= parents.length) {
      val newLength = parents.length * 2
      val newParents = new AtomicReferenceArray[EClassCall](newLength)
      var i = 0
      while (i < parents.length) {
        newParents.set(i, parents.get(i))
        i += 1
      }
      parents = newParents
    }

    currentSize += 1
    parents.set(key.id, EClassCall(key, SlotMap.identity(slots)))
    key
  }
}
