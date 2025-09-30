package foresight.eqsat.hashCons.mutable

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassCall, EClassRef}
import foresight.eqsat.hashCons.AbstractMutableSlottedUnionFind

import scala.collection.mutable

/**
 * A mutable union-find data structure that uses a hash map to store parent pointers.
  *
  * Write operations to this implementation is not thread-safe.
  */
private[hashCons] final class SlottedUnionFind extends AbstractMutableSlottedUnionFind {
  private val parents: mutable.ArrayBuffer[EClassCall] = mutable.ArrayBuffer.empty

  override def update(key: EClassRef, value: EClassCall): Unit = {
    parents.update(key.id, value)
  }

  override protected def getParentOrNull(ref: EClassRef): EClassCall = {
    if (ref.id >= parents.length) null
    else parents(ref.id)
  }

  override def size: Int = parents.size

  override def add(slots: SlotSet): EClassRef = {
    val key = EClassRef(size)
    parents.append(EClassCall(key, SlotMap.identity(slots)))
    key
  }
}
