package foresight.eqsat.hashCons.immutable

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.hashCons.AbstractMutableSlottedUnionFind
import foresight.eqsat.{EClassCall, EClassRef}

private final class SlottedUnionFindBuilder(var parents: Map[EClassRef, EClassCall])
  extends AbstractMutableSlottedUnionFind {

  def toImmutable: SlottedUnionFind = SlottedUnionFind(parents)

  override def update(key: EClassRef, value: EClassCall): Unit = {
    parents = parents + (key -> value)
  }

  override protected def getParentOrNull(ref: EClassRef): EClassCall = {
    parents.getOrElse(ref, null)
  }

  override def size: Int = parents.size

  /**
   * Adds a new key to the union-find with itself as its own parent and the given slots.
   *
   * @param slots The slots for the new key.
   */
  override def add(slots: SlotSet): EClassRef = {
    val key = new EClassRef(size)
    update(key, EClassCall(key, SlotMap.identity(slots)))
    key
  }
}
