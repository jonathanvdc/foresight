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
}
