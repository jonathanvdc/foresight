package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractSlottedUnionFind
import foresight.eqsat.{EClassCall, EClassRef}

private[hashCons] final case class SlottedUnionFind(parents: Map[EClassRef, EClassCall])
  extends AbstractSlottedUnionFind {

  override protected def getParentOrNull(ref: EClassRef): EClassCall = {
    parents.getOrElse(ref, null)
  }
}

private object SlottedUnionFind {
  /**
   * Creates a new union-find with no elements.
   * @return An empty union-find.
   */
  def empty: SlottedUnionFind = SlottedUnionFind(Map.empty)
}
