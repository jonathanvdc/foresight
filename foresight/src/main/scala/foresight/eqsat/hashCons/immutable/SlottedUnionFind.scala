package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractSlottedUnionFind
import foresight.eqsat.{EClassCall, EClassRef}

private[hashCons] final case class SlottedUnionFind(parents: Vector[EClassCall])
  extends AbstractSlottedUnionFind {

  override protected def getParentOrNull(ref: EClassRef): EClassCall = {
    if (ref.id >= parents.size) null
    else parents(ref.id)
  }
}

private object SlottedUnionFind {
  /**
   * Creates a new union-find with no elements.
   * @return An empty union-find.
   */
  def empty: SlottedUnionFind = SlottedUnionFind(Vector.empty)
}
