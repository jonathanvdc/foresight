package fixpoint.eqsat.hashCons

import fixpoint.eqsat.{AppliedRef, EClassRef}
import fixpoint.eqsat.slots.Slot

private final class MutableSlottedUnionFind(var set: SlottedUnionFind) {
  def add(key: EClassRef, slots: Set[Slot]): Unit = {
    set = set.add(key, slots)
  }

  def update(key: EClassRef, value: AppliedRef): Unit = {
    set = set.update(key, value)
  }

  def tryFindAndCompress(key: EClassRef): Option[AppliedRef] = {
    foldUpdateIntoSet(set.tryFindAndCompress(key))
  }

  def findAndCompress(ref: EClassRef): AppliedRef = {
    val (result, newSet) = set.findAndCompress(ref)
    set = newSet
    result
  }

  def tryFindAndCompress(ref: AppliedRef): Option[AppliedRef] = {
    foldUpdateIntoSet(set.tryFindAndCompress(ref))
  }

  def findAndCompress(ref: AppliedRef): AppliedRef = {
    val (result, newSet) = set.findAndCompress(ref)
    set = newSet
    result
  }

  private def foldUpdateIntoSet[A](pair: Option[(A, SlottedUnionFind)]): Option[A] = {
    pair match {
      case None => None
      case Some((result, newSet)) =>
        set = newSet
        Some(result)
    }
  }
}
