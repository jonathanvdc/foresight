package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef, Slot}

private final class MutableSlottedUnionFind(var set: SlottedUnionFind) {
  def add(key: EClassRef, slots: Set[Slot]): Unit = {
    set = set.add(key, slots)
  }

  def update(key: EClassRef, value: EClassCall): Unit = {
    set = set.update(key, value)
  }

  def tryFindAndCompress(key: EClassRef): Option[EClassCall] = {
    foldUpdateIntoSet(set.tryFindAndCompress(key))
  }

  def findAndCompress(ref: EClassRef): EClassCall = {
    val (result, newSet) = set.findAndCompress(ref)
    set = newSet
    result
  }

  def tryFindAndCompress(ref: EClassCall): Option[EClassCall] = {
    foldUpdateIntoSet(set.tryFindAndCompress(ref))
  }

  def findAndCompress(ref: EClassCall): EClassCall = {
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
