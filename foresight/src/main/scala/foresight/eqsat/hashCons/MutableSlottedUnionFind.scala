package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef, Slot}

private final class MutableSlottedUnionFind(var set: SlottedUnionFind) {
  def add(key: EClassRef, slots: Set[Slot]): Unit = {
    set = set.add(key, slots)
  }

  def update(key: EClassRef, value: EClassCall): Unit = {
    set = set.update(key, value)
  }
  
  def isCanonical(ref: EClassRef): Boolean = {
    set.isCanonical(ref)
  }

  def findAndCompressOrNull(key: EClassRef): EClassCall = {
    foldUpdateIntoSet(set.findAndCompressOrNull(key))
  }

  def findAndCompress(ref: EClassRef): EClassCall = {
    val (result, newSet) = set.findAndCompress(ref)
    set = newSet
    result
  }

  def findAndCompressOrNull(ref: EClassCall): EClassCall = {
    foldUpdateIntoSet(set.findAndCompressOrNull(ref))
  }

  def findAndCompress(ref: EClassCall): EClassCall = {
    val (result, newSet) = set.findAndCompress(ref)
    set = newSet
    result
  }

  private def foldUpdateIntoSet[A](pair: (A, SlottedUnionFind)): A = {
    val (result , newSet) = pair
    if (result == null) {
      null.asInstanceOf[A]
    } else {
      set = newSet
      result
    }
  }
}
