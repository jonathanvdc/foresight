package foresight.eqsat.hashCons

import foresight.eqsat.collections.SlotSet
import foresight.eqsat.{EClassCall, EClassRef, Slot, SlotMap}

private final class MutableSlottedUnionFind(var parents: Map[EClassRef, EClassCall]) {
  def toImmutable: SlottedUnionFind = SlottedUnionFind(parents)

  /**
   * Checks if the given e-class is the canonical representative of its set. Assumes that the e-class is in the union-find.
   * @param ref The e-class to check.
   * @return True if the e-class is the canonical representative of its set, false otherwise.
   */
  def isCanonical(ref: EClassRef): Boolean = {
    // require(parents.contains(ref), s"EClassRef $ref is not in the union-find.")
    parents(ref).ref == ref
  }

  def add(key: EClassRef, slots: SlotSet): Unit = {
    update(key, EClassCall(key, SlotMap.identity(slots)))
  }

  def update(key: EClassRef, value: EClassCall): Unit = {
    parents = parents + (key -> value)
  }

  /**
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, null is
   * returned; otherwise, the representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key, if the key is in the union-find.
   *         Null otherwise.
   */
  def findAndCompressOrNull(ref: EClassRef): EClassCall = {
    val parent = parents.getOrElse(ref, return null)
    if (parent.ref == ref) {
      parent
    } else {
      val grandparent = findAndCompress(parent)
      update(ref, grandparent)
      grandparent
    }
  }

  /**
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, an
   * exception is thrown; otherwise, the representative of the key and the new union-find are returned.
   *
   * @param ref The key to find.
   * @return The representative of the key and the new union-find.
   */
  def findAndCompress(ref: EClassRef): EClassCall = {
    val result = findAndCompressOrNull(ref)
    if (result == null) {
      throw new NoSuchElementException(s"EClassRef $ref is not in the union-find.")
    } else {
      result
    }
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, an exception is thrown; otherwise,
   * the representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key.
   */
  def findAndCompress(ref: EClassCall): EClassCall = {
    val parent = findAndCompress(ref.ref)
    if (parent == null) {
      throw new NoSuchElementException(s"EClassRef ${ref.ref} is not in the union-find.")
    }
    parent.rename(ref.args)
  }
}
