package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef}
import foresight.eqsat.collections.SlotSet

/**
 * A mutable union-find data structure that supports path compression. The keys are e-class references
 * and the values are e-class calls, which include the renaming of the slots.
 */
private[hashCons] abstract class AbstractMutableSlottedUnionFind extends AbstractSlottedUnionFind {
  /**
   * The number of elements in the union-find.
   */
  def size: Int

  /**
   * Updates the parent of the given key to the given value.
   *
   * @param key   The key to update.
   * @param value The new parent of the key.
   */
  def update(key: EClassRef, value: EClassCall): Unit

  /**
   * Adds a new key to the union-find with itself as its own parent and the given slots.
   * @param slots The slots for the new key.
   */
  def add(slots: SlotSet): EClassRef

  /**
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, null is
   * returned; otherwise, the representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key, if the key is in the union-find.
   *         Null otherwise.
   */
  final def findAndCompressOrNull(ref: EClassRef): EClassCall = {
    val parent = getParentOrNull(ref)
    if (parent == null) {
      null
    } else if (parent.ref == ref) {
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
  final def findAndCompress(ref: EClassRef): EClassCall = {
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
  final def findAndCompress(ref: EClassCall): EClassCall = {
    val parent = findAndCompress(ref.ref)
    if (parent == null) {
      throw new NoSuchElementException(s"EClassRef ${ref.ref} is not in the union-find.")
    }
    parent.rename(ref.args)
  }
}
