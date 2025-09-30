package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef}

/**
 * A union-find data structure that supports path compression. The keys are e-class references and
 * the values are e-class calls, which include the renaming of the slots.
 */
private[hashCons] abstract class AbstractSlottedUnionFind {
  /**
   * Finds the representative of the given key. If the key is not in the union-find, null is returned; otherwise, the
   * representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key, if the key is in the union-find. Null otherwise.
   */
  protected def getParentOrNull(ref: EClassRef): EClassCall

  /**
   * Constructs an e-class call with the given e-class reference and an empty slot map.
   * @param ref The e-class reference.
   * @return An e-class call with the given e-class reference and an empty slot map.
   */
  def callWithoutSlots(ref: EClassRef): EClassCall = ref.callWithoutSlots

  /**
   * Checks if the given e-class is the canonical representative of its set. Assumes that the e-class is in the union-find.
   * @param ref The e-class to check.
   * @return True if the e-class is the canonical representative of its set, false otherwise.
   */
  final def isCanonical(ref: EClassRef): Boolean = {
    // require(parents.contains(ref), s"EClassRef $ref must be in the union-find.")
    getParentOrNull(ref).ref == ref
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, null is returned; otherwise, the
   * representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key, if the key is in the union-find. Null otherwise.
   */
  def findOrNull(ref: EClassCall): EClassCall = {
    val parent = findOrNull(ref.ref)
    if (parent == null) {
      null
    } else {
      parent.rename(ref.args)
    }
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, an exception is thrown; otherwise,
   * the representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key.
   */
  def find(ref: EClassCall): EClassCall = {
    val result = findOrNull(ref)
    if (result == null) {
      throw new NoSuchElementException(s"EClassRef ${ref.ref} is not in the union-find.")
    } else {
      result
    }
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, null is returned; otherwise, the
   * representative of the key is returned.
   *
   * @param key The key to find.
   * @return The representative of the key, if the key is in the union-find. Null otherwise.
   */
  def findOrNull(key: EClassRef): EClassCall = {
    val parent = getParentOrNull(key)
    if (parent == null) {
      null
    } else if (parent.ref == key) {
      parent
    } else {
      findOrNull(parent)
    }
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, an exception is thrown; otherwise,
   * the representative of the key is returned.
   *
   * @param ref The key to find.
   * @return The representative of the key.
   */
  def find(ref: EClassRef): EClassCall = {
    val result = findOrNull(ref)
    if (result == null) {
      throw new NoSuchElementException(s"EClassRef $ref is not in the union-find.")
    } else {
      result
    }
  }
}
