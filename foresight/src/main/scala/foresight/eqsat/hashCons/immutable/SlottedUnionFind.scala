package foresight.eqsat.hashCons.immutable

import foresight.eqsat.{EClassCall, EClassRef}

private[hashCons] final case class SlottedUnionFind(parents: Map[EClassRef, EClassCall]) {
  /**
   * Checks if the given e-class is the canonical representative of its set. Assumes that the e-class is in the union-find.
   * @param ref The e-class to check.
   * @return True if the e-class is the canonical representative of its set, false otherwise.
   */
  def isCanonical(ref: EClassRef): Boolean = {
    // require(parents.contains(ref), s"EClassRef $ref is not in the union-find.")
    parents(ref).ref == ref
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
    val parent = parents.getOrElse(key, null)
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

private object SlottedUnionFind {
  /**
   * Creates a new union-find with no elements.
   * @return An empty union-find.
   */
  def empty: SlottedUnionFind = SlottedUnionFind(Map.empty)
}
