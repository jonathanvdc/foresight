package foresight.eqsat.hashCons

import foresight.eqsat.{EClassCall, EClassRef, Slot, SlotMap}

private[hashCons] final case class SlottedUnionFind(parents: Map[EClassRef, EClassCall]) {
  /**
   * Updates the union-find with the given key and value.
   * @param key The key to update.
   * @param value The new value of the key.
   * @return The new union-find with the key updated.
   */
  def update(key: EClassRef, value: EClassCall): SlottedUnionFind = {
    SlottedUnionFind(parents + (key -> value))
  }

  /**
   * Adds a new e-class to the union-find with the given slots.
   * @param key The e-class to add.
   * @param slots The slots of the e-class.
   * @return The new union-find with the e-class added.
   */
  def add(key: EClassRef, slots: Set[Slot]): SlottedUnionFind = {
    update(key, EClassCall(key, SlotMap.identity(slots)))
  }

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
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, null is
   * returned; otherwise, the representative of the key and the new union-find are returned.
   *
   * @param ref The key to find.
   * @return The representative of the key and the new union-find, if the key is in the union-find.
   *         Null and the current union-find otherwise.
   */
  def findAndCompressOrNull(ref: EClassRef): (EClassCall, SlottedUnionFind) = {
    val parent = parents.getOrElse(ref, return (null, this))
    if (parent.ref == ref) {
      (parent, this)
    } else {
      val (grandparent, newDisjointSet) = findAndCompress(parent)
      val newMap = newDisjointSet.parents + (ref -> grandparent)
      (grandparent, SlottedUnionFind(newMap))
    }
  }

  /**
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, an
   * exception is thrown; otherwise, the representative of the key and the new union-find are returned.
   *
   * @param ref The key to find.
   * @return The representative of the key and the new union-find.
   */
  def findAndCompress(ref: EClassRef): (EClassCall, SlottedUnionFind) = {
    val (result, disjointSet) = findAndCompressOrNull(ref)
    if (result == null) {
      throw new NoSuchElementException(s"EClassRef $ref is not in the union-find.")
    } else {
      (result, disjointSet)
    }
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, null is returned; otherwise, the
   * representative of the key and the new union-find are returned.
   *
   * @param ref The key to find.
   * @return The representative of the key and the new union-find, if the key is in the union-find.
   *         Null and the current union-find otherwise.
   */
  def findAndCompressOrNull(ref: EClassCall): (EClassCall, SlottedUnionFind) = {
    val (parent, newDisjointSet) = findAndCompressOrNull(ref.ref)
    if (parent == null) {
      (null, this)
    } else {
      (parent.rename(ref.args), newDisjointSet)
    }
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
  def findAndCompress(ref: EClassCall): (EClassCall, SlottedUnionFind) = {
    val (parent, newDisjointSet) = findAndCompress(ref.ref)
    if (parent == null) {
      throw new NoSuchElementException(s"EClassRef ${ref.ref} is not in the union-find.")
    }
    (parent.rename(ref.args), newDisjointSet)
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
    val parent = parents.getOrElse(key, return null)
    if (parent.ref == key) {
      parent
    } else {
      findOrNull(parent)
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
