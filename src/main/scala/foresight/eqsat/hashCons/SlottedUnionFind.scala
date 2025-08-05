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
   * Finds the representative of the given key and compresses the path. If the key is not in the union-find, None is
   * returned; otherwise, the representative of the key, the renaming of the path, and the new union-find are
   * returned.
   *
   * @param key The key to find.
   * @return The representative of the key, the renaming of the path, and the new union-find, if the key is in the
   *         union-find. None otherwise.
   */
  def tryFindAndCompress(key: EClassRef): Option[(EClassCall, SlottedUnionFind)] = {
    parents.get(key) match {
      case None => None
      case Some(parent) if parent.ref == key => Some((parent, this))
      case Some(parent) =>
        val (grandparent, newDisjointSet) = findAndCompress(parent)
        val newMap = newDisjointSet.parents + (key -> grandparent)
        Some((grandparent, SlottedUnionFind(newMap)))
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
    tryFindAndCompress(ref).get
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, None is returned; otherwise, the
   * representative of the key and the new union-find are returned.
   *
   * @param ref The key to find.
   * @return The representative of the key and the new union-find, if the key is in the union-find. None otherwise.
   */
  def tryFindAndCompress(ref: EClassCall): Option[(EClassCall, SlottedUnionFind)] = {
    tryFindAndCompress(ref.ref).map {
      case (rep, disjointSet) => (rep.rename(ref.args), disjointSet)
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
    tryFindAndCompress(ref).get
  }

  /**
   * Finds the representative of the given key. If the key is not in the union-find, None is returned; otherwise, the
   * representative of the key is returned.
   *
   * @param key The key to find.
   * @return The representative of the key, if the key is in the union-find. None otherwise.
   */
  def tryFind(key: EClassRef): Option[EClassCall] = {
    tryFindAndCompress(key).map { _._1 }
  }
}

private object SlottedUnionFind {
  /**
   * Creates a new union-find with no elements.
   * @return An empty union-find.
   */
  def empty: SlottedUnionFind = SlottedUnionFind(Map.empty)
}
