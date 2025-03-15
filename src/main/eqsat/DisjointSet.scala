package eqsat

/**
 * An immutable disjoint set data structure.
 */
final case class DisjointSet[A](parents: Map[A, A], ranks: Map[A, Int]) {
  /**
   * Finds the representative of the set that contains the given element and compresses the path.
   * @param a The element to find the representative of.
   * @return The representative of the set that contains a and the new disjoint set with the path compressed.
   */
  private def findAndCompress(a: A): (A, DisjointSet[A]) = {
    parents.get(a) match {
      case None => (a, this)
      case Some(parent) if parent == a => (a, this)
      case Some(parent) =>
        val (root, updatedSet) = findAndCompress(parent)
        val newParents = updatedSet.parents + (a -> root)
        (root, DisjointSet(newParents, updatedSet.ranks))
    }
  }

  /**
   * Finds the representative of the set that contains the given element.
   * @param a The element to find the representative of.
   * @return The representative of the set that contains a.
   */
  def find(a: A): A = findAndCompress(a)._1

  /**
   * Unions the sets that contain the given elements.
   * @param a The first element.
   * @param b The second element.
   * @return The new disjoint set with the sets containing a and b unioned.
   */
  def union(a: A, b: A): DisjointSet[A] = {
    val (rootA, s1) = this.findAndCompress(a)
    val (rootB, s2) = s1.findAndCompress(b)
    if (rootA == rootB) this
    else {
      val rankA = s2.ranks.getOrElse(rootA, 0)
      val rankB = s2.ranks.getOrElse(rootB, 0)
      if (rankA < rankB) {
        DisjointSet(s2.parents + (rootA -> rootB), s2.ranks)
      } else if (rankA > rankB) {
        DisjointSet(s2.parents + (rootB -> rootA), s2.ranks)
      } else {
        DisjointSet(s2.parents + (rootB -> rootA), s2.ranks + (rootA -> (rankA + 1)))
      }
    }
  }
}

/**
 * A companion object for the disjoint set data structure.
 */
object DisjointSet {
  /**
   * Creates a new disjoint set with no elements.
   * @tparam A The type of the elements in the disjoint set.
   * @return An empty disjoint set.
   */
  def empty[A]: DisjointSet[A] = DisjointSet(Map.empty, Map.empty)
}
