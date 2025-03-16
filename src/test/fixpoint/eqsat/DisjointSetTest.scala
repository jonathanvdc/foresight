package fixpoint.eqsat

import org.junit.Test

/**
 * Tests for the correctness of the disjoint set data structure.
 */
class DisjointSetTest {
  /**
   * Test that the find operation on an empty disjoint set returns the element itself.
   */
  @Test
  def trivialFind(): Unit = {
    val ds = DisjointSet.empty[Int]
    assert(ds.find(0) == 0)
  }

  @Test
  def findAfterUnion(): Unit = {
    val ds = DisjointSet.empty[Int]
    val ds2 = ds.union(0, 1)
    assert(ds2.find(0) == ds2.find(1))
  }

  @Test
  def unionIsTransitive(): Unit = {
    val ds = DisjointSet.empty[Int]
    val ds2 = ds.union(0, 1)
    val ds3 = ds2.union(1, 2)
    assert(ds3.find(0) == ds3.find(2))
  }

  @Test
  def unionIsReflexive(): Unit = {
    val ds = DisjointSet.empty[Int]
    val ds2 = ds.union(0, 0)
    assert(ds2.find(0) == 0)
  }

  @Test
  def unionIsSymmetric(): Unit = {
    val ds = DisjointSet.empty[Int]
    val ds2 = ds.union(0, 1)
    val ds3 = ds2.union(1, 0)
    assert(ds3.find(0) == ds3.find(1))
  }
}
