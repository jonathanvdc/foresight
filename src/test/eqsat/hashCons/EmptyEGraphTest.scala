package eqsat.hashCons

import eqsat.EClassRef
import org.junit.Test

/**
 * Tests the behavior of empty e-graphs.
 */
class EmptyEGraphTest {
  /**
   * Test that creating an empty e-graph works.
   */
  @Test
  def createEmpty(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    assert(egraph.classes.isEmpty)
  }

  /**
   * Test that canonicalizing an unknown e-class returns None.
   */
  @Test
  def tryCanonicalizeUnknown(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    assert(egraph.tryCanonicalize(new EClassRef()).isEmpty)
  }

  /**
   * Test that canonicalizing an unknown e-class throws an exception.
   */
  @Test
  def canonicalizeUnknownThrows(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val ref = new EClassRef()
    try {
      egraph.canonicalize(ref)
      assert(false)
    } catch {
      case _: NoSuchElementException =>
    }
  }
}
