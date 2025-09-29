package foresight.eqsat.hashCons

import foresight.eqsat.EClassRef
import org.junit.Test
import foresight.eqsat.readonly

/**
 * Tests for empty e-graphs using the common readonly.EGraph API.
 * Subclasses only need to provide `empty[A]` that returns a concrete
 * implementation of `readonly.EGraph[A]`.
 */
abstract class AbstractEmptyEGraphTest {
  /** Create an empty e-graph for the given node type. */
  def empty[A]: readonly.EGraph[A]

  @Test
  def createEmpty(): Unit = {
    val g = empty[Int]
    assert(g.classes.isEmpty)
  }

  @Test
  def tryCanonicalizeUnknown(): Unit = {
    val g = empty[Int]
    assert(g.tryCanonicalize(new EClassRef()).isEmpty)
  }

  @Test
  def canonicalizeUnknownThrows(): Unit = {
    val g = empty[Int]
    val ref = new EClassRef()
    try {
      g.canonicalize(ref)
      assert(false)
    } catch {
      case _: NoSuchElementException => ()
    }
  }
}
