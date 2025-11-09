package foresight.eqsat

import org.junit.Test
import org.junit.Assert._
import scala.collection.compat.immutable.ArraySeq

class ENodePoolTest {
  // Helpers to build sized sequences without needing concrete Slot/EClassCall values
  private def slots(n: Int): Seq[Slot] = {
    // Backed by a real Array[Slot] of length n; elements are null and that's fine for pooling
    ArraySeq.unsafeWrapArray(new Array[Slot](n))
  }
  private def calls(n: Int): Seq[EClassCall] = {
    ArraySeq.unsafeWrapArray(new Array[EClassCall](n))
  }

  @Test
  def createPool(): Unit = {
    val p = ENode.newPool(8)
    assertNotNull(p)
    val d = ENode.defaultPool
    assertNotNull(d)
    assertNotSame("newPool returns a different instance than the thread-local default", p, d)
  }

  @Test
  def acquireReleaseReacquireReusesNodeObjectWhenShapesMatch(): Unit = {
    val p = ENode.newPool(64)

    val n1 = p.acquire(nodeType = "add", definitions = slots(2), uses = slots(1), args = calls(3))
    val defs1 = n1.unsafeDefinitionsArray
    val uses1 = n1.unsafeUsesArray
    val args1 = n1.unsafeArgsArray

    // Release to the pool
    p.release(n1)

    // Re-acquire with the same shapes; node object and arrays should be reused
    val n2 = p.acquire(nodeType = "add", definitions = slots(2), uses = slots(1), args = calls(3))

    // The node instance itself should be reused
    assertTrue("Node object is expected to be reused", (n1 eq n2))

    // The backing arrays should be reused by identity when lengths match
    assertTrue("Definitions array should be reused by identity", (defs1 eq n2.unsafeDefinitionsArray))
    assertTrue("Uses array should be reused by identity", (uses1 eq n2.unsafeUsesArray))
    assertTrue("Args array should be reused by identity", (args1 eq n2.unsafeArgsArray))

    // Clean up
    p.release(n2)
  }

  @Test
  def acquireWithDifferentLengthsDoesNotReuseArraysAcrossBuckets(): Unit = {
    val p = ENode.newPool(64)

    val n1 = p.acquire(nodeType = "mul", definitions = slots(1), uses = slots(1), args = calls(2))
    val defs1 = n1.unsafeDefinitionsArray
    val uses1 = n1.unsafeUsesArray
    val args1 = n1.unsafeArgsArray
    p.release(n1)

    // Change shapes so that buckets don't match; identities should differ
    val n2 = p.acquire(nodeType = "mul", definitions = slots(2), uses = slots(1), args = calls(1))
    assertFalse(defs1 eq n2.unsafeDefinitionsArray)
    assertTrue(uses1 eq n2.unsafeUsesArray) // same length 1 -> should reuse
    assertFalse(args1 eq n2.unsafeArgsArray)

    p.release(n2)
  }

  @Test
  def zeroLengthArraysUseSharedEmptyButStillReuseNode(): Unit = {
    val p = ENode.newPool(64)

    val n1 = p.acquire(nodeType = "leaf", definitions = slots(0), uses = slots(0), args = calls(0))
    val defs1 = n1.unsafeDefinitionsArray
    val uses1 = n1.unsafeUsesArray
    val args1 = n1.unsafeArgsArray
    assertEquals(0, defs1.length)
    assertEquals(0, uses1.length)
    assertEquals(0, args1.length)

    p.release(n1)

    val n2 = p.acquire(nodeType = "leaf", definitions = slots(0), uses = slots(0), args = calls(0))
    // Node object should be reused even though arrays are zero-length
    assertTrue(n1 eq n2)

    // Zero-length arrays are the canonical empty arrays (identity may or may not match across acquires),
    // but lengths must be 0 and nothing should crash.
    assertEquals(0, n2.unsafeDefinitionsArray.length)
    assertEquals(0, n2.unsafeUsesArray.length)
    assertEquals(0, n2.unsafeArgsArray.length)

    p.release(n2)
  }

  @Test
  def pooledNodeBehavesLikeRegularRenameAndEquality(): Unit = {
    val p = ENode.newPool(64)

    val x = Slot.fresh()
    val y = Slot.fresh()

    val pooled = p.acquire(nodeType = 0, definitions = Seq(x), uses = Seq.empty, args = Seq.empty)
    // Rename on a pooled node should behave identically to a regular node
    val renamed = pooled.rename(collections.SlotMap.from(x -> y))
    assert(renamed == ENode(0, Seq(y), Seq.empty, Seq.empty))

    // Release only the pooled node; renamed is a fresh ENode not owned by the pool
    p.release(pooled)
  }

  @Test
  def pooledNodeAsShapeCallRoundTrip(): Unit = {
    val p = ENode.newPool(64)

    val x = Slot.fresh(); val y = Slot.fresh(); val z = Slot.fresh();
    val w = Slot.fresh(); val v = Slot.fresh()
    val c = new EClassRef(42)

    val pooled = p.acquire(
      nodeType = 0,
      definitions = Seq(x, y),
      uses = Seq(z),
      args = Seq(EClassCall(c, collections.SlotMap.from(w -> v)))
    )

    val call@ShapeCall(shape, args) = pooled.asShapeCall
    assert(shape == ENode(0,
      Seq(Slot.numeric(0), Slot.numeric(1)),
      Seq(Slot.numeric(2)),
      Seq(EClassCall(c, collections.SlotMap.from(w -> Slot.numeric(3))))
    ))
    assert(args(Slot.numeric(0)) == x)
    assert(args(Slot.numeric(1)) == y)
    assert(args(Slot.numeric(2)) == z)
    assert(args(Slot.numeric(3)) == v)
    assert(call.asNode == pooled)

    p.release(pooled)
  }

  @Test
  def interleavedAcquireReleaseReusesCorrectBuckets(): Unit = {
    val p = ENode.newPool(64)

    // Acquire A (1,0,0)
    val a = p.acquire(nodeType = "A", definitions = slots(1), uses = slots(0), args = calls(0))
    val aDefs = a.unsafeDefinitionsArray
    // Acquire B (2,1,3)
    val b = p.acquire(nodeType = "B", definitions = slots(2), uses = slots(1), args = calls(3))
    val bDefs = b.unsafeDefinitionsArray; val bUses = b.unsafeUsesArray; val bArgs = b.unsafeArgsArray

    // Release in reverse order
    p.release(b)
    p.release(a)

    // Reacquire C matching B's shapes
    val c = p.acquire(nodeType = "C", definitions = slots(2), uses = slots(1), args = calls(3))
    assertTrue(bDefs eq c.unsafeDefinitionsArray)
    assertTrue("Uses array reuses one of the previously released length-1 slot arrays", (bUses eq c.unsafeUsesArray) || (aDefs eq c.unsafeUsesArray))
    assertTrue(bArgs eq c.unsafeArgsArray)

    // Reacquire D matching A's shapes
    val d = p.acquire(nodeType = "D", definitions = slots(1), uses = slots(0), args = calls(0))
    assertTrue("Definitions array reuses one of the previously released length-1 slot arrays", (aDefs eq d.unsafeDefinitionsArray) || (bUses eq d.unsafeDefinitionsArray))

    p.release(c); p.release(d)
  }

  @Test
  def nodeTypeCanChangeAcrossReuseAndAccessorRemainsCorrect(): Unit = {
    val p = ENode.newPool(64)

    val n1 = p.acquire(nodeType = 0, definitions = slots(1), uses = slots(0), args = calls(0))
    val sameObj1 = n1
    assertEquals(0, sameObj1.nodeType)
    p.release(n1)

    // Reuse same object but with a different nodeType type (String)
    val n2 = p.acquire(nodeType = "op", definitions = slots(1), uses = slots(0), args = calls(0))
    assertTrue("Object identity reused", (sameObj1 eq n2))
    assertEquals("op", n2.nodeType)
    p.release(n2)
  }

  @Test
  def equalsAndHashRemainStableAfterReuse(): Unit = {
    val p = ENode.newPool(64)

    val x = Slot.fresh(); val y = Slot.fresh(); val z = Slot.fresh()

    val n1 = p.acquire(nodeType = 1, definitions = Seq(x, y), uses = Seq(z), args = Seq.empty)
    val expected = ENode(1, Seq(x, y), Seq(z), Seq.empty)
    assert(n1 == expected)
    val h1 = n1.hashCode
    p.release(n1)

    val n2 = p.acquire(nodeType = 1, definitions = Seq(x, y), uses = Seq(z), args = Seq.empty)
    assert(n2 == expected)
    val h2 = n2.hashCode

    // Not guaranteed to be identical across implementations, but should be consistent for equal nodes
    assertEquals(expected.hashCode, h2)

    p.release(n2)
  }

  @Test
  def poolIgnoresForeignNodesOnRelease(): Unit = {
    val p = ENode.newPool(64)

    // Create a non-pooled node
    val foreign = ENode("x", Seq.empty, Seq.empty, Seq.empty)

    // Releasing should be a no-op (and must not throw)
    p.release(foreign)

    // Pool still hands out valid nodes afterwards
    val n = p.acquire(nodeType = "y", definitions = slots(1), uses = slots(0), args = calls(0))
    assertNotNull(n)
    p.release(n)
  }
}
