package foresight.eqsat

import org.junit.Test
import org.junit.Assert._

import java.util.concurrent.Executors

class SlotTest {

  // Helpers
  private def numeric(n: Int): Slot = Slot.numeric(n)

  @Test
  def numberedInterningSmallTableSameRef(): Unit = {
    val a = numeric(0)
    val b = numeric(0)
    val c = numeric(128)
    val d = numeric(128)
    assertTrue("0 must intern to the same reference", (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]))
    assertTrue("128 must intern to the same reference", (c.asInstanceOf[AnyRef] eq d.asInstanceOf[AnyRef]))
    assertTrue("0 and 128 must be distinct instances", !(a.asInstanceOf[AnyRef] eq c.asInstanceOf[AnyRef]))
  }

  @Test
  def numberedInterningGlobalMapSameRef(): Unit = {
    val x1 = numeric(-1)
    val x2 = numeric(-1)
    val y1 = numeric(129)
    val y2 = numeric(129)
    assertTrue("-1 must intern to the same reference", (x1.asInstanceOf[AnyRef] eq x2.asInstanceOf[AnyRef]))
    assertTrue("129 must intern to the same reference", (y1.asInstanceOf[AnyRef] eq y2.asInstanceOf[AnyRef]))
    assertTrue("-1 and 129 must be distinct", !(x1.asInstanceOf[AnyRef] eq y1.asInstanceOf[AnyRef]))
  }

  @Test
  def numberedBoundaryValues(): Unit = {
    val n0 = numeric(0)
    val n128 = numeric(128)
    val n129 = numeric(129)
    val nNeg = numeric(-1)
    assertTrue(n0.isNumbered)
    assertFalse(n0.isUnique)
    assertTrue(n128.isNumbered)
    assertFalse(n128.isUnique)
    assertTrue(n129.isNumbered)
    assertFalse(n129.isUnique)
    assertTrue(nNeg.isNumbered)
    assertFalse(nNeg.isUnique)

    // reference equality invariants
    assertTrue(n0.asInstanceOf[AnyRef] eq Slot.NumberedSlot(0))
    assertTrue(n128.asInstanceOf[AnyRef] eq Slot.NumberedSlot(128))
    assertTrue(n129.asInstanceOf[AnyRef] eq Slot.NumberedSlot(129))
    assertTrue(nNeg.asInstanceOf[AnyRef] eq Slot.NumberedSlot(-1))
  }

  @Test
  def uniqueFreshAreAlwaysDistinct(): Unit = {
    val u1 = Slot.fresh()
    val u2 = Slot.fresh()
    assertTrue(u1.isUnique)
    assertFalse(u1.isNumbered)
    assertTrue(u2.isUnique)
    assertFalse(u2.isNumbered)
    assertFalse("fresh must return distinct objects", u1.asInstanceOf[AnyRef] eq u2.asInstanceOf[AnyRef])
  }

  @Test
  def orderingRulesNumberedBeforeUniqueAndByValue(): Unit = {
    val s0 = numeric(0)
    val s1 = numeric(1)
    val u = Slot.fresh()

    // numbered < unique
    assertTrue("numbered must sort before unique", s0.compare(u) < 0)
    assertTrue("numbered must sort before unique", s1.compare(u) < 0)

    // numeric ascending
    assertTrue("0 < 1", s0.compare(s1) < 0)
    assertTrue("1 > 0", s1.compare(s0) > 0)
    assertEquals("0 == 0", 0, s0.compare(numeric(0)))

    // list sort sanity
    val sorted = List(u, s1, s0).sorted
    assertEquals(List(s0, s1), sorted.take(2))
  }

  @Test
  def unapplyPatternMatching(): Unit = {
    (numeric(42): Slot) match {
      case Slot.NumberedSlot(n) => assertEquals(42, n)
      case _ => fail("Expected NumberedSlot")
    }
    (Slot.fresh(): Slot) match {
      case Slot.NumberedSlot(_) => fail("UniqueSlot must not match NumberedSlot")
      case _ => succeed()
    }
  }

  @Test
  def toStringFormat(): Unit = {
    assertEquals("NumberedSlot(7)", numeric(7).toString)
  }

  @Test
  def multithreadedInterningSameRef(): Unit = {
    val pool = Executors.newFixedThreadPool(math.max(4, Runtime.getRuntime.availableProcessors()))
    try {
      val Ns = List(-1, 0, 1, 64, 127, 128, 129, 10000)
      val tasksJ = new java.util.ArrayList[java.util.concurrent.Callable[(Int, Slot)]]()
      for (n <- Ns; _ <- 1 to 64) {
        tasksJ.add(new java.util.concurrent.Callable[(Int, Slot)] {
          override def call(): (Int, Slot) = (n, numeric(n))
        })
      }

      val futuresJ = pool.invokeAll(tasksJ)
      val resultsBuf = scala.collection.mutable.ListBuffer.empty[(Int, Slot)]
      val it = futuresJ.iterator()
      while (it.hasNext) {
        resultsBuf += it.next().get()
      }
      val results: List[(Int, Slot)] = resultsBuf.toList

      // For each n, all slots are the exact same reference
      Ns.foreach { n =>
        val group = results.collect { case (m, s) if m == n => s.asInstanceOf[AnyRef] }
        val head = group.head
        group.foreach(ref => assertTrue(s"All instances for $n must be the same ref", ref eq head))
      }
    } finally {
      pool.shutdownNow()
    }
  }

  private def succeed(): Unit = assertTrue(true)
}