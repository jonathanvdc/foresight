package foresight.eqsat.collections

import org.junit.Test
import org.junit.Assert._
import foresight.eqsat.Slot

import scala.util.Random

class SlotSetTest {
  /** Centralize how tests construct Slots + small helpers. Update only here. */
  private object TestSlots {
    def slot(i: Int): Slot = {
      Slot.numeric(i)
    }

    def indexOfSlot(s: Slot): Int = s match {
      case Slot.NumberedSlot(n) => n
      case _ => throw new IllegalArgumentException(s"Expected NumberedSlot, got $s")
    }

    def slots(is: Iterable[Int]): Iterable[Slot] = is.map(slot)

    /** Convert a SlotSet (or any Set[Slot]) to a Set[Int] by extracting a stable id. */
    def toIntSet(s: Set[Slot]): Set[Int] = {
      // Prefer a real id if available (e.g., _.id). Fallback to hashCode if equality aligns with it.
      s.iterator.map(indexOfSlot).toSet
    }

    def mkSlotSet(xs: Iterable[Int]): SlotSet =
      SlotSet.from(slots(xs))
  }

  import TestSlots._

  // ---------- Basics ----------
  @Test def emptyBasics(): Unit = {
    val e = SlotSet.empty
    assertEquals(0, e.size)
    assertTrue(e.isEmpty)
    assertFalse(e.iterator.hasNext)
    assertFalse(e.contains(slot(0)))
  }

  @Test def applyRemovesDuplicatesAndSorts(): Unit = {
    val s = mkSlotSet(List(3, 1, 2, 2, 1))
    assertEquals(3, s.size)
    val it = s.iterator.toList
    assertEquals(it.distinct, it)
    assertEquals(it.sorted, it)
  }

  @Test def inclInsertsNewAndKeepsOrder(): Unit = {
    val s0 = mkSlotSet(List(1, 3, 5))
    val s1 = s0.incl(slot(4))
    assertEquals(4, s1.size)
    assertEquals(List(1,3,4,5).map(slot), s1.iterator.toList)
  }

  @Test def inclExistingReturnsSameInstance(): Unit = {
    val s0 = mkSlotSet(List(1, 3, 5))
    val s1 = s0.incl(slot(3))
    assertTrue(s1.asInstanceOf[AnyRef] eq s0.asInstanceOf[AnyRef])
  }

  @Test def exclExistingShrinks(): Unit = {
    val s0 = mkSlotSet(List(1, 2, 3))
    val s1 = s0.excl(slot(2)).asInstanceOf[SlotSet]
    assertEquals(2, s1.size)
    assertFalse(s1.contains(slot(2)))
    assertEquals(List(1,3).map(slot), s1.iterator.toList)
  }

  @Test def exclMissingReturnsSameInstance(): Unit = {
    val s0 = mkSlotSet(List(1, 2, 3))
    val s1 = s0.excl(slot(99))
    assertTrue(s1.asInstanceOf[AnyRef] eq s0.asInstanceOf[AnyRef])
  }

  @Test def containsAgreesWithConstruction(): Unit = {
    val s = mkSlotSet(List(2, 4, 6))
    assertTrue(s.contains(slot(2)))
    assertFalse(s.contains(slot(5)))
  }

  @Test def iteratorSortedNoDuplicates(): Unit = {
    val s = mkSlotSet(List(4, 1, 1, 3, 2))
    val it = s.iterator.toList
    assertEquals(it.distinct, it)
    assertEquals(it.sorted, it)
  }

  // ---------- Equality / hash ----------
  @Test def equalsHashSameContent(): Unit = {
    val s1 = mkSlotSet(List(1, 2, 3))
    val s2 = mkSlotSet(List(1, 2, 3))
    assertEquals(s1, s2)
    assertEquals(s1.hashCode(), s2.hashCode())
  }

  @Test def equalsDifferentContent(): Unit = {
    val a = mkSlotSet(List(1,2,3))
    val b = mkSlotSet(List(1,2,4))
    assertNotEquals(a, b)
  }

  // ---------- Derived Set[Slot] operators ----------
  @Test def unionMatchesScalaSet(): Unit = {
    val a = Set(1,2,3,5,8)
    val b = Set(2,3,4,9)
    val sa = mkSlotSet(a)
    val sb = mkSlotSet(b)
    val su = (sa | sb)
    assertEquals(a union b, toIntSet(su))
  }

  @Test def intersectMatchesScalaSet(): Unit = {
    val a = Set(1,2,3,5,8)
    val b = Set(2,3,4,9)
    val sa = mkSlotSet(a)
    val sb = mkSlotSet(b)
    val si = (sa & sb)
    assertEquals(a intersect b, toIntSet(si))
  }

  @Test def diffMatchesScalaSet(): Unit = {
    val a = Set(1,2,3,5,8)
    val b = Set(2,3,4,9)
    val sa = mkSlotSet(a)
    val sb = mkSlotSet(b)
    val sd = (sa &~ sb)
    assertEquals(a diff b, toIntSet(sd))
  }

  @Test def subsetOfMatchesScalaSet(): Unit = {
    val a = Set(1,3,5)
    val b = Set(1,2,3,4,5)
    assertTrue(mkSlotSet(a).subsetOf(mkSlotSet(b)))
    assertEquals(a.subsetOf(b), mkSlotSet(a).subsetOf(mkSlotSet(b)))
    assertEquals(b.subsetOf(a), mkSlotSet(b).subsetOf(mkSlotSet(a)))
  }

  @Test def plusPlusMatchesConcat(): Unit = {
    val a = Set(1,3,5)
    val b = Set(1,2,3,4,5)
    val r = mkSlotSet(a) ++ slots(b)
    assertEquals(a ++ b, toIntSet(r))
  }

  @Test def minusMinusMatchesDiffIterable(): Unit = {
    val a = Set(1,2,3,4,5,6)
    val b = Set(2,4,6,8)
    val r = (mkSlotSet(a) -- slots(b)).asInstanceOf[SlotSet]
    assertEquals(a -- b, toIntSet(r))
  }

  @Test def mapIdentityFilterEvens(): Unit = {
    val a = Set(0,1,2,3,4,5,6,7,8)
    val s = mkSlotSet(a)

    // map(identity) keeps type and content for Set[Slot]
    val sm = s.map(identity)
    assertEquals(a, toIntSet(sm))

    // filter evens
    val sf = s.filter(sl => (indexOfSlot(sl) & 1) == 0)
    assertEquals(a.filter(_ % 2 == 0), toIntSet(sf))
  }

  @Test def forallExistsContains(): Unit = {
    val a = Set(2,4,6,8)
    val s = mkSlotSet(a)
    assertTrue(s.forall(indexOfSlot(_) % 2 == 0))
    assertTrue(s.exists(indexOfSlot(_) == 4))
    assertFalse(s.contains(slot(3)))
  }

  // ---------- Edge cases ----------
  @Test def inclBeginningMiddleEnd(): Unit = {
    val base = mkSlotSet(List(10, 20, 30))
    assertEquals(List(5,10,20,30).map(slot),  base.incl(slot(5)).iterator.toList)
    assertEquals(List(10,20,25,30).map(slot), base.incl(slot(25)).iterator.toList)
    assertEquals(List(10,20,30,40).map(slot), base.incl(slot(40)).iterator.toList)
  }

  @Test def exclSingleToEmpty(): Unit = {
    val s = mkSlotSet(List(42))
    val e = s.excl(slot(42)).asInstanceOf[SlotSet]
    assertEquals(SlotSet.empty, e)
    assertTrue(e.isEmpty)
  }

  @Test def randomizedParityAgainstScalaSet(): Unit = {
    val rnd = new Random(0xF00DCAFE)
    val xs = Vector.fill(200)(rnd.nextInt(1000) - 500)
    val a = xs.toSet
    val b = xs.map(_ + 1).toSet
    val sa = mkSlotSet(a)
    val sb = mkSlotSet(b)

    assertEquals(a, toIntSet(sa))
    assertEquals(b, toIntSet(sb))
    assertEquals(a | b, toIntSet(sa | sb))
    assertEquals(a & b, toIntSet(sa & sb))
    assertEquals(a &~ b, toIntSet(sa &~ sb))
    assertEquals(a.subsetOf(b), sa.subsetOf(sb))
  }

  // ---------- Construction contracts ----------
  @Test def applyEmptyIsEmptySingleton(): Unit = {
    val e = SlotSet.from(Iterable.empty)
    assertEquals(SlotSet.empty, e)
    assertTrue(e.isEmpty)
  }

  @Test def fromArrayUnsafeRespectsGivenArray(): Unit = {
    val arr = Array(slot(1), slot(3), slot(7))
    val s = SlotSet.fromArrayUnsafe(arr)
    assertEquals(List(1,3,7).map(slot), s.iterator.toList)
    assertEquals(3, s.size)
  }
}