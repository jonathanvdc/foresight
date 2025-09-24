package foresight.eqsat.collections

import org.junit.Assert._
import org.junit.Test
import foresight.eqsat.Slot

/**
 * Tests for SlotSeq (an AbstractArraySeq[Slot]).
 *
 * These tests focus on correctness and copy-on-write behaviour of map,
 * while exercising basic Seq semantics across Scala 2.11–3.x.
 */
class SlotSeqTest {

  // --- Helpers ----------------------------------------------------------------

  private def mkSeq(arr: Array[Slot], len: Int): SlotSeq = SlotSeq.unsafeWrapArray(arr, len)
  private def mkSeq(arr: Array[Slot]): SlotSeq = SlotSeq.unsafeWrapArray(arr)

  private def freshSlots(n: Int): Array[Slot] = {
    val a = new Array[Slot](n)
    var i = 0
    while (i < n) { a(i) = Slot.fresh(); i += 1 }
    a
  }

  // --- Basic construction & Seq behaviour ------------------------------------

  @Test
  def emptySeqBasics(): Unit = {
    val s = mkSeq(new Array[Slot](0), 0)
    assertEquals(0, s.length)
    assertTrue(s.isEmpty)
    assertEquals(0, s.iterator.size)
  }

  @Test
  def applyAndIteration(): Unit = {
    val base = freshSlots(5)
    val s = mkSeq(base, 5)
    // length
    assertEquals(5, s.length)
    // apply
    var i = 0
    while (i < s.length) { assertTrue(s(i) eq base(i)); i += 1 }
    // iterator preserves order
    assertArrayEquals(base.asInstanceOf[Array[AnyRef]], s.iterator.toArray.asInstanceOf[Array[AnyRef]])
    // foreach visits all
    var seen = List.empty[Slot]
    s.foreach(x => seen ::= x)
    assertEquals(base.toList, seen.reverse)
  }

  // --- Constructors / factories / extractors ---------------------------------

  @Test
  def emptyIsSingletonAndEmpty(): Unit = {
    val e = SlotSeq.empty
    assertEquals(0, e.length)
    assertTrue(e.isEmpty)
    // empty should reuse the same instance on multiple accesses
    assertTrue((SlotSeq.empty.asInstanceOf[AnyRef] eq e.asInstanceOf[AnyRef]))
  }

  @Test
  def unsafeWrapArrayRespectsProvidedLength(): Unit = {
    val backing = freshSlots(5)
    val s = SlotSeq.unsafeWrapArray(backing, 3)
    assertEquals(3, s.length)
    // ensure logical prefix is visible and trailing items are ignored
    assertTrue(s(0) eq backing(0))
    assertTrue(s(1) eq backing(1))
    assertTrue(s(2) eq backing(2))
  }

  @Test
  def unsafeWrapArrayUsesWholeArrayLength(): Unit = {
    val backing = freshSlots(4)
    val s = SlotSeq.unsafeWrapArray(backing)
    assertEquals(4, s.length)
    var i = 0
    while (i < s.length) { assertTrue(s(i) eq backing(i)); i += 1 }
  }

  @Test
  def applyCopiesInputAndDoesNotAliasCallerArray(): Unit = {
    val src = freshSlots(3)
    val s = SlotSeq(src: _*)
    // Modifying the source array must not affect s, so arrays must not be aliased
    assertFalse("apply should copy elements into a new array",
      (s.unsafeArray.asInstanceOf[AnyRef] eq src.asInstanceOf[AnyRef]))
    // contents (by identity) are preserved
    assertTrue(s(0) eq src(0))
    assertTrue(s(1) eq src(1))
    assertTrue(s(2) eq src(2))
  }

  @Test
  def fromCopiesForNonSlotSeqAndAvoidsCopyForSlotSeq(): Unit = {
    val baseArr = freshSlots(3)
    val nonSS: Iterable[Slot] = baseArr.toIndexedSeq
    val s1 = SlotSeq.from(nonSS)
    assertEquals(3, s1.length)
    // must allocate a new backing array (no aliasing to baseArr)
    assertFalse(s1.unsafeArray.asInstanceOf[AnyRef] eq baseArr.asInstanceOf[AnyRef])

    // If input is already a SlotSeq, from must avoid copying and return it as-is
    val ss = SlotSeq.unsafeWrapArray(baseArr)
    val s2 = SlotSeq.from(ss)
    assertTrue("from(SlotSeq) should avoid copying and return the same instance",
      (s2.asInstanceOf[AnyRef] eq ss.asInstanceOf[AnyRef]))
  }

  @Test
  def unsafeArrayExposesBackingArrayAndIsTrulyUnsafe(): Unit = {
    val arr = freshSlots(2)
    val s = SlotSeq.unsafeWrapArray(arr)
    val exposed = s.unsafeArray
    val replacement = Slot.fresh()
    exposed(1) = replacement // mutate through the exposed reference
    // Observing through the Seq should now reflect the mutation
    assertTrue(s(1) eq replacement)
  }

  // --- map: copy-on-write semantics ------------------------------------------

  @Test
  def mapIdentityReturnsSameInstance(): Unit = {
    val base = freshSlots(4)
    val s = mkSeq(base, 4)
    val t = s.map((x: Slot) => x) // identity by reference
    assertTrue("map(identity) must return the same instance", (t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef]))
    // also ensure elements are unchanged by identity
    var i = 0
    while (i < t.length) { assertTrue(t(i) eq base(i)); i += 1 }
  }

  @Test
  def mapFirstDivergenceInMiddle(): Unit = {
    val base = freshSlots(6)
    val s = mkSeq(base, 6)
    val replaceAt = 3
    val replacement = Slot.fresh()

    // mapping function: replace exactly base(replaceAt) by a fresh Slot; else keep identity
    val f: Slot => Slot = (x: Slot) => if (x eq base(replaceAt)) replacement else x

    val t = s.map(f)

    // Different instance (copy-on-write triggered)
    assertFalse(t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef])

    // Prefix up to replaceAt-1 must be the same objects by identity (copied from original array)
    var i = 0
    while (i < replaceAt) { assertTrue("prefix must share identity", t(i) eq base(i)); i += 1 }

    // At replaceAt we must see the replacement (different identity)
    assertTrue(t(replaceAt) eq replacement)
    assertFalse(t(replaceAt) eq base(replaceAt))

    // After replaceAt, function returns identity, so new array should contain the same object refs
    i = replaceAt + 1
    while (i < t.length) { assertTrue("suffix should be identity-equal", t(i) eq base(i)); i += 1 }
  }

  @Test
  def mapDivergenceAtZero(): Unit = {
    val base = freshSlots(3)
    val s = mkSeq(base, 3)
    val replacement0 = Slot.fresh()

    val t = s.map((x: Slot) => if (x eq base(0)) replacement0 else x)

    // must be a new instance
    assertFalse(t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef])
    // position 0 replaced
    assertTrue(t(0) eq replacement0)
    // remaining positions unchanged by identity
    assertTrue(t(1) eq base(1))
    assertTrue(t(2) eq base(2))
  }

  @Test
  def mapAllElementsChange(): Unit = {
    val base = freshSlots(4)
    val s = mkSeq(base, 4)
    val repl = Array.fill[Slot](4)(Slot.fresh())

    val t = s.map((x: Slot) => repl(base.indexOf(x)))

    // must be new instance
    assertFalse(t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef])
    // every element should be the corresponding replacement (not the original)
    var i = 0
    while (i < 4) {
      assertTrue(t(i) eq repl(i))
      assertFalse(t(i) eq base(i))
      i += 1
    }
  }

  @Test
  def mapOnEmptyIsIdentityInstance(): Unit = {
    val s = mkSeq(new Array[Slot](0), 0)
    val t = s.map((x: Slot) => x)
    assertTrue(t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef])
    assertEquals(0, t.length)
  }

  // --- Safety: length vs backing array size ----------------------------------

  @Test
  def oversizedBackingArrayHonorsLogicalLength(): Unit = {
    val backing = freshSlots(5)
    val s = mkSeq(backing, 3) // only first 3 are logically valid

    assertEquals(3, s.length)
    assertArrayEquals(Array(backing(0), backing(1), backing(2)).asInstanceOf[Array[AnyRef]], s.iterator.toArray.asInstanceOf[Array[AnyRef]])

    // mapping that keeps identity should preserve instance
    val t = s.map((x: Slot) => x)
    assertTrue(t.asInstanceOf[AnyRef] eq s.asInstanceOf[AnyRef])
  }

  // --- Equality & hashing -----------------------------------------------------

  @Test
  def equalitySameInstance(): Unit = {
    val arr = freshSlots(3)
    val s = mkSeq(arr)
    assertTrue(s == s)
    assertEquals(s.hashCode, s.hashCode)
  }

  @Test
  def equalitySameContentSameRefs(): Unit = {
    val arr = freshSlots(4)
    val s1 = mkSeq(arr)
    val s2 = mkSeq(arr.clone()) // clone preserves element refs

    // Same elements in same order => equal and same hash
    assertTrue(s1 == s2)
    assertEquals(s1.hashCode, s2.hashCode)

    // Cross-collection equality with standard Scala collections
    val asList = arr.toList
    assertTrue(s1 == asList)
    assertTrue(asList == s1)
    assertEquals(asList.hashCode, s1.hashCode)
  }

  @Test
  def equalityDifferentElementNotEqual(): Unit = {
    val arr = freshSlots(4)
    val s1 = mkSeq(arr)
    val arr2 = arr.clone()
    arr2(2) = Slot.fresh() // differ at one position
    val s2 = mkSeq(arr2)

    assertFalse(s1 == s2)
  }

  @Test
  def equalityDifferentLengthsNotEqual(): Unit = {
    val arr = freshSlots(4)
    val sShort = mkSeq(arr, 3)
    val sLong  = mkSeq(arr, 4)
    assertFalse(sShort == sLong)
    assertFalse(sLong == sShort)
  }

  @Test
  def equalityEmptyCollections(): Unit = {
    val e1 = SlotSeq.empty
    val e2 = mkSeq(new Array[Slot](0))
    assertTrue(e1 == e2)
    assertEquals(e1.hashCode, e2.hashCode)
    assertTrue(e1 == List.empty[Slot])
    assertEquals(e1.hashCode, List.empty[Slot].hashCode)
  }

  @Test
  def hashCodeConsistentWithEqualsAfterMapIdentity(): Unit = {
    val arr = freshSlots(5)
    val s1 = mkSeq(arr)
    val s2 = s1.map((x: Slot) => x) // same instance by our COW map(identity)
    assertTrue(s1 == s2)
    assertEquals(s1.hashCode, s2.hashCode)
  }
}
