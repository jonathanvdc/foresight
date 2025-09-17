package foresight.eqsat.collections

import foresight.eqsat.Slot
import org.junit.Test

class SlotMapTest {
  @Test
  def testEmpty(): Unit = {
    val map = SlotMap.empty
    assert(map.size == 0)
    assert(!map.contains(Slot.numeric(0)))
    assert(map.get(Slot.numeric(0)).isEmpty)
    assert(map.keys.isEmpty)
    assert(map.values.isEmpty)
    assert(map.isBijection)
    assert(map.isPermutation)
    map.check()
  }

  @Test
  def testInsert(): Unit = {
    val map2 = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1))
    assert(map2.size == 1)
    assert(map2.contains(Slot.numeric(0)))
    assert(map2.get(Slot.numeric(0)).contains(Slot.numeric(1)))
    assert(map2.keySet == Set(Slot.numeric(0)))
    assert(map2.valueSet == Set(Slot.numeric(1)))
    assert(map2.isBijection)
    assert(!map2.isPermutation)
    map2.check()
  }

  @Test
  def testCompose(): Unit = {
    val map1 = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1))
    val map2 = SlotMap.from(Slot.numeric(1) -> Slot.numeric(2))
    val map3 = map1.compose(map2)
    assert(map3.size == 1)
    assert(map3.contains(Slot.numeric(0)))
    assert(map3.get(Slot.numeric(0)).contains(Slot.numeric(2)))
    assert(map3.keySet == Set(Slot.numeric(0)))
    assert(map3.valueSet == Set(Slot.numeric(2)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testComposePartial(): Unit = {
    val map1 = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1))
    val map2 = SlotMap.from(Slot.numeric(1) -> Slot.numeric(2))
    val map3 = map1.composePartial(map2)
    assert(map3.size == 1)
    assert(map3.contains(Slot.numeric(0)))
    assert(map3.get(Slot.numeric(0)).contains(Slot.numeric(2)))
    assert(map3.keySet == Set(Slot.numeric(0)))
    assert(map3.valueSet == Set(Slot.numeric(2)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testComposeFresh(): Unit = {
    val known = Set(Slot.numeric(0), Slot.numeric(1), Slot.numeric(2), Slot.numeric(3))
    val map1 = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1), Slot.numeric(1) -> Slot.numeric(2))
    val map2 = SlotMap.from(Slot.numeric(2) -> Slot.numeric(3))
    val map3 = map1.composeFresh(map2)
    assert(map3.size == 2)
    assert(map3.contains(Slot.numeric(0)))
    assert(!map3.get(Slot.numeric(0)).exists(known.contains))
    assert(map3.keySet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map3.values.size == 2)
    assert(map3.values.contains(Slot.numeric(3)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testIdentity(): Unit = {
    val map = SlotMap.identity(SlotSet(Slot.numeric(0), Slot.numeric(1)))
    assert(map.size == 2)
    assert(map.contains(Slot.numeric(0)))
    assert(map.get(Slot.numeric(0)).contains(Slot.numeric(0)))
    assert(map.contains(Slot.numeric(1)))
    assert(map.get(Slot.numeric(1)).contains(Slot.numeric(1)))
    assert(map.keySet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.valueSet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.isBijection)
    assert(map.isPermutation)
    map.check()
  }

  @Test
  def testBijectionFromSetToFresh(): Unit = {
    val map = SlotMap.bijectionFromSetToFresh(Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.size == 2)
    assert(map.keySet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.isBijection)
    assert(!map.isPermutation)
    map.check()
  }

  @Test
  def testInverse(): Unit = {
    val map = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1), Slot.numeric(1) -> Slot.numeric(2))
    val inv = map.inverse
    assert(inv.size == 2)
    assert(inv.contains(Slot.numeric(1)))
    assert(inv.get(Slot.numeric(1)).contains(Slot.numeric(0)))
    assert(inv.contains(Slot.numeric(2)))
    assert(inv.get(Slot.numeric(2)).contains(Slot.numeric(1)))
    assert(inv.keySet == Set(Slot.numeric(1), Slot.numeric(2)))
    assert(inv.valueSet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(inv.isBijection)
    assert(!inv.isPermutation)
    inv.check()
  }

  @Test
  def testInverseNoBijection(): Unit = {
    val map = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1), Slot.numeric(2) -> Slot.numeric(1))
    try {
      map.inverse
      assert(false, "Expected exception")
    } catch {
      case _: IllegalStateException => // Expected
    }
  }

  @Test
  def testConcat(): Unit = {
    val map1 = SlotMap.from(Slot.numeric(0) -> Slot.numeric(1))
    val map2 = SlotMap.from(Slot.numeric(1) -> Slot.numeric(2))
    val map3 = map1.concat(map2)
    assert(map3.size == 2)
    assert(map3.contains(Slot.numeric(0)))
    assert(map3.get(Slot.numeric(0)).contains(Slot.numeric(1)))
    assert(map3.contains(Slot.numeric(1)))
    assert(map3.get(Slot.numeric(1)).contains(Slot.numeric(2)))
    assert(map3.keySet == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map3.valueSet == Set(Slot.numeric(1), Slot.numeric(2)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }
}
