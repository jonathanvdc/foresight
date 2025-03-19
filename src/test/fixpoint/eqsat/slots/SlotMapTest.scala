package fixpoint.eqsat.slots

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
    val map = SlotMap.empty
    val map2 = map.insert(Slot.numeric(0), Slot.numeric(1))
    assert(map2.size == 1)
    assert(map2.contains(Slot.numeric(0)))
    assert(map2.get(Slot.numeric(0)).contains(Slot.numeric(1)))
    assert(map2.keys == Set(Slot.numeric(0)))
    assert(map2.values == Set(Slot.numeric(1)))
    assert(map2.isBijection)
    assert(!map2.isPermutation)
    map2.check()
  }

  @Test
  def testCompose(): Unit = {
    val map1 = SlotMap.empty.insert(Slot.numeric(0), Slot.numeric(1))
    val map2 = SlotMap.empty.insert(Slot.numeric(1), Slot.numeric(2))
    val map3 = map1.compose(map2)
    assert(map3.size == 1)
    assert(map3.contains(Slot.numeric(0)))
    assert(map3.get(Slot.numeric(0)).contains(Slot.numeric(2)))
    assert(map3.keys == Set(Slot.numeric(0)))
    assert(map3.values == Set(Slot.numeric(2)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testComposePartial(): Unit = {
    val map1 = SlotMap.empty.insert(Slot.numeric(0), Slot.numeric(1))
    val map2 = SlotMap.empty.insert(Slot.numeric(1), Slot.numeric(2))
    val map3 = map1.composePartial(map2)
    assert(map3.size == 1)
    assert(map3.contains(Slot.numeric(0)))
    assert(map3.get(Slot.numeric(0)).contains(Slot.numeric(2)))
    assert(map3.keys == Set(Slot.numeric(0)))
    assert(map3.values == Set(Slot.numeric(2)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testComposeFresh(): Unit = {
    val known = Set(Slot.numeric(0), Slot.numeric(1), Slot.numeric(2), Slot.numeric(3))
    val map1 = SlotMap.empty.insert(Slot.numeric(0), Slot.numeric(1)).insert(Slot.numeric(1), Slot.numeric(2))
    val map2 = SlotMap.empty.insert(Slot.numeric(2), Slot.numeric(3))
    val map3 = map1.composeFresh(map2)
    assert(map3.size == 2)
    assert(map3.contains(Slot.numeric(0)))
    assert(!map3.get(Slot.numeric(0)).exists(known.contains))
    assert(map3.keys == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map3.values.size == 2)
    assert(map3.values.contains(Slot.numeric(3)))
    assert(map3.isBijection)
    assert(!map3.isPermutation)
    map3.check()
  }

  @Test
  def testIdentity(): Unit = {
    val map = SlotMap.identity(Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.size == 2)
    assert(map.contains(Slot.numeric(0)))
    assert(map.get(Slot.numeric(0)).contains(Slot.numeric(0)))
    assert(map.contains(Slot.numeric(1)))
    assert(map.get(Slot.numeric(1)).contains(Slot.numeric(1)))
    assert(map.keys == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.values == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.isBijection)
    assert(map.isPermutation)
    map.check()
  }

  @Test
  def testBijectionFromSetToFresh(): Unit = {
    val map = SlotMap.bijectionFromSetToFresh(Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.size == 2)
    assert(map.keys == Set(Slot.numeric(0), Slot.numeric(1)))
    assert(map.isBijection)
    assert(!map.isPermutation)
    map.check()
  }
}
