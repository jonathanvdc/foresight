package foresight.eqsat.hashCons

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.Slot
import org.junit.Test

import scala.collection.mutable

class PermutationGroupTest {
  @Test
  def testEmpty(): Unit = {
    val group = PermutationGroup.identity(SlotMap.empty)
    assert(group.isTrivial)
  }

  @Test
  def testOrbit1(): Unit = {
    val slot0 = Slot.numeric(0)
    val slot1 = Slot.numeric(1)
    val slot2 = Slot.numeric(2)

    val perm = SlotMap.from(slot0 -> slot1, slot1 -> slot2, slot2 -> slot0)

    val identity = SlotMap.identity(SlotSet(slot0, slot1, slot2))
    val group = PermutationGroup(identity, Set(perm))

    val orbit = group.orbit(slot0)
    assert(orbit == Set(slot0, slot1, slot2))
  }

  @Test
  def testOrbit2(): Unit = {
    val slot0 = Slot.numeric(0)
    val slot1 = Slot.numeric(1)
    val slot2 = Slot.numeric(2)

    // One permutation swaps 0 and 1, the other swaps 0 and 2
    val perm1 = SlotMap.from(slot0 -> slot1, slot1 -> slot0, slot2 -> slot2)
    val perm2 = SlotMap.from(slot0 -> slot2, slot1 -> slot1, slot2 -> slot0)

    val identity = SlotMap.identity(SlotSet(slot0, slot1, slot2))
    val group = PermutationGroup(identity, Set(perm1, perm2))

    val orbit = group.orbit(slot0)
    assert(orbit == Set(slot0, slot1, slot2))
  }

  @Test
  def testOrbit3(): Unit = {
    val slot0 = Slot.numeric(0)
    val slot1 = Slot.numeric(1)
    val slot2 = Slot.numeric(2)

    // The permutation swaps 0 and 1
    val perm = SlotMap.from(slot0 -> slot1, slot1 -> slot0, slot2 -> slot2)

    val identity = SlotMap.identity(SlotSet(slot0, slot1, slot2))
    val group = PermutationGroup(identity, Set(perm))

    val orbit = group.orbit(slot0)
    assert(orbit == Set(slot0, slot1))
  }

  @Test
  def groupTest1(): Unit = {
    val perm1 = flip(4, 0, 1)
    val perm2 = flip(4, 2, 3)
    checkGroup(Set(perm1, perm2))
  }

  @Test
  def groupTest2(): Unit = {
    val perm1 = shift(4)
    val perm2 = flip(4, 0, 1)
    checkGroup(Set(perm1, perm2))
  }

  @Test
  def groupTest3(): Unit = {
    val perm1 = shift(4)
    val perm2 = flip(4, 0, 2)
    checkGroup(Set(perm1, perm2))
  }

  private def shift(n: Int): SlotMap = {
    mkPerm(n, i => (i + 1) % n)
  }

  private def flip(n: Int, x: Int, y: Int): SlotMap = {
    mkPerm(n, i => if (i == x) y else if (i == y) x else i)
  }

  private def mkPerm(n: Int, f: Int => Int): SlotMap = {
    SlotMap.fromPairs((0 until n).map(i => Slot.numeric(i) -> Slot.numeric(f(i))))
  }

  private def checkGroup(generators: Set[SlotMap]): Unit = {
    val omega = generators.head.valueSet
    val identity = SlotMap.identity(omega)
    val group = PermutationGroup(identity, generators)
    val l = group.allPerms
    val r = enrich(generators)
    assert(l == r)
  }

  private def enrich(perms: Set[SlotMap]): Set[SlotMap] = {
    val permsMutable = mutable.Set(perms.toSeq: _*)
    assert(permsMutable.nonEmpty)

    var changed = true
    while (changed) {
      val copy = permsMutable.clone()
      for {
        x <- copy
        y <- copy
      } {
        permsMutable += x.compose(y)
      }
      changed = copy.size != permsMutable.size
    }
    permsMutable.toSet
  }
}
