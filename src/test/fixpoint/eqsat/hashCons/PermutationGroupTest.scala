package fixpoint.eqsat.hashCons

import fixpoint.eqsat.{Slot, SlotMap}
import org.junit.Test

import scala.collection.mutable

class PermutationGroupTest {
  @Test
  def testEmpty(): Unit = {
    val group = PermutationGroup.identity(SlotMap.empty)
    assert(group.isTrivial)
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
    SlotMap((0 until n).map(i => Slot.numeric(i) -> Slot.numeric(f(i))).toMap)
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
