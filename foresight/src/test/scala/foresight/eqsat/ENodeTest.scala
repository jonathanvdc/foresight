package foresight.eqsat

import foresight.eqsat.collections.SlotMap

import org.junit.Test

class ENodeTest {
  @Test
  def renameDefinition(): Unit = {
    val x = Slot.fresh()
    val y = Slot.fresh()

    val node = ENode(0, Seq(x), Seq.empty, Seq.empty)

    val node2 = node.rename(SlotMap.from(x -> y))
    assert(node2 == ENode(0, Seq(y), Seq.empty, Seq.empty))
  }

  @Test
  def renameUse(): Unit = {
    val x = Slot.fresh()
    val y = Slot.fresh()

    val node = ENode(0, Seq.empty, Seq(x), Seq.empty)

    val node2 = node.rename(SlotMap.from(x -> y))
    assert(node2 == ENode(0, Seq.empty, Seq(y), Seq.empty))
  }

  @Test
  def renameBoth(): Unit = {
    val x = Slot.fresh()
    val y = Slot.fresh()

    val node = ENode(0, Seq(x), Seq(x), Seq.empty)

    val node2 = node.rename(SlotMap.from(x -> y))
    assert(node2 == ENode(0, Seq(y), Seq(y), Seq.empty))
  }

  @Test
  def decomposeIntoShape(): Unit = {
    val x = Slot.fresh()
    val y = Slot.fresh()
    val z = Slot.fresh()

    val node = ENode(0, Seq(x, y), Seq(z), Seq.empty)

    val call@ShapeCall(shape, args) = node.asShapeCall
    assert(shape == ENode(0, Seq(Slot.numeric(0), Slot.numeric(1)), Seq(Slot.numeric(2)), Seq.empty))
    assert(args(Slot.numeric(0)) == x)
    assert(args(Slot.numeric(1)) == y)
    assert(args(Slot.numeric(2)) == z)
    assert(call.asNode == node)
  }

  @Test
  def decomposeIntoShape2(): Unit = {
    val x = Slot.fresh()
    val y = Slot.fresh()
    val z = Slot.fresh()
    val w = Slot.fresh()
    val v = Slot.fresh()

    val c = new EClassRef()

    val node = ENode(0, Seq(x, y), Seq(z), Seq(EClassCall(c, SlotMap.from(w -> v))))

    val call@ShapeCall(shape, args) = node.asShapeCall
    assert(shape == ENode(0, Seq(Slot.numeric(0), Slot.numeric(1)), Seq(Slot.numeric(2)), Seq(EClassCall(c, SlotMap.from(w -> Slot.numeric(3))))))
    assert(args(Slot.numeric(0)) == x)
    assert(args(Slot.numeric(1)) == y)
    assert(args(Slot.numeric(2)) == z)
    assert(args(Slot.numeric(3)) == v)
    assert(call.asNode == node)
  }
}
