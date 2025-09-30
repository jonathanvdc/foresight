package foresight.eqsat.hashCons

import foresight.eqsat.{ENode, Slot, Tree}
import foresight.eqsat.mutable.{EGraph => MutableEGraph}
import org.junit.Test

/**
 * Union/rebuild scenarios expressed against the mutable.EGraph surface.
 * Subclasses provide an empty graph and an invariant checker.
 */
abstract class AbstractUnionEGraphTest {
  type EGraphT[A] <: MutableEGraph[A]
  def empty[A]: EGraphT[A]
  def checkInvariants[A](g: EGraphT[A]): Unit

  /** Creates an e-graph containing two leaf nodes, then unions them. */
  @Test
  def unionTwoLeaves(): Unit = {
    val g = empty[Int]
    val node1 = ENode.unslotted(1, Seq.empty)
    val node2 = ENode.unslotted(2, Seq.empty)
    val c1 = g.add(node1)
    val c2 = g.add(node2)

    assert(g.classes.size == 2)

    g.unionMany(Seq((c1, c2)))

    assert(g.classes.size == 1)
    assert(g.nodes(c1) == g.nodes(c2))
    assert(g.nodes(c1) == Set(node1, node2))

    checkInvariants(g)
  }

  /** Creates an e-graph containing two nodes with a common argument, then unions them. */
  @Test
  def unionTwoNodesWithCommonArgument(): Unit = {
    val g = empty[Int]
    val arg = ENode.unslotted(0, Seq.empty)
    val c1 = g.add(arg)

    val node1 = ENode.unslotted(1, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c1))
    val c2 = g.add(node1)
    val c3 = g.add(node2)

    assert(g.classes.size == 3)

    g.unionMany(Seq((c2, c3)))

    assert(g.classes.size == 2)
    assert(g.nodes(c2) == g.nodes(c3))
    assert(g.nodes(c2) == Set(node1, node2))

    checkInvariants(g)
  }

  /** Creates an e-graph containing three nodes, then unions them. */
  @Test
  def unionThreeNodes(): Unit = {
    val g = empty[Int]
    val node1 = ENode.unslotted(1, Seq.empty)
    val node2 = ENode.unslotted(2, Seq.empty)
    val node3 = ENode.unslotted(3, Seq.empty)
    val c1 = g.add(node1)
    val c2 = g.add(node2)
    val c3 = g.add(node3)

    assert(g.classes.size == 3)

    // Equivalent to requiresRebuild on a pending-unions wrapper: some pairs are not yet same.
    assert(!g.areSame(c1, c2) || !g.areSame(c1, c3))

    g.unionMany(Seq((c1, c2), (c1, c3)))

    assert(g.classes.size == 1)
    assert(g.nodes(c1) == g.nodes(c2))
    assert(g.nodes(c1) == g.nodes(c3))
    assert(g.nodes(c1) == Set(node1, node2, node3))

    checkInvariants(g)
  }

  /** Creates an e-graph containing a node with two arguments, then unions the arguments. */
  @Test
  def unionArgumentNodes(): Unit = {
    val g = empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val c1 = g.add(arg1)
    val c2 = g.add(arg2)

    val node = ENode.unslotted(2, Seq(c1, c2))
    val c3 = g.add(node)

    assert(g.classes.size == 3)

    g.unionMany(Seq((c1, c2)))
    val argClass = g.canonicalize(c1)

    assert(g.classes.size == 2)
    assert(g.nodes(c1) == g.nodes(c2))
    assert(g.nodes(c1) == Set(arg1, arg2))
    assert(g.nodes(c3) == Set(ENode.unslotted(2, Seq(argClass, argClass))))

    checkInvariants(g)
  }

  /** Upward merge caused by bottom union. */
  @Test
  def upwardMerge(): Unit = {
    val g = empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val c1 = g.add(arg1)
    val c2 = g.add(arg2)

    val node1 = ENode.unslotted(2, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c2))
    val c3 = g.add(node1)
    val c4 = g.add(node2)

    assert(g.classes.size == 4)

    g.unionMany(Seq((c1, c2)))
    val argClass = g.canonicalize(c1)

    assert(g.classes.size == 2)
    assert(g.canonicalize(c1) == g.canonicalize(c2))
    assert(g.canonicalize(c3) == g.canonicalize(c4))
    assert(g.nodes(c1) == g.nodes(c2))
    assert(g.nodes(c1) == Set(arg1, arg2))
    assert(g.nodes(c3) == Set(ENode.unslotted(2, Seq(argClass))))

    checkInvariants(g)
  }

  /** Union leaves but not their parents. */
  @Test
  def noUpwardMerge(): Unit = {
    val g = empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val c1 = g.add(arg1)
    val c2 = g.add(arg2)

    val node1 = ENode.unslotted(2, Seq(c1))
    val node2 = ENode.unslotted(3, Seq(c2))
    val c3 = g.add(node1)
    val c4 = g.add(node2)

    assert(g.classes.size == 4)

    g.unionMany(Seq((c1, c2)))

    assert(g.classes.size == 3)
    assert(g.canonicalize(c1) == g.canonicalize(c2))
    assert(g.nodes(c1) == g.nodes(c2))
    assert(g.nodes(c1) == Set(arg1, arg2))
    assert(g.nodes(c3) == Set(ENode.unslotted(2, Seq(g.canonicalize(c1)))))
    assert(g.nodes(c4) == Set(ENode.unslotted(3, Seq(g.canonicalize(c1)))))

    checkInvariants(g)
  }

  /** Self-cycle via union. */
  @Test
  def selfCycle(): Unit = {
    val g = empty[Int]
    val node = ENode.unslotted(0, Seq.empty)
    val c1 = g.add(node)

    val selfCycle = ENode.unslotted(1, Seq(c1))
    val c2 = g.add(selfCycle)

    assert(g.classes.size == 2)

    g.unionMany(Seq((c1, c2)))
    val argClass = g.canonicalize(c1)

    assert(g.classes.size == 1)
    assert(g.nodes(c1) == Set(node, ENode.unslotted(1, Seq(argClass))))

    checkInvariants(g)
  }

  @Test
  def xUnionYEliminatesSlot(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val a = g.add(ENode(0, Seq.empty, Seq(x), Seq.empty))
    val b = g.add(ENode(0, Seq.empty, Seq(y), Seq.empty))

    assert(g.classes.size == 1)

    assert(a.args.size == 1)
    assert(b.args.size == 1)

    g.unionMany(Seq((a, b)))

    val canonical = g.canonicalize(a)
    assert(g.classes.size == 1)
    assert(canonical.args.size == 0)
  }

  @Test
  def xyUnionYzEliminatesSlots(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()
    val z = Slot.fresh()

    val a = g.add(ENode(0, Seq.empty, Seq(x, y), Seq.empty))
    val b = g.add(ENode(0, Seq.empty, Seq(y, z), Seq.empty))

    assert(g.classes.size == 1)

    assert(a.args.size == 2)
    assert(b.args.size == 2)

    g.unionMany(Seq((a, b)))
    assert(g.classes.size == 1)

    val canonical = g.canonicalize(a)
    assert(canonical.args.size == 0)
  }

  @Test
  def xyUnionYxProducesPermutation(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val a = g.add(node1)
    val b = g.add(node2)

    val node3 = ENode(1, Seq(x, y), Seq.empty, Seq(a))
    val node4 = ENode(1, Seq(x, y), Seq.empty, Seq(b))

    val c = g.add(node3)
    val d = g.add(node4)

    assert(g.classes.size == 3)
    assert(c != d)

    assert(a.args.size == 2)
    assert(b.args.size == 2)
    assert(c.args.size == 0)
    assert(d.args.size == 0)

    assert(!g.areSame(a, b))

    g.unionMany(Seq((a, b)))
    assert(g.classes.size == 2)

    val aCan = g.canonicalize(a)
    val bCan = g.canonicalize(b)
    val cCan = g.canonicalize(c)
    val dCan = g.canonicalize(d)

    assert(aCan.args.size == 2)
    assert(bCan.args.size == 2)
    assert(cCan.args.size == 0)
    assert(dCan.args.size == 0)

    assert(cCan == dCan)
    assert(g.areSame(a, b))
  }

  @Test
  def xMinusXUnionZeroEliminatesSlot(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Minus extends NodeType

    val g = empty[NodeType]

    val x = Slot.fresh()

    val xMinusX = Tree(Minus, Seq.empty, Seq.empty, Seq(
      Tree(Var, Seq.empty, Seq(x), Seq.empty),
      Tree(Var, Seq.empty, Seq(x), Seq.empty)))

    val c = g.add(xMinusX)

    assert(g.classes.size == 2)
    assert(c.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val d = g.add(zero)

    assert(g.classes.size == 3)
    assert(d.args.size == 0)

    g.unionMany(Seq((c, d)))

    assert(g.classes.size == 2)
    assert(g.canonicalize(c).args.size == 0)
    assert(g.canonicalize(c) == g.canonicalize(d))
  }

  @Test
  def zeroTimesXUnionZeroEliminatesSlot(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Mul extends NodeType

    val g = empty[NodeType]

    val x = Slot.fresh()

    val zeroTimesX = Tree.unslotted(
      Mul,
      Seq(
        Tree.unslotted(Const(0), Seq.empty),
        Tree(Var, Seq.empty, Seq(x), Seq.empty)))

    val c = g.add(zeroTimesX)

    assert(g.classes.size == 3)
    assert(c.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val d = g.add(zero)

    assert(g.classes.size == 3)
    assert(d.args.size == 0)

    g.unionMany(Seq((c, d)))

    assert(g.classes.size == 2)
    assert(g.canonicalize(c).args.size == 0)
    assert(g.canonicalize(c) == g.canonicalize(d))
  }

  @Test
  def zeroTimesXUnionZeroEliminatesSlot2(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Mul extends NodeType

    val g = empty[NodeType]

    val x = Slot.fresh()

    val zeroTimesX = Tree.unslotted(
      Mul,
      Seq(
        Tree.unslotted(Const(0), Seq.empty),
        Tree(Var, Seq.empty, Seq(x), Seq.empty)))
    val zeroTimesXTimesOne = Tree.unslotted(
      Mul,
      Seq(
        zeroTimesX,
        Tree.unslotted(Const(1), Seq.empty)))

    val c = g.add(zeroTimesX)
    val e = g.add(zeroTimesXTimesOne)

    assert(g.classes.size == 5)
    assert(c.args.size == 1)
    assert(e.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val d = g.add(zero)

    assert(g.classes.size == 5)
    assert(d.args.size == 0)

    g.unionMany(Seq((c, d)))

    assert(g.classes.size == 4)
    assert(g.canonicalize(c).args.size == 0)
    assert(g.canonicalize(c) == g.canonicalize(d))
    assert(g.canonicalize(e).args.size == 0)
  }

  @Test
  def canonicalizeAfterShrinking(): Unit = {
    val g = empty[Int]
    val x = Slot.fresh()
    val a = g.add(ENode(0, Seq.empty, Seq(x), Seq.empty))
    val b = g.add(ENode(1, Seq.empty, Seq(x), Seq.empty))
    val c = g.add(ENode(2, Seq.empty, Seq(x), Seq.empty))

    g.unionMany(Seq((a, b), (a, c)))
    val canonical = g.canonicalize(a)
    assert(canonical.args.size == 1)

    checkInvariants(g)

    val d = g.add(ENode(2, Seq.empty, Seq.empty, Seq.empty))
    g.unionMany(Seq((a, d)))
    val canonical2 = g.canonicalize(a)
    assert(canonical2.args.size == 0)
    val canonical3 = g.canonicalize(b)
    assert(canonical3.args.size == 0)
    val canonical4 = g.canonicalize(c)
    assert(canonical4.args.size == 0)

    checkInvariants(g)
  }

  @Test
  def upwardMergeDueToSlotElimination(): Unit = {
    // Similar to upwardMerge, but upward merge is due to slot elimination.
    val g = empty[Int]
    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create the same node twice and instantiate it with different slots.
    val node1 = ENode(0, Seq.empty, Seq(x), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y), Seq.empty)
    val c1 = g.add(node1)
    val c2 = g.add(node2)

    // Then create a pair of nodes that take the first two nodes as arguments. The nodes are different only because
    // c1 and c2 are currently different.
    val node3 = ENode.unslotted(1, Seq(c1, c2))
    val node4 = ENode.unslotted(1, Seq(c1, c1))
    val c3 = g.add(node3)
    val c4 = g.add(node4)

    val node5 = ENode.unslotted(2, Seq.empty)
    val c5 = g.add(node5)

    assert(g.classes.size == 4)
    assert(!g.areSame(c1, c2))
    assert(!g.areSame(c3, c4))

    // We now use a union with c5 to delete the slot in c1/c2 that distinguishes c3 and c4.
    g.unionMany(Seq((c1, c5)))

    // Since the slot has been deleted from c1/c2, calls to them with different arguments will now be the same.
    // Consequently, c3 and c4 now contain identical nodes. Check that an upward merge has unified c3 and c4.
    assert(g.classes.size == 2)
    assert(g.areSame(c1, c2))
    assert(g.areSame(c3, c4))
  }

  @Test
  def eliminateRedundantSlotDueToOrbit(): Unit = {
    val g = empty[Int]
    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots.
    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val c1 = g.add(node1)
    val c2 = g.add(node2)

    assert(g.classes.size == 1)
    assert(!g.areSame(c1, c2))

    g.unionMany(Seq((c1, c2)))

    assert(g.classes.size == 1)
    assert(g.areSame(c1, c2))

    // Now create another node that takes only slot x as an argument.
    val node3 = ENode(1, Seq.empty, Seq(x), Seq.empty)
    val c3 = g.add(node3)

    assert(g.classes.size == 2)
    assert(!g.areSame(c1, c3))
    assert(!g.areSame(c2, c3))

    // Now we union c1/c2 with c3. This will eliminate the slots x and y from the class.
    g.unionMany(Seq((c1, c3)))

    assert(g.classes.size == 1)
    assert(g.areSame(c1, c2))
    assert(g.areSame(c1, c3))
    assert(g.areSame(c2, c3))

    // Check that the slots have been eliminated from the class.
    val canonical = g.canonicalize(c1)
    assert(canonical.args.size == 0)
  }

  @Test
  def propagateSymmetriesOnAdd(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots.
    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val c1 = g.add(node1)
    val c2 = g.add(node2)

    assert(g.classes.size == 1)
    assert(!g.areSame(c1, c2))

    g.unionMany(Seq((c1, c2)))

    assert(g.classes.size == 1)
    assert(g.areSame(c1, c2))

    // Now create another node that takes the node as an argument, as well as its swapped version.
    val node3 = ENode(1, Seq.empty, Seq.empty, Seq(c1))
    val node4 = ENode(1, Seq.empty, Seq.empty, Seq(c2))

    val c3 = g.add(node3)
    val c4 = g.add(node4)

    // Check that the permutation has been propagated to the new node.
    assert(g.classes.size == 2)
    assert(g.areSame(c3, c4))
  }

  @Test
  def propagateSymmetriesOnUnion(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots.
    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val c1 = g.add(node1)
    val c2 = g.add(node2)

    assert(g.classes.size == 1)
    assert(!g.areSame(c1, c2))

    val node3 = ENode(1, Seq.empty, Seq.empty, Seq(c1))
    val node4 = ENode(1, Seq.empty, Seq.empty, Seq(c2))

    val c3 = g.add(node3)
    val c4 = g.add(node4)

    assert(g.classes.size == 2)
    assert(!g.areSame(c3, c4))

    g.unionMany(Seq((c3, c4)))

    // Check that the permutation has been propagated to the new node.
    assert(g.classes.size == 2)
    assert(g.areSame(c3, c4))
  }

  @Test
  def propagateIntoRedundantSlots(): Unit = {
    val g = empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()
    val z = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots. Adding and unifying these
    // nodes will result in a single class with a permutation.
    val node1 = ENode(0, Seq.empty, Seq(x, y, z), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, z, x), Seq.empty)

    val c1 = g.add(node1)
    val c2 = g.add(node2)

    // Now we create a class of two nodes that depends on c1, but in which slot y is redundant.
    val node3 = ENode(1, Seq.empty, Seq.empty, Seq(c1))
    val node4 = ENode(2, Seq.empty, Seq(x, z), Seq.empty)

    val c3 = g.add(node3)
    val c4 = g.add(node4)

    assert(g.classes.size == 3)
    assert(!g.areSame(c1, c2))
    assert(!g.areSame(c3, c4))

    // First, union c3 and c4. This will eliminate the slot y from c3/c4.
    g.unionMany(Seq((c3, c4)))

    assert(g.classes.size == 2)
    assert(g.areSame(c3, c4))
    assert(g.canonicalize(c3).args.size == 2)

    // Now we union c1 and c2. This will create a permutation that eliminates all slots from c3/c4.
    g.unionMany(Seq((c1, c2)))

    assert(g.classes.size == 2)
    assert(g.areSame(c1, c2))
    assert(g.areSame(c3, c4))
    assert(g.canonicalize(c3).args.size == 0)
  }
}
