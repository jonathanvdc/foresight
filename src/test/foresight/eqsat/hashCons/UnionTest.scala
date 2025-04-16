package foresight.eqsat.hashCons

import foresight.eqsat.{ENode, Slot, Tree}
import org.junit.Test

class UnionTest {
  /**
   * Creates an e-graph containing two leaf nodes, then unions them.
   */
  @Test
  def unionTwoLeaves(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node1 = ENode.unslotted(1, Seq.empty)
    val node2 = ENode.unslotted(2, Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)

    assert(egraph3.classes.size == 2)

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(egraph4.classes.size == 1)
    assert(egraph4.nodes(c1) == egraph4.nodes(c2))
    assert(egraph4.nodes(c1) == Set(node1, node2))

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing two nodes with a common argument, then unions them.
   */
  @Test
  def unionTwoNodesWithCommonArgument(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val arg = ENode.unslotted(0, Seq.empty)
    val (c1, egraph2) = egraph.add(arg)

    val node1 = ENode.unslotted(1, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c1))
    val (c2, egraph3) = egraph2.add(node1)
    val (c3, egraph4) = egraph3.add(node2)

    assert(egraph4.classes.size == 3)

    val egraph5 = egraph4.union(c2, c3).rebuilt

    assert(egraph5.classes.size == 2)
    assert(egraph5.nodes(c2) == egraph5.nodes(c3))
    assert(egraph5.nodes(c2) == Set(node1, node2))

    egraph5.checkInvariants()
  }

  /**
   * Creates an e-graph containing three nodes, then unions them.
   */
  @Test
  def unionThreeNodes(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node1 = ENode.unslotted(1, Seq.empty)
    val node2 = ENode.unslotted(2, Seq.empty)
    val node3 = ENode.unslotted(3, Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)
    val (c3, egraph4) = egraph3.add(node3)

    assert(egraph4.classes.size == 3)

    val egraph5 = egraph4.union(c1, c2).rebuilt
    val egraph6 = egraph5.union(c1, c3).rebuilt

    assert(egraph6.classes.size == 1)
    assert(egraph6.nodes(c1) == egraph6.nodes(c2))
    assert(egraph6.nodes(c1) == egraph6.nodes(c3))
    assert(egraph6.nodes(c1) == Set(node1, node2, node3))

    assert(egraph4.union(c1, c2).union(c1, c3).requiresRebuild)
    val egraph7 = egraph4.union(c1, c2).union(c1, c3).rebuilt

    assert(egraph7.classes.size == 1)
    assert(egraph7.nodes(c1) == egraph7.nodes(c2))
    assert(egraph7.nodes(c1) == egraph7.nodes(c3))
    assert(egraph7.nodes(c1) == Set(node1, node2, node3))

    egraph7.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node with two arguments, then unions the arguments.
   */
  @Test
  def unionArgumentNodes(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val (c1, egraph2) = egraph.add(arg1)
    val (c2, egraph3) = egraph2.add(arg2)

    val node = ENode.unslotted(2, Seq(c1, c2))
    val (c3, egraph4) = egraph3.add(node)

    assert(egraph4.classes.size == 3)

    val egraph5 = egraph4.union(c1, c2).rebuilt
    val argClass = egraph5.canonicalize(c1)

    assert(egraph5.classes.size == 2)
    assert(egraph5.nodes(c1) == egraph5.nodes(c2))
    assert(egraph5.nodes(c1) == Set(arg1, arg2))
    assert(egraph5.nodes(c3) == Set(ENode.unslotted(2, Seq(argClass, argClass))))

    egraph5.checkInvariants()
  }

  /**
   * Creates an e-graph containing two leaf nodes and two nodes that reference the leaf nodes, then unions the leaf nodes.
   * The union operation unifies the leaf nodes as well as the nodes that reference them.
   */
  @Test
  def upwardMerge(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val (c1, egraph2) = egraph.add(arg1)
    val (c2, egraph3) = egraph2.add(arg2)

    val node1 = ENode.unslotted(2, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c2))
    val (c3, egraph4) = egraph3.add(node1)
    val (c4, egraph5) = egraph4.add(node2)

    assert(egraph5.classes.size == 4)

    val egraph6 = egraph5.union(c1, c2).rebuilt
    val argClass = egraph6.canonicalize(c1)

    assert(egraph6.classes.size == 2)
    assert(egraph6.canonicalize(c1) == egraph6.canonicalize(c2))
    assert(egraph6.canonicalize(c3) == egraph6.canonicalize(c4))
    assert(egraph6.nodes(c1) == egraph6.nodes(c2))
    assert(egraph6.nodes(c1) == Set(arg1, arg2))
    assert(egraph6.nodes(c3) == Set(ENode.unslotted(2, Seq(argClass))))

    egraph6.checkInvariants()
  }

  /**
   * Creates an e-graph containing two leaf nodes and two nodes that reference the leaf nodes, then unions the leaf nodes.
   * The union operation unifies the leaf nodes but not the nodes that reference them.
   */
  @Test
  def noUpwardMerge(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val (c1, egraph2) = egraph.add(arg1)
    val (c2, egraph3) = egraph2.add(arg2)

    val node1 = ENode.unslotted(2, Seq(c1))
    val node2 = ENode.unslotted(3, Seq(c2))
    val (c3, egraph4) = egraph3.add(node1)
    val (c4, egraph5) = egraph4.add(node2)

    assert(egraph5.classes.size == 4)

    val egraph6 = egraph5.union(c1, c2).rebuilt

    assert(egraph6.classes.size == 3)
    assert(egraph6.canonicalize(c1) == egraph6.canonicalize(c2))
    assert(egraph6.nodes(c1) == egraph6.nodes(c2))
    assert(egraph6.nodes(c1) == Set(arg1, arg2))
    assert(egraph6.nodes(c3) == Set(ENode.unslotted(2, Seq(egraph6.canonicalize(c1)))))
    assert(egraph6.nodes(c4) == Set(ENode.unslotted(3, Seq(egraph6.canonicalize(c1)))))

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node that references by using the union operation.
   */
  @Test
  def selfCycle(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node = ENode.unslotted(0, Seq.empty)
    val (c1, egraph2) = egraph.add(node)

    val selfCycle = ENode.unslotted(1, Seq(c1))
    val (c2, egraph3) = egraph2.add(selfCycle)

    assert(egraph3.classes.size == 2)

    val egraph4 = egraph3.union(c1, c2).rebuilt
    val argClass = egraph4.canonicalize(c1)

    assert(egraph4.classes.size == 1)
    assert(egraph4.nodes(c1) == Set(node, ENode.unslotted(1, Seq(argClass))))

    egraph4.checkInvariants()
  }

  @Test
  def xUnionYEliminatesSlot(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val (a, egraph2) = egraph.add(ENode(0, Seq.empty, Seq(x), Seq.empty))
    val (b, egraph3) = egraph2.add(ENode(0, Seq.empty, Seq(y), Seq.empty))

    assert(egraph3.classes.size == 1)

    assert(a.args.size == 1)
    assert(b.args.size == 1)

    val egraph4 = egraph3.union(a, b).rebuilt

    val canonical = egraph4.canonicalize(a)
    assert(egraph4.classes.size == 1)
    assert(canonical.args.size == 0)
  }

  @Test
  def xyUnionYzEliminatesSlots(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()
    val z = Slot.fresh()

    val (a, egraph2) = egraph.add(ENode(0, Seq.empty, Seq(x, y), Seq.empty))
    val (b, egraph3) = egraph2.add(ENode(0, Seq.empty, Seq(y, z), Seq.empty))

    assert(egraph3.classes.size == 1)

    assert(a.args.size == 2)
    assert(b.args.size == 2)

    val egraph4 = egraph3.union(a, b).rebuilt
    assert(egraph4.classes.size == 1)

    val canonical = egraph4.canonicalize(a)
    assert(canonical.args.size == 0)
  }

  @Test
  def xyUnionYxProducesPermutation(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val (a, egraph2) = egraph.add(node1)
    val (b, egraph3) = egraph2.add(node2)

    val node3 = ENode(1, Seq(x, y), Seq.empty, Seq(a))
    val node4 = ENode(1, Seq(x, y), Seq.empty, Seq(b))

    val (c, egraph4) = egraph3.add(node3)
    val (d, egraph5) = egraph4.add(node4)

    assert(egraph5.classes.size == 3)
    assert(c != d)

    assert(a.args.size == 2)
    assert(b.args.size == 2)
    assert(c.args.size == 0)
    assert(d.args.size == 0)

    assert(!egraph5.areSame(a, b))

    val egraph6 = egraph5.union(a, b).rebuilt
    assert(egraph6.classes.size == 2)

    val aCan = egraph6.canonicalize(a)
    val bCan = egraph6.canonicalize(b)
    val cCan = egraph6.canonicalize(c)
    val dCan = egraph6.canonicalize(d)

    assert(aCan.args.size == 2)
    assert(bCan.args.size == 2)
    assert(cCan.args.size == 0)
    assert(dCan.args.size == 0)

    assert(cCan == dCan)
    assert(egraph6.areSame(a, b))
  }

  @Test
  def xMinusXUnionZeroEliminatesSlot(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Minus extends NodeType

    val egraph = HashConsEGraph.empty[NodeType]

    val x = Slot.fresh()

    val xMinusX = Tree(Minus, Seq.empty, Seq.empty, Seq(
      Tree(Var, Seq.empty, Seq(x), Seq.empty),
      Tree(Var, Seq.empty, Seq(x), Seq.empty)))

    val (c, egraph2) = egraph.add(xMinusX)

    assert(egraph2.classes.size == 2)
    assert(c.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val (d, egraph3) = egraph2.add(zero)

    assert(egraph3.classes.size == 3)
    assert(d.args.size == 0)

    val egraph4 = egraph3.union(c, d).rebuilt

    assert(egraph4.classes.size == 2)
    assert(egraph4.canonicalize(c).args.size == 0)
    assert(egraph4.canonicalize(c) == egraph4.canonicalize(d))
  }

  @Test
  def zeroTimesXUnionZeroEliminatesSlot(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Mul extends NodeType

    val egraph = HashConsEGraph.empty[NodeType]

    val x = Slot.fresh()

    val zeroTimesX = Tree.unslotted(
      Mul,
      Seq(
        Tree.unslotted(Const(0), Seq.empty),
        Tree(Var, Seq.empty, Seq(x), Seq.empty)))

    val (c, egraph2) = egraph.add(zeroTimesX)

    assert(egraph2.classes.size == 3)
    assert(c.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val (d, egraph3) = egraph2.add(zero)

    assert(egraph3.classes.size == 3)
    assert(d.args.size == 0)

    val egraph4 = egraph3.union(c, d).rebuilt

    assert(egraph4.classes.size == 2)
    assert(egraph4.canonicalize(c).args.size == 0)
    assert(egraph4.canonicalize(c) == egraph4.canonicalize(d))
  }

  @Test
  def zeroTimesXUnionZeroEliminatesSlot2(): Unit = {
    sealed trait NodeType
    case object Var extends NodeType
    case class Const(value: Int) extends NodeType
    case object Mul extends NodeType

    val egraph = HashConsEGraph.empty[NodeType]

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

    val (c, egraph2) = egraph.add(zeroTimesX)
    val (e, egraph3) = egraph2.add(zeroTimesXTimesOne)

    assert(egraph3.classes.size == 5)
    assert(c.args.size == 1)
    assert(e.args.size == 1)

    val zero = ENode(Const(0), Seq.empty, Seq.empty, Seq.empty)
    val (d, egraph4) = egraph3.add(zero)

    assert(egraph4.classes.size == 5)
    assert(d.args.size == 0)

    val egraph5 = egraph4.union(c, d).rebuilt

    assert(egraph5.classes.size == 4)
    assert(egraph5.canonicalize(c).args.size == 0)
    assert(egraph5.canonicalize(c) == egraph5.canonicalize(d))
    assert(egraph5.canonicalize(e).args.size == 0)
  }

  @Test
  def canonicalizeAfterShrinking(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val x = Slot.fresh()
    val (a, egraph2) = egraph.add(ENode(0, Seq.empty, Seq(x), Seq.empty))
    val (b, egraph3) = egraph2.add(ENode(1, Seq.empty, Seq(x), Seq.empty))
    val (c, egraph4) = egraph3.add(ENode(2, Seq.empty, Seq(x), Seq.empty))
    val egraph5 = egraph4.union(a, b).union(a, c).rebuilt
    val canonical = egraph5.canonicalize(a)
    assert(canonical.args.size == 1)

    egraph5.checkInvariants()

    val (d, egraph6) = egraph5.add(ENode(2, Seq.empty, Seq.empty, Seq.empty))
    val egraph7 = egraph6.union(a, d).rebuilt
    val canonical2 = egraph7.canonicalize(a)
    assert(canonical2.args.size == 0)
    val canonical3 = egraph7.canonicalize(b)
    assert(canonical3.args.size == 0)
    val canonical4 = egraph7.canonicalize(c)
    assert(canonical4.args.size == 0)

    egraph7.checkInvariants()
  }

  @Test
  def upwardMergeDueToSlotElimination(): Unit = {
    // This test is similar to the upwardMerge test, but the upward merge is due to slot elimination.
    val egraph = HashConsEGraph.empty[Int]
    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create the same node twice and instantiate it with different slots.
    val node1 = ENode(0, Seq.empty, Seq(x), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y), Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)

    // Then create a pair of nodes that take the first two nodes as arguments. The nodes are different only because
    // c1 and c2 are currently different.
    val node3 = ENode.unslotted(1, Seq(c1, c2))
    val node4 = ENode.unslotted(1, Seq(c1, c1))
    val (c3, egraph4) = egraph3.add(node3)
    val (c4, egraph5) = egraph4.add(node4)

    val node5 = ENode.unslotted(2, Seq.empty)
    val (c5, egraph6) = egraph5.add(node5)

    assert(egraph6.classes.size == 4)
    assert(!egraph6.areSame(c1, c2))
    assert(!egraph6.areSame(c3, c4))

    // We now use a union with c5 to delete the slot in c1/c2 that distinguishes c3 and c4.
    val egraph7 = egraph6.union(c1, c5).rebuilt

    // Since the slot has been deleted from c1/c2, calls to them with different arguments will now be the same.
    // Consequently, c3 and c4 now contain identical nodes. Check that an upward merge has unified c3 and c4.
    assert(egraph7.classes.size == 2)
    assert(egraph7.areSame(c1, c2))
    assert(egraph7.areSame(c3, c4))
  }

  @Test
  def eliminateRedundantSlotDueToOrbit(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots. Adding and unifying these
    // nodes will result in a single class with a permutation indicating that x and y can be swapped.
    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)

    assert(egraph3.classes.size == 1)
    assert(!egraph3.areSame(c1, c2))

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(egraph4.classes.size == 1)
    assert(egraph4.areSame(c1, c2))

    // Now create another node that takes only slot x as an argument. We will merge this node with c1/c2, which will
    // eliminate both slots x and y from the class.
    val node3 = ENode(1, Seq.empty, Seq(x), Seq.empty)

    val (c3, egraph5) = egraph4.add(node3)

    assert(egraph5.classes.size == 2)
    assert(!egraph5.areSame(c1, c3))
    assert(!egraph5.areSame(c2, c3))

    // Now we union c1/c2 with c3. This will eliminate the slots x and y from the class, y is not used in c3 and x is
    // in y's orbit.
    val egraph6 = egraph5.union(c1, c3).rebuilt

    assert(egraph6.classes.size == 1)
    assert(egraph6.areSame(c1, c2))
    assert(egraph6.areSame(c1, c3))
    assert(egraph6.areSame(c2, c3))

    // Check that the slots have been eliminated from the class.
    val canonical = egraph6.canonicalize(c1)
    assert(canonical.args.size == 0)
  }

  @Test
  def propagateSymmetries(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    // First create two nodes that are identical except for the order of their slots. Adding and unifying these
    // nodes will result in a single class with a permutation indicating that x and y can be swapped.
    val node1 = ENode(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENode(0, Seq.empty, Seq(y, x), Seq.empty)

    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)

    assert(egraph3.classes.size == 1)
    assert(!egraph3.areSame(c1, c2))

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(egraph4.classes.size == 1)
    assert(egraph4.areSame(c1, c2))

    // Now create another node that takes the node as an argument, as well as its swapped version.
    val node3 = ENode(1, Seq.empty, Seq.empty, Seq(c1))
    val node4 = ENode(1, Seq.empty, Seq.empty, Seq(c2))

    val (c3, egraph5) = egraph4.add(node3)
    val (c4, egraph6) = egraph5.add(node4)

    // Check that the permutation has been propagated to the new node.
    assert(egraph6.classes.size == 2)
    assert(egraph6.areSame(c3, c4))
  }
}
