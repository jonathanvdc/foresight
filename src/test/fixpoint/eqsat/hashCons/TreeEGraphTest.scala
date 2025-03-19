package fixpoint.eqsat.hashCons

import fixpoint.eqsat.slots.Slot
import fixpoint.eqsat.{ENode, Tree}
import org.junit.Test

/**
 * Tests for the behavior of hash-consed e-graphs containing trees and directed acyclic graphs.
 */
class TreeEGraphTest {
  /**
   * Creates an e-graph containing a single node.
   */
  @Test
  def singleNode(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node = ENode.unslotted(0, Seq.empty)
    val (c, egraph2) = egraph.add(node)
    assert(egraph2.classes.size == 1)
    assert(egraph2.classes.head == c.ref)
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head == node)
    assert(egraph2.users(c.ref).isEmpty)

    assert(egraph2.canonicalize(c) == c)
    assert(egraph2.canonicalize(node).asNode == node)

    egraph2.checkInvariants()
  }

  /**
   * Creates an e-graph containing two disjoint nodes.
   */
  @Test
  def twoNodes(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node1 = ENode.unslotted(0, Seq.empty)
    val node2 = ENode.unslotted(1, Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)
    assert(egraph3.classes.size == 2)
    assert(egraph3.classes.toSeq.contains(c1.ref))
    assert(egraph3.classes.toSeq.contains(c2.ref))
    assert(egraph3.nodes(c1).size == 1)
    assert(egraph3.nodes(c1).head == node1)
    assert(egraph3.nodes(c2).size == 1)
    assert(egraph3.nodes(c2).head == node2)
    assert(egraph3.users(c1.ref).isEmpty)
    assert(egraph3.users(c2.ref).isEmpty)

    assert(egraph3.canonicalize(c1) == c1)
    assert(egraph3.canonicalize(c2) == c2)
    assert(egraph3.canonicalize(node1).asNode == node1)
    assert(egraph3.canonicalize(node2).asNode == node2)

    egraph3.checkInvariants()
  }

  /**
   * Creates an e-graph containing two nodes with a common argument.
   */
  @Test
  def twoNodesWithCommonArgument(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg = ENode.unslotted(0, Seq.empty)
    val (c1, egraph2) = egraph.add(arg)

    val node1 = ENode.unslotted(1, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c1))

    val (c2, egraph3) = egraph2.add(node1)
    val (c3, egraph4) = egraph3.add(node2)

    assert(egraph4.classes.size == 3)
    assert(egraph4.classes.toSeq.contains(c1.ref))
    assert(egraph4.classes.toSeq.contains(c2.ref))
    assert(egraph4.classes.toSeq.contains(c3.ref))
    assert(egraph4.nodes(c1).size == 1)
    assert(egraph4.nodes(c1).head == arg)
    assert(egraph4.nodes(c2).size == 1)
    assert(egraph4.nodes(c2).head == node1)
    assert(egraph4.nodes(c3).size == 1)
    assert(egraph4.nodes(c3).head == node2)
    assert(egraph4.users(c1.ref) == Set(egraph4.canonicalize(node1).shape, egraph4.canonicalize(node2).shape))
    assert(egraph4.users(c2.ref).isEmpty)
    assert(egraph4.users(c3.ref).isEmpty)

    assert(egraph4.canonicalize(c1) == c1)
    assert(egraph4.canonicalize(c2) == c2)
    assert(egraph4.canonicalize(c3) == c3)
    assert(egraph4.canonicalize(arg).asNode == arg)
    assert(egraph4.canonicalize(node1).asNode == node1)
    assert(egraph4.canonicalize(node2).asNode == node2)

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node with two arguments.
   */
  @Test
  def nodeWithTwoArguments(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val (c1, egraph2) = egraph.add(arg1)
    val (c2, egraph3) = egraph2.add(arg2)

    val node = ENode.unslotted(2, Seq(c1, c2))
    val (c3, egraph4) = egraph3.add(node)

    assert(egraph4.classes.size == 3)
    assert(egraph4.classes.toSeq.contains(c1.ref))
    assert(egraph4.classes.toSeq.contains(c2.ref))
    assert(egraph4.classes.toSeq.contains(c3.ref))
    assert(egraph4.nodes(c1).size == 1)
    assert(egraph4.nodes(c1).head == arg1)
    assert(egraph4.nodes(c2).size == 1)
    assert(egraph4.nodes(c2).head == arg2)
    assert(egraph4.nodes(c3).size == 1)
    assert(egraph4.nodes(c3).head == node)
    assert(egraph4.users(c1.ref) == Set(egraph4.canonicalize(node).shape))
    assert(egraph4.users(c2.ref) == Set(egraph4.canonicalize(node).shape))
    assert(egraph4.users(c3.ref).isEmpty)

    assert(egraph4.canonicalize(c1) == c1)
    assert(egraph4.canonicalize(c2) == c2)
    assert(egraph4.canonicalize(c3) == c3)
    assert(egraph4.canonicalize(arg1).asNode == arg1)
    assert(egraph4.canonicalize(arg2).asNode == arg2)
    assert(egraph4.canonicalize(node).asNode == node)

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node with two identical arguments.
   */
  @Test
  def nodeWithTwoIdenticalArguments(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg = ENode.unslotted(0, Seq.empty)
    val (c1, egraph2) = egraph.add(arg)

    val node = ENode.unslotted(1, Seq(c1, c1))
    val (c2, egraph3) = egraph2.add(node)

    assert(egraph3.classes.size == 2)
    assert(egraph3.classes.toSeq.contains(c1.ref))
    assert(egraph3.classes.toSeq.contains(c2.ref))
    assert(egraph3.nodes(c1).size == 1)
    assert(egraph3.nodes(c1).head == arg)
    assert(egraph3.nodes(c2).size == 1)
    assert(egraph3.nodes(c2).head == node)
    assert(egraph3.users(c1.ref) == Set(egraph3.canonicalize(node).shape))
    assert(egraph3.users(c2.ref).isEmpty)

    assert(egraph3.canonicalize(c1) == c1)
    assert(egraph3.canonicalize(c2) == c2)
    assert(egraph3.canonicalize(arg).asNode == arg)
    assert(egraph3.canonicalize(node).asNode == node)

    egraph3.checkInvariants()
  }

  /**
   * Creates an e-graph containing a tree.
   */
  @Test
  def fromTree(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val tree = Tree.unslotted(0, Seq(Tree.unslotted(1, Seq.empty), Tree.unslotted(2, Seq.empty)))
    val (c, egraph2) = egraph.add(tree)
    assert(egraph2.classes.size == 3)
    assert(egraph2.classes.toSeq.contains(c.ref))
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head.nodeType == 0)

    val firstArg = egraph2.nodes(c).head.args.head
    val secondArg = egraph2.nodes(c).head.args(1)
    assert(egraph2.nodes(firstArg).head.nodeType == 1)
    assert(egraph2.nodes(secondArg).head.nodeType == 2)

    assert(egraph2.canonicalize(c) == c)

    egraph2.checkInvariants()
  }

  @Test
  def singleSlottedNode(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val v0 = Slot.fresh()
    val v1 = Slot.fresh()

    val node = ENode(0, Seq.empty, Seq(v0, v1), Seq.empty)
    val (c, egraph2) = egraph.add(node)

    assert(c.args.size == 2)

    assert(egraph2.classes.size == 1)
    assert(egraph2.classes.toSeq.contains(c.ref))
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head == node)
    assert(egraph2.users(c.ref).isEmpty)

    assert(egraph2.canonicalize(c) == c)
    assert(egraph2.canonicalize(node).asNode == node)

    egraph2.checkInvariants()
  }

  @Test
  def lambdaAndVar(): Unit = {
    sealed trait NodeType
    case object Lambda extends NodeType
    case object Var extends NodeType

    val egraph = HashConsEGraph.empty[NodeType]

    val x = Slot.fresh()

    val varAccess = ENode(Var, Seq.empty, Seq(x), Seq.empty)
    val (c1, egraph2) = egraph.add(varAccess)
    assert(egraph2.classes.size == 1)
    assert(c1.args.size == 1)

    val lambda = ENode(Lambda, Seq(x), Seq.empty, Seq(c1))
    val (c2, egraph3) = egraph2.add(lambda)
    assert(egraph3.classes.size == 2)
    assert(c2.args.size == 0)

    assert(egraph3.canonicalize(c1) == c1)
    assert(egraph3.canonicalize(c2) == c2)

    egraph3.checkInvariants()
  }
}
