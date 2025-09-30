package foresight.eqsat.hashCons

import foresight.eqsat.{ENode, Slot, Tree}
import foresight.eqsat.mutable.{EGraph => MutableEGraph}
import org.junit.Test

/**
 * Tests for trees and DAGs over a mutable e-graph interface.
 * Subclasses only need to provide `empty[A]` returning a concrete `mutable.EGraph[A]`.
 */
abstract class AbstractTreeEGraphTest {
  type EGraphT[A] <: MutableEGraph[A]

  /** Create an empty mutable e-graph. */
  def empty[A]: EGraphT[A]

  /** Check the invariants of the given e-graph. */
  def checkInvariants[A](g: EGraphT[A]): Unit

  /**
   * Creates an e-graph containing a single node.
   */
  @Test
  def singleNode(): Unit = {
    val g = empty[Int]
    val node = ENode.unslotted(0, Seq.empty)
    val c = g.add(node)
    assert(g.classes.size == 1)
    assert(g.classes.head == c.ref)
    assert(g.nodes(c).size == 1)
    assert(g.nodes(c).head == node)
    assert(g.users(c.ref).isEmpty)

    assert(g.canonicalize(c) == c)
    assert(g.canonicalize(node).asNode == node)

    checkInvariants(g)
  }

  /**
   * Creates an e-graph containing two disjoint nodes.
   */
  @Test
  def twoNodes(): Unit = {
    val g = empty[Int]
    val node1 = ENode.unslotted(0, Seq.empty)
    val node2 = ENode.unslotted(1, Seq.empty)
    val c1 = g.add(node1)
    val c2 = g.add(node2)
    assert(g.classes.size == 2)
    assert(g.classes.toSeq.contains(c1.ref))
    assert(g.classes.toSeq.contains(c2.ref))
    assert(g.nodes(c1).size == 1)
    assert(g.nodes(c1).head == node1)
    assert(g.nodes(c2).size == 1)
    assert(g.nodes(c2).head == node2)
    assert(g.users(c1.ref).isEmpty)
    assert(g.users(c2.ref).isEmpty)

    assert(g.canonicalize(c1) == c1)
    assert(g.canonicalize(c2) == c2)
    assert(g.canonicalize(node1).asNode == node1)
    assert(g.canonicalize(node2).asNode == node2)

    checkInvariants(g)
  }

  /**
   * Creates an e-graph containing two nodes with a common argument.
   */
  @Test
  def twoNodesWithCommonArgument(): Unit = {
    val g = empty[Int]

    val arg = ENode.unslotted(0, Seq.empty)
    val c1 = g.add(arg)

    val node1 = ENode.unslotted(1, Seq(c1))
    val node2 = ENode.unslotted(2, Seq(c1))

    val c2 = g.add(node1)
    val c3 = g.add(node2)

    assert(g.classes.size == 3)
    assert(g.classes.toSeq.contains(c1.ref))
    assert(g.classes.toSeq.contains(c2.ref))
    assert(g.classes.toSeq.contains(c3.ref))
    assert(g.nodes(c1).size == 1)
    assert(g.nodes(c1).head == arg)
    assert(g.nodes(c2).size == 1)
    assert(g.nodes(c2).head == node1)
    assert(g.nodes(c3).size == 1)
    assert(g.nodes(c3).head == node2)
    assert(g.users(c1.ref) == Set(g.canonicalize(node1).shape, g.canonicalize(node2).shape))
    assert(g.users(c2.ref).isEmpty)
    assert(g.users(c3.ref).isEmpty)

    assert(g.canonicalize(c1) == c1)
    assert(g.canonicalize(c2) == c2)
    assert(g.canonicalize(c3) == c3)
    assert(g.canonicalize(arg).asNode == arg)
    assert(g.canonicalize(node1).asNode == node1)
    assert(g.canonicalize(node2).asNode == node2)

    checkInvariants(g)
  }

  /**
   * Creates an e-graph containing a node with two arguments.
   */
  @Test
  def nodeWithTwoArguments(): Unit = {
    val g = empty[Int]

    val arg1 = ENode.unslotted(0, Seq.empty)
    val arg2 = ENode.unslotted(1, Seq.empty)
    val c1 = g.add(arg1)
    val c2 = g.add(arg2)

    val node = ENode.unslotted(2, Seq(c1, c2))
    val c3 = g.add(node)

    assert(g.classes.size == 3)
    assert(g.classes.toSeq.contains(c1.ref))
    assert(g.classes.toSeq.contains(c2.ref))
    assert(g.classes.toSeq.contains(c3.ref))
    assert(g.nodes(c1).size == 1)
    assert(g.nodes(c1).head == arg1)
    assert(g.nodes(c2).size == 1)
    assert(g.nodes(c2).head == arg2)
    assert(g.nodes(c3).size == 1)
    assert(g.nodes(c3).head == node)
    assert(g.users(c1.ref) == Set(g.canonicalize(node).shape))
    assert(g.users(c2.ref) == Set(g.canonicalize(node).shape))
    assert(g.users(c3.ref).isEmpty)

    assert(g.canonicalize(c1) == c1)
    assert(g.canonicalize(c2) == c2)
    assert(g.canonicalize(c3) == c3)
    assert(g.canonicalize(arg1).asNode == arg1)
    assert(g.canonicalize(arg2).asNode == arg2)
    assert(g.canonicalize(node).asNode == node)

    checkInvariants(g)
  }

  /**
   * Creates an e-graph containing a node with two identical arguments.
   */
  @Test
  def nodeWithTwoIdenticalArguments(): Unit = {
    val g = empty[Int]

    val arg = ENode.unslotted(0, Seq.empty)
    val c1 = g.add(arg)

    val node = ENode.unslotted(1, Seq(c1, c1))
    val c2 = g.add(node)

    assert(g.classes.size == 2)
    assert(g.classes.toSeq.contains(c1.ref))
    assert(g.classes.toSeq.contains(c2.ref))
    assert(g.nodes(c1).size == 1)
    assert(g.nodes(c1).head == arg)
    assert(g.nodes(c2).size == 1)
    assert(g.nodes(c2).head == node)
    assert(g.users(c1.ref) == Set(g.canonicalize(node).shape))
    assert(g.users(c2.ref).isEmpty)

    assert(g.canonicalize(c1) == c1)
    assert(g.canonicalize(c2) == c2)
    assert(g.canonicalize(arg).asNode == arg)
    assert(g.canonicalize(node).asNode == node)

    checkInvariants(g)
  }

  /**
   * Creates an e-graph containing a tree.
   */
  @Test
  def fromTree(): Unit = {
    val g = empty[Int]
    val tree = Tree.unslotted(0, Seq(Tree.unslotted(1, Seq.empty), Tree.unslotted(2, Seq.empty)))
    val c = g.add(tree)
    assert(g.classes.size == 3)
    assert(g.classes.toSeq.contains(c.ref))
    assert(g.nodes(c).size == 1)
    assert(g.nodes(c).head.nodeType == 0)

    val firstArg = g.nodes(c).head.args.head
    val secondArg = g.nodes(c).head.args(1)
    assert(g.nodes(firstArg).head.nodeType == 1)
    assert(g.nodes(secondArg).head.nodeType == 2)

    assert(g.canonicalize(c) == c)

    checkInvariants(g)
  }

  @Test
  def singleSlottedNode(): Unit = {
    val g = empty[Int]

    val v0 = Slot.fresh()
    val v1 = Slot.fresh()

    val node = ENode(0, Seq.empty, Seq(v0, v1), Seq.empty)
    val c = g.add(node)

    assert(c.args.size == 2)

    assert(g.classes.size == 1)
    assert(g.classes.toSeq.contains(c.ref))
    assert(g.nodes(c).size == 1)
    assert(g.nodes(c).head == node)
    assert(g.users(c.ref).isEmpty)

    assert(g.canonicalize(c) == c)
    assert(g.canonicalize(node).asNode == node)

    checkInvariants(g)
  }

  @Test
  def lambdaAndVar(): Unit = {
    sealed trait NodeType
    case object Lambda extends NodeType
    case object Var extends NodeType

    val g = empty[NodeType]

    val x = Slot.fresh()

    val varAccess = ENode(Var, Seq.empty, Seq(x), Seq.empty)
    val c1 = g.add(varAccess)
    assert(g.classes.size == 1)
    assert(c1.args.size == 1)

    val lambda = ENode(Lambda, Seq(x), Seq.empty, Seq(c1))
    val c2 = g.add(lambda)
    assert(g.classes.size == 2)
    assert(c2.args.size == 0)

    assert(g.canonicalize(c1) == c1)
    assert(g.canonicalize(c2) == c2)

    val lambdaNode = g.nodes(c2).head
    assert(lambdaNode.definitions.head == lambdaNode.args.head.args.values.head)

    checkInvariants(g)
  }
}
