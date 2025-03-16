package fixpoint.eqsat.hashCons

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
    val node = ENode(0, Seq.empty)
    val (c, egraph2) = egraph.add(node)
    assert(egraph2.classes.size == 1)
    assert(egraph2.classes.head == c)
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head == node)
    assert(egraph2.parents(c).isEmpty)

    assert(egraph2.canonicalize(c) == c)
    assert(egraph2.canonicalize(node) == node)

    egraph2.checkInvariants()
  }

  /**
   * Creates an e-graph containing two disjoint nodes.
   */
  @Test
  def twoNodes(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node1 = ENode(0, Seq.empty)
    val node2 = ENode(1, Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)
    assert(egraph3.classes.size == 2)
    assert(egraph3.classes.toSeq.contains(c1))
    assert(egraph3.classes.toSeq.contains(c2))
    assert(egraph3.nodes(c1).size == 1)
    assert(egraph3.nodes(c1).head == node1)
    assert(egraph3.nodes(c2).size == 1)
    assert(egraph3.nodes(c2).head == node2)
    assert(egraph3.parents(c1).isEmpty)
    assert(egraph3.parents(c2).isEmpty)

    assert(egraph3.canonicalize(c1) == c1)
    assert(egraph3.canonicalize(c2) == c2)
    assert(egraph3.canonicalize(node1) == node1)
    assert(egraph3.canonicalize(node2) == node2)

    egraph3.checkInvariants()
  }

  /**
   * Creates an e-graph containing two nodes with a common argument.
   */
  @Test
  def twoNodesWithCommonArgument(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg = ENode(0, Seq.empty)
    val (c1, egraph2) = egraph.add(arg)

    val node1 = ENode(1, Seq(c1))
    val node2 = ENode(2, Seq(c1))

    val (c2, egraph3) = egraph2.add(node1)
    val (c3, egraph4) = egraph3.add(node2)

    assert(egraph4.classes.size == 3)
    assert(egraph4.classes.toSeq.contains(c1))
    assert(egraph4.classes.toSeq.contains(c2))
    assert(egraph4.classes.toSeq.contains(c3))
    assert(egraph4.nodes(c1).size == 1)
    assert(egraph4.nodes(c1).head == arg)
    assert(egraph4.nodes(c2).size == 1)
    assert(egraph4.nodes(c2).head == node1)
    assert(egraph4.nodes(c3).size == 1)
    assert(egraph4.nodes(c3).head == node2)
    assert(egraph4.parents(c1) == Set(c2, c3))
    assert(egraph4.parents(c2).isEmpty)
    assert(egraph4.parents(c3).isEmpty)

    assert(egraph4.canonicalize(c1) == c1)
    assert(egraph4.canonicalize(c2) == c2)
    assert(egraph4.canonicalize(c3) == c3)
    assert(egraph4.canonicalize(arg) == arg)
    assert(egraph4.canonicalize(node1) == node1)
    assert(egraph4.canonicalize(node2) == node2)

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node with two arguments.
   */
  @Test
  def nodeWithTwoArguments(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg1 = ENode(0, Seq.empty)
    val arg2 = ENode(1, Seq.empty)
    val (c1, egraph2) = egraph.add(arg1)
    val (c2, egraph3) = egraph2.add(arg2)

    val node = ENode(2, Seq(c1, c2))
    val (c3, egraph4) = egraph3.add(node)

    assert(egraph4.classes.size == 3)
    assert(egraph4.classes.toSeq.contains(c1))
    assert(egraph4.classes.toSeq.contains(c2))
    assert(egraph4.classes.toSeq.contains(c3))
    assert(egraph4.nodes(c1).size == 1)
    assert(egraph4.nodes(c1).head == arg1)
    assert(egraph4.nodes(c2).size == 1)
    assert(egraph4.nodes(c2).head == arg2)
    assert(egraph4.nodes(c3).size == 1)
    assert(egraph4.nodes(c3).head == node)
    assert(egraph4.parents(c1) == Set(c3))
    assert(egraph4.parents(c2) == Set(c3))
    assert(egraph4.parents(c3).isEmpty)

    assert(egraph4.canonicalize(c1) == c1)
    assert(egraph4.canonicalize(c2) == c2)
    assert(egraph4.canonicalize(c3) == c3)
    assert(egraph4.canonicalize(arg1) == arg1)
    assert(egraph4.canonicalize(arg2) == arg2)
    assert(egraph4.canonicalize(node) == node)

    egraph4.checkInvariants()
  }

  /**
   * Creates an e-graph containing a node with two identical arguments.
   */
  @Test
  def nodeWithTwoIdenticalArguments(): Unit = {
    val egraph = HashConsEGraph.empty[Int]

    val arg = ENode(0, Seq.empty)
    val (c1, egraph2) = egraph.add(arg)

    val node = ENode(1, Seq(c1, c1))
    val (c2, egraph3) = egraph2.add(node)

    assert(egraph3.classes.size == 2)
    assert(egraph3.classes.toSeq.contains(c1))
    assert(egraph3.classes.toSeq.contains(c2))
    assert(egraph3.nodes(c1).size == 1)
    assert(egraph3.nodes(c1).head == arg)
    assert(egraph3.nodes(c2).size == 1)
    assert(egraph3.nodes(c2).head == node)
    assert(egraph3.parents(c1) == Set(c2))
    assert(egraph3.parents(c2).isEmpty)

    assert(egraph3.canonicalize(c1) == c1)
    assert(egraph3.canonicalize(c2) == c2)
    assert(egraph3.canonicalize(arg) == arg)
    assert(egraph3.canonicalize(node) == node)

    egraph3.checkInvariants()
  }

  /**
   * Creates an e-graph containing a tree.
   */
  @Test
  def fromTree(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val tree = Tree(0, Seq(Tree(1, Seq.empty), Tree(2, Seq.empty)))
    val (c, egraph2) = egraph.add(tree)
    assert(egraph2.classes.size == 3)
    assert(egraph2.classes.toSeq.contains(c))
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head.nodeType == 0)

    val firstArg = egraph2.nodes(c).head.args.head
    val secondArg = egraph2.nodes(c).head.args(1)
    assert(egraph2.nodes(firstArg).head.nodeType == 1)
    assert(egraph2.nodes(secondArg).head.nodeType == 2)

    assert(egraph2.canonicalize(c) == c)

    egraph2.checkInvariants()
  }
}
