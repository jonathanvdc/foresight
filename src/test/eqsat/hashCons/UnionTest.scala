package eqsat.hashCons

import eqsat.ENode
import org.junit.Test

class UnionTest {
  /**
   * Creates an e-graph containing two leaf nodes, then unions them.
   */
  @Test
  def unionTwoLeaves(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node1 = ENode(1, Seq.empty)
    val node2 = ENode(2, Seq.empty)
    val (c1, egraph2) = egraph.add(node1)
    val (c2, egraph3) = egraph2.add(node2)

    assert(egraph3.classes.size == 2)

    val egraph4 = egraph3.union(c1, c2).rebuilt

    assert(egraph4.classes.size == 1)
    assert(egraph4.nodes(c1) == egraph4.nodes(c2))
    assert(egraph4.nodes(c1) == Set(node1, node2))
  }
}
