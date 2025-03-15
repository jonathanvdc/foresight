package eqsat.hashCons

import eqsat.ENode
import org.junit.Test

/**
 * Tests for the behavior of hash-consed e-graphs containing trees.
 */
class TreeEGraphTest {
  /**
   * Test that creating an e-graph containing a single node works.
   */
  @Test
  def singleNode(): Unit = {
    val egraph = HashConsEGraph.empty[Int]
    val node = ENode(0, Seq.empty)
    val (c, egraph2) = egraph.add(node)
    assert(egraph2.nodes(c).size == 1)
    assert(egraph2.nodes(c).head == node)
  }
}
