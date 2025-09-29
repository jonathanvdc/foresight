package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractTreeEGraphTest
import foresight.eqsat.mutable.FreezableEGraph

/** Wraps the immutable hashcons e-graph implementation in a mutable facade. */
class ImmutableTreeEGraphTest extends AbstractTreeEGraphTest {
  type EGraphT[A] = FreezableEGraph[A, HashConsEGraph[A]]

  def empty[A]: EGraphT[A] = FreezableEGraph(HashConsEGraph.empty[A])
  def checkInvariants[A](g: EGraphT[A]): Unit = {
    g.freeze().checkInvariants()
  }
}
