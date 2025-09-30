package foresight.eqsat.hashCons.mutable

import foresight.eqsat.hashCons.AbstractTreeEGraphTest

class MutableTreeEGraphTest extends AbstractTreeEGraphTest {
  type EGraphT[A] = HashConsEGraph[A]

  def empty[A]: EGraphT[A] = HashConsEGraph.empty[A]
  def checkInvariants[A](g: EGraphT[A]): Unit = {
    g.checkInvariants()
  }
}

