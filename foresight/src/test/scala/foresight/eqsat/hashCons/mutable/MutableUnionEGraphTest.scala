package foresight.eqsat.hashCons.mutable

import foresight.eqsat.hashCons.AbstractUnionEGraphTest

class MutableUnionEGraphTest extends AbstractUnionEGraphTest {
  type EGraphT[A] = HashConsEGraph[A]
  def empty[A]: EGraphT[A] = HashConsEGraph.empty[A]
  def checkInvariants[A](g: EGraphT[A]): Unit = g.checkInvariants()
}
