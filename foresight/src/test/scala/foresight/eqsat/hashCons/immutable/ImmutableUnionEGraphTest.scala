package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractUnionEGraphTest
import foresight.eqsat.mutable.FreezableEGraph

class ImmutableUnionEGraphTest extends AbstractUnionEGraphTest {
  type EGraphT[A] = FreezableEGraph[A, HashConsEGraph[A]]
  def empty[A]: EGraphT[A] = FreezableEGraph(HashConsEGraph.empty[A])
  def checkInvariants[A](g: EGraphT[A]): Unit = g.freeze().checkInvariants()
}
