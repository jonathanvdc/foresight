package foresight.eqsat.hashCons.mutable

import foresight.eqsat.hashCons.AbstractEmptyEGraphTest
import foresight.eqsat.readonly

class MutableEmptyEGraphTest extends AbstractEmptyEGraphTest {
  def empty[A]: readonly.EGraph[A] = HashConsEGraph.empty[A]
}
