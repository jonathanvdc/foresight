package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractEmptyEGraphTest
import foresight.eqsat.readonly

class ImmutableEmptyEGraphTest extends AbstractEmptyEGraphTest {
  def empty[A]: readonly.EGraph[A] = HashConsEGraph.empty[A]
}
