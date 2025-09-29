package foresight.eqsat.hashCons.immutable

import foresight.eqsat.hashCons.AbstractEmptyEGraphTest
import foresight.eqsat.readonly

/** Concrete instantiation for the hash-consed immutable e-graph implementation. */
class ImmutableEmptyEGraphTest extends AbstractEmptyEGraphTest {
  def empty[A]: readonly.EGraph[A] = HashConsEGraph.empty[A]
}
