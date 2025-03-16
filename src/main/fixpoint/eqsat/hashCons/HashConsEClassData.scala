package fixpoint.eqsat.hashCons

import fixpoint.eqsat.{EClassRef, ENode}

/**
 * The data of an e-class in a hash-consed e-graph.
 *
 * @param nodes The e-nodes in the e-class.
 * @param parents The parents of the e-class.
 * @tparam NodeT The type of the nodes described by the e-nodes in the e-class.
 */
private final case class HashConsEClassData[NodeT](nodes: Set[ENode[NodeT]], parents: Set[EClassRef])
