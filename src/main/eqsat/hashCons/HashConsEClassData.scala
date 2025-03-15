package eqsat.hashCons

import eqsat.{EClassRef, ENode}

/**
 * The data of an e-class in a hash-consed e-graph.
 * @param nodes The e-nodes in the e-class.
 * @param parents The parents of the e-class.
 * @tparam ExprT The type of the expression that the e-graph represents.
 */
private[eqsat] final case class HashConsEClassData[ExprT](nodes: Set[ENode[ExprT]], parents: Set[EClassRef])
