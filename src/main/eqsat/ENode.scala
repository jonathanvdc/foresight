package eqsat

/**
 * An e-node in an e-graph. E-nodes are used to represent expressions in an e-graph. Each e-node has a node type and a
 * sequence of e-class references that represent the arguments of the e-node.
 * @param nodeType The node type of the e-node.
 * @param args The arguments of the e-node.
 * @tparam ExprT The type of the expression that the e-node represents.
 */
final case class ENode[ExprT](nodeType: NodeType[ExprT], args: Seq[EClassRef])
