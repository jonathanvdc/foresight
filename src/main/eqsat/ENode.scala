package eqsat

/**
 * An e-node in an e-graph. E-nodes are used to represent expressions in an e-graph. Each e-node has a node type and a
 * sequence of e-class references that represent the arguments of the e-node.
 *
 * @param nodeType The node type of the e-node.
 * @param args The arguments of the e-node.
 * @tparam NodeT The type of the node that the e-node represents. Node types contain all information required to form
 *               an expression aside from its arguments.
 */
final case class ENode[NodeT](nodeType: NodeT, args: Seq[EClassRef])
