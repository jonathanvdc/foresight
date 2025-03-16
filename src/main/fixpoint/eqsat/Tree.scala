package fixpoint.eqsat

/**
 * A tree data structure.
 * @param nodeType The type of the node.
 * @param args The children of the node.
 * @tparam NodeT The type of the node.
 */
final case class Tree[NodeT](nodeType: NodeT, args: Seq[Tree[NodeT]])
