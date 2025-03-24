package fixpoint.eqsat

/**
 * A mixed tree that can contain both nodes and e-class calls.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam CallT The type of the e-class calls in the tree.
 */
trait MixedTree[+NodeT, +CallT] {
  /**
   * Maps the e-class calls in the tree.
   * @param f The function to map the e-class calls.
   * @tparam NewCallT The type of the new e-class calls.
   * @return The tree with the e-class calls mapped.
   */
  def mapCalls[NewCallT](f: CallT => NewCallT): MixedTree[NodeT, NewCallT] = this match {
    case MixedTree.Node(nodeType, definitions, uses, children) =>
      MixedTree.Node(nodeType, definitions, uses, children.map(_.mapCalls[NewCallT](f)))

    case MixedTree.Call(call) =>
      MixedTree.Call(f(call))
  }
}

/**
 * A companion object for mixed trees.
 */
object MixedTree {
  /**
   * A node in a mixed tree.
   * @param nodeType The type of the node.
   * @param definitions The slots that are defined directly by the node.
   * @param uses The slots that are used directly by the node.
   * @param children The children of the node.
   * @tparam NodeT The type of the node.
   * @tparam CallT The type of the e-class calls.
   */
  final case class Node[NodeT, CallT](nodeType: NodeT,
                                      definitions: Seq[Slot],
                                      uses: Seq[Slot],
                                      children: Seq[MixedTree[NodeT, CallT]]) extends MixedTree[NodeT, CallT]

  /**
   * A call in a mixed tree.
   * @param call The e-class call.
   * @tparam NodeT The type of the nodes.
   * @tparam CallT The type of the e-class calls.
   */
  final case class Call[NodeT, CallT](call: CallT) extends MixedTree[NodeT, CallT]
}
