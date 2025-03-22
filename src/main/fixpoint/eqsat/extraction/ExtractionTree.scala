package fixpoint.eqsat.extraction

import fixpoint.eqsat.{Slot, Tree}

/**
 * A tree data structure for extraction.
 * @param cost The cost of the tree.
 * @param nodeType The type of the tree's root.
 * @param definitions The slots of the root that are defined by the root itself.
 * @param uses The slots of the root that are used by the root and are defined elsewhere.
 * @param args The children of the root node.
 * @tparam NodeT The type of the nodes in the tree.
 * @tparam C The type of the cost.
 */
final case class ExtractionTree[+NodeT, C](cost: C,
                                           nodeType: NodeT,
                                           definitions: Seq[Slot],
                                           uses: Seq[Slot],
                                           args: Seq[ExtractionTreeCall[NodeT, C]]) {
  /**
   * Gets the size of the tree.
   * @return The size of the tree.
   */
  val size: Int = args.map(_.size).sum + 1

  /**
   * Gets the depth of the tree.
   * @return The depth of the tree.
   */
  val depth: Int = (args.map(_.depth) :+ 0).max + 1

  /**
   * Turns the extraction tree into an expression tree.
   * @return The expression tree.
   */
  def toTree: Tree[NodeT] = {
    val newArgs = args.map(_.applied.toTree)
    Tree(nodeType, definitions, uses, newArgs)
  }
}
