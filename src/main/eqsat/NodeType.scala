package eqsat

/**
 * A trait that represents the type of node in an e-graph or in an expression. This trait is used to build expressions
 * from e-nodes. Node types contain all information required to form an expression aside from its arguments.
 * Hence, equivalent e-nodes have the same node type.
 * @tparam ExprT The type of the expression that the node type builds.
 */
trait NodeType[ExprT] {
  /**
   * Builds an expression from the given arguments.
   * @param args The arguments to build the expression from.
   * @return The expression built from the arguments.
   */
  def build(args: Seq[ExprT]): ExprT
}
