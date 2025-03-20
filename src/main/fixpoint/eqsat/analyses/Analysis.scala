package fixpoint.eqsat.analyses

import fixpoint.eqsat.{ENode, SlotMap}

/**
 * An analysis that can be performed on an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam A The type of the analysis result.
 */
trait Analysis[NodeT, A] {
  /**
   * Renames the slots in an analysis result.
   * @param result The analysis result to rename.
   * @param renaming The renaming to apply to the slots. The keys of the map are the slots as they appear in the result,
   *                 and the values are the slots to which they are renamed.
   * @return The analysis result with the slots renamed.
   */
  def rename(result: A, renaming: SlotMap): A

  /**
   * Makes an analysis result for a node.
   * @param node The node to make the analysis result for.
   * @param args The analysis results for the arguments to the node.
   * @return The analysis result for the node.
   */
  def make(node: ENode[NodeT], args: Seq[A]): A

  /**
   * Joins two analysis results.
   * @param left The left analysis result.
   * @param right The right analysis result.
   * @return The joined analysis result.
   */
  def join(left: A, right: A): A
}
