package fixpoint.eqsat.rewriting.patterns

/**
 * A pattern that represents a tree of nodes, variables, and substitutions. The pattern is used to match against
 * e-classes in e-graphs.
 * @tparam NodeT The type of the nodes in the pattern.
 */
sealed trait Pattern[NodeT] {
  /**
   * Converts the pattern to a pattern applier.
   * @return The pattern applier.
   */
  def toApplier: PatternApplier[NodeT] = PatternApplier(this)
}

/**
 * A companion object for patterns.
 */
object Pattern {
  /**
   * A node in a pattern.
   * @param nodeType The type of the node.
   * @param definitions The slots that are defined directly by the node.
   * @param uses The slots that are used directly by the node.
   * @param children The children of the node.
   * @tparam NodeT The type of the node.
   */
  final case class Node[NodeT](nodeType: NodeT,
                               definitions: Seq[SlotVar],
                               uses: Seq[SlotVar],
                               children: Seq[Pattern[NodeT]]) extends Pattern[NodeT]

  /**
   * A variable in a pattern.
   * @tparam NodeT The type of the nodes.
   */
  final class Var[NodeT] extends Pattern[NodeT]
}
