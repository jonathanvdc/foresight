package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.EGraph
import fixpoint.eqsat.rewriting.{Applier, Searcher}

/**
 * A pattern that represents a tree of nodes, variables, and substitutions. The pattern is used to match against
 * e-classes in e-graphs.
 *
 * @tparam NodeT The type of the nodes in the pattern.
 */
sealed trait Pattern[NodeT] {
  /**
   * Converts the pattern to a searcher.
   * @return The searcher.
   */
  def toSearcher: Searcher[NodeT, Seq[PatternMatch[NodeT]], EGraph[NodeT]] = {
    Searcher(MachineSearcherPhase[NodeT, EGraph[NodeT]](PatternCompiler.compile(this)))
  }

  /**
   * Converts the pattern to a pattern applier.
   * @return The pattern applier.
   */
  def toApplier: Applier[NodeT, PatternMatch[NodeT], EGraph[NodeT]] = PatternApplier(this)
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

  /**
   * A companion object for Var.
   */
  object Var {
    /**
     * Creates a fresh variable.
     * @tparam NodeT The type of the nodes.
     * @return The fresh variable.
     */
    def fresh[NodeT](): Var[NodeT] = new Var[NodeT]
  }
}
