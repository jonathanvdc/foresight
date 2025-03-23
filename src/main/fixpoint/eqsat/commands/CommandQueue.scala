package fixpoint.eqsat.commands

import fixpoint.eqsat.EGraph

import scala.collection.mutable.ArrayBuffer

/**
 * A queue of commands that can be applied to an e-graph.
 * @param egraph A base e-graph to which the commands in the queue can be applied. Commands may also be applied to a
 *               derived version of the e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
final class CommandQueue[NodeT](egraph: EGraph[NodeT]) {
  private val cmds: ArrayBuffer[Command[NodeT]] = ArrayBuffer.empty

  /**
   * All commands that have been added to the mutator.
   */
  def commands: Seq[Command[NodeT]] = cmds

  /**
   * Adds a new e-node to the e-graph.
   * @param node The e-node to add.
   * @return The e-class symbol that represents the added e-node.
   */
  def add(node: ENodeSymbol[NodeT]): EClassSymbol = {
    val result = EClassSymbol.virtual()
    cmds += AddCommand(node, result)
    result
  }

  /**
   * Unions two e-classes in the e-graph.
   * @param a The first e-class to union.
   * @param b The second e-class to union.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    cmds += UnionManyCommand(Seq(a -> b))
  }
}
