package fixpoint.eqsat.commands

/**
 * Constructs a queue of commands that can be applied to an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
final class CommandQueueBuilder[NodeT] {
  private var queueUnderConstruction: CommandQueue[NodeT] = CommandQueue.empty

  /**
   * The command queue that is being built.
   */
  def queue: CommandQueue[NodeT] = queueUnderConstruction

  /**
   * Appends a command that adds an e-node to the e-graph.
   * @param node The e-node to add.
   * @return The e-class symbol that represents the added e-node.
   */
  def add(node: ENodeSymbol[NodeT]): EClassSymbol = {
    val (symbol, newQueue) = queueUnderConstruction.add(node)
    queueUnderConstruction = newQueue
    symbol
  }

  /**
   * Appends a command that unions two e-classes in the e-graph.
   * @param a The first e-class to union.
   * @param b The second e-class to union.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    queueUnderConstruction = queueUnderConstruction.union(a, b)
  }
}
