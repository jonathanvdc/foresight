package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A queue of commands to be applied to an e-graph. The queue can itself be applied as a command to an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param commands The commands to be applied to the e-graph.
 */
final case class CommandQueue[NodeT](commands: Seq[Command[NodeT]]) extends Command[NodeT] {
  override def args: Seq[EClassSymbol] = commands.flatMap(_.args)

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[VirtualEClassSymbol, EClassCall]): (Option[Repr], Map[VirtualEClassSymbol, EClassCall]) = {
    var newEGraph: Option[Repr] = None
    var newReification = reification
    for (command <- commands) {
      val (newEGraphOpt, newReificationPart) = command.apply(egraph, newReification)
      newEGraph = newEGraphOpt.orElse(newEGraph)
      newReification ++= newReificationPart
    }
    (newEGraph, newReification)
  }

  /**
   * Appends a command to the queue that adds a new e-node to the e-graph.
   * @param node The e-node to add to the e-graph.
   * @return The virtual e-class symbol that represents the added e-node and the new command queue.
   */
  def add(node: ENodeSymbol[NodeT]): (EClassSymbol, CommandQueue[NodeT]) = {
    val result = EClassSymbol.virtual()
    (result, CommandQueue(commands :+ AddCommand(node, result)))
  }

  /**
   * Appends a command to the queue that unions two e-classes in the e-graph.
   * @param left The first e-class to union.
   * @param right The second e-class to union.
   * @return The new command queue.
   */
  def union(left: EClassSymbol, right: EClassSymbol): CommandQueue[NodeT] = {
    CommandQueue(commands :+ UnionManyCommand(Seq((left, right))))
  }
}

/**
 * A companion object for command queues.
 */
object CommandQueue {
  /**
   * An empty command queue.
   * @tparam NodeT The node type of the expressions that the e-graph represents.
   */
  def empty[NodeT]: CommandQueue[NodeT] = CommandQueue(Seq.empty)
}

