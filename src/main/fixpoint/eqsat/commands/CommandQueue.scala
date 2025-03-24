package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A queue of commands to be applied to an e-graph. The queue can itself be applied as a command to an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param commands The commands to be applied to the e-graph.
 */
final case class CommandQueue[NodeT](commands: Seq[Command[NodeT]]) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol] = commands.flatMap(_.uses)
  override def definitions: Seq[VirtualEClassSymbol] = commands.flatMap(_.definitions)

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

  /**
   * Chains a command to the end of the queue.
   * @param command The command to chain.
   * @return The new command queue.
   */
  def chain(command: Command[NodeT]): CommandQueue[NodeT] = {
    CommandQueue(commands :+ command)
  }

  /**
   * Chains a command queue to the end of the queue.
   * @param commandQueue The command queue to chain.
   * @return The new command queue.
   */
  def chain(commandQueue: CommandQueue[NodeT]): CommandQueue[NodeT] = {
    CommandQueue(commands ++ commandQueue.commands)
  }

  /**
   * Optimizes the command queue by merging union commands. Commands may be reordered in order to facilitate this.
   * @return The optimized command queue.
   */
  def optimized: CommandQueue[NodeT] = {
    val (unions, otherCommands) = flatCommands.partition {
      case _: UnionManyCommand[NodeT] => true
      case _ => false
    } match {
      case (left, right) => (left.collect { case u: UnionManyCommand[NodeT] => u }, right)
    }

    val unionPairs = unions.flatMap(_.pairs)
    CommandQueue(otherCommands :+ UnionManyCommand(unionPairs))
  }

  /**
   * Recursively flattens the command queue into a sequence of commands.
   * @return The sequence of commands.
   */
  private def flatCommands: Seq[Command[NodeT]] = {
    commands.flatMap {
      case queue: CommandQueue[NodeT] => queue.flatCommands
      case command => Seq(command)
    }
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
