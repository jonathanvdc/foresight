package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree}

/**
 * A queue of commands to be applied to an e-graph. The queue can itself be applied as a command to an e-graph.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param commands The commands to be applied to the e-graph.
 */
final case class CommandQueue[NodeT](commands: Seq[Command[NodeT]]) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol] = commands.flatMap(_.uses)
  override def definitions: Seq[EClassSymbol.Virtual] = commands.flatMap(_.definitions)

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[EClassSymbol.Virtual, EClassCall],
                                                                         parallelize: ParallelMap): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    var newEGraph: Option[Repr] = None
    var newReification = reification
    for (command <- commands) {
      val (newEGraphOpt, newReificationPart) = command.apply(newEGraph.getOrElse(egraph), newReification, parallelize)
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
    (result, CommandQueue(commands :+ AddManyCommand(Seq(result -> node))))
  }

  /**
   * Appends a command to the queue that adds a new tree to the e-graph.
   * @param tree The tree to add to the e-graph.
   * @return The virtual e-class symbol that represents the added tree and the new command queue.
   */
  def add(tree: MixedTree[NodeT, EClassSymbol]): (EClassSymbol, CommandQueue[NodeT]) = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val (addedArgs, newQueue) = args.foldLeft((Seq.empty[EClassSymbol], this)) {
          case ((added, queue), arg: MixedTree[NodeT, EClassSymbol]) =>
            val (result, newQueue) = queue.add(arg)
            (added :+ result, newQueue)
        }
        newQueue.add(ENodeSymbol(t, defs, uses, addedArgs))

      case MixedTree.Call(call) =>
        (call, this)
    }
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

  override def simplify(egraph: EGraph[NodeT],
                        partialReification: Map[EClassSymbol.Virtual, EClassCall]): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    val (newQueue, newReification) = flatCommands.foldLeft((CommandQueue.empty[NodeT], partialReification)) {
      case ((newQueue, reification), command) =>
        val (simplified, newReification) = command.simplify(egraph, reification)
        (newQueue.chain(simplified), reification ++ newReification)
    }

    (newQueue.optimized, newReification)
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
