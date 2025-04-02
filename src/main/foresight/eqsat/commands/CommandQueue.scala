package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree}

import scala.collection.mutable

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
    CommandQueue(CommandQueue.optimizeCommands(flatCommands))
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

  private def optimizeCommands[NodeT](commands: Seq[Command[NodeT]]): Seq[Command[NodeT]] = {
    mergeUnions(commands, others => {
      val adds = others.collect { case cmd: AddManyCommand[NodeT] => cmd }
      if (adds.size == others.size) {
        optimizeAdds(adds)
      } else {
        CommandQueue.independentGroups(others).flatMap(CommandQueue.optimizeIndependentGroup)
      }
    })
  }

  private def mergeUnions[NodeT](group: Seq[Command[NodeT]],
                                 processRemaining: Seq[Command[NodeT]] => Seq[Command[NodeT]]): Seq[Command[NodeT]] = {
    // Partition the remaining commands into union commands and other commands.
    val (unionCommands, remainingCommands) = group.partition {
      case _: UnionManyCommand[NodeT] => true
      case _ => false
    } match {
      case (left, right) => (left.collect { case u: UnionManyCommand[NodeT] => u }, right)
    }

    // Merge all the union commands.
    val unionPairs = unionCommands.flatMap(_.pairs)
    unionPairs match {
      case Seq() => processRemaining(remainingCommands)
      case Seq(_*) => processRemaining(remainingCommands) :+ UnionManyCommand[NodeT](unionPairs)
    }
  }

  private def optimizeAdds[NodeT](group: Seq[AddManyCommand[NodeT]]): Seq[Command[NodeT]] = {
    // Our aim is to partition the add commands into batches of independent additions. We do this by tracking the
    // batches in which each node is defined. When we encounter a fresh addition, we add it to batch i + 1 such that
    // i is the highest batch in which any of its dependencies are defined.
    val batches = mutable.ArrayBuffer.empty[mutable.ArrayBuffer[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]]
    val defs = mutable.Map.empty[EClassSymbol, Int]

    for (command <- group) {
      val highestDependency = (-1 +: command.uses.collect {
        case use: EClassSymbol.Virtual if defs.contains(use) => defs(use)
      }).max

      val batchIndex = highestDependency + 1
      if (batchIndex == batches.size) {
        val newBatch = mutable.ArrayBuffer[(EClassSymbol.Virtual, ENodeSymbol[NodeT])](command.nodes: _*)
        batches += newBatch
      } else {
        batches(batchIndex).appendAll(command.nodes)
      }

      for (node <- command.nodes) {
        defs(node._1) = batchIndex
      }
    }

    // Merge all the addition commands in each batch.
    batches.map(AddManyCommand[NodeT])
  }

  private def optimizeIndependentGroup[NodeT](group: Seq[Command[NodeT]]): Seq[Command[NodeT]] = {
    // Partition the commands into addition commands and other commands.
    val (addCommands, remainingCommands) = group.partition {
      case _: AddManyCommand[NodeT] => true
      case _ => false
    } match {
      case (left, right) => (left.collect { case a: AddManyCommand[NodeT] => a }, right)
    }

    // Merge all the addition and union commands.
    val addPairs = addCommands.flatMap(_.nodes)
    addPairs match {
      case Seq() => remainingCommands
      case Seq(_*) => remainingCommands :+ AddManyCommand[NodeT](addPairs)
    }
  }

  /**
   * Groups a sequence of commands into independent command groups. Within each group, the commands may run in any
   * order.
   * @return A sequence of independent command groups.
   */
  private def independentGroups[NodeT](commands: Seq[Command[NodeT]]): Seq[Seq[Command[NodeT]]] = {
    val commandNumbers = commands.indices

    val defs = commandNumbers.flatMap(i => {
      commands(i).definitions.map(_ -> i)
    }).toMap

    // Step 1: Create a dependency graph
    val dependencyGraph = mutable.Map.empty[Int, Set[Int]]
    val reverseDependencies = mutable.Map.empty[Int, Set[Int]]

    for (command <- commandNumbers) {
      dependencyGraph(command) = Set.empty
      reverseDependencies(command) = Set.empty
    }

    for (i <- commandNumbers) {
      for (use <- commands(i).uses.collect { case u: EClassSymbol.Virtual => u }) {
        defs.get(use) match {
          case Some(j) if j != i =>
            dependencyGraph(i) += j
            reverseDependencies(j) += i
          case _ =>
        }
      }
    }

    // Step 2: Topological sort to find independent sets of commands
    val sortedCommands = mutable.ArrayBuffer.empty[Int]
    val noIncomingEdges = mutable.Queue(commandNumbers.filter(c => dependencyGraph(c).isEmpty): _*)

    while (noIncomingEdges.nonEmpty) {
      val command = noIncomingEdges.dequeue()
      sortedCommands += command

      for (dependent <- reverseDependencies(command)) {
        dependencyGraph(dependent) -= command
        if (dependencyGraph(dependent).isEmpty) {
          noIncomingEdges.enqueue(dependent)
        }
      }
    }

    // Step 3: Merge independent commands
    val groups = mutable.ArrayBuffer.empty[Seq[Command[NodeT]]]
    var currentBatch = mutable.ArrayBuffer.empty[Command[NodeT]]
    var currentBatchDefs = Set.empty[EClassSymbol]

    for (command <- sortedCommands) {
      val cmd = commands(command)
      if (currentBatch.nonEmpty && cmd.uses.toSet.intersect(currentBatchDefs).nonEmpty) {
        groups += currentBatch
        currentBatch = mutable.ArrayBuffer.empty
        currentBatchDefs = Set.empty
      }

      currentBatch += cmd
      currentBatchDefs ++= cmd.definitions
    }

    if (currentBatch.nonEmpty) {
      groups += currentBatch
    }

    groups
  }
}
