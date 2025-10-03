package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassSymbol, ENodeSymbol, MixedTree}
import foresight.eqsat.mutable.{EGraph => MutableEGraph}
import foresight.eqsat.readonly

import scala.collection.mutable
import scala.collection.compat.immutable.ArraySeq

/**
 * A composable batch of [[Command]]s that itself behaves as a single [[Command]].
 *
 * Queues enable staging, simplification, and reordering of edits before applying them to a [[MutableEGraph]].
 * When applied, commands run in sequence, threading the evolving reification map through each step.
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 * @param commands The commands in execution order (prior to [[optimized]]).
 */
final case class CommandQueue[NodeT](commands: Seq[Command[NodeT]]) extends Command[NodeT] {

  /** All symbols used by the commands in this queue. */
  override def uses: Seq[EClassSymbol] = commands.flatMap(_.uses)

  /** All virtual symbols defined by the commands in this queue. */
  override def definitions: Seq[EClassSymbol.Virtual] = commands.flatMap(_.definitions)

  /**
   * Applies each command in order, threading the latest graph and reification.
   *
   * For each step, the current graph and the accumulated virtual-to-real bindings are passed to the next command.
   *
   * @param egraph Initial graph snapshot.
   * @param reification Initial virtual-to-concrete bindings available to the first command. This map is
   *                    mutated in place to include new bindings from each command.
   * @param parallelize Strategy for distributing work across commands that can parallelize internally.
   * @return `true` if at least one command changed the graph, otherwise `false`.
   */
  override def apply(egraph: MutableEGraph[NodeT],
                     reification: mutable.Map[EClassSymbol.Virtual, EClassCall],
                     parallelize: ParallelMap): Boolean = {
    var anyChanges: Boolean = false
    for (command <- commands) {
      val changed = command.apply(egraph, reification, parallelize)
      anyChanges ||= changed
    }
    anyChanges
  }

  /**
   * Appends an insertion of a single [[ENodeSymbol]] as an [[AddManyCommand]] of size 1.
   *
   * @param node Node to add.
   * @return The fresh output symbol for the node and the extended queue.
   *
   * @example
   * {{{
   * val (sym, q2) = q1.add(ENodeSymbol(op, defs, uses, args))
   * }}}
   */
  def add(node: ENodeSymbol[NodeT]): (EClassSymbol, CommandQueue[NodeT]) = {
    val result = EClassSymbol.virtual()
    (result, CommandQueue(commands :+ AddManyCommand(ArraySeq(result -> node))))
  }

  /**
   * Appends an insertion of a [[MixedTree]].
   *
   * Child subtrees are added first (depth-first), then a final node is inserted referencing
   * their produced symbols. If the tree is a [[MixedTree.Atom]], it is treated as already-real
   * and no command is added.
   *
   * @param tree Tree to insert.
   * @return The symbol for the tree's root and the extended queue.
   * @example
   * {{{
   * val (root, q2) = q1.add(myTree)
   * }}}
   */
  def add(tree: MixedTree[NodeT, EClassSymbol]): (EClassSymbol, CommandQueue[NodeT]) = {
    val builder = new CommandQueueBuilder[NodeT]()
    builder.appendAll(commands)
    val result = builder.add(tree)
    (result, builder.result())
  }

  /**
   * Appends a [[UnionManyCommand]] of size 1.
   *
   * @param left Left class symbol.
   * @param right Right class symbol.
   * @return The extended queue.
   *
   * @example
   * {{{
   * val q2 = q1.union(a, b)
   * }}}
   */
  def union(left: EClassSymbol, right: EClassSymbol): CommandQueue[NodeT] =
    CommandQueue(commands :+ UnionManyCommand(Seq((left, right))))

  /**
   * Appends a single [[Command]] to the end of this queue.
   *
   * @param command Command to add.
   * @return The extended queue.
   */
  def chain(command: Command[NodeT]): CommandQueue[NodeT] =
    CommandQueue(commands :+ command)

  /**
   * Concatenates another [[CommandQueue]] to the end of this queue.
   *
   * @param commandQueue Queue to append.
   * @return The extended queue.
   */
  def chain(commandQueue: CommandQueue[NodeT]): CommandQueue[NodeT] =
    CommandQueue(commands ++ commandQueue.commands)

  /**
   * Rewrites this queue into an equivalent but cheaper sequence by:
   *   - flattening nested queues,
   *   - merging adjacent [[UnionManyCommand]]s,
   *   - batching independent [[AddManyCommand]]s and layering dependent ones.
   *
   * Commands may be reordered where independence permits.
   *
   * @return An optimized queue; does not modify this instance.
   *
   * @example
   * {{{
   * val qOptim = q.optimized
   * }}}
   */
  def optimized: CommandQueue[NodeT] =
    CommandQueue(CommandQueue.optimizeCommands(flatCommands))

  /**
   * Recursively flattens nested [[CommandQueue]]s.
   *
   * @return A sequence of leaf commands in program order.
   */
  private def flatCommands: Seq[Command[NodeT]] =
    commands.flatMap {
      case queue: CommandQueue[NodeT] => queue.flatCommands
      case command => Seq(command)
    }

  /**
   * Simplifies each command against the current e-graph and threads partial reification.
   *
   * Each command is simplified in sequence, accumulating any discovered bindings. The resulting queue
   * is then [[optimized]] to merge unions and batch adds.
   *
   * @param egraph Context graph used by sub-command simplifications.
   * @param partialReification Upstream bindings available prior to running this queue.
   * @return The simplified-and-optimized queue and the accumulated partial bindings.
   *
   * @example
   * {{{
   * val (simp, partial) = q.simplify(g, Map.empty)
   * val (maybeG, finalRefs) = simp.apply(g, partial, parallel)
   * }}}
   */
  override def simplify(
                         egraph: readonly.EGraph[NodeT],
                         partialReification: Map[EClassSymbol.Virtual, EClassCall]
                       ): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    val newQueue = Seq.newBuilder[Command[NodeT]]
    var newReification = partialReification
    for (command <- flatCommands) {
      val (simplified, newReificationPart) = command.simplify(egraph, newReification)
      newQueue += simplified
      newReification ++= newReificationPart
    }
    (CommandQueue(newQueue.result()), newReification)
  }
}

/**
 * Helpers for constructing and optimizing [[CommandQueue]]s.
 */
object CommandQueue {

  private val emptyQueue: CommandQueue[_] = CommandQueue(Seq.empty)

  /**
   * Creates an empty queue.
   *
   * @tparam NodeT Node type for expressions represented by the e-graph.
   */
  def empty[NodeT]: CommandQueue[NodeT] = emptyQueue.asInstanceOf[CommandQueue[NodeT]]

  /** Applies union-merge and add-batching passes to a flat list of commands. */
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

  /**
   * Merges all [[UnionManyCommand]]s in a group into a single command, then processes the rest.
   *
   * @param group Flat list of commands.
   * @param processRemaining Pass to handle non-union commands once unions are merged.
   * @return Optimized sequence with at most one [[UnionManyCommand]] in the tail.
   */
  private def mergeUnions[NodeT](
                                  group: Seq[Command[NodeT]],
                                  processRemaining: Seq[Command[NodeT]] => Seq[Command[NodeT]]
                                ): Seq[Command[NodeT]] = {
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
      case Seq()   => processRemaining(remainingCommands)
      case Seq(_*) => processRemaining(remainingCommands) :+ UnionManyCommand[NodeT](unionPairs)
    }
  }

  /**
   * Batches independent [[AddManyCommand]]s into layers based on their intra-batch dependencies.
   *
   * Nodes whose arguments are produced in earlier layers are scheduled in later layers; unrelated
   * nodes share a layer and can be added together.
   *
   * @param group Only add-commands (pre-filtered).
   * @return A sequence of batched [[AddManyCommand]]s in dependency order.
   */
  private def optimizeAdds[NodeT](group: Seq[AddManyCommand[NodeT]]): Seq[Command[NodeT]] = {
    // Our aim is to partition the add commands into batches of independent additions. We do this by tracking the
    // batches in which each node is defined. When we encounter a fresh addition, we add it to batch i + 1 such that
    // i is the highest batch in which any of its dependencies are defined.
    type ArraySeqBuilder = mutable.Builder[(EClassSymbol.Virtual, ENodeSymbol[NodeT]), ArraySeq[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]]
    val batches = mutable.ArrayBuffer.empty[ArraySeqBuilder]
    val defs = mutable.HashMap.empty[EClassSymbol, Int]

    for (command <- group) {
      val highestDependency = (-1 +: command.uses.collect {
        case use: EClassSymbol.Virtual if defs.contains(use) => defs(use)
      }).max

      val batchIndex = highestDependency + 1
      if (batchIndex == batches.size) {
        val newBatch = ArraySeq.newBuilder[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]
        newBatch ++= command.nodes
        batches += newBatch
      } else {
        batches(batchIndex) ++= command.nodes
      }

      // Record the batch in which each node is defined.
      var i = 0
      while (i < command.nodes.length) {
        val node = command.nodes(i)
        defs(node._1) = batchIndex
        i += 1
      }
    }

    // Merge all the addition commands in each batch.
    batches.map(_.result()).map(AddManyCommand[NodeT]).toSeq
  }

  /**
   * Merges independent subgroups: batches adjacent [[AddManyCommand]]s and leaves others as-is.
   *
   * @param group A set of commands known to be independent of each other.
   * @return Optimized sequence for that group.
   */
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
      case Seq()   => remainingCommands
      case Seq(_*) => remainingCommands :+ AddManyCommand[NodeT](ArraySeq(addPairs: _*))
    }
  }

  /**
   * Groups commands into independent sets using virtual-symbol dataflow.
   *
   * A command A depends on B if A uses a virtual symbol that B defines. The result is
   * a topologically ordered partition where commands inside the same group have no such
   * dependencies and may run in any order.
   *
   * @return A sequence of independent command groups in execution order.
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
        groups += currentBatch.toSeq
        currentBatch = mutable.ArrayBuffer.empty
        currentBatchDefs = Set.empty
      }

      currentBatch += cmd
      currentBatchDefs ++= cmd.definitions
    }

    if (currentBatch.nonEmpty) {
      groups += currentBatch.toSeq
    }

    groups.toSeq
  }
}
