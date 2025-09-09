package foresight.eqsat.commands

import foresight.eqsat.MixedTree

/**
 * Incrementally constructs a [[CommandQueue]] for later execution.
 *
 * This builder is a mutable convenience wrapper around [[CommandQueue]]’s
 * immutable API. It allows code to append commands without repeatedly
 * reassigning the queue, and to retrieve the finished queue via [[queue]].
 *
 * Typical usage is to:
 *   1. Create a `CommandQueueBuilder`
 *   2. Append additions or unions in the desired order
 *   3. Retrieve the resulting [[CommandQueue]] for simplification/optimization/application
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 *
 * @example
 * {{{
 * val b = new CommandQueueBuilder[MyNode]
 * val a = b.add(myTree)                // add a tree, get its symbol
 * val bSym = b.add(ENodeSymbol(op, Nil, Nil, Seq(a)))
 * b.union(a, bSym)                     // request that their classes be merged
 * val q: CommandQueue[MyNode] = b.queue.optimized
 * }}}
 */
final class CommandQueueBuilder[NodeT] {
  private val commands = Seq.newBuilder[Command[NodeT]]

  /**
   * The [[CommandQueue]] accumulated so far.
   */
  def result(): CommandQueue[NodeT] = CommandQueue(commands.result())

  /**
   * Appends a [[Command]] to the queue.
   *
   * @param cmd Command to append.
   */
  def append(cmd: Command[NodeT]): Unit = {
    commands += cmd
  }

  /**
   * Appends multiple [[Command]]s to the queue.
   *
   * @param cmds Commands to append.
   */
  def appendAll(cmds: Iterable[Command[NodeT]]): Unit = {
    commands ++= cmds
  }

  /**
   * Appends an insertion of an [[ENodeSymbol]].
   *
   * Internally wraps the node in a single-node [[AddManyCommand]].
   *
   * @param node Node to insert.
   * @return The fresh [[EClassSymbol.Virtual]] assigned to the inserted node’s e-class.
   */
  def add(node: ENodeSymbol[NodeT]): EClassSymbol = {
    val result = EClassSymbol.virtual()
    commands += AddManyCommand(Seq(result -> node))
    result
  }

  /**
   * Appends an insertion of a [[MixedTree]].
   *
   * Child subtrees are inserted first, then a final node referencing their
   * symbols is added. If the tree is a [[MixedTree.Atom]], no new command
   * is added and the existing [[EClassSymbol.Real]] is returned.
   *
   * @param tree Tree to insert.
   * @return The [[EClassSymbol]] for the tree’s root e-class.
   */
  def add(tree: MixedTree[NodeT, EClassSymbol]): EClassSymbol = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val result = EClassSymbol.virtual()
        commands += AddManyCommand(Seq(result -> ENodeSymbol(t, defs, uses, args.map(add))))
        result

      case MixedTree.Atom(call) =>
        call
    }
  }

  /**
   * Appends a [[UnionManyCommand]] request to merge two e-classes.
   *
   * @param a First class symbol.
   * @param b Second class symbol.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    commands += UnionManyCommand(Seq((a, b)))
  }
}
