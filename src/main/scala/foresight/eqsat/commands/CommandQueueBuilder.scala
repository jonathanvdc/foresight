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
  private var queueUnderConstruction: CommandQueue[NodeT] = CommandQueue.empty

  /**
   * The [[CommandQueue]] accumulated so far.
   *
   * This value is updated after each call to [[add(ENodeSymbol) add(node)]],
   * [[add(MixedTree) add(tree)]], or [[union]].
   */
  def queue: CommandQueue[NodeT] = queueUnderConstruction

  /**
   * Appends an insertion of an [[ENodeSymbol]].
   *
   * Internally wraps the node in a single-node [[AddManyCommand]].
   *
   * @param node Node to insert.
   * @return The fresh [[EClassSymbol.Virtual]] assigned to the inserted node’s e-class.
   */
  def add(node: ENodeSymbol[NodeT]): EClassSymbol = {
    val (symbol, newQueue) = queueUnderConstruction.add(node)
    queueUnderConstruction = newQueue
    symbol
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
    val (symbol, newQueue) = queueUnderConstruction.add(tree)
    queueUnderConstruction = newQueue
    symbol
  }

  /**
   * Appends a [[UnionManyCommand]] request to merge two e-classes.
   *
   * @param a First class symbol.
   * @param b Second class symbol.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    queueUnderConstruction = queueUnderConstruction.union(a, b)
  }
}
