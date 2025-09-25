package foresight.eqsat.commands

import foresight.eqsat._
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat._
import scala.collection.mutable

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
  private var commands: mutable.Builder[Command[NodeT], Seq[Command[NodeT]]] = null

  /**
   * The [[CommandQueue]] accumulated so far.
   */
  def result(): CommandQueue[NodeT] = {
    if (commands == null) CommandQueue.empty
    else CommandQueue(commands.result())
  }

  private def initCommands(): Unit = {
    if (commands == null) commands = Seq.newBuilder[Command[NodeT]]
  }

  /**
   * Appends a [[Command]] to the queue.
   *
   * @param cmd Command to append.
   */
  def append(cmd: Command[NodeT]): Unit = {
    initCommands()
    commands += cmd
  }

  /**
   * Appends multiple [[Command]]s to the queue.
   *
   * @param cmds Commands to append.
   */
  def appendAll(cmds: Iterable[Command[NodeT]]): Unit = {
    initCommands()
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
    append(AddManyCommand(Seq(result -> node)))
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
        append(AddManyCommand(Seq(result -> ENodeSymbol(t, defs, uses, args.map(add)))))
        result

      case MixedTree.Atom(call) =>
        call
    }
  }

  /**
   * Appends an insertion of a [[MixedTree]], using the provided e-graph
   * to simplify the insertion.
   *
   * Child subtrees are inserted first, then a final node referencing their
   * symbols is added. If the tree is a [[MixedTree.Atom]], no new command
   * is added and the existing [[EClassSymbol.Real]] is returned.
   *
   * @param tree Tree to insert.
   * @return The [[EClassSymbol]] for the tree’s root e-class.
   */
  def addSimplified(tree: MixedTree[NodeT, EClassSymbol], egraph: ReadOnlyEGraph[NodeT]): EClassSymbol = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val argSymbols = CommandQueueBuilder.symbolArrayFrom(args, addSimplified(_, egraph))
        addSimplifiedNode(t, defs, uses, argSymbols, egraph)

      case MixedTree.Atom(call) => call
    }
  }

  private[eqsat] def addSimplifiedNode(nodeType: NodeT,
                                       definitions: Seq[Slot],
                                       uses: Seq[Slot],
                                       args: Array[EClassSymbol],
                                       egraph: ReadOnlyEGraph[NodeT]): EClassSymbol = {

    // Check if all children are already in the graph
    val argCalls = CommandQueueBuilder.resolveAllOrNull(args)

    // If the children are already present, we might not need to add a new node
    if (argCalls != null) {
      val candidateNode = ENode.unsafeWrapArrays(nodeType, definitions, uses, argCalls)
      egraph.findOrNull(candidateNode) match {
        case null =>
          // Node does not exist in the graph; we will add it below

        case existingCall =>
          // Node already exists in the graph; reuse its class
          return EClassSymbol.real(existingCall)
      }
    }

    val result = EClassSymbol.virtual()
    val candidateNode = ENodeSymbol[NodeT](nodeType, definitions, uses, UnsafeSeqFromArray(args))
    append(AddManyCommand(Seq(result -> candidateNode)))
    result
  }

  private[eqsat] def addSimplifiedReal(tree: MixedTree[NodeT, EClassCall], egraph: ReadOnlyEGraph[NodeT]): EClassSymbol = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val argSymbols = CommandQueueBuilder.symbolArrayFrom(args, addSimplifiedReal(_, egraph))
        addSimplifiedNode(t, defs, uses, argSymbols, egraph)

      case MixedTree.Atom(call) => EClassSymbol.real(call)
    }
  }

  /**
   * Appends a [[UnionManyCommand]] request to merge two e-classes.
   *
   * @param a First class symbol.
   * @param b Second class symbol.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    append(UnionManyCommand(Seq((a, b))))
  }

  /**
   * Appends a [[UnionManyCommand]] request to merge two e-classes,
   * but only if they are not already known to be equivalent in the
   * provided e-graph.
   *
   * If both `a` and `b` are [[EClassSymbol.Real]], their canonical
   * representatives in the e-graph are compared; if they differ, a
   * union command is added. If either is virtual, a union command is
   * always added.
   *
   * @param a First class symbol.
   * @param b Second class symbol.
   * @param egraph E-graph used to check existing equivalences.
   */
  def unionSimplified(a: EClassSymbol, b: EClassSymbol, egraph: ReadOnlyEGraph[NodeT]): Unit = {
    (a, b) match {
      case (callA: EClassCall, callB: EClassCall) =>
        if (egraph.canonicalize(callA) != egraph.canonicalize(callB)) {
          append(UnionManyCommand(Seq((a, b))))
        }
      case _ =>
        append(UnionManyCommand(Seq((a, b))))
    }
  }
}

private[eqsat] object CommandQueueBuilder {
  def symbolArrayFrom[A](values: immutable.ArraySeq[A], valueToSymbol: A => EClassSymbol): Array[EClassSymbol] = {
    // Try to avoid allocating an array of EClassSymbol if all entries are EClassCall.
    // The common case is that all children are already in the e-graph, and we will
    // want to construct an ENode with an Array[EClassCall].
    // If we find any entry that is not an EClassCall, we fall back to allocating
    // an Array[EClassSymbol] and copying the prefix of calls.
    val n = values.length
    val calls = new Array[EClassCall](n)
    var i = 0
    while (i < n) {
      valueToSymbol(values(i)) match {
        case c: EClassCall =>
          calls(i) = c
        case other =>
          // Fallback: allocate symbols array, copy the prefix of calls, and finish filling
          val syms = new Array[EClassSymbol](n)
          var j = 0
          while (j < i) { syms(j) = calls(j); j += 1 }
          syms(i) = other
          j = i + 1
          while (j < n) { syms(j) = valueToSymbol(values(j)); j += 1 }
          return syms
      }
      i += 1
    }
    // All entries were EClassCall. Perform a safe upcast to Array[EClassSymbol]
    calls.asInstanceOf[Array[EClassSymbol]]
  }

  def resolveAllOrNull(args: Array[EClassSymbol]): Array[EClassCall] = {
    if (args.isInstanceOf[Array[EClassCall]])
      args.asInstanceOf[Array[EClassCall]]
    else
      null
  }
}
