package foresight.eqsat.commands

import foresight.eqsat._
import foresight.util.collections.UnsafeSeqFromArray

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
        val argSymbols = args.map(addSimplified(_, egraph))
        addSimplifiedNode(t, defs, uses, argSymbols, egraph)

      case MixedTree.Atom(call) => call
    }
  }

  private[eqsat] def addSimplifiedNode(nodeType: NodeT,
                                       definitions: Seq[Slot],
                                       uses: Seq[Slot],
                                       args: Seq[EClassSymbol],
                                       egraph: ReadOnlyEGraph[NodeT]): EClassSymbol = {

    // Check if all children are already in the graph
    val argCalls = CommandQueueBuilder.resolveAllOrNull(args)

    // If the children are already present, we might not need to add a new node
    if (argCalls != null) {
      val candidateNode = ENode(nodeType, definitions, uses, UnsafeSeqFromArray(argCalls))
      egraph.find(candidateNode) match {
        case Some(existingCall) =>
          // Node already exists in the graph; reuse its class
          return EClassSymbol.real(existingCall)

        case None =>
          // Node does not exist; we will add it below
      }
    }

    val result = EClassSymbol.virtual()
    val candidateNode = ENodeSymbol[NodeT](nodeType, definitions, uses, args)
    commands += AddManyCommand(Seq(result -> candidateNode))
    result
  }

  private[eqsat] def addSimplifiedReal(tree: MixedTree[NodeT, EClassCall], egraph: ReadOnlyEGraph[NodeT]): EClassSymbol = {
    tree match {
      case MixedTree.Node(t, defs, uses, args) =>
        val argSymbols = args.map(addSimplifiedReal(_, egraph))
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
    commands += UnionManyCommand(Seq((a, b)))
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
      case (EClassSymbol.Real(callA), EClassSymbol.Real(callB)) =>
        if (egraph.canonicalize(callA) != egraph.canonicalize(callB)) {
          commands += UnionManyCommand(Seq((a, b)))
        }
      case _ =>
        commands += UnionManyCommand(Seq((a, b)))
    }
  }
}

private object CommandQueueBuilder {
  def resolveAllOrNull(args: Seq[EClassSymbol]): Array[EClassCall] = {
    if (args.forall(_.isReal)) {
      val arr = new Array[EClassCall](args.length)
      var i = 0
      while (i < args.length) {
        args(i) match {
          case EClassSymbol.Real(call) => arr(i) = call
          case _ => throw new IllegalStateException("Unreachable")
        }
        i += 1
      }
      arr
    } else {
      null
    }
  }
}
