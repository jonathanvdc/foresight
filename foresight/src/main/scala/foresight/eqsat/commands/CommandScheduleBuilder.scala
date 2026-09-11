package foresight.eqsat.commands

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.{CallTree, EClassCall, EClassSymbol, ENode, ENodeSymbol, MixedTree}
import foresight.util.Debug
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat.immutable.ArraySeq

/**
 * Constructs commands for later execution. Commands are scheduled in batches.
 * Union commands are executed at the end of the schedule while add commands
 * are grouped into batches. Additions with lower batch numbers
 * are always executed before additions with higher batch numbers. There is no ordering
 * guarantee between additions with the same batch number.
 */
trait CommandScheduleBuilder[NodeT] {
  /**
   * Appends an add command to the schedule. This command adds the given node
   * to the e-graph, producing an e-class with the given symbolic name.
   * @param symbol The e-class symbol of the e-class that contains or will contain the node.
   * @param node The node to add.
   * @param batch The batch number for scheduling.
   */
  def add(symbol: EClassSymbol.Virtual, node: ENodeSymbol[NodeT], batch: Int): Unit

  /**
   * Appends a union command to the schedule. This command unions the two given e-classes
   * in the e-graph.
   * @param a The first e-class symbol to union.
   * @param b The second e-class symbol to union.
   */
  def union(a: EClassSymbol, b: EClassSymbol): Unit

  /**
   * Synthesizes the accumulated additions and unions into a single command.
   *
   * Calling this method finalizes the schedule. No further additions or unions
   * may be appended after calling this method.
   *
   * @return The additions and unions as a single command.
   */
  def result(): CommandSchedule[NodeT]

  /**
   * Appends an add command to the schedule. This command adds the given node
   * to the e-graph, producing a new e-class with a fresh symbolic name.
   *
   * @param node  The node to add.
   * @param batch The batch number for scheduling.
   * @return The fresh e-class symbol assigned to the added node's e-class.
   */
  final def add(node: ENodeSymbol[NodeT], batch: Int): EClassSymbol.Virtual = {
    val symbol = EClassSymbol.virtual()
    add(symbol, node, batch)
    symbol
  }

  /**
   * Appends a request to merge two e-classes, but only if they are
   * not already known to be equivalent in the provided e-graph.
   *
   * If both `a` and `b` are [[EClassSymbol.Real]], their canonical
   * representatives in the e-graph are compared; if they differ, a
   * union command is added. If either is virtual, a union command is
   * always added.
   *
   * @param a      First class symbol.
   * @param b      Second class symbol.
   * @param egraph E-graph used to check existing equivalences.
   */
  final def unionSimplified(a: EClassSymbol, b: EClassSymbol, egraph: EGraph[NodeT]): Unit = {
    (a, b) match {
      case (callA: EClassCall, callB: EClassCall) =>
        if (egraph.canonicalize(callA) != egraph.canonicalize(callB)) {
          union(a, b)
        }
      case _ =>
        union(a, b)
    }
  }

  private[eqsat] def addSimplifiedReal(tree: CallTree[NodeT],
                                       egraph: EGraph[NodeT]): EClassSymbol = {
    val refPool = IntRef.defaultPool
    val maxBatch = refPool.acquire(0)
    val result = addSimplifiedReal(tree, egraph, maxBatch, ENode.defaultPool, refPool)
    refPool.release(maxBatch)
    result
  }

  private[eqsat] def addSimplifiedReal(tree: CallTree[NodeT],
                                       egraph: EGraph[NodeT],
                                       maxBatch: IntRef,
                                       nodePool: ENode.Pool,
                                       refPool: IntRef.Pool): EClassSymbol = {
    tree match {
      case CallTree.Node(t, defs, uses, args) =>
        // Local accumulator for children of this node.
        val childMax = refPool.acquire(0)
        val argSymbols = CommandScheduleBuilder.symbolArrayFrom(
          args,
          childMax,
          nodePool,
          (child: CallTree[NodeT], mb: IntRef) => addSimplifiedReal(child, egraph, mb, nodePool, refPool)
        )
        val sym = addSimplifiedNode(t, defs, uses, argSymbols, childMax, egraph, nodePool)
        // Propagate maximum required batch up to the caller's accumulator.
        if (childMax.elem > maxBatch.elem) maxBatch.elem = childMax.elem
        refPool.release(childMax)
        sym

      case call: EClassCall =>
        // No insertion required; keep caller's accumulator unchanged.
        EClassSymbol.real(call)
    }
  }

  private[eqsat] def addSimplifiedNode(nodeType: NodeT,
                                       definitions: SlotSeq,
                                       uses: SlotSeq,
                                       args: Array[EClassSymbol],
                                       maxBatch: IntRef,
                                       egraph: EGraph[NodeT],
                                       pool: ENode.Pool): EClassSymbol = {

    // Check if all children are already in the graph.
    val argCalls = CommandScheduleBuilder.resolveAllOrNull(args)

    // If the children are already present, we might not need to add a new node.
    if (argCalls != null) {
      if (Debug.isEnabled) {
        assert(maxBatch.elem == 0)
      }

      val candidateNode = pool.acquireUnsafe(
        nodeType,
        pool.acquireAndFillSlotArray(definitions),
        pool.acquireAndFillSlotArray(uses),
        argCalls)

      egraph.findOrNull(candidateNode) match {
        case null =>
          // Node does not exist in the graph but its children do exist in the graph.
          // Queue it for insertion in batch zero.
          add(candidateNode, 0)

        case existingCall =>
          // Node already exists in the graph; reuse its class.
          pool.release(candidateNode)
          EClassSymbol.real(existingCall)
      }
    } else {
      val candidateNode = ENodeSymbol[NodeT](nodeType, definitions, uses, UnsafeSeqFromArray(args))
      maxBatch.elem += 1
      add(candidateNode, maxBatch.elem)
    }
  }
}

/**
 * Companion object for [[CommandScheduleBuilder]].
 */
object CommandScheduleBuilder {
  /**
   * Creates a new concurrent command schedule builder. This builder can safely be used
   * from multiple threads.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @return A new concurrent command schedule builder.
   */
  def newConcurrentBuilder[NodeT]: CommandScheduleBuilder[NodeT] = new ConcurrentCommandScheduleBuilder[NodeT]()

  private[eqsat] def symbolArrayFrom[A](values: ArraySeq[A], maxBatch: IntRef, pool: ENode.Pool, valueToSymbol: (A, IntRef) => EClassSymbol): Array[EClassSymbol] = {
    // Try to avoid allocating an array of EClassSymbol if all entries are EClassCall.
    // The common case is that all children are already in the e-graph, and we will
    // want to construct an ENode with an Array[EClassCall].
    // If we find any entry that is not an EClassCall, we fall back to allocating
    // an Array[EClassSymbol] and copying the prefix of calls.
    val n = values.length
    val calls = pool.acquireCallArray(n)
    var i = 0
    while (i < n) {
      valueToSymbol(values(i), maxBatch) match {
        case c: EClassCall =>
          calls(i) = c
        case other =>
          // Fallback: allocate symbols array, copy the prefix of calls, and finish filling
          val syms = new Array[EClassSymbol](n)
          var j = 0
          while (j < i) {
            syms(j) = calls(j)
            j += 1
          }
          pool.releaseCallArray(calls)
          syms(i) = other
          j = i + 1
          while (j < n) {
            syms(j) = valueToSymbol(values(j), maxBatch); j += 1
          }
          return syms
      }
      i += 1
    }
    // All entries were EClassCall. Perform a safe upcast to Array[EClassSymbol]
    calls.asInstanceOf[Array[EClassSymbol]]
  }

  private[eqsat] def resolveAllOrNull(args: Array[EClassSymbol]): Array[EClassCall] = {
    if (args.isInstanceOf[Array[EClassCall]])
      args.asInstanceOf[Array[EClassCall]]
    else
      null
  }
}
