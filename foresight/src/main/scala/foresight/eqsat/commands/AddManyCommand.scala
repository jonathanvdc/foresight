package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat._

/**
 * A [[Command]] that inserts multiple [[ENodeSymbol]]s into an e-graph in one batch.
 *
 * Each entry in [[nodes]] associates:
 *   - a fresh [[EClassSymbol.Virtual]] that will represent the root e-class of the node once inserted, and
 *   - the [[ENodeSymbol]] itself, which may reference real or virtual arguments.
 *
 * Nodes in this batch are *independently* reifiable and can be processed in parallel.
 * No reification state is shared between them during insertion.
 *
 * @tparam NodeT Node type for expressions represented by the e-graph.
 * @param nodes Sequence of `(outputSymbol, node)` pairs to insert. The `outputSymbol`
 *              is bound to the resulting e-class after insertion. The node’s arguments
 *              are resolved via the reification map provided to [[apply]].
 */
final case class AddManyCommand[NodeT](
                                        nodes: Seq[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]
                                      ) extends Command[NodeT] {

  /** All argument symbols used by every node in this batch. */
  override def uses: Seq[EClassSymbol] = nodes.flatMap(_._2.args)

  /** All virtual symbols that will be defined as a result of this command. */
  override def definitions: Seq[EClassSymbol.Virtual] = nodes.map(_._1)

  /**
   * Reifies all nodes in [[nodes]] using the given map, then inserts them
   * into the e-graph with [[EGraph.tryAddMany]].
   *
   * Nodes are processed independently, allowing parallel insertion.
   * The returned reification map binds each output symbol from [[nodes]]
   * to the [[EClassCall]] of its inserted or matched e-class.
   *
   * @param egraph Target e-graph to update.
   * @param reification Mapping from virtual e-class symbols to concrete calls,
   *                    used to resolve each node’s arguments before insertion.
   * @param parallelize Strategy for distributing the work.
   * @return
   *   - `Some(newGraph)` if at least one node was newly added, or `None` if all were already present.
   *   - A reification map binding each node’s output symbol to its resulting e-class.
   *
   * @example
   * {{{
   * val v = EClassSymbol.virtual()
   * val n = ENodeSymbol(nodeType, defs, uses, args = Seq(v))
   * val cmd = AddManyCommand(Seq(v -> n))
   * val (maybeNewGraph, newRefs) = cmd(egraph, Map(v -> existingCall), parallel)
   * }}}
   */
  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](
                                                                          egraph: Repr,
                                                                          reification: Map[EClassSymbol.Virtual, EClassCall],
                                                                          parallelize: ParallelMap
                                                                        ): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    val reifiedNodes = nodes.map(_._2.reify(reification))
    val (addResults, newEGraph) = egraph.tryAddMany(reifiedNodes, parallelize)

    val newReification = nodes.zip(addResults).map {
      case ((symbol, _), result) => (symbol, result.call)
    }.toMap

    val anyChanges = addResults.exists {
      case AddNodeResult.Added(_) => true
      case AddNodeResult.AlreadyThere(_) => false
    }

    (if (anyChanges) Some(newEGraph) else None, newReification)
  }

  /**
   * Simplifies this command for the given e-graph and partial reification.
   *
   * For each node:
   *   - All argument symbols are refined using the partial reification.
   *   - If all arguments are real, the node is fully reified and checked
   *     against the e-graph:
   *       - If already present, the output symbol is bound immediately.
   *       - If absent, the node remains for insertion.
   *
   * @param egraph Target e-graph for context.
   * @param partialReification Known bindings for virtual symbols.
   * @return
   *   - A simplified command containing only unresolved insertions,
   *     or [[CommandQueue.empty]] if all nodes were already present.
   *   - An updated partial reification containing all newly resolved outputs.
   */
  override def simplify(
                         egraph: EGraph[NodeT],
                         partialReification: Map[EClassSymbol.Virtual, EClassCall]
                       ): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {

    val resolvedBuilder = Map.newBuilder[EClassSymbol.Virtual, EClassCall]
    val unresolvedBuilder = Seq.newBuilder[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]

    def resolveAllOrNull(args: Seq[EClassSymbol]): Seq[EClassCall] = {
      val resolvedArgs = Seq.newBuilder[EClassCall]
      for (arg <- args) {
        arg match {
          case EClassSymbol.Real(call) =>
            resolvedArgs += call
          case v: EClassSymbol.Virtual if partialReification.contains(v) =>
            resolvedArgs += partialReification(v)
          case _ =>
            // Argument is virtual and not in the partial reification.
            // Cannot fully resolve this node.
            return null
        }
      }
      resolvedArgs.result()
    }

    for ((result, node) <- nodes) {
      val resolvedArgs = resolveAllOrNull(node.args)
      if (resolvedArgs != null) {
        val reifiedNode = ENode(node.nodeType, node.definitions, node.uses, resolvedArgs)
        egraph.find(reifiedNode) match {
          case Some(call) =>
            resolvedBuilder += (result -> call)
          case None =>
            val refined = node.copy(args = resolvedArgs.map(EClassSymbol.Real(_)))
            unresolvedBuilder += (result -> refined)
        }
      } else {
        val refined = node.copy(args = node.args.map(_.refine(partialReification)))
        unresolvedBuilder += (result -> refined)
      }
    }

    val resolved = resolvedBuilder.result()
    val unresolved = unresolvedBuilder.result()

    (
      if (unresolved.isEmpty) CommandQueue.empty else AddManyCommand(unresolved),
      resolved
    )
  }
}
