package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{AddNodeResult, EClassCall, EGraph, EGraphLike}

/**
 * A command that adds e-nodes to an e-graph.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param nodes The e-nodes to add to the e-graph, along with the symbols that represent their e-classes. These nodes
 *              are independently reifiable, so they can be added in parallel. Reification information is not carried
 *              over from one node addition to another.
 */
final case class AddManyCommand[NodeT](nodes: Seq[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol] = nodes.flatMap(_._2.args)
  override def definitions: Seq[EClassSymbol.Virtual] = nodes.map(_._1)

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[EClassSymbol.Virtual, EClassCall],
                                                                         parallelize: ParallelMap): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    val reifiedNodes = nodes.map(_._2.reify(reification))
    val (addResults, newEGraph) = egraph.tryAddMany(reifiedNodes, parallelize)
    val newReification = nodes.zip(addResults).map {
      case ((symbol, _), result) => (symbol, result.call)
    }.toMap
    val anyChanges = addResults.exists({
      case AddNodeResult.Added(_) => true
      case AddNodeResult.AlreadyThere(_) => false
    })
    (if (anyChanges) Some(newEGraph) else None, newReification)
  }

  override def simplify(egraph: EGraph[NodeT],
                        partialReification: Map[EClassSymbol.Virtual, EClassCall]): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    val refinedNodes = nodes.map {
      case (result, node) =>
        val newArgs = node.args.map(_.refine(partialReification))
        val refined = node.copy(args = newArgs)
        if (newArgs.forall(_.isReal)) {
          val reifiedNode = refined.reify(partialReification)
          egraph.find(reifiedNode) match {
            case Some(call) => (refined, result, Some(call))
            case None => (refined, result, None)
          }
        } else {
          (refined, result, None)
        }
    }

    val resolved = refinedNodes.collect {
      case (_, result, Some(call)) => result -> call
    }

    val unresolved = refinedNodes.collect {
      case (node, result, None) => result -> node
    }

    (if (unresolved.isEmpty) CommandQueue.empty else AddManyCommand(unresolved), resolved.toMap)
  }
}
