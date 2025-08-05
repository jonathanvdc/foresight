package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree}

/**
 * A command that can be applied to an e-graph.
 *
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
trait Command[NodeT] {
  /**
   * All e-class symbols that need to be reified to run the command.
   */
  def uses: Seq[EClassSymbol]

  /**
   * All e-class symbols that are defined by the command.
   */
  def definitions: Seq[EClassSymbol.Virtual]

  /**
   * Applies the command to the given e-graph.
   * @param egraph The e-graph to which the command should be applied.
   * @param reification A map from virtual e-class symbols to e-class calls that are used to reify the virtual e-class
   *                    symbols.
   * @param parallelize The parallelization strategy to use.
   * @return The new e-graph, if it was changed, and a map from virtual e-class symbols to real e-class symbols.
   *         The map contains an entry for each virtual e-class symbol that is created by the command.
   */
  def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                reification: Map[EClassSymbol.Virtual, EClassCall],
                                                                parallelize: ParallelMap): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall])

  /**
   * Simplifies the command for a given e-graph. This method can be used to optimize the command for a specific e-graph.
   * For example, a command that unions two e-classes that are already in the same e-class can be simplified to a no-op.
   * Similarly, a command that adds an e-node that is already in the e-graph can be simplified to a reification map
   * assignment.
   * @param egraph The e-graph for which the command is to be simplified.
   * @param partialReification A map from virtual e-class symbols to e-class calls that are used to reify the virtual
   *                           e-clas symbols.
   * @return The simplified command and a partial reification map for this command.
   */
  def simplify(egraph: EGraph[NodeT],
               partialReification: Map[EClassSymbol.Virtual, EClassCall]): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall])

  /**
   * Simplifies the command for a given e-graph. This method can be used to optimize the command for a specific e-graph.
   * For example, a command that unions two e-classes that are already in the same e-class can be simplified to a no-op.
   * Similarly, a command that adds an e-node that is already in the e-graph can be simplified to a reification map
   * assignment.
   * @param egraph The e-graph for which the command is to be simplified.
   * @return The simplified command.
   */
  final def simplify(egraph: EGraph[NodeT]): Command[NodeT] = simplify(egraph, Map.empty)._1
}

/**
 * A companion object for [[Command]].
 */
object Command {
  /**
   * Creates a command that unifies an e-class symbol with an expression tree.
   * @param symbol The e-class symbol to unify.
   * @param tree The expression tree to unify with the e-class symbol.
   * @tparam NodeT The node type of the expression tree.
   * @return The command that unifies the e-class symbol with the expression tree.
   */
  def addEquivalentTree[NodeT](symbol: EClassSymbol, tree: MixedTree[NodeT, EClassSymbol]): Command[NodeT] = {
    val builder = new CommandQueueBuilder[NodeT]
    val c = builder.add(tree)
    builder.union(symbol, c)
    builder.queue
  }
}
