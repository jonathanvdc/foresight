package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A command that can be applied to an e-graph.
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
  def definitions: Seq[VirtualEClassSymbol]

  /**
   * Applies the command to the given e-graph.
   * @param egraph The e-graph to which the command should be applied.
   * @param reification A map from virtual e-class symbols to e-class calls that are used to reify the virtual e-class
   *                    symbols.
   * @return The new e-graph, if it was changed, and a map from virtual e-class symbols to real e-class symbols.
   *         The map contains an entry for each virtual e-class symbol that is created by the command.
   */
  def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                reification: Map[VirtualEClassSymbol, EClassCall]): (Option[Repr], Map[VirtualEClassSymbol, EClassCall])
}
