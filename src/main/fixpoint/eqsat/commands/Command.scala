package fixpoint.eqsat.commands

import fixpoint.eqsat.{EGraph, EGraphLike}

/**
 * A command that can be applied to an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
trait Command[NodeT] {
  /**
   * Applies the command to the given e-graph.
   * @param egraph The e-graph to which the command should be applied.
   * @return The new e-graph and a map from virtual e-class symbols to real e-class symbols.
   *         The map contains an entry for each virtual e-class symbol that is created by the command.
   */
  def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr): (Repr, Map[VirtualEClassSymbol, RealEClassSymbol])
}
