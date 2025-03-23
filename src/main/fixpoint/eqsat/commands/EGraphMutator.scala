package fixpoint.eqsat.commands

import fixpoint.eqsat.EGraph

final class EGraphMutator[NodeT](egraph: EGraph[NodeT], var commands: List[Command[NodeT]]) {
  def add(node: ENodeSymbol[NodeT]): EClassSymbol = ???

  def union(a: EClassSymbol, b: EClassSymbol): EClassSymbol = ???
}
