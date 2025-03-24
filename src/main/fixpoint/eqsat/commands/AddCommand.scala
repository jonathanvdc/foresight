package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A command that adds an e-node to an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param node The e-node to add to the e-graph.
 * @param result The e-class symbol that represents the added e-node.
 */
final case class AddCommand[NodeT](node: ENodeSymbol[NodeT], result: EClassSymbol.Virtual) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol.Virtual] = node.args.collect { case v: EClassSymbol.Virtual => v }
  override def definitions: Seq[EClassSymbol.Virtual] = Seq(result)

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[EClassSymbol.Virtual, EClassCall]): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    val reifiedNode = node.reify(reification)
    val (newEClass, newEGraph) = egraph.tryAdd(reifiedNode)
    (newEGraph, Map(result -> newEClass))
  }
}
