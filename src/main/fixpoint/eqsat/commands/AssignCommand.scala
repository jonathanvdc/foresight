package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A command that assigns a real or virtual e-class symbol to a virtual e-class symbol.
 * @param value The e-class symbol to assign.
 * @param result The virtual e-class symbol to which to assign the value.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 */
final case class AssignCommand[NodeT](value: EClassSymbol, result: EClassSymbol.Virtual) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol] = Seq(value)

  override def definitions: Seq[EClassSymbol.Virtual] = Seq(result)

  override def simplify(egraph: EGraph[NodeT],
                        partialReification: Map[EClassSymbol.Virtual, EClassCall]): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    value match {
      case _: EClassSymbol.Virtual => (this, Map.empty)
      case EClassSymbol.Real(call) => (this, Map(result -> call))
    }
  }

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[EClassSymbol.Virtual, EClassCall]): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    (None, Map(result -> value.reify(reification)))
  }
}
