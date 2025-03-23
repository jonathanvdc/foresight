package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, EGraphWithPendingUnions}

/**
 * A command that unions many e-classes in an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param pairs The pairs of e-class symbols to union.
 */
final case class UnionManyCommand[NodeT](pairs: Seq[(EClassSymbol, EClassSymbol)]) extends Command[NodeT] {
  override def args: Seq[EClassSymbol] = pairs.flatMap(pair => Seq(pair._1, pair._2))

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[VirtualEClassSymbol, EClassCall]): (Option[Repr], Map[VirtualEClassSymbol, EClassCall]) = {
    val withUnions = pairs.foldLeft(EGraphWithPendingUnions(egraph)) { (acc, pair) =>
      val reifiedPair = (pair._1.reify(reification), pair._2.reify(reification))
      acc.egraph.union(reifiedPair._1, reifiedPair._2)
    }

    if (withUnions.requiresRebuild) {
      (Some(withUnions.egraph), Map.empty)
    } else {
      (None, Map.empty)
    }
  }
}
