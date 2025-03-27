package fixpoint.eqsat.commands

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, EGraphWithPendingUnions}

/**
 * A command that unions many e-classes in an e-graph.
 * @tparam NodeT The node type of the expressions that the e-graph represents.
 * @param pairs The pairs of e-class symbols to union.
 */
final case class UnionManyCommand[NodeT](pairs: Seq[(EClassSymbol, EClassSymbol)]) extends Command[NodeT] {
  override def uses: Seq[EClassSymbol] = pairs
    .flatMap(pair => Seq(pair._1, pair._2))

  override def definitions: Seq[EClassSymbol.Virtual] = Seq.empty

  override def apply[Repr <: EGraphLike[NodeT, Repr] with EGraph[NodeT]](egraph: Repr,
                                                                         reification: Map[EClassSymbol.Virtual, EClassCall]): (Option[Repr], Map[EClassSymbol.Virtual, EClassCall]) = {
    val withUnions = pairs.foldLeft(EGraphWithPendingUnions(egraph)) { (acc, pair) =>
      val reifiedPair = (pair._1.reify(reification), pair._2.reify(reification))
      acc.union(reifiedPair._1, reifiedPair._2)
    }

    if (withUnions.requiresRebuild) {
      (Some(withUnions.rebuilt), Map.empty)
    } else {
      (None, Map.empty)
    }
  }

  override def simplify(egraph: EGraph[NodeT],
                        partialReification: Map[EClassSymbol.Virtual, EClassCall]): (Command[NodeT], Map[EClassSymbol.Virtual, EClassCall]) = {
    pairs.map {
      case (left, right) => (left.refine(partialReification), right.refine(partialReification))
    }.filter {
      case (EClassSymbol.Real(left), EClassSymbol.Real(right)) => !egraph.areSame(left, right)
      case _ => true
    } match {
      case Seq() => (CommandQueue.empty, Map.empty)
      case simplifiedPairs => (UnionManyCommand(simplifiedPairs), Map.empty)
    }
  }
}
