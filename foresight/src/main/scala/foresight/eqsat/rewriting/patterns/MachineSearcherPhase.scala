package foresight.eqsat.rewriting.patterns

import foresight.eqsat.rewriting.{Applier, ReversibleSearcherPhase}
import foresight.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike}

/**
 * A phase of a searcher that searches for matches of a pattern machine in an e-graph.
 *
 * @param pattern The pattern to search for.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
final case class MachineSearcherPhase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](pattern: CompiledPattern[NodeT, EGraphT])
  extends ReversibleSearcherPhase[NodeT, Unit, Seq[PatternMatch[NodeT]], PatternMatch[NodeT], EGraphT] {

  override def search(call: EClassCall, egraph: EGraphT, input: Unit): Seq[PatternMatch[NodeT]] = {
    pattern.search(call, egraph)
  }

  override def aggregate(matches: Map[EClassRef, Seq[PatternMatch[NodeT]]]): Seq[PatternMatch[NodeT]] = {
    matches.values.flatten.toSeq
  }

  override def tryReverse(nextPhase: Applier[NodeT, Unit, EGraphT]): Option[Applier[NodeT, PatternMatch[NodeT], EGraphT]] = {
    Some(pattern.pattern.toApplier)
  }
}
