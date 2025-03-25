package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.{EClassCall, EClassRef, EGraph, EGraphLike}
import fixpoint.eqsat.rewriting.SearcherPhase

/**
 * A phase of a searcher that searches for matches of a pattern machine in an e-graph.
 * @param instructions The instructions of the pattern machine.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
final case class MachineSearcherPhase[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](instructions: List[Instruction[NodeT, EGraphT]])
  extends SearcherPhase[NodeT, Unit, Seq[PatternMatch[NodeT]], Seq[PatternMatch[NodeT]], EGraphT] {

  override def search(call: EClassCall, egraph: EGraphT, input: Unit): Seq[PatternMatch[NodeT]] = {
    val state = MachineState(call)
    Machine.run(instructions, state, instructions).map { state =>
      PatternMatch(call, state.boundVars, state.boundSlots)
    }.toSeq
  }

  override def aggregate(matches: Map[EClassRef, Seq[PatternMatch[NodeT]]]): Seq[PatternMatch[NodeT]] = {
    matches.values.flatten.toSeq
  }
}
