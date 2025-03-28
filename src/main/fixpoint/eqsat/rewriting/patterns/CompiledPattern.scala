package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike}

/**
 * A compiled pattern.
 * @param instructions The instructions of the compiled pattern.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
 */
final case class CompiledPattern[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](instructions: List[Instruction[NodeT, EGraphT]]) {
  /**
   * Searches for matches of the pattern in an e-graph.
   * @param call The e-class application to search for.
   * @param egraph The e-graph to search in.
   * @return The matches of the pattern in the e-graph.
   */
  def search(call: EClassCall, egraph: EGraphT): Seq[PatternMatch[NodeT]] = {
    val state = MachineState[NodeT](call)
    Machine.run(egraph, state, instructions).map { state =>
      PatternMatch(call, state.boundVars, state.boundSlots)
    }.toSeq
  }

  /**
   * Checks if an e-class application in an e-graph contains at least one match of the pattern.
   * @param call The e-class application to check.
   * @param egraph The e-graph to check in.
   * @return True if the pattern matches the e-class application, false otherwise.
   */
  def matches(call: EClassCall, egraph: EGraphT): Boolean = {
    val state = MachineState[NodeT](call)
    Machine.run(egraph, state, instructions).nonEmpty
  }
}
