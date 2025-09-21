package foresight.eqsat.rewriting.patterns

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, ReadOnlyEGraph}

/**
 * A compiled pattern.
 *
 * @param instructions The instructions of the compiled pattern.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
 */
final case class CompiledPattern[NodeT, -EGraphT <: ReadOnlyEGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var],
                                                                           instructions: List[Instruction[NodeT, EGraphT]]) {
  /**
   * Searches for matches of the pattern in an e-graph.
   * @param call The e-class application to search for.
   * @param egraph The e-graph to search in.
   * @return The matches of the pattern in the e-graph.
   */
  def search(call: EClassCall, egraph: EGraphT): Seq[PatternMatch[NodeT]] = {
    val state = MachineState[NodeT](call)
    val matches = Machine.run(egraph, state, instructions).map { state =>
      PatternMatch(call, state.boundVars, state.boundSlots)
    }

    if (matches.size == 1) {
      // If there's only one match, skip the distinct call for performance
      matches
    } else {
      matches.distinct
    }
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

/**
 * A companion object for [[CompiledPattern]].
 */
object CompiledPattern {
  /**
   * Compiles a pattern into a compiled pattern.
   * @param pattern The pattern to compile.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the pattern is compiled for.
   * @return The compiled pattern.
   */
  def apply[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var]): CompiledPattern[NodeT, EGraphT] = {
    CompiledPattern(pattern, PatternCompiler.compile[NodeT, EGraphT](pattern))
  }
}
