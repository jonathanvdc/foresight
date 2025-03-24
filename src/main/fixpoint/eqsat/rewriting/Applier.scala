package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EGraph, EGraphLike}
import fixpoint.eqsat.commands.Command

/**
 * An applier that applies a match to an e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MatchT The type of the match.
 * @tparam EGraphT The type of the e-graph that the applier applies the match to.
 */
trait Applier[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Creates a command that applies a match to an e-graph.
   * @param m The match to apply.
   * @param egraph The e-graph in which the match was found.
   * @return The command that represents the application of the match.
   */
  def apply(m: MatchT, egraph: EGraphT): Command[NodeT]
}
