package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EGraph, EGraphLike}
import fixpoint.eqsat.commands.CommandQueue

/**
 * A rule that can be applied to an e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MatchT The type of the match.
 * @tparam EGraphT The type of the e-graph that the rule can be applied to.
 * @param name The name of the rule.
 * @param searcher The searcher that finds matches of the rule in an e-graph.
 * @param applier The applier that applies a match to an e-graph.
 */
final case class Rule[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](name: String,
                                                                                               searcher: Searcher[NodeT, Seq[MatchT], EGraphT],
                                                                                               applier: Applier[NodeT, MatchT, EGraphT]) {
  /**
   * Tries to apply the rule to an e-graph.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize Whether to parallelize the search.
   * @return The e-graph after applying the rule, or None if the rule made no changes to the e-graph.
   */
  def tryApply(egraph: EGraphT, parallelize: Boolean = true): Option[EGraphT] = {
    val matches = searcher.search(egraph, parallelize)
    val commands = if (parallelize)
      matches.par.map(applier.apply(_, egraph)).seq
    else
      matches.map(applier.apply(_, egraph))

    CommandQueue(commands).optimized(egraph, Map())._1
  }

  /**
   * Applies the rule to an e-graph.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize Whether to parallelize the search.
   * @return The e-graph after applying the rule.
   */
  def apply(egraph: EGraphT, parallelize: Boolean = true): EGraphT = {
    tryApply(egraph, parallelize).getOrElse(egraph)
  }
}
