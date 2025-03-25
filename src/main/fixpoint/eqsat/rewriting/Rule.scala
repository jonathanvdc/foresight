package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EGraph, EGraphLike}
import fixpoint.eqsat.commands.{Command, CommandQueue}
import fixpoint.eqsat.parallel.ParallelMap

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
   * Finds all matches of the rule's searcher and applies each match to an e-graph. If the rule made no changes to the
   * e-graph, returns None.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize The parallelization strategy to use.
   * @return The e-graph after applying the rule, or None if the rule made no changes to the e-graph.
   */
  def tryApply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.parallel): Option[EGraphT] = {
    val matches = searcher.search(egraph, parallelize)
    val commands = parallelize[MatchT, Command[NodeT]](matches, applier.apply(_, egraph)).toSeq
    CommandQueue(commands).optimized(egraph, Map())._1
  }

  /**
   * Finds all matches of the rule's searcher and applies each match to an e-graph.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize The parallelization strategy to use.
   * @return The e-graph after applying the rule.
   */
  def apply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.parallel): EGraphT = {
    tryApply(egraph, parallelize).getOrElse(egraph)
  }
}
