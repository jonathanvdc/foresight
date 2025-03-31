package foresight.eqsat.rewriting

import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap

/**
 * A rule that can be applied to an e-graph.
 *
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
  def tryApply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Option[EGraphT] = {
    delayed(egraph, parallelize)(egraph, Map())._1
  }

  /**
   * Finds all matches of the rule's searcher and applies each match to an e-graph.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize The parallelization strategy to use.
   * @return The e-graph after applying the rule.
   */
  def apply(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): EGraphT = {
    tryApply(egraph, parallelize).getOrElse(egraph)
  }

  /**
   * Finds all matches of the rule's searcher.
   * @param egraph The e-graph to search in.
   * @param parallelize The parallelization strategy to use.
   * @return The matches found in the e-graph.
   */
  def search(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Seq[MatchT] = {
    searcher.search(egraph, parallelize)
  }

  /**
   * Finds all matches of the rule's searcher and constructs a command that applies each match to an e-graph.
   * @param egraph The e-graph to apply the rule to.
   * @param parallelize The parallelization strategy to use.
   * @return The command that applies the rule's matches to the e-graph.
   */
  def delayed(egraph: EGraphT, parallelize: ParallelMap = ParallelMap.default): Command[NodeT] = {
    val matches = search(egraph, parallelize)
    delayed(matches, egraph, parallelize)
  }

  /**
   * Constructs a command that applies each match to an e-graph.
   * @param matches The matches to apply.
   * @param egraph The e-graph to apply the matches to.
   * @param parallelize The parallelization strategy to use.
   * @return The command that applies the matches to the e-graph.
   */
  def delayed(matches: Seq[MatchT], egraph: EGraphT, parallelize: ParallelMap): Command[NodeT] = {
    val commands = parallelize[MatchT, Command[NodeT]](matches, applier.apply(_, egraph).simplify(egraph)).toSeq
    CommandQueue(commands).optimized
  }
}
