package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.rewriting.{PortableMatch, Rule}
import foresight.eqsat.saturation.priorities.MatchPriorities
import foresight.eqsat.util.random.Random

/**
 * A strategy that applies a sequence of rules in a stochastic manner, caching the matches that have already
 * been applied. This strategy operates on an [[EGraphWithRecordedApplications]].
 */
object StochasticRuleApplicationWithCaching {
  /**
   * Creates a strategy that applies a sequence of rules in a stochastic manner, caching the matches that have already
   * been applied. This strategy operates on an [[EGraphWithRecordedApplications]].
   *
   * @param rules The rules to apply.
   * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
   * @param random A random number generator used for selecting matches randomly.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A strategy that applies the rules with caching.
   */
  def apply[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT <: PortableMatch[NodeT, MatchT]
  ](
     rules: Seq[Rule[NodeT, MatchT, EGraphT]],
     priorities: MatchPriorities[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT],
     random: Random
   ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    StochasticRuleApplication(
      rules, SearchAndApply.withCaching[NodeT, EGraphT, MatchT], priorities, random)
  }

  /**
   * Creates a strategy that applies a sequence of rules in a stochastic manner, caching the matches that have already
   * been applied. This strategy operates on an [[EGraphWithRecordedApplications]].
   *
   * @param rules The rules to apply.
   * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
   * @return A strategy that applies the rules with caching, using a default random number generator with seed 0.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   */
  def apply[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT <: PortableMatch[NodeT, MatchT]
  ](
    rules: Seq[Rule[NodeT, MatchT, EGraphT]],
    priorities: MatchPriorities[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT]
  ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    apply(rules, priorities, Random(0))
  }
}
