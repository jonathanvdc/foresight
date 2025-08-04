package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule
import foresight.util.random.DiscreteDistribution

/**
 * A prioritization that reweights the priorities of matches based on a curve-fitted distribution.
 *
 * @param originalPriorities The original match priorities to be reweighted.
 * @param distribution The discrete distribution used for reweighting the priorities.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
final case class CurveFittedPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT](originalPriorities: MatchPriorities[NodeT, RuleT, MatchT],
                                                                                       distribution: DiscreteDistribution)
    extends ReweightedPriorities[NodeT, RuleT, MatchT] {

  /**
   * Reweights the priorities of the matches.
   *
   * @param matches A sequence of prioritized matches.
   * @return A sequence of prioritized matches with reweighted priorities.
   */
  override def reweight(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Seq[PrioritizedMatch[RuleT, MatchT]] = {
    distribution.prioritiesToProbabilities(matches.map(m => m -> m.priority))
      .map { case (matchInfo, weight) => matchInfo.copy(priority = weight) }
  }
}
