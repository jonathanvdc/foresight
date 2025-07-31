package foresight.eqsat.saturation.prioritization

import foresight.eqsat.rewriting.Rule

/**
 * A trait that defines a prioritization strategy for matches found during rule application.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
trait MatchPrioritizer[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT] {
  /**
   * Prioritizes matches based on some criteria.
   *
   * @param matches A sequence of pairs, each containing a rule and a match.
   * @return A sequence of triples, each containing a rule, a match, and a priority score.
   */
  def prioritize(matches: Seq[(RuleT, MatchT)]): Seq[(RuleT, MatchT, Double)]

  /**
   * Determines the batch size based on the prioritized matches.
   *
   * @param matches A sequence of triples, each containing a rule, a match, and a priority score.
   * @return The number of matches to apply in this batch.
   */
  def batchSize(matches: Seq[(RuleT, MatchT, Double)]): Int
}
