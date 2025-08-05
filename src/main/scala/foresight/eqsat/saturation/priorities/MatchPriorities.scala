package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule

/**
 * A trait that defines a prioritization for matches found during rule application.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
trait MatchPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT] {
  /**
   * Prioritizes matches based on some criteria.
   *
   * @param matches A sequence of pairs, each containing a rule and a match.
   * @return A sequence of triples, each containing a rule, a match, and a priority score.
   */
  def prioritize(matches: Seq[(RuleT, MatchT)]): Seq[PrioritizedMatch[RuleT, MatchT]]

  /**
   * Determines the batch size based on the prioritized matches.
   *
   * @param matches A sequence of triples, each containing a rule, a match, and a priority score.
   * @return The number of matches to apply in this batch.
   */
  def batchSize(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Int
}
