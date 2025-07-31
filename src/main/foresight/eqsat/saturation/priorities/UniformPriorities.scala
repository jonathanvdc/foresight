package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule

/**
 * A prioritization that assigns a uniform priority to all matches.
 *
 * @param maxBatchSize The maximum number of matches to process in a single batch.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
final case class UniformPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT](maxBatchSize: Int)
    extends MatchPriorities[NodeT, RuleT, MatchT] {

  override def prioritize(matches: Seq[(RuleT, MatchT)]): Seq[(RuleT, MatchT, Double)] = {
    matches.map { case (rule, match_) => (rule, match_, 1.0) }
  }

  override def batchSize(matches: Seq[(RuleT, MatchT, Double)]): Int = {
    math.min(maxBatchSize, matches.size)
  }
}
