package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule

/**
 * A simple prioritization strategy that assigns equal weight to all rule matches.
 *
 * Each match receives a uniform priority score of `1.0`, meaning no preference is given
 * to any rule or match based on cost, structure, or context. This is equivalent to uniform
 * random sampling when used with a stochastic strategy.
 *
 * This strategy is useful as a baseline, or in situations where no domain-specific
 * prioritization is available or desired. It ensures fairness among matches while
 * limiting batch size to control saturation performance.
 *
 * @param maxBatchSize The maximum number of matches to select and apply in one iteration.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam RuleT The type of rewrite rules.
 * @tparam MatchT The type of matches returned by each rule.
 */
final case class UniformPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT](maxBatchSize: Int)
    extends MatchPriorities[NodeT, RuleT, MatchT] {

  override def prioritize(matches: Seq[(RuleT, MatchT)]): Seq[PrioritizedMatch[RuleT, MatchT]] = {
    matches.map { case (rule, match_) => PrioritizedMatch(rule, match_, 1.0) }
  }

  override def batchSize(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Int = {
    math.min(maxBatchSize, matches.size)
  }
}
