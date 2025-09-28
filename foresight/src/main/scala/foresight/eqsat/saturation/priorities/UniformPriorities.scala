package foresight.eqsat.saturation.priorities

import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.Rewrite

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
 * @tparam EGraphT The type of e-graph to operate on.
 * @tparam MatchT The type of matches returned by each rule.
 */
final case class UniformPriorities[NodeT, -RuleT <: Rewrite[NodeT, MatchT, _], -EGraphT <: EGraph[NodeT], MatchT](maxBatchSize: Int)
    extends MatchPriorities[NodeT, RuleT, EGraphT, MatchT] {

  override def prioritize(rules: Seq[RuleT], matches: Map[String, Seq[MatchT]], egraph: EGraphT): Map[String, Seq[PrioritizedMatch[MatchT]]] = {
    matches.map { case (ruleName, ms) =>
      ruleName -> ms.map(m => PrioritizedMatch(m, 1.0))
    }
  }

  override def batchSize(rules: Seq[RuleT], matches: Map[String, Seq[PrioritizedMatch[MatchT]]], egraph: EGraphT): Int = {
    math.min(maxBatchSize, matches.values.map(_.size).sum)
  }
}
