package foresight.eqsat.saturation.priorities

import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.Rewrite

/**
 * A match prioritization strategy that adjusts the output of an existing [[MatchPriorities]] instance.
 *
 * This trait wraps a base prioritizer (`originalPriorities`) and overrides its behavior by reweighting
 * the computed priority scores. This allows users to apply transformations such as scaling, clipping,
 * normalization, or curve-fitting without reimplementing the original matching logic.
 *
 * Implementations only need to define how to reweight a list of [[PrioritizedMatch]]es;
 * all other behavior (e.g. rule searching, batch sizing) is delegated to the wrapped prioritizer.
 *
 * This is commonly used in stochastic rewrite strategies to modify sampling behavior
 * without altering match discovery or ranking heuristics.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam RuleT The type of rules used to generate matches.
 * @tparam EGraphT The type of e-graph to operate on.
 * @tparam MatchT The type of matches found for each rule.
 */
trait ReweightedPriorities[NodeT, RuleT <: Rewrite[NodeT, MatchT, _], EGraphT <: EGraph[NodeT], MatchT]
  extends MatchPriorities[NodeT, RuleT, EGraphT, MatchT] {

  /**
   * The underlying prioritizer that computes the initial (raw) priority scores.
   */
  def originalPriorities: MatchPriorities[NodeT, RuleT, EGraphT, MatchT]

  /**
   * Reweights the priority scores of an already-prioritized list of matches.
   *
   * The returned sequence must preserve the same matches and rules, but may replace
   * the priority values based on any desired transformation.
   *
   * @param matches A sequence of prioritized matches.
   * @return A sequence of the same matches with updated priority scores.
   */
  def reweight(rule: RuleT, matches: Seq[PrioritizedMatch[MatchT]]): Seq[PrioritizedMatch[MatchT]]

  override def prioritize(rules: Seq[RuleT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphT): Map[String, Seq[PrioritizedMatch[MatchT]]] = {

    val prioritized = originalPriorities.prioritize(rules, matches, egraph)
    prioritized.map { case (ruleName, pmatches) =>
      val rule = rules.find(_.name == ruleName).getOrElse(
        throw new IllegalArgumentException(s"Rule with name $ruleName not found in provided rules.")
      )
      ruleName -> reweight(rule, pmatches)
    }
  }

  override def batchSize(rules: Seq[RuleT], matches: Map[String, Seq[PrioritizedMatch[MatchT]]], egraph: EGraphT): Int = {
    originalPriorities.batchSize(rules, matches, egraph)
  }
}
