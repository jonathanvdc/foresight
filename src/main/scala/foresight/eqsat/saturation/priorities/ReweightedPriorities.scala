package foresight.eqsat.saturation.priorities

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.rewriting.Rule

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
trait ReweightedPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], MatchT]
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
  def reweight(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Seq[PrioritizedMatch[RuleT, MatchT]]

  override def prioritize(matches: Seq[(RuleT, MatchT)], egraph: EGraphT): Seq[PrioritizedMatch[RuleT, MatchT]] = {
    val prioritized = originalPriorities.prioritize(matches, egraph)
    reweight(prioritized)
  }

  override def batchSize(matches: Seq[PrioritizedMatch[RuleT, MatchT]], egraph: EGraphT): Int = {
    originalPriorities.batchSize(matches, egraph)
  }
}
