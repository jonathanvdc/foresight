package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule

/**
 * A prioritization that reweights the priorities of matches as originally weighted by another [[MatchPriorities]]
 * instance.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
trait ReweightedPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], MatchT] extends MatchPriorities[NodeT, RuleT, MatchT] {
  /**
   * The original priorities that this reweighted prioritization is based on.
   */
  def originalPriorities: MatchPriorities[NodeT, RuleT, MatchT]

  /**
   * Reweights the priorities of the matches.
   *
   * @param matches A sequence of prioritized matches.
   * @return A sequence of prioritized matches with reweighted priorities.
   */
  def reweight(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Seq[PrioritizedMatch[RuleT, MatchT]]

  override def prioritize(matches: Seq[(RuleT, MatchT)]): Seq[PrioritizedMatch[RuleT, MatchT]] = {
    val prioritized = originalPriorities.prioritize(matches)
    reweight(prioritized)
  }

  override def batchSize(matches: Seq[PrioritizedMatch[RuleT, MatchT]]): Int = {
    originalPriorities.batchSize(matches)
  }
}
