package foresight.eqsat.saturation.priorities

import foresight.eqsat.ReadOnlyEGraph
import foresight.eqsat.rewriting.Rewrite
import foresight.util.random.DiscreteDistribution

/**
 * A match prioritization strategy that adjusts (reweights) priority scores using a fitted probability distribution.
 *
 * This class wraps an existing [[MatchPriorities]] instance and transforms its output priorities
 * via a user-supplied [[foresight.util.random.DiscreteDistribution]]. The distribution defines how to rescale or reshape
 * the priority values, for example to emphasize rare or high-value matches more strongly.
 *
 * This is useful in stochastic rule application settings where the raw priorities may not reflect
 * the desired sampling behavior—for instance, when encouraging exploration, controlling match diversity,
 * or enforcing soft caps on overly dominant matches.
 *
 * @example
 * To apply a geometric decay to the top-N matches:
 * {{{
 * val basePriorities = MyDomainPriorities
 * val decay = ShiftedGeometricDistribution(0.5)
 * val fitted = CurveFittedPriorities(basePriorities, decay)
 * }}}
 *
 * @param originalPriorities The base prioritization whose output will be reweighted.
 * @param distribution A discrete probability distribution used to convert raw priorities into normalized weights.
 *
 * @tparam NodeT The type of e-graph nodes.
 * @tparam RuleT The type of rules being applied.
 * @tparam EGraphT The type of e-graph to operate on.
 * @tparam MatchT The type of matches returned by the rules.
 */
final case class CurveFittedPriorities[
  NodeT,
  RuleT <: Rewrite[NodeT, MatchT, _],
  EGraphT <: ReadOnlyEGraph[NodeT],
  MatchT
](originalPriorities: MatchPriorities[NodeT, RuleT, EGraphT, MatchT],
  distribution: DiscreteDistribution)
    extends ReweightedPriorities[NodeT, RuleT, EGraphT, MatchT] {

  /**
   * Applies the distribution to reweight the priorities of previously scored matches.
   *
   * The order of matches is preserved, and the distribution is applied positionally
   * to the priority values (e.g., higher-ranked matches may receive lower weights
   * depending on the shape of the distribution).
   *
   * @param rule The rule associated with the matches being reweighted.
   * @param matches A sequence of matches with initial priority scores.
   * @return The same matches, reweighted using the provided distribution.
   */
  override def reweight(rule: RuleT, matches: Seq[PrioritizedMatch[MatchT]]): Seq[PrioritizedMatch[MatchT]] = {
    distribution.prioritiesToProbabilities(matches.map(m => m -> m.priority))
      .map { case (matchInfo, weight) => matchInfo.copy(priority = weight) }
  }
}
