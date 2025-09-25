package foresight.eqsat.saturation.priorities

import foresight.eqsat.rewriting.Rule
import foresight.eqsat.immutable.{EGraph, EGraphLike}

/**
 * Defines a strategy for prioritizing rule matches during stochastic rule application.
 *
 * This trait is used to guide the selection of which matches to apply in a given iteration
 * of a saturation loop, such as in [[foresight.eqsat.saturation.StochasticRuleApplication]].
 * It assigns a numeric priority to each match and determines how many to apply at once.
 *
 * Typical implementations might prioritize based on match cost, rule type, rule name,
 * or other domain-specific heuristics. This enables the system to explore the search space
 * more effectively or efficiently than by applying all matches or selecting them uniformly at random.
 *
 * @tparam NodeT The type of nodes in the e-graph.
 * @tparam RuleT The type of rewrite rules, which must produce matches of type `MatchT`.
 * @tparam EGraphT The type of e-graph to operate on.
 * @tparam MatchT The type of matches returned by applying a rule.
 */
trait MatchPriorities[NodeT, RuleT <: Rule[NodeT, MatchT, _], EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], MatchT] {
  /**
   * Assigns a priority score to each match, indicating its relative importance or desirability.
   *
   * @param matches A sequence of (rule, match) pairs found during rule search.
   * @param egraph The e-graph in which the matches were found, used for context or additional information.
   * @return A sequence of [[PrioritizedMatch]] instances containing each rule, match, and its computed priority.
   */
  def prioritize(matches: Seq[(RuleT, MatchT)], egraph: EGraphT): Seq[PrioritizedMatch[RuleT, MatchT]]

  /**
   * Determines how many of the prioritized matches to apply in this iteration.
   *
   * This can be constant or depend on the distribution of priorities, the total number of matches,
   * or other criteria such as heuristics or exploration/exploitation trade-offs.
   *
   * @param matches The prioritized matches.
   * @param egraph The e-graph in which the matches were found, used for context or additional information.
   * @return The number of matches to apply.
   */
  def batchSize(matches: Seq[PrioritizedMatch[RuleT, MatchT]], egraph: EGraphT): Int
}
