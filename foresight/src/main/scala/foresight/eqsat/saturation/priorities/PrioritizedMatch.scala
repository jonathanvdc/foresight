package foresight.eqsat.saturation.priorities

/**
 * Represents a rule match annotated with a priority score.
 *
 * This class is used in saturation strategies (e.g., [[foresight.eqsat.saturation.StochasticRuleApplication]])
 * to pair a rule and its associated match with a numeric score that reflects its importance or desirability.
 * The priority score typically guides probabilistic selection, such as weighted sampling or batch filtering.
 *
 * @param rule The rewrite rule that produced the match.
 * @param ruleMatch The match found by applying the rule.
 * @param priority A non-negative score indicating the importance of this match (higher means more likely to be chosen).
 *
 * @tparam RuleT The type of the rule (often a subclass of [[foresight.eqsat.rewriting.Rule]]).
 * @tparam MatchT The type of the match returned by the rule's search.
 */
final case class PrioritizedMatch[RuleT, MatchT](rule: RuleT,
                                                 ruleMatch: MatchT,
                                                 priority: Double)
