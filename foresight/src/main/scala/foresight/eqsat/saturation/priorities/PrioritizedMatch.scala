package foresight.eqsat.saturation.priorities

/**
 * Represents a rule match annotated with a priority score.
 *
 * This class is used in saturation strategies (e.g., [[foresight.eqsat.saturation.StochasticRuleApplication]])
 * to pair a rule match with a numeric score that reflects its importance or desirability.
 * The priority score typically guides probabilistic selection, such as weighted sampling or batch filtering.
 *
 * @param ruleMatch The match found by applying the rule.
 * @param priority A non-negative score indicating the importance of this match (higher means more likely to be chosen).
 *
 * @tparam MatchT The type of the match returned by the rule's search.
 */
final case class PrioritizedMatch[+MatchT](ruleMatch: MatchT,
                                           priority: Double)
