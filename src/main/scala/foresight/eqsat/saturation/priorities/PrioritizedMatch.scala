package foresight.eqsat.saturation.priorities

/**
 * A case class representing a prioritized match.
 *
 * @param rule      The rule associated with the match.
 * @param ruleMatch The match found by applying the rule.
 * @param priority  The priority score assigned to this match.
 * @tparam RuleT  The type of the rule.
 * @tparam MatchT The type of the match.
 */
final case class PrioritizedMatch[RuleT, MatchT](rule: RuleT, ruleMatch: MatchT, priority: Double)
