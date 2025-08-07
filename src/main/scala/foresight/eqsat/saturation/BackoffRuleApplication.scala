package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, EGraphLike}

import scala.collection.mutable
import scala.util.Random

/**
 * Annotates a rewrite rule with initial scheduling parameters for backoff.
 *
 * The backoff strategy limits how often a rule can fire. Each rule is given an initial match limit
 * (i.e., how many matches it may apply before being paused) and an initial ban length (how long it is
 * paused once exhausted). Both are doubled after each cooldown cycle.
 *
 * @param rule The rewrite rule.
 * @param initialMatchLimit The initial number of matches this rule can apply before being temporarily banned.
 * @param initialBanLength The number of iterations the rule is banned after hitting its match limit.
 */
final case class BackoffRule[NodeT,
                             RuleT <: Rule[NodeT, MatchT, _],
                             MatchT](rule: RuleT, initialMatchLimit: Int, initialBanLength: Int)

/**
 * Tracks dynamic backoff statistics for a single rule.
 *
 * These stats are used to determine whether a rule is active, when it becomes eligible again,
 * and how many more matches it may apply in the current activation window.
 *
 * @param rule The rewrite rule being tracked.
 * @param matchLimit The total number of matches this rule can apply during its current active window.
 * @param banLength The number of iterations the rule will be banned after hitting its match limit.
 * @param bannedUntil If banned, the iteration number at which the rule will become eligible again.
 * @param remainingMatches The number of matches this rule can still apply before triggering a ban.
 */
final case class RuleStats[NodeT,
                           RuleT <: Rule[NodeT, MatchT, _],
                           MatchT](rule: RuleT, matchLimit: Int, banLength: Int, bannedUntil: Option[Int],
                                   remainingMatches: Int)

/**
 * Represents the state of a backoff strategy across iterations.
 *
 * Used as the persistent state (`DataT`) for [[BackoffRuleApplication]], it tracks the number
 * of iterations so far, all rule-specific scheduling state, and the random generator used for sampling.
 *
 * @param iteration The current iteration count.
 * @param stats Per-rule statistics, keyed by rule name.
 * @param random Random number generator used to shuffle matches.
 */
final case class BackoffRuleStats[NodeT,
                                  RuleT <: Rule[NodeT, MatchT, _],
                                  MatchT](iteration: Int,
                                          stats: Map[String, RuleStats[NodeT, RuleT, MatchT]],
                                          random: Random)

/**
 * A saturation strategy that applies rules with a backoff heuristic.
 *
 * Rules are initially allowed to apply a limited number of matches. Once this quota is exhausted,
 * the rule is banned for a fixed number of iterations. After the ban period ends, the rule is re-enabled
 * with both its match limit and ban duration doubled. This encourages frequent early exploration and gradual
 * fading out of overly eager rules.
 *
 * Rules apply up to their remaining match limits each iteration, chosen uniformly at random from the match set.
 *
 * @param rules Rules annotated with initial match limits and ban lengths.
 * @param searchAndApply Strategy for finding and applying rule matches to the e-graph.
 *
 * @tparam NodeT The node type in the e-graph.
 * @tparam RuleT The rewrite rule type.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of rule match.
 */
final case class BackoffRuleApplication[NodeT,
                                        RuleT <: Rule[NodeT, MatchT, _],
                                        EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                        MatchT](
  rules: Seq[BackoffRule[NodeT, RuleT, MatchT]],
  searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT]
) extends Strategy[NodeT, EGraphT, BackoffRuleStats[NodeT, RuleT, MatchT]] {

  override def initialData: BackoffRuleStats[NodeT, RuleT, MatchT] = {
    BackoffRuleStats(
      0,
      rules.map(rule =>
        rule.rule.name ->
          RuleStats[NodeT, RuleT, MatchT](
            rule.rule, rule.initialMatchLimit, rule.initialBanLength, None, rule.initialMatchLimit)).toMap,
      new Random(0)
    )
  }

  override def apply(egraph: EGraphT,
                     data: BackoffRuleStats[NodeT, RuleT, MatchT],
                     parallelize: ParallelMap
  ): (Option[EGraphT], BackoffRuleStats[NodeT, RuleT, MatchT]) = {
    val iteration = data.iteration

    // First, unban previously-banned rules.
    val mutableStats = mutable.Map[String, RuleStats[NodeT, RuleT, MatchT]]() ++= data.stats
    for ((ruleName, ruleStats) <- mutableStats) {
      if (ruleStats.bannedUntil.exists(_ <= iteration)) {
        mutableStats(ruleName) = RuleStats(
          ruleStats.rule,
          ruleStats.matchLimit * 2,
          ruleStats.banLength * 2,
          None,
          ruleStats.matchLimit * 2
        )
      }
    }

    // Filter out rules that are currently banned.
    val notBanned = mutableStats.keys.filter(mutableStats(_).bannedUntil.forall(_ < iteration)).toSeq

    // Search for matches of the rules that are not banned.
    val matches = searchAndApply.search(notBanned.map(mutableStats(_).rule), egraph, parallelize)

    // For each rule, take a random selection of rules to apply, up to the match limit. Decrement remaining match counts
    // on the fly and ban rules that have reached their match limit.
    val selectedMatches = matches.map { case (rule, matches) =>
      val ruleStats = mutableStats(rule)
      val limit = ruleStats.remainingMatches min matches.length
      val remainingMatches = ruleStats.remainingMatches - limit
      if (ruleStats.remainingMatches == 0) {
        mutableStats(rule) = ruleStats.copy(bannedUntil = Some(iteration + ruleStats.banLength))
      } else {
        mutableStats(rule) = ruleStats.copy(remainingMatches = remainingMatches)
      }
      (rule, data.random.shuffle(matches).take(limit))
    }

    // Build strategy data for the next iteration.
    val newData = BackoffRuleStats(
      iteration + 1,
      mutableStats.toMap,
      data.random
    )

    // Apply the selected matches to the e-graph.
    val newEGraph = searchAndApply.apply(notBanned.map(mutableStats(_).rule), selectedMatches, egraph, parallelize)
    (newEGraph, newData)
  }
}
