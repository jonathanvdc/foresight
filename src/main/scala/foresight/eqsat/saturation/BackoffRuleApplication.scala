package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, EGraphLike}

import scala.collection.mutable
import scala.util.Random

/**
 * A rewrite rule annotated with a match limit and a ban length. The match limit is the maximum number of matches that
 * can be applied for this rule. Once this limit is reached, the rule is banned for a number of iterations equal to the
 * ban length. After the ban length has passed, the rule can be applied again. At this point both the match limit and
 * the ban length are doubled.
 *
 * @param rule The rewrite rule.
 * @param initialMatchLimit The initial match limit.
 * @param initialBanLength The initial ban length.
 */
final case class BackoffRule[NodeT,
                             RuleT <: Rule[NodeT, MatchT, _],
                             MatchT](rule: RuleT, initialMatchLimit: Int, initialBanLength: Int)

/**
 * Statistics for a rule in the backoff strategy. This includes the rule itself, the match limit, the ban length,
 * the time until which the rule is banned, and the number of remaining matches that can be applied before the rule
 * is banned.
 * @param rule The rewrite rule.
 * @param matchLimit The maximum number of matches that can be applied for this rule before it is banned.
 * @param banLength The number of iterations for which the rule is banned once its match limit is reached.
 * @param bannedUntil The iteration until which the rule is banned, if it is currently banned.
 * @param remainingMatches The number of matches that can still be applied for this rule before it is banned.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
final case class RuleStats[NodeT,
                           RuleT <: Rule[NodeT, MatchT, _],
                           MatchT](rule: RuleT, matchLimit: Int, banLength: Int, bannedUntil: Option[Int],
                                   remainingMatches: Int)

/**
 * Statistics for the backoff rule application strategy. This includes the current iteration, a map of rule names
 * to their statistics, and a random number generator used for selecting matches randomly.
 * @param iteration The current iteration of the backoff strategy.
 * @param stats A map from rule names to their statistics.
 * @param random A random number generator used for selecting matches randomly.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rule.
 * @tparam MatchT The type of the matches produced by the rule.
 */
final case class BackoffRuleStats[NodeT,
                                  RuleT <: Rule[NodeT, MatchT, _],
                                  MatchT](iteration: Int,
                                          stats: Map[String, RuleStats[NodeT, RuleT, MatchT]],
                                          random: Random)

/**
 * A strategy that applies a sequence of rules with backoff. Each rule has a match limit and a ban length. Once the
 * match limit is reached, the rule is banned for a number of iterations equal to the ban length. After the ban
 * length has passed, the rule can be applied again, at which point both the match limit and the ban length are
 * doubled.
 * @param rules The sequence of rules to apply, each with its own match limit and ban length.
 * @param searchAndApply The search and apply strategy that finds matches of the rules and applies them to the e-graph.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to apply, which must extend the `Rule` trait.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
final case class BackoffRuleApplication[NodeT,
                                        RuleT <: Rule[NodeT, MatchT, _],
                                        EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                        MatchT](
  rules: Seq[BackoffRule[NodeT, RuleT, MatchT]],
  searchAndApply: SearchAndApply[RuleT, EGraphT, MatchT]
) extends Strategy[EGraphT, BackoffRuleStats[NodeT, RuleT, MatchT]] {

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
