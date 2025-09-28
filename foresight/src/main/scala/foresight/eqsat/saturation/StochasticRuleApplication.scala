package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rewrite
import foresight.eqsat.saturation.priorities.{MatchPriorities, PrioritizedMatch}
import foresight.eqsat.immutable.{EGraph, EGraphLike}
import foresight.util.random.{Random, Sample}

/**
 * A saturation strategy that applies rewrite rules stochastically, based on user-defined priorities.
 *
 * This strategy repeatedly:
 *   1. Searches for matches of the given rewrite rules in the e-graph.
 *   2. Assigns priorities to each match using the provided [[foresight.eqsat.saturation.priorities.MatchPriorities]].
 *   3. Selects a batch of matches to apply, using weighted random sampling without replacement.
 *   4. Applies the selected matches using a configurable [[SearchAndApply]] strategy.
 *
 * This approach is useful when it's expensive or unnecessary to apply every match,
 * or when exploration is guided by domain-specific heuristics (e.g., rule priority, match cost).
 *
 * @example
 * To use this strategy without caching, you can create an instance like this:
 * {{{
 * val strategy = StochasticRuleApplication(
 *   rules = myRules,
 *   searchAndApply = SearchAndApply.withoutCaching,
 *   priorities = MyCustomPriorities,
 *   random = Random(42)
 * )
 * }}}
 *
 * @param rules The set of rewrite rules to apply.
 * @param searchAndApply A search and apply strategy to find and apply rule matches.
 * @param priorities A prioritization mechanism for matches, including batch size selection.
 * @param random A random number generator used to sample matches based on priority.
 *
 * @tparam NodeT The type of e-graph nodes.
 * @tparam RuleT The type of rewrite rules.
 * @tparam EGraphT The type of e-graph, combining [[EGraphLike]] and [[EGraph]].
 * @tparam MatchT The type of matches returned by rules.
 */
final case class StochasticRuleApplication[
  NodeT,
  RuleT <: Rewrite[NodeT, MatchT, _],
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
  MatchT](rules: Seq[RuleT],
          searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT],
          priorities: MatchPriorities[NodeT, RuleT, EGraphT, MatchT],
          random: Random) extends Strategy[EGraphT, Random] {

  /**
   * A lookup table from rule names to rule instances, used during match annotation and application.
   */
  private val rulesByName = rules.map(rule => rule.name -> rule).toMap

  override def initialData: Random = random

  override def apply(egraph: EGraphT, data: Random, parallelize: ParallelMap): (Option[EGraphT], Random) = {
    val matches = searchAndApply.search(rules, egraph, parallelize)

    val prioritizedMatches = priorities.prioritize(rules, matches, egraph)
    val batchSize = priorities.batchSize(rules, prioritizedMatches, egraph)

    val (selectedMatches, newRng) = selectMatches(
      prioritizedMatches.toSeq.flatMap { case (ruleName, pms) =>
        val rule = rulesByName(ruleName)
        pms.map(pm => (rule, pm))
      },
      batchSize)
    val selectedByRule = selectedMatches.groupBy { case (r, _) => r.name }
    val groupedSelectedMatches = rules.map { rule =>
      rule.name -> selectedByRule.getOrElse(rule.name, Seq.empty).map(_._2)
    }.toMap

    val newEGraph = searchAndApply.apply(
      rules,
      groupedSelectedMatches,
      egraph,
      parallelize)

    (newEGraph, newRng)
  }

  /**
   * Selects a batch of matches without replacement, using priority as selection weight.
   *
   * @param prioritizedMatches The list of prioritized matches.
   * @param batchSize The number of matches to select.
   * @return A tuple of selected (rule, match) pairs and the updated RNG state.
   */
  private def selectMatches(
    prioritizedMatches: Seq[(RuleT, PrioritizedMatch[MatchT])],
    batchSize: Int
  ): (Seq[(RuleT, MatchT)], Random) = {
    Sample.withoutReplacement(
      prioritizedMatches.map { case (rule, PrioritizedMatch(matchT, priority)) => ((rule, matchT), priority) },
      batchSize,
      random)
  }
}

/**
 * Factory methods for creating [[StochasticRuleApplication]] strategies.
 *
 * These overloaded `apply` methods allow users to omit explicit construction of
 * [[SearchAndApply]] or [[foresight.util.random.Random]] instances if defaults are acceptable.
 */
object StochasticRuleApplication {
  /**
   * Creates a new instance of [[StochasticRuleApplication]] with the given rules, search and apply strategy, and
   * prioritizer.
   *
   * @param rules The rules to apply.
   * @param searchAndApply The search and apply strategy to find and apply matches.
   * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam RuleT The type of the rules to apply.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   */
  def apply[
    NodeT,
    RuleT <: Rewrite[NodeT, MatchT, _],
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ](
    rules: Seq[RuleT],
    searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT],
    priorities: MatchPriorities[NodeT, RuleT, EGraphT, MatchT]
  ): StochasticRuleApplication[NodeT, RuleT, EGraphT, MatchT] = {
    new StochasticRuleApplication(rules, searchAndApply, priorities, Random(0))
  }

  /**
   * Creates a new instance of [[StochasticRuleApplication]] with the given rules and a default search and apply strategy
   * that does not cache results, along with a prioritizer.
   *
   * @param rules The rules to apply.
   * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
   * @param random A random number generator used for selecting matches randomly.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   */
  def apply[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ](
     rules: Seq[Rewrite[NodeT, MatchT, EGraphT]],
     priorities: MatchPriorities[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT],
     random: Random
  ): StochasticRuleApplication[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, SearchAndApply.immutable[NodeT, EGraphT, MatchT], priorities)
  }

  /**
   * Creates a new instance of [[StochasticRuleApplication]] with the given rules and a default search and apply strategy
   * that does not cache results, along with a prioritizer. Uses a default random number generator with seed 0.
   *
   * @param rules The rules to apply.
   * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   */
  def apply[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ](
    rules: Seq[Rewrite[NodeT, MatchT, EGraphT]],
    priorities: MatchPriorities[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT]
  ): StochasticRuleApplication[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, priorities, Random(0))
  }
}
