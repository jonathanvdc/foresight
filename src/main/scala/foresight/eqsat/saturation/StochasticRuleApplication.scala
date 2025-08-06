package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.saturation.priorities.{MatchPriorities, PrioritizedMatch}
import foresight.util.random.{Random, Sample}

/**
 * A strategy that applies a sequence of rules in a stochastic manner. It searches for matches of the rules in an e-graph,
 * prioritizes them based on a given function, and applies a batch of matches selected randomly, weighing the selection
 * by their priority.
 *
 * @param rules The rules to apply.
 * @param searchAndApply The search and apply strategy to find and apply matches.
 * @param priorities The prioritizer that determines the priority of matches and the batch size to apply.
 * @param random A random number generator used for selecting matches randomly.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to apply.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
final case class StochasticRuleApplication[
  NodeT,
  RuleT <: Rule[NodeT, MatchT, _],
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
  MatchT](rules: Seq[RuleT],
          searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT],
          priorities: MatchPriorities[NodeT, RuleT, MatchT],
          random: Random) extends Strategy[NodeT, EGraphT, Random] {

  /**
   * A map from rule names to the rules themselves. This is used to quickly look up a rule by its name.
   */
  private val rulesByName = rules.map(rule => rule.name -> rule).toMap

  override def initialData: Random = random

  override def apply(egraph: EGraphT, data: Random, parallelize: ParallelMap): (Option[EGraphT], Random) = {
    val matches = searchAndApply.search(rules, egraph, parallelize).toSeq.flatMap {
      case (ruleName, matches) =>
        val rule = rulesByName(ruleName)
        matches.map(m => (rule, m))
    }

    val prioritizedMatches = priorities.prioritize(matches)
    val batchSize = priorities.batchSize(prioritizedMatches)

    val (selectedMatches, newRng) = selectMatches(prioritizedMatches, batchSize)
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

  private def selectMatches(
    prioritizedMatches: Seq[PrioritizedMatch[RuleT, MatchT]],
    batchSize: Int
  ): (Seq[(RuleT, MatchT)], Random) = {
    Sample.withoutReplacement(
      prioritizedMatches.map { case PrioritizedMatch(rule, matchT, priority) => ((rule, matchT), priority) },
      batchSize,
      random)
  }
}

/**
 * Companion object for [[StochasticRuleApplication]] that provides factory methods to create instances.
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
    RuleT <: Rule[NodeT, MatchT, _],
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ](
    rules: Seq[RuleT],
    searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT],
    priorities: MatchPriorities[NodeT, RuleT, MatchT]
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
     rules: Seq[Rule[NodeT, MatchT, EGraphT]],
     priorities: MatchPriorities[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT],
     random: Random
  ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, SearchAndApply.withoutCaching[NodeT, EGraphT, MatchT], priorities)
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
    rules: Seq[Rule[NodeT, MatchT, EGraphT]],
    priorities: MatchPriorities[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT]
  ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, priorities, Random(0))
  }
}
