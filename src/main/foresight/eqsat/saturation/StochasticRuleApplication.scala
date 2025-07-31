package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.saturation.priorities.MatchPrioritizer
import foresight.eqsat.util.RandomSampling

import scala.util.Random

/**
 * A strategy that applies a sequence of rules in a stochastic manner. It searches for matches of the rules in an e-graph,
 * prioritizes them based on a given function, and applies a batch of matches selected randomly, weighing the selection
 * by their priority.
 *
 * @param rules The rules to apply.
 * @param searchAndApply The search and apply strategy to find and apply matches.
 * @param prioritizer The prioritizer that determines the priority of matches and the batch size to apply.
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
          searchAndApply: SearchAndApply[RuleT, EGraphT, MatchT],
          prioritizer: MatchPrioritizer[NodeT, RuleT, MatchT],
          random: Random) extends Strategy[EGraphT, Unit] {

  /**
   * A map from rule names to the rules themselves. This is used to quickly look up a rule by its name.
   */
  private val rulesByName = rules.map(rule => rule.name -> rule).toMap

  override def initialData: Unit = ()

  override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
    val matches = searchAndApply.search(rules, egraph, parallelize).toSeq.flatMap {
      case (ruleName, matches) =>
        val rule = rulesByName(ruleName)
        matches.map(m => (rule, m))
    }

    val prioritizedMatches = prioritizer.prioritize(matches)
    val batchSize = prioritizer.batchSize(prioritizedMatches)

    val selectedMatches = selectMatches(prioritizedMatches, batchSize)
    val selectedByRule = selectedMatches.groupBy { case (r, _) => r.name }
    val groupedSelectedMatches = rules.map { rule =>
      rule.name -> selectedByRule.getOrElse(rule.name, Seq.empty).map(_._2)
    }.toMap

    val newEGraph = searchAndApply.apply(
      rules,
      groupedSelectedMatches,
      egraph,
      parallelize)

    (newEGraph, ())
  }

  private def selectMatches(
    prioritizedMatches: Seq[(RuleT, MatchT, Double)],
    batchSize: Int
  ): Seq[(RuleT, MatchT)] = {
    RandomSampling.sampleWithoutReplacement(
      prioritizedMatches.map { case (rule, matchT, priority) => ((rule, matchT), priority) },
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
   * @param prioritizer The prioritizer that determines the priority of matches and the batch size to apply.
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
    searchAndApply: SearchAndApply[RuleT, EGraphT, MatchT],
    prioritizer: MatchPrioritizer[NodeT, RuleT, MatchT]
  ): StochasticRuleApplication[NodeT, RuleT, EGraphT, MatchT] = {
    new StochasticRuleApplication(rules, searchAndApply, prioritizer, new Random(0))
  }

  /**
   * Creates a new instance of [[StochasticRuleApplication]] with the given rules and a default search and apply strategy
   * that does not cache results, along with a prioritizer.
   *
   * @param rules The rules to apply.
   * @param prioritizer The prioritizer that determines the priority of matches and the batch size to apply.
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
    prioritizer: MatchPrioritizer[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT],
    random: Random
  ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, SearchAndApply.withoutCaching[NodeT, EGraphT, MatchT], prioritizer)
  }

  /**
   * Creates a new instance of [[StochasticRuleApplication]] with the given rules and a default search and apply strategy
   * that does not cache results, along with a prioritizer. Uses a default random number generator with seed 0.
   *
   * @param rules The rules to apply.
   * @param prioritizer The prioritizer that determines the priority of matches and the batch size to apply.
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
    prioritizer: MatchPrioritizer[NodeT, Rule[NodeT, MatchT, EGraphT], MatchT]
  ): StochasticRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    apply(rules, prioritizer, new Random(0))
  }
}
