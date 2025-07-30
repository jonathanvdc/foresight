package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule

import scala.util.Random

final case class StochasticRuleApplication[
  NodeT,
  RuleT <: Rule[NodeT, MatchT, _],
  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
  MatchT,
  Priority](rules: Seq[RuleT],
            searchAndApply: SearchAndApply[RuleT, EGraphT, MatchT],
            prioritize: Seq[(RuleT, MatchT)] => Seq[(RuleT, MatchT, Priority)],
            batchSize: Seq[(RuleT, MatchT, Priority)] => Int,
            random: Random = new Random(0)) extends Strategy[EGraphT, Unit] {

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

    val prioritizedMatches = prioritize(matches)
    val matchesToApply = batchSize(prioritizedMatches)

    val selectedMatches = selectMatches(prioritizedMatches, matchesToApply)
    val groupedSelectedMatches = selectedMatches.groupBy(_._1.name).map {
      case (ruleName, matches) => ruleName -> matches.map(_._2)
    }

    val newEGraph = searchAndApply.apply(
      rules,
      groupedSelectedMatches,
      egraph,
      parallelize)

    (newEGraph, ())
  }

  private def selectMatches(
    prioritizedMatches: Seq[(RuleT, MatchT, Priority)],
    batchSize: Int
  ): Seq[(RuleT, MatchT)] = {
    if (prioritizedMatches.isEmpty) return Seq.empty

    ???
  }
}
