package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}

/**
 * A strategy that, for a sequence of rules, applies every match for each rule. This strategy caches the matches that
 * have already been applied, so that they are not applied again. This strategy operates on an
 * [[EGraphWithRecordedApplications]].
 *
 * @param rules The rules to apply.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph.
 */
final case class MaximalRuleApplicationWithCaching[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], MatchT <: PortableMatch[NodeT, MatchT]](rules: Seq[Rule[NodeT, MatchT, EGraphT]])
  extends Strategy[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], Unit] {

  override def initialData: Unit = ()

  private def findNewMatchesByRule(egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                                   parallelize: ParallelMap): Map[String, Seq[MatchT]] = {
    val ruleMatchingParallelize = parallelize.child("rule matching")
    ruleMatchingParallelize(
      rules, { rule: Rule[NodeT, MatchT, EGraphT] =>
        val matches = rule.search(egraph.egraph, ruleMatchingParallelize)
        val oldMatches = egraph.applications(rule.name)
        val newMatches = matches.filterNot(oldMatches.contains)
        rule.name -> newMatches
      }
    ).toMap
  }

  override def apply(egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                     data: Unit,
                     parallelize: ParallelMap): (Option[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]], Unit) = {

    val newEGraph = SearchAndApply.withCaching.apply(
      rules,
      SearchAndApply.withCaching.search(rules, egraph, parallelize),
      egraph,
      parallelize)

    (newEGraph, ())
  }
}
