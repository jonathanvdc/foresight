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

  override def apply(egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                     data: Unit,
                     parallelize: ParallelMap): (Option[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]], Unit) = {

    // Find all matches for each rule, then remove the matches that have already been applied.
    val ruleMatchingParallelize = parallelize.child("rule matching")
    val newMatchesByRule = ruleMatchingParallelize(
      rules, { rule: Rule[NodeT, MatchT, EGraphT] => {
        val matches = rule.search(egraph.egraph, ruleMatchingParallelize)
        val oldMatches = egraph.applications(rule.name)
        val newMatches = matches.filterNot(oldMatches.contains)
        rule.name -> newMatches
      } }).toMap

    val ruleApplicationParallelize = parallelize.child("rule application")
    val updateCommands = ruleApplicationParallelize[Rule[NodeT, MatchT, EGraphT], Command[NodeT]](rules, { rule: Rule[NodeT, MatchT, EGraphT] =>
      val newMatches = newMatchesByRule(rule.name)
      rule.delayed(newMatches, egraph.egraph, ruleApplicationParallelize)
    }).toSeq

    // Construct a command that applies the new matches to the e-graph.
    val update = CommandQueue(updateCommands).optimized

    // Apply the new matches to the e-graph.
    val recorded = newMatchesByRule.mapValues(_.toSet)
    val (newEGraph, _) = update(egraph.record(recorded), Map.empty, parallelize)
    (newEGraph, ())
  }
}
