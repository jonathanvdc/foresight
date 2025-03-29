package fixpoint.eqsat.saturation

import fixpoint.eqsat.{EGraph, EGraphLike}
import fixpoint.eqsat.commands.CommandQueue
import fixpoint.eqsat.parallel.ParallelMap
import fixpoint.eqsat.rewriting.{PortableMatch, Rule}

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
    val newMatches = rules.map { rule =>
      val matches = rule.search(egraph.egraph, parallelize)
      val oldMatches = egraph.applications(rule.name)
      val newMatches = matches.filterNot(oldMatches.contains)
      (rule.name, newMatches.toSet, rule.delayed(newMatches, egraph.egraph, parallelize))
    }

    // Construct a command that applies the new matches to the e-graph.
    val update = CommandQueue(newMatches.map(_._3)).optimized

    // Apply the new matches to the e-graph.
    val recorded = newMatches.map { case (ruleName, newMatches, _) => ruleName -> newMatches }.toMap
    val (newEGraph, _) = update(egraph.record(recorded), Map.empty)
    (newEGraph, ())
  }
}
