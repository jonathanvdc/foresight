package foresight.eqsat.saturation

import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}
import foresight.eqsat.{EGraph, EGraphLike}

trait SearchAndApply[RuleT <: Rule[_, MatchT, _], EGraphT <: EGraphLike[_, EGraphT] with EGraph[_], MatchT] {
  def search(rules: Seq[RuleT],
             egraph: EGraphT,
             parallelize: ParallelMap): Map[String, Seq[MatchT]]

  def apply(rules: Seq[RuleT],
            matches: Map[String, Seq[MatchT]],
            egraph: EGraphT,
            parallelize: ParallelMap): Option[EGraphT]
}

object SearchAndApply {
  def withCaching[NodeT,
                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                  MatchT <: PortableMatch[NodeT, MatchT]]: SearchAndApply[Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    new SearchAndApply[Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] {
      override def search(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Map[String, Seq[MatchT]] = {
        val ruleMatchingParallelize = parallelize.child("rule matching")
        ruleMatchingParallelize(
          rules, { rule: Rule[_, MatchT, EGraphT] =>
            val matches = rule.search(egraph.egraph, ruleMatchingParallelize)
            val oldMatches = egraph.applications(rule.name)
            val newMatches = matches.filterNot(oldMatches.contains)
            rule.name -> newMatches
          }
        ).toMap
      }

      override def apply(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                         matches: Map[String, Seq[MatchT]],
                         egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                         parallelize: ParallelMap): Option[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]] = {
        val ruleApplicationParallelize = parallelize.child("rule application")
        val updateCommands = ruleApplicationParallelize[Rule[NodeT, MatchT, EGraphT], Command[NodeT]](rules, { rule: Rule[NodeT, MatchT, EGraphT] =>
          val newMatches = matches(rule.name)
          rule.delayed(newMatches, egraph.egraph, ruleApplicationParallelize)
        }).toSeq

        // Construct a command that applies the new matches to the e-graph.
        val update = CommandQueue(updateCommands).optimized

        // Apply the new matches to the e-graph.
        val recorded = matches.mapValues(_.toSet)
        val (newEGraph, _) = update(egraph.record(recorded), Map.empty, parallelize)
        newEGraph
      }
    }
  }
}
