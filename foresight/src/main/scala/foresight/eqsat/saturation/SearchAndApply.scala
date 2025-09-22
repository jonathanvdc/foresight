package foresight.eqsat.saturation

import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}
import foresight.eqsat.{EGraph, EGraphLike}
import foresight.util.collections.StrictMapOps.toStrictMapOps

/**
 * A strategy that searches for matches of a set of rules in an e-graph and applies them.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to search and apply.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
trait SearchAndApply[NodeT, RuleT <: Rule[NodeT, MatchT, _], EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], MatchT] {
  /**
   * Searches for matches of the given rules in the e-graph.
   *
   * @param rules The rules to search for matches.
   * @param egraph The e-graph to search in.
   * @param parallelize A parallelization strategy for searching.
   * @return A map from rule names to sequences of matches found for each rule.
   */
  def search(rules: Seq[RuleT],
             egraph: EGraphT,
             parallelize: ParallelMap): Map[String, Seq[MatchT]]

  /**
   * Applies the matches found for the given rules to the e-graph.
   * @param rules The rules whose matches are to be applied.
   * @param matches A map from rule names to sequences of matches found for each rule.
   * @param egraph The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @return An updated e-graph with the matches applied, or None if no matches were applied.
   */
  def apply(rules: Seq[RuleT],
            matches: Map[String, Seq[MatchT]],
            egraph: EGraphT,
            parallelize: ParallelMap): Option[EGraphT]

  /**
   * A convenience method that combines searching and applying in one step.
   * It first searches for matches of the given rules in the e-graph; then it
   * applies them to the e-graph.
   * @param rules The rules to search and apply.
   * @param egraph The e-graph to search and apply matches to.
   * @param parallelize A parallelization strategy for both searching and applying.
   * @return An updated e-graph with the matches applied, or None if no matches were found.
   */
  def apply(rules: Seq[RuleT],
            egraph: EGraphT,
            parallelize: ParallelMap): Option[EGraphT] = {
    val matches = search(rules, egraph, parallelize)
    apply(rules, matches, egraph, parallelize)
  }
}

/**
 * A companion object for the [[SearchAndApply]] trait that provides a factory method for creating a
 * [[SearchAndApply]] instance with caching capabilities.
 */
object SearchAndApply {
  /**
   * Creates a [[SearchAndApply]] instance that does not cache matches.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A [[SearchAndApply]] instance that does not cache matches.
   */
  def withoutCaching[NodeT,
                     EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                     MatchT]: SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    new SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] {
      override def search(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                          egraph: EGraphT,
                          parallelize: ParallelMap): Map[String, Seq[MatchT]] = {
        val ruleMatchingParallelize = parallelize.child("rule matching")
        ruleMatchingParallelize(
          rules, (rule: Rule[NodeT, MatchT, EGraphT]) => {
            rule.name -> rule.search(egraph, ruleMatchingParallelize)
          }
        ).toMap
      }

      override def apply(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                         matches: Map[String, Seq[MatchT]],
                         egraph: EGraphT,
                         parallelize: ParallelMap): Option[EGraphT] = {
        val ruleApplicationParallelize = parallelize.child("rule application")
        val updateCommands = ruleApplicationParallelize[Rule[NodeT, MatchT, EGraphT], Command[NodeT]](rules, (rule: Rule[NodeT, MatchT, EGraphT]) => {
          val newMatches = matches(rule.name)
          rule.delayed(newMatches, egraph, ruleApplicationParallelize)
        }).toSeq

        // Construct a command that applies the new matches to the e-graph.
        val update = CommandQueue(updateCommands).optimized

        // Apply the new matches to the e-graph.
        val (newEGraph, _) = update(egraph, Map.empty, parallelize)
        newEGraph
      }
    }
  }

  /**
   * Creates a [[SearchAndApply]] instance that caches the matches that have already been applied to the e-graph.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A [[SearchAndApply]] instance that caches matches.
   */
  def withCaching[NodeT,
                  EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                  MatchT <: PortableMatch[NodeT, MatchT]]: SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    new SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] {
      override def search(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Map[String, Seq[MatchT]] = {
        val ruleMatchingParallelize = parallelize.child("rule matching")
        ruleMatchingParallelize(
          rules, (rule: Rule[NodeT, MatchT, EGraphT]) => {
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
        val updateCommands = ruleApplicationParallelize[Rule[NodeT, MatchT, EGraphT], Command[NodeT]](rules, (rule: Rule[NodeT, MatchT, EGraphT]) => {
          val newMatches = matches(rule.name)
          rule.delayed(newMatches, egraph.egraph, ruleApplicationParallelize)
        }).toSeq

        // Construct a command that applies the new matches to the e-graph.
        val update = CommandQueue(updateCommands).optimized

        // Apply the new matches to the e-graph.
        val recorded = matches.mapValuesStrict(_.toSet)
        val (newEGraph, _) = update(egraph.record(recorded), Map.empty, parallelize)
        newEGraph
      }
    }
  }
}
