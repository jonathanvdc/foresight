package foresight.eqsat.saturation

import foresight.eqsat.ReadOnlyEGraph
import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithRecordedApplications}
import foresight.eqsat.mutable.{EGraph => MutableEGraph}
import foresight.eqsat.mutable.FreezableEGraph
import foresight.util.collections.StrictMapOps.toStrictMapOps

/**
 * A strategy that searches for matches of a set of rules in an e-graph and applies them.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to search and apply.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
trait SearchAndApply[NodeT, RuleT <: Rule[NodeT, MatchT, _], EGraphT <: ReadOnlyEGraph[NodeT], MatchT] {
  /**
   * Searches for matches of the given rule in the e-graph.
   *
   * @param rule The rule to search for matches.
   * @param egraph The e-graph to search in.
   * @param parallelize A parallelization strategy for searching.
   * @return A sequence of matches found for the rule.
   */
  def search(rule: RuleT,
             egraph: EGraphT,
             parallelize: ParallelMap): Seq[MatchT]

  /**
   * Produces a command that applies the given matches of the rule to the e-graph.
   *
   * @param rule        The rule whose matches are to be applied.
   * @param matches     The matches to be applied.
   * @param egraph      The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @return A command that applies the matches to the e-graph.
   */
  def delayed(rule: RuleT,
              matches: Seq[MatchT],
              egraph: EGraphT,
              parallelize: ParallelMap): Command[NodeT]

  /**
   * Applies the given command to the e-graph.
   * @param command The command to apply.
   * @param matches A map from rule names to sequences of matches found for each rule. This is provided for
   *                commands that may need to know which matches were found for which rules (e.g., for caching).
   * @param egraph The e-graph to which the command is applied. Depending on the implementation, this may be a mutable
   *               or immutable e-graph. If it is mutable, the command may modify it in place. If it is immutable,
   *               the command will return a new e-graph if it makes any changes.
   * @param parallelize A parallelization strategy for applying the command.
   * @return An updated e-graph with the command applied, or None if the command made no changes.
   */
  def update(command: Command[NodeT],
             matches: Map[String, Seq[MatchT]],
             egraph: EGraphT,
             parallelize: ParallelMap): Option[EGraphT]

  /**
   * Searches for matches of the given rules in the e-graph.
   *
   * @param rules The rules to search for matches.
   * @param egraph The e-graph to search in.
   * @param parallelize A parallelization strategy for searching.
   * @return A map from rule names to sequences of matches found for each rule.
   */
  final def search(rules: Seq[RuleT],
                   egraph: EGraphT,
                   parallelize: ParallelMap): Map[String, Seq[MatchT]] = {
    val ruleMatchingParallelize = parallelize.child("rule matching")
    ruleMatchingParallelize(
      rules, (rule: RuleT) => {
        rule.name -> search(rule, egraph, ruleMatchingParallelize)
      }
    ).toMap
  }

  /**
   * Produces a sequence of commands that apply the given matches of the rules to the e-graph.
   * @param rules The rules whose matches are to be applied.
   * @param matches A map from rule names to sequences of matches found for each rule.
   * @param egraph The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @return A sequence of commands that apply the matches to the e-graph.
   */
  final def delayed(rules: Seq[RuleT],
                    matches: Map[String, Seq[MatchT]],
                    egraph: EGraphT,
                    parallelize: ParallelMap): Seq[Command[NodeT]] = {
    val ruleApplicationParallelize = parallelize.child("rule application")
    ruleApplicationParallelize[RuleT, Command[NodeT]](rules, (rule: RuleT) => {
      val newMatches = matches(rule.name)
      delayed(rule, newMatches, egraph, ruleApplicationParallelize)
    }).toSeq
  }

  /**
   * Applies the given sequence of commands to the e-graph. This method first optimizes the sequence of commands
   * and then applies the optimized command to the e-graph.
   * @param commands The sequence of commands to apply.
   * @param matches A map from rule names to sequences of matches found for each rule. This is provided for
   *                commands that may need to know which matches were found for which rules (e.g., for caching).
   * @param egraph The e-graph to which the commands are applied. Depending on the implementation, this may be a mutable
   *               or immutable e-graph. If it is mutable, the commands may modify it in place. If it is immutable,
   *               the commands will return a new e-graph if they make any changes.
   * @param parallelize A parallelization strategy for applying the commands.
   * @return An updated e-graph with the commands applied, or None if no commands made any changes.
   */
  final def update(commands: Seq[Command[NodeT]],
                   matches: Map[String, Seq[MatchT]],
                   egraph: EGraphT,
                   parallelize: ParallelMap): Option[EGraphT] = {
    val command = CommandQueue(commands).optimized
    update(command, matches, egraph, parallelize)
  }

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
            parallelize: ParallelMap): Option[EGraphT] = {
    update(delayed(rules, matches, egraph, parallelize), matches, egraph, parallelize)
  }

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
   * Creates a [[SearchAndApply]] instance that operates on mutable e-graphs.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A [[SearchAndApply]] instance that operates on mutable e-graphs.
   */
  def mutable[
    NodeT,
    EGraphT <: MutableEGraph[NodeT],
    MatchT
  ]: SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    new NoMatchCaching[NodeT, EGraphT, MatchT] {
      override def update(command: Command[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphT,
                          parallelize: ParallelMap): Option[EGraphT] = {
        val (anyChanges, _) = command(egraph, Map.empty, parallelize)
        if (anyChanges) Some(egraph) else None
      }
    }
  }

  /**
   * Creates a [[SearchAndApply]] instance that operates on immutable e-graphs.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A [[SearchAndApply]] instance that operates on immutable e-graphs.
   */
  def immutable[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ]: SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    new NoMatchCaching[NodeT, EGraphT, MatchT] {
      override def update(command: Command[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphT,
                          parallelize: ParallelMap): Option[EGraphT] = {
        val (newEGraph, _) = command.applyImmutable(egraph, Map.empty, parallelize)
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
  def immutableWithCaching[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT <: PortableMatch[NodeT, MatchT]
  ]: SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    new SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] {
      override def search(rule: Rule[NodeT, MatchT, EGraphT],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Seq[MatchT] = {
        val matches = rule.search(egraph.egraph, parallelize)
        val oldMatches = egraph.applications(rule.name)
        matches.filterNot(oldMatches.contains)
      }

      override def delayed(rule: Rule[NodeT, MatchT, EGraphT],
                           matches: Seq[MatchT],
                           egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                           parallelize: ParallelMap): Command[NodeT] = {
        rule.delayed(matches, egraph.egraph, parallelize)
      }

      override def update(command: Command[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Option[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]] = {
        val recorded = matches.mapValuesStrict(_.toSet)
        val mutEGraph = FreezableEGraph[NodeT, EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]](egraph.record(recorded))
        val (anyChanges, _) = command(mutEGraph, Map.empty, parallelize)
        if (anyChanges) Some(mutEGraph.freeze()) else None
      }
    }
  }

  private abstract class NoMatchCaching[
    NodeT,
    EGraphT <: ReadOnlyEGraph[NodeT],
    MatchT
  ] extends SearchAndApply[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] {
    final override def search(rule: Rule[NodeT, MatchT, EGraphT],
                              egraph: EGraphT,
                              parallelize: ParallelMap): Seq[MatchT] = {
      rule.search(egraph, parallelize)
    }

    final override def delayed(rule: Rule[NodeT, MatchT, EGraphT],
                               matches: Seq[MatchT],
                               egraph: EGraphT,
                               parallelize: ParallelMap): Command[NodeT] = {
      rule.delayed(matches, egraph, parallelize)
    }

    final override def apply(rules: Seq[Rule[NodeT, MatchT, EGraphT]],
                             egraph: EGraphT,
                             parallelize: ParallelMap): Option[EGraphT] = {
      val ruleMatchingAndApplicationParallelize = parallelize.child("rule matching+application")
      val updates = ruleMatchingAndApplicationParallelize(
        rules,
        (rule: Rule[NodeT, MatchT, EGraphT]) => {
          rule.delayed(egraph, ruleMatchingAndApplicationParallelize)
        }
      ).toSeq

      update(updates, Map.empty[String, Seq[MatchT]], egraph, parallelize)
    }
  }
}
