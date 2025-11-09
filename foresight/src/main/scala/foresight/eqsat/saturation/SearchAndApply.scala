package foresight.eqsat.saturation

import foresight.eqsat.commands.{CommandSchedule, CommandScheduleBuilder}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{EClassSearcher, PortableMatch, Rewrite}
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithRecordedApplications}
import foresight.eqsat.mutable.{FreezableEGraph, EGraph => MutableEGraph}
import foresight.eqsat.readonly
import foresight.util.collections.StrictMapOps.toStrictMapOps
import foresight.util.collections.UnsafeSeqFromArray

/**
 * A strategy that searches for matches of a set of rules in an e-graph and applies them.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to search and apply.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
trait SearchAndApply[NodeT, -RuleT <: Rewrite[NodeT, MatchT, _], EGraphT <: readonly.EGraph[NodeT], MatchT] {
  /**
   * Searches for matches of the given rule in the e-graph.
   *
   * @param rule The rule to search for matches.
   * @param egraph The e-graph to search in.
   * @param parallelize A parallelization strategy for searching.
   * @return A sequence of matches found for the rule.
   */
  protected def search(rule: RuleT,
                       egraph: EGraphT,
                       parallelize: ParallelMap): Seq[MatchT]

  /**
   * Converts the matches found for the given rule into commands that can be applied to the e-graph.
   *
   * @param rule        The rule whose matches are to be applied.
   * @param matches     The matches to be applied.
   * @param egraph      The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @param collector   A command schedule builder to collect the generated commands.
   *
   */
  protected def delayed(rule: RuleT,
                        matches: Seq[MatchT],
                        egraph: EGraphT,
                        parallelize: ParallelMap,
                        collector: CommandScheduleBuilder[NodeT]): Unit

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
  protected def update(command: CommandSchedule[NodeT],
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
   * Converts the matches found for the given rules into commands that can be applied to the e-graph.
   * @param rules The rules whose matches are to be applied.
   * @param matches A map from rule names to sequences of matches found for each rule.
   * @param egraph The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @param builder A command schedule builder to collect the generated commands.
   */
  final def delayed(rules: Seq[RuleT],
                    matches: Map[String, Seq[MatchT]],
                    egraph: EGraphT,
                    parallelize: ParallelMap,
                    builder: CommandScheduleBuilder[NodeT]): Unit = {

    val ruleApplicationParallelize = parallelize.child("rule application")
    ruleApplicationParallelize[RuleT, Unit](rules, (rule: RuleT) => {
      val newMatches = matches(rule.name)
      delayed(rule, newMatches, egraph, ruleApplicationParallelize, builder)
    })
  }

  /**
   * Converts the matches found for the given rules into commands that can be applied to the e-graph.
   * @param rules The rules whose matches are to be applied.
   * @param matches A map from rule names to sequences of matches found for each rule.
   * @param egraph The e-graph to which the matches are applied.
   * @param parallelize A parallelization strategy for applying the matches.
   * @return A command schedule that applies the matches to the e-graph.
   */
  final def delayed(rules: Seq[RuleT],
                    matches: Map[String, Seq[MatchT]],
                    egraph: EGraphT,
                    parallelize: ParallelMap): CommandSchedule[NodeT] = {
    // FIXME: build sequential variant that avoids concurrency overheads when parallelism is disabled
    val collector = CommandScheduleBuilder.newConcurrentBuilder[NodeT]
    delayed(rules, matches, egraph, parallelize, collector)
    collector.result()
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
  ]: SearchAndApply[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    new NoMatchCaching[NodeT, EGraphT, MatchT] {
      override def searchLoopInterchange: Boolean = false

      override def update(command: CommandSchedule[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphT,
                          parallelize: ParallelMap): Option[EGraphT] = {
        val anyChanges = command(egraph, parallelize)
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
  ]: SearchAndApply[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {
    new NoMatchCaching[NodeT, EGraphT, MatchT] {
      override def searchLoopInterchange: Boolean = false

      override def update(command: CommandSchedule[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphT,
                          parallelize: ParallelMap): Option[EGraphT] = {
        command.applyImmutable(egraph, parallelize)
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
  ]: SearchAndApply[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] = {
    new SearchAndApply[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT] {
      override def search(rule: Rewrite[NodeT, MatchT, EGraphT],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Seq[MatchT] = {
        val matches = rule.search(egraph.egraph, parallelize)
        val oldMatches = egraph.applications(rule.name)
        matches.filterNot(oldMatches.contains)
      }

      override def delayed(rule: Rewrite[NodeT, MatchT, EGraphT],
                           matches: Seq[MatchT],
                           egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                           parallelize: ParallelMap,
                           builder: CommandScheduleBuilder[NodeT]): Unit = {
        rule.delayed(matches, egraph.egraph, parallelize, builder)
      }

      override def update(command: CommandSchedule[NodeT],
                          matches: Map[String, Seq[MatchT]],
                          egraph: EGraphWithRecordedApplications[NodeT, EGraphT, MatchT],
                          parallelize: ParallelMap): Option[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]] = {
        val recorded = matches.mapValuesStrict(_.toSet)
        val mutEGraph = FreezableEGraph[NodeT, EGraphWithRecordedApplications[NodeT, EGraphT, MatchT]](egraph.record(recorded))
        val anyChanges = command(mutEGraph, parallelize)
        if (anyChanges) Some(mutEGraph.freeze()) else None
      }
    }
  }

  private abstract class NoMatchCaching[
    NodeT,
    EGraphT <: readonly.EGraph[NodeT],
    MatchT
  ] extends SearchAndApply[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] {
    /**
     * Whether to perform the search-loop interchange optimization, which groups rules that search the same
     * e-classes together to reduce redundant work.
     * @return True to enable search-loop interchange, false to disable it.
     */
    def searchLoopInterchange: Boolean

    final override def search(rule: Rewrite[NodeT, MatchT, EGraphT],
                              egraph: EGraphT,
                              parallelize: ParallelMap): Seq[MatchT] = {
      rule.search(egraph, parallelize)
    }

    final override def delayed(rule: Rewrite[NodeT, MatchT, EGraphT],
                               matches: Seq[MatchT],
                               egraph: EGraphT,
                               parallelize: ParallelMap,
                               builder: CommandScheduleBuilder[NodeT]): Unit = {
      rule.delayed(matches, egraph, parallelize, builder)
    }

    final override def apply(rules: Seq[Rewrite[NodeT, MatchT, EGraphT]],
                             egraph: EGraphT,
                             parallelize: ParallelMap): Option[EGraphT] = {
      val ruleMatchingAndApplicationParallelize = parallelize.child("rule matching+application")

      // FIXME: build sequential variant that avoids concurrency overheads when parallelism is disabled
      val collector = CommandScheduleBuilder.newConcurrentBuilder[NodeT]

      if (!searchLoopInterchange || egraph.classCount <= EClassSearcher.smallEGraphThreshold) {
        // Small e-graph optimization: for small e-graphs, the overhead of partitioning and
        // fusing rule applications outweighs the benefits. Just process each rule normally.
        ruleMatchingAndApplicationParallelize(
          rules,
          (rule: Rewrite[NodeT, MatchT, EGraphT]) => {
            rule.delayed(egraph, ruleMatchingAndApplicationParallelize, collector)
          }
        )
      } else {
        // Idea: EClassSearcher rules are the common case, and they apply in parallel over a subset of
        // e-classes in the e-graph. If multiple rules share the same subset of e-classes to search,
        // we can group them together to fuse iterations over those e-classes. Fusion both reduces
        // redundant work and increases the amount of work done per e-class per iteration, allowing
        // for better parallelism.
        val partitioned = EClassSearcher.partitionRulesBySharedEClassesToSearch(rules)

        // Process regular rules normally.
        for (rule <- partitioned.regularRules) {
          rule.delayed(egraph, ruleMatchingAndApplicationParallelize, collector)
        }

        // Process shared EClassesToSearch rules together.
        for (eclassesToSearch <- partitioned.rulesPerSharedEClassToSearch.keys) {
          val commandSearchers = partitioned.commandSearchers(eclassesToSearch, collector)
          EClassSearcher.searchMultiple(
            UnsafeSeqFromArray(commandSearchers.toArray),
            eclassesToSearch(egraph),
            egraph,
            ruleMatchingAndApplicationParallelize
          )
        }
      }

      update(collector.result(), Map.empty[String, Seq[MatchT]], egraph, parallelize)
    }
  }
}
