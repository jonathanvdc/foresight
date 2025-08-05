package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}

/**
 * A strategy that, for a sequence of rules, applies every match for each rule. This strategy caches the matches that
 * have already been applied, so that they are not applied again. This strategy operates on an
 * [[EGraphWithRecordedApplications]].
 */
object MaximalRuleApplicationWithCaching {
  /**
   * Creates a strategy that applies every match for each rule, caching the matches that have already been applied.
   * This strategy operates on an [[EGraphWithRecordedApplications]].
   *
   * @param rules The rules to apply.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A strategy that applies the rules with caching.
   */
  def apply[NodeT,
            EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
            MatchT <: PortableMatch[NodeT, MatchT]](
      rules: Seq[Rule[NodeT, MatchT, EGraphT]]
    ): Strategy[EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], Unit] = {
    MaximalRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT](
      rules, SearchAndApply.withCaching[NodeT, EGraphT, MatchT])
  }
}
