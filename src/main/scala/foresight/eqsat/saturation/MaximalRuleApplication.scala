package foresight.eqsat.saturation

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.commands.CommandQueue
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rule

/**
 * A strategy that, for a sequence of rules, applies every match for each rule.
 *
 * @param rules The rules to apply.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam RuleT The type of the rules to apply.
 * @tparam EGraphT The type of the e-graph.
 * @tparam MatchT The type of the matches produced by the rules.
 */
final case class MaximalRuleApplication[NodeT,
                                        RuleT <: Rule[NodeT, MatchT, _],
                                        EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
                                        MatchT](
  rules: Seq[RuleT],
  searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT]
) extends Strategy[EGraphT, Unit] {

  override def initialData: Unit = ()

  override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
    val newEGraph = searchAndApply.apply(
      rules,
      searchAndApply.search(rules, egraph, parallelize),
      egraph,
      parallelize)

    (newEGraph, ())
  }
}

/**
 * A companion object for the [[MaximalRuleApplication]] strategy that provides a factory method for creating a
 * [[MaximalRuleApplication]] instance without caching.
 */
object MaximalRuleApplication {
  /**
   * Creates a strategy that applies every match for each rule without caching the matches that have already been applied.
   * @param rules The rules to apply.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph.
   * @tparam MatchT The type of the matches produced by the rules.
   * @return A strategy that applies the rules without caching.
   */
  def apply[NodeT,
            EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
            MatchT](
    rules: Seq[Rule[NodeT, MatchT, EGraphT]]
  ): MaximalRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {

    new MaximalRuleApplication(rules, SearchAndApply.withoutCaching)
  }
}