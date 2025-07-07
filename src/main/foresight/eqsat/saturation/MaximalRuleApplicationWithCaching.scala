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

    val newEGraph = SearchAndApply.withCaching.apply(
      rules,
      SearchAndApply.withCaching.search(rules, egraph, parallelize),
      egraph,
      parallelize)

    (newEGraph, ())
  }
}
