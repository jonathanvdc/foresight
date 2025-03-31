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
 * @tparam EGraphT The type of the e-graph.
 */
final case class MaximalRuleApplication[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](rules: Seq[Rule[NodeT, _, EGraphT]])
  extends Strategy[EGraphT, Unit] {

  override def initialData: Unit = ()

  override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
    // Find all matches for each rule and construct a command that applies each match to the e-graph.
    val update = CommandQueue(rules.map(_.delayed(egraph, parallelize))).optimized

    // Apply the command to the e-graph.
    val (newEGraph, _) = update(egraph, Map.empty, parallelize)
    (newEGraph, ())
  }
}
