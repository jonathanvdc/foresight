package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.commands.{Command, CommandQueue}

/**
 * An applier that applies a match to an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MatchT The type of the match.
 * @tparam EGraphT The type of the e-graph that the applier applies the match to.
 */
trait Applier[NodeT, -MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * Creates a command that applies a match to an e-graph.
   * @param m The match to apply.
   * @param egraph The e-graph in which the match was found.
   * @return The command that represents the application of the match.
   */
  def apply(m: MatchT, egraph: EGraphT): Command[NodeT]
}

/**
 * The companion object for the [[Applier]] trait.
 */
object Applier {
  /**
   * An applier that does nothing. It ignores the match and returns an empty command.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the match.
   * @tparam EGraphT The type of the e-graph that the applier applies the match to.
   * @return An applier that ignores the match and returns an empty command.
   */
  def ignore[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]: Applier[NodeT, MatchT, EGraphT] =
    new ReversibleApplier[NodeT, MatchT, EGraphT] {
      override def apply(m: MatchT, egraph: EGraphT): Command[NodeT] = CommandQueue.empty
      override def tryReverse: Option[Searcher[NodeT, Seq[MatchT], EGraphT]] = Some(Searcher.empty)
    }

  /**
   * An applier that applies matches only if a filter returns true.
   * @param applier The applier to apply the match.
   * @param filter The filter that determines whether to apply the match.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the match.
   * @tparam EGraphT The type of the e-graph that the applier applies the match to.
   */
  final case class Filter[NodeT,
                          MatchT,
                          EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](applier: Applier[NodeT, MatchT, EGraphT],
                                                                                    filter: (MatchT, EGraphT) => Boolean)
    extends ReversibleApplier[NodeT, MatchT, EGraphT] {

    override def apply(m: MatchT, egraph: EGraphT): Command[NodeT] = {
      if (filter(m, egraph)) applier.apply(m, egraph) else CommandQueue.empty
    }

    override def tryReverse: Option[Searcher[NodeT, Seq[MatchT], EGraphT]] = applier match {
      case r: ReversibleApplier[NodeT, MatchT, EGraphT] => r.tryReverse.map(Searcher.Filter(_, filter))
      case _ => None
    }
  }
}
