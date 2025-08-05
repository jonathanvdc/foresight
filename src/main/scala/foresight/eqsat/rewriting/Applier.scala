package foresight.eqsat.rewriting

import foresight.eqsat.{EGraph, EGraphLike}
import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.saturation.EGraphWithRoot

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

  /**
   * An applier that maps a match to a new match and applies it to an e-graph.
   * @param applier The applier to apply the match.
   * @param f The function that maps the match to a new match.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT1 The type of the match to map.
   * @tparam MatchT2 The type of the match to apply.
   * @tparam EGraphT The type of the e-graph that the applier applies the match to.
   */
  final case class Map[NodeT,
                       MatchT1,
                       MatchT2,
                       EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](applier: Applier[NodeT, MatchT2, EGraphT],
                                                                                 f: (MatchT1, EGraphT) => MatchT2)
    extends Applier[NodeT, MatchT1, EGraphT] {

    override def apply(m: MatchT1, egraph: EGraphT): Command[NodeT] = {
      applier.apply(f(m, egraph), egraph)
    }
  }

  /**
   * An applier that first applies a function to its matches, flattens the result, and applies it to an e-graph.
   * @param applier The applier to apply the match.
   * @param f The function that maps the match to a new match.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT1 The type of the match to map.
   * @tparam MatchT2 The type of the match to apply.
   * @tparam EGraphT The type of the e-graph that the applier applies the match to.
   */
  final case class FlatMap[NodeT,
                           MatchT1,
                           MatchT2,
                           EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](applier: Applier[NodeT, MatchT2, EGraphT],
                                                                                      f: (MatchT1, EGraphT) => Iterable[MatchT2])
    extends Applier[NodeT, MatchT1, EGraphT] {

    override def apply(m: MatchT1, egraph: EGraphT): Command[NodeT] = {
      CommandQueue(f(m, egraph).map(applier.apply(_, egraph)).toSeq)
    }
  }

  /**
   * An implicit class that adds operations to the [[Applier]] trait.
   * @param applier The applier to add operations to.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the match.
   * @tparam EGraphT The type of the e-graph that the applier applies the match to.
   */
  implicit class ApplierOps[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](private val applier: Applier[NodeT, MatchT, EGraphT])
    extends AnyVal {
    /**
     * Creates a new applier that applies the match only if the filter returns true.
     *
     * @param filter The filter that determines whether to apply the match.
     * @return The new applier that applies the match only if the filter returns true.
     */
    def filter(filter: (MatchT, EGraphT) => Boolean): Applier[NodeT, MatchT, EGraphT] = {
      Filter(applier, filter)
    }

    /**
     * Creates a new applier that maps the match to a new match and applies it to an e-graph.
     *
     * @param f The function that maps the match to a new match.
     * @tparam MatchT2 The type of the match to apply.
     * @return The new applier that maps the match to a new match and applies it to an e-graph.
     */
    def map[MatchT2](f: (MatchT2, EGraphT) => MatchT): Applier[NodeT, MatchT2, EGraphT] = {
      Map(applier, f)
    }

    /**
     * Creates a new applier that flattens a sequence of matches and applies them to an e-graph.
     * @param f The function that maps the match to a new match.
     * @tparam MatchT2 The type of the match to apply.
     * @return The new applier that flattens a sequence of matches and applies them to an e-graph.
     */
    def flatMap[MatchT2](f: (MatchT2, EGraphT) => Iterable[MatchT]): Applier[NodeT, MatchT2, EGraphT] = {
      FlatMap(applier, f)
    }
  }
}
