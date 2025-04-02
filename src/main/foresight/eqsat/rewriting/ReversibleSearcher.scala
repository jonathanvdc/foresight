package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A searcher that can be reversed into an applier.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam MatchT The type of the match.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait ReversibleSearcher[NodeT, MatchT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]]
  extends Searcher[NodeT, Seq[MatchT], EGraphT] {

  /**
   * Tries to reverse the searcher, turning it into an applier.
   * @return If the searcher is reversible, an applier.
   */
  def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]]
}

/**
 * The companion object for [[ReversibleSearcher]].
 */
object ReversibleSearcher {

  /**
   * Creates a reversible searcher from a reversible searcher phase.
   * @param phase The reversible searcher phase to create the searcher from.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam MatchT The type of the match.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @return A reversible searcher that uses the given phase.
   */
  def apply[NodeT,
            MatchT,
            EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](phase: ReversibleSearcherPhase[NodeT, Unit, _, MatchT, EGraphT]): ReversibleSearcher[NodeT, MatchT, EGraphT] = {
    new ReversibleSearcher[NodeT, MatchT, EGraphT] {
      override def search(egraph: EGraphT, parallelize: ParallelMap): Seq[MatchT] = phase.search(egraph, (), parallelize)

      override def tryReverse: Option[Applier[NodeT, MatchT, EGraphT]] = {
        phase.tryReverse(Applier.ignore)
      }
    }
  }
}
