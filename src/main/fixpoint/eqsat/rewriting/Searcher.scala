package fixpoint.eqsat.rewriting

import fixpoint.eqsat.{EClassRef, EGraph, EGraphLike}

/**
 * A searcher that searches for matches in an e-graph.
 *
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam OutputT The type of the output that the searcher produces.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
trait Searcher[NodeT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]] {
  /**
   * The phases of the searcher.
   */
  def phases: Seq[SearcherPhase[NodeT, Any, Any, Any, EGraphT]]

  /**
   * Searches for matches in an e-graph.
   * @param egraph The e-graph to search in.
   * @param parallelize Whether to parallelize the search.
   * @return The output of the searcher.
   */
  final def search(egraph: EGraphT, parallelize: Boolean = true): OutputT = {
    var input: Any = ()
    val classes = egraph.classes
    for (phase <- phases) {
      def searchClass(c: EClassRef) = {
        c -> phase.search(egraph.canonicalize(c), egraph, input)
      }

      val matches = if (parallelize)
        classes.par.map(searchClass).seq.toMap
      else
        classes.map(searchClass).toMap

      input = phase.aggregate(matches)
    }
    input.asInstanceOf[OutputT]
  }
}

/**
 * A companion object for Searcher.
 */
object Searcher {
  /**
   * Creates a searcher with a single phase.
   *
   * @param phase1 The phase of the searcher.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam OutputT The type of the output that the searcher produces.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @tparam T1 The type of the intermediate output of the first phase.
   * @return A searcher with a single phase.
   */
  def apply[NodeT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], T1](phase1: SearcherPhase[NodeT, Unit, T1, OutputT, EGraphT]):
  Searcher[NodeT, OutputT, EGraphT] = {
    new Searcher[NodeT, OutputT, EGraphT] {
      override def phases: Seq[SearcherPhase[NodeT, Any, Any, Any, EGraphT]] = Seq(phase1.erase)
    }
  }

  /**
   * Creates a searcher with two phases.
   *
   * @param phase1 The first phase of the searcher.
   * @param phase2 The second phase of the searcher.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam OutputT The type of the output that the searcher produces.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @tparam T1 The type of the intermediate output of the first phase.
   * @tparam T2 The type of the result of the first phase and the input to the second phase.
   * @tparam T3 The type of the intermediate output of the second phase.
   * @return A searcher with two phases.
   */
  def apply[NodeT, OutputT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], T1, T2, T3](phase1: SearcherPhase[NodeT, Unit, T1, T2, EGraphT],
                                                                                                  phase2: SearcherPhase[NodeT, T2, T3, OutputT, EGraphT]):
  Searcher[NodeT, OutputT, EGraphT] = {
    new Searcher[NodeT, OutputT, EGraphT] {
      override def phases: Seq[SearcherPhase[NodeT, Any, Any, Any, EGraphT]] = Seq(phase1.erase, phase2.erase)
    }
  }
}
