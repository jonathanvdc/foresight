package foresight.eqsat.rewriting

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.EClassCall
import foresight.eqsat.readonly.EGraph
import foresight.util.collections.UnsafeSeqFromArray

import scala.collection.compat.immutable.ArraySeq

/**
 * A searcher that searches for matches within individual e-classes.
 *
 * @tparam NodeT     Node payload type.
 * @tparam MatchT    The match type emitted by this searcher.
 * @tparam EGraphT   Concrete e-graph type (must mix in [[EGraphLike]] and [[EGraph]]).
 */
trait EClassSearcher[
  NodeT,
  MatchT,
  EGraphT <: EGraph[NodeT]
] extends Searcher[NodeT, MatchT, EGraphT]
  with SearcherLike[NodeT, MatchT, EGraphT, EClassSearcher[NodeT, MatchT, EGraphT]] {

  /**
   * Searches for matches within a single e-class
   *
   * Implementations can assume `call` refers to the class's canonical representative.
   *
   * @param call         Canonical e-class application to search within.
   * @param egraph       Immutable e-graph snapshot.
   * @param continuation Continuation to call with each match found.
   */
  protected def search(call: EClassCall, egraph: EGraphT, continuation: Continuation): Unit

  /**
   * Searches for matches within a single e-class
   * @param call        Canonical e-class application
   * @param egraph      Immutable e-graph snapshot
   */
  final def search(call: EClassCall, egraph: EGraphT): Unit = {
    search(call, egraph, continuation)
  }

  /**
   * Searches for matches within multiple e-classes in parallel.
   *
   * Implementations can assume each `call` refers to the class's canonical representative.
   * @param calls       Canonical e-class applications to search within.
   * @param egraph      Immutable e-graph snapshot.
   * @param parallelize Parallel map implementation to use for distributing work.
   */
  final def search(calls: ArraySeq[EClassCall], egraph: EGraphT, parallelize: ParallelMap): Unit = {
    object HaltSearchException extends Throwable

    val baseContinuation = continuation
    val cont = (m: MatchT, e: EGraphT) => {
      if (!baseContinuation(m, e)) throw HaltSearchException
      true
    }
    val searchClass = (call: EClassCall) => {
      search(call, egraph, cont)
    }

    try {
      parallelize(calls, searchClass)
    } catch {
      case HaltSearchException => // Swallow the exception to halt the search early
    }
  }

  override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {
    search(UnsafeSeqFromArray(egraph.classes.view.map(egraph.canonicalize).toArray), egraph, parallelize)
  }
}
