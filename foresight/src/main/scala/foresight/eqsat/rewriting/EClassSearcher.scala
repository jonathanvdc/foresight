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
   * @param egraph       Read-only e-graph snapshot.
   * @param continuation Continuation to call with each match found.
   */
  protected def search(call: EClassCall, egraph: EGraphT, continuation: Continuation): Unit

  /**
   * Searches for matches within a single e-class
   * @param call        Canonical e-class application
   * @param egraph      Read-only e-graph snapshot
   */
  final def search(call: EClassCall, egraph: EGraphT): Unit = {
    search(call, egraph, continuation)
  }

  /**
   * Describes which e-classes to search in the e-graph. By default, searches all e-classes.
   * Implementations can override this to customize which classes are searched.
   */
  def classesToSearch: EClassesToSearch[EGraphT] = EClassesToSearch.all

  /**
   * Searches for matches within multiple e-classes in parallel.
   *
   * Implementations can assume each `call` refers to the class's canonical representative.
   * @param calls       Canonical e-class applications to search within.
   * @param egraph      Read-only e-graph snapshot.
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
      parallelize.processBlocks(calls, EClassSearcher.blockSize, searchClass)
    } catch {
      case HaltSearchException => // Swallow the exception to halt the search early
    }
  }

  final override def search(egraph: EGraphT, parallelize: ParallelMap): Unit = {
    search(classesToSearch(egraph), egraph, parallelize)
  }
}

private object EClassSearcher {
  /**
   * The block size to use when parallelizing searches over e-classes.
   */
  final val blockSize: Int = 64
}
