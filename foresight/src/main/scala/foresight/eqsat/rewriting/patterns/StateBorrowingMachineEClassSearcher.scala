package foresight.eqsat.rewriting.patterns

import foresight.eqsat.rewriting.{Applier, EClassSearcher, ReversibleSearcher, SearcherContinuation}
import foresight.eqsat.EClassCall
import foresight.eqsat.readonly.EGraph

/**
 * A phase of a searcher that searches for matches of a pattern machine in an e-graph.
 *
 * This searcher uses a borrowing mechanism for the machine state, allowing for more efficient
 * searches by reusing state without the overhead of copying.
 *
 * @param pattern The pattern to search for.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
final case class StateBorrowingMachineEClassSearcher[
  NodeT,
  EGraphT <: EGraph[NodeT]
](pattern: CompiledPattern[NodeT, EGraphT],
  buildContinuation: SearcherContinuation.ContinuationBuilder[NodeT, AbstractPatternMatch[NodeT], EGraphT])

  extends EClassSearcher[NodeT, AbstractPatternMatch[NodeT], EGraphT] {

  protected override def search(call: EClassCall, egraph: EGraphT, continuation: SearcherContinuation.Continuation[NodeT, AbstractPatternMatch[NodeT], EGraphT]): Unit = {
    pattern.searchBorrowed(call, egraph, continuation)
  }

  override def withContinuationBuilder(continuation: SearcherContinuation.ContinuationBuilder[NodeT, AbstractPatternMatch[NodeT], EGraphT]): StateBorrowingMachineEClassSearcher[NodeT, EGraphT] = {
    copy(buildContinuation = continuation)
  }
}

object StateBorrowingMachineEClassSearcher {
  /**
   * Creates a `StateBorrowingMachineEClassSearcher` from a compiled pattern.
   *
   * @param pattern The pattern to search for.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @return A new `StateBorrowingMachineEClassSearcher` instance.
   */
  def apply[
    NodeT,
    EGraphT <: EGraph[NodeT]
  ](pattern: CompiledPattern[NodeT, EGraphT]): StateBorrowingMachineEClassSearcher[NodeT, EGraphT] = {
    StateBorrowingMachineEClassSearcher(pattern, SearcherContinuation.identityBuilder)
  }
}
