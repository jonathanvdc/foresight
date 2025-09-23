package foresight.eqsat.rewriting.patterns

import foresight.eqsat.rewriting.{Applier, EClassSearcher, ReversibleSearcher, SearcherContinuation}
import foresight.eqsat.{EClassCall, ReadOnlyEGraph}

/**
 * A phase of a searcher that searches for matches of a pattern machine in an e-graph.
 *
 * @param pattern The pattern to search for.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the searcher searches in.
 */
final case class MachineEClassSearcher[
  NodeT,
  EGraphT <: ReadOnlyEGraph[NodeT]
](pattern: CompiledPattern[NodeT, EGraphT],
  buildContinuation: SearcherContinuation.ContinuationBuilder[NodeT, PatternMatch[NodeT], EGraphT])

  extends EClassSearcher[NodeT, PatternMatch[NodeT], EGraphT]
  with ReversibleSearcher[NodeT, PatternMatch[NodeT], EGraphT] {

  protected override def search(call: EClassCall, egraph: EGraphT, continuation: SearcherContinuation.Continuation[NodeT, PatternMatch[NodeT], EGraphT]): Unit = {
    pattern.search(call, egraph, continuation)
  }

  override def withContinuationBuilder(continuation: SearcherContinuation.ContinuationBuilder[NodeT, PatternMatch[NodeT], EGraphT]): MachineEClassSearcher[NodeT, EGraphT] = {
    copy(buildContinuation = continuation)
  }

  override def tryReverse: Option[Applier[NodeT, PatternMatch[NodeT], EGraphT]] = {
    buildContinuation.tryReverse(pattern.pattern.toApplier)
  }
}

object MachineEClassSearcher {
  /**
   * Creates a `MachineEClassSearcher` from a compiled pattern.
   *
   * @param pattern The pattern to search for.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph that the searcher searches in.
   * @return A new `MachineEClassSearcher` instance.
   */
  def apply[
    NodeT,
    EGraphT <: ReadOnlyEGraph[NodeT]
  ](pattern: CompiledPattern[NodeT, EGraphT]): MachineEClassSearcher[NodeT, EGraphT] = {
    MachineEClassSearcher(pattern, SearcherContinuation.identityBuilder)
  }
}