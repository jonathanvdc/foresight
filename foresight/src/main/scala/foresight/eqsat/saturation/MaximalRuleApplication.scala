package foresight.eqsat.saturation

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.Rewrite
import foresight.eqsat.immutable.{EGraph, EGraphLike}
import foresight.eqsat.mutable.{EGraph => MutableEGraph}
import foresight.eqsat.readonly.ReadOnlyEGraph

/**
 * A strategy that performs a single maximal rewrite pass over a set of rules.
 *
 * For each rule in the given sequence, this strategy:
 *   1. Searches for **all matches** of that rule in the e-graph.
 *   2. Applies **every match** found, in one batch.
 *
 * This process is repeated once per strategy iteration. Importantly, this strategy does not independently
 * saturate to a fixpoint for each rule—it performs **only one pass** per rule per iteration.
 *
 * ## Characteristics
 *   - Exhaustive per-iteration application: all matches of all rules are applied.
 *   - No internal fixpointing per rule or across the e-graph.
 *   - Deterministic, non-prioritized, and non-stochastic.
 *   - Can be composed with [[Strategy.repeatUntilStable]] to run to convergence.
 *
 * ## Use cases
 *   - Baseline comparison against other selective or prioritized strategies.
 *   - Deterministic debugging and validation of rewrite rule behavior.
 *   - Scenarios where exhaustive coverage per iteration is preferred.
 *
 * ## Caution
 * This strategy may apply redundant or overlapping rewrites and can cause
 * rapid growth of the e-graph in large or densely connected rule sets.
 *
 * @param rules The rules to apply.
 * @param searchAndApply A strategy for rule search and application (may or may not cache matches).
 * @tparam NodeT   The type of nodes in the e-graph.
 * @tparam RuleT   The type of rules used.
 * @tparam EGraphT The e-graph implementation.
 * @tparam MatchT  The type of matches returned by rule search.
 */
final case class MaximalRuleApplication[NodeT,
                                        RuleT <: Rewrite[NodeT, MatchT, _],
                                        EGraphT <: ReadOnlyEGraph[NodeT],
                                        MatchT](
  rules: Seq[RuleT],
  searchAndApply: SearchAndApply[NodeT, RuleT, EGraphT, MatchT]
) extends Strategy[EGraphT, Unit] {

  override def initialData: Unit = ()

  override def apply(egraph: EGraphT, data: Unit, parallelize: ParallelMap): (Option[EGraphT], Unit) = {
    val newEGraph = searchAndApply.apply(
      rules,
      egraph,
      parallelize)

    (newEGraph, ())
  }
}


/**
 * Factory methods for constructing [[MaximalRuleApplication]] strategies.
 *
 * These overloads simplify creation of maximal application strategies by configuring default
 * search behavior without caching.
 */
object MaximalRuleApplication {
  /**
   * Creates a [[MaximalRuleApplication]] strategy that applies all matches of all rules
   * in a single iteration, without caching, for immutable e-graphs.
   *
   * This variant does not perform saturation to a fixpoint on its own.
   * To run to convergence, wrap it with [[Strategy.repeatUntilStable]].
   *
   * @example
   * {{{
   * val strategy =
   *   MaximalRuleApplication(rules)
   *     .repeatUntilStable
   *
   * val finalGraph = strategy.run(initialEGraph)
   * }}}
   * @param rules The rules to apply.
   */
  def apply[
    NodeT,
    EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
    MatchT
  ](
    rules: Seq[Rewrite[NodeT, MatchT, EGraphT]]
  ): MaximalRuleApplication[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {

    new MaximalRuleApplication(rules, SearchAndApply.immutable)
  }

  /**
   * Creates a [[MaximalRuleApplication]] strategy that applies all matches of all rules
   * in a single iteration, without caching, for mutable e-graphs.
   *
   * This variant does not perform saturation to a fixpoint on its own.
   * To run to convergence, wrap it with [[Strategy.repeatUntilStable]].
   *
   * @example
   * {{{
   * val strategy =
   *   MaximalRuleApplication.mutable(rules)
   *     .repeatUntilStable
   *
   * val finalGraph = strategy.run(initialMutableEGraph)
   * }}}
   * @param rules The rules to apply.
   */
  def mutable[
    NodeT,
    EGraphT <: MutableEGraph[NodeT],
    MatchT
  ](
    rules: Seq[Rewrite[NodeT, MatchT, EGraphT]]
  ): MaximalRuleApplication[NodeT, Rewrite[NodeT, MatchT, EGraphT], EGraphT, MatchT] = {

    new MaximalRuleApplication(rules, SearchAndApply.mutable)
  }
}
