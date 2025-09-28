package foresight.eqsat.saturation

import foresight.eqsat.commands.{Command, CommandQueue}
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{PortableMatch, Rule}
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithRecordedApplications}

/**
 * A saturation strategy that exhaustively applies all rule matches **once**, with match caching to prevent reapplication.
 *
 * This is a cached variant of [[MaximalRuleApplication]] that operates on
 * [[foresight.eqsat.saturation.EGraphWithRecordedApplications]], which tracks which rule–match pairs
 * have already been applied. Once a match has been applied, it is **not re-applied in future iterations**.
 *
 * Like its uncached counterpart, this strategy does not saturate to a fixpoint per iteration.
 * Each iteration simply performs a full match-and-apply sweep over all rules,
 * skipping matches that were already applied previously.
 *
 * ## Key characteristics
 *   - Applies all previously unapplied matches per rule, per iteration.
 *   - Skips any matches that have already been applied in prior iterations.
 *   - Avoids redundant reapplication of the same rewrite.
 *   - Can be composed with [[Strategy.repeatUntilStable]] to run until no unapplied matches remain.
 *
 * ## Use cases
 *   - Efficient and deterministic saturation when repeat application is unnecessary or undesirable.
 *   - Gradual saturation where each match is only applied once.
 *
 * @example
 * {{{
 * val strategy =
 *   MaximalRuleApplicationWithCaching(rules)
 *     .repeatUntilStable
 *
 * val saturated = strategy.run(EGraphWithRecordedApplications(initialEGraph))
 * }}}
 */
object MaximalRuleApplicationWithCaching {
  /**
   * Constructs a strategy that applies all unapplied matches for each rule, caching matches as they are applied.
   *
   * This strategy requires the input e-graph to be wrapped in [[EGraphWithRecordedApplications]],
   * which tracks which rewrites have been applied so far.
   *
   * @param rules The rules to apply exhaustively.
   * @tparam NodeT   The type of nodes in the e-graph.
   * @tparam EGraphT The base type of the e-graph (must implement [[EGraphLike]] and [[EGraph]]).
   * @tparam MatchT  The type of matches produced by the rules (must extend [[foresight.eqsat.rewriting.PortableMatch]]).
   * @return A strategy that performs cached maximal application of rules.
   */
  def apply[NodeT,
            EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT],
            MatchT <: PortableMatch[NodeT, MatchT]](
      rules: Seq[Rule[NodeT, MatchT, EGraphT]]
    ): Strategy[NodeT, EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], Unit] = {
    MaximalRuleApplication[NodeT, Rule[NodeT, MatchT, EGraphT], EGraphWithRecordedApplications[NodeT, EGraphT, MatchT], MatchT](
      rules, SearchAndApply.immutableWithCaching[NodeT, EGraphT, MatchT])
  }
}
