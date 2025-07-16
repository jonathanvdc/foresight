package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, EGraphLike}

/**
 * A collection of strategies that involve rebasing an e-graph after applying a sequence of rules.
 */
object RebasingStrategies {
  /**
   * A strategy that applies a sequence of rules to an e-graph, extracts a tree from the e-graph using the provided
   * extractor, and then rebases the e-graph with that tree. This strategy is designed to be used in a
   * SymPy-like context where the e-graph is repeatedly transformed and rebased to extract new information.
   * @param rules The rules to apply to the e-graph.
   * @param extractor The extractor to use for extracting a tree from the e-graph.
   * @param cycles The number of cycles to run the strategy for. Each cycle applies the rules and rebases the e-graph.
   * @param buildCycle A function that takes a strategy for applying rules and returns a strategy that applies those
   *                   rules in a cycle. This can be used to customize the behavior of the cycle, such as setting
   *                   iteration limits or applying the strategy until a fixpoint is reached.
   * @param ruleApplicationLimit The maximum number of matches that can be applied for each rule before it is banned.
   * @param ruleBanLength The number of iterations a rule is banned for after reaching the rule application limit.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph, which must be a subtype of `EGraphLike` and `EGraph`.
   * @return A strategy that applies the rules, extracts a tree, and rebases the e-graph.
   */
  def sympy[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](rules: Seq[Rule[NodeT, _, EGraphT]],
                                                                             extractor: Extractor[NodeT, EGraphT],
                                                                             cycles: Int = 2,
                                                                             buildCycle: Strategy[EGraphWithRoot[NodeT, EGraphT], _] => Strategy[EGraphWithRoot[NodeT, EGraphT], _] = _.withIterationLimit(30).untilFixpoint,
                                                                             ruleApplicationLimit: Int = 2500,
                                                                             ruleBanLength: Int = 5): Strategy[EGraphWithRoot[NodeT, EGraphT], Unit] = {
    val baseStrategy = BackoffRuleApplication(
      rules.map(BackoffRule(_, ruleApplicationLimit, ruleBanLength)),
      SearchAndApply.withoutCaching[NodeT, EGraphWithRoot[NodeT, EGraphT], _])

    buildCycle(baseStrategy)
      .chain(Rebase(extractor))
      .withIterationLimit(cycles)
      .untilFixpoint
      .dropData
  }
}
