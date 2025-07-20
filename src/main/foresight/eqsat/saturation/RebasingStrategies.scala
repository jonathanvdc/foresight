package foresight.eqsat.saturation

import foresight.eqsat.extraction.Extractor
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, EGraphLike, Tree}

import scala.concurrent.duration.Duration

/**
 * A collection of strategies that involve rebasing an e-graph after applying a sequence of rules.
 */
object RebasingStrategies {
  /**
   * A trait that defines a method for building a cycle strategy. The cycle strategy applies a given strategy
   * repeatedly until a fixpoint is reached or a specified iteration limit is reached.
   */
  trait CycleBuilder {
    def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](
        strategy: Strategy[EGraphT, _]
    ): Strategy[EGraphT, Unit]
  }

  /**
   * A companion object for the [[CycleBuilder]] trait that provides a default implementation of the cycle strategy.
   * This implementation applies the given strategy until a fixpoint is reached, dropping any data after each iteration.
   */
  object CycleBuilder {
    /**
     * Creates a cycle builder that applies the given strategy until a fixpoint is reached, dropping any data after
     * each iteration. The cycle will continue until the specified limit is reached.
     * @param limit The maximum number of iterations to perform.
     * @return A cycle builder that applies the strategy with the specified iteration limit.
     */
    def iterate(limit: Int): CycleBuilder = new CycleBuilder {
      override def apply[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT]](
          strategy: Strategy[EGraphT, _]
      ): Strategy[EGraphT, Unit] = strategy.withIterationLimit(limit).untilFixpoint.dropData
    }
  }

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
   * @tparam EGraphT The type of the e-graph, which must be a subtype of [[EGraphLike]] and [[EGraph]].
   * @return A strategy that applies the rules, extracts a tree, and rebases the e-graph.
   */
  def sympy[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], MatchT](rules: Seq[Rule[NodeT, MatchT, EGraphWithRoot[NodeT, EGraphT]]],
                                                                                     extractor: Extractor[NodeT, EGraphT],
                                                                                     cycles: Int = 2,
                                                                                     buildCycle: CycleBuilder = CycleBuilder.iterate(30),
                                                                                     ruleApplicationLimit: Int = 2500,
                                                                                     ruleBanLength: Int = 5): Strategy[EGraphWithRoot[NodeT, EGraphT], Unit] = {
    val baseStrategy = BackoffRuleApplication(
      rules.map(BackoffRule[NodeT, Rule[NodeT, MatchT, EGraphWithRoot[NodeT, EGraphT]], MatchT](_, ruleApplicationLimit, ruleBanLength)),
      SearchAndApply.withoutCaching[NodeT, EGraphWithRoot[NodeT, EGraphT], MatchT])

    buildCycle[NodeT, EGraphWithRoot[NodeT, EGraphT]](baseStrategy)
      .thenRebase(extractor)
      .withIterationLimit(cycles)
      .untilFixpoint
      .dropData
  }

  /**
   * A strategy that operates in two phases: a recurrent phase and a final phase. In the recurrent phase, it applies a
   * transformation strategy to the e-graph, extracts a tree using the provided extractor, and rebases the e-graph
   * repeatedly until either a fixpoint is reached or the specified timeout is exceeded. In the final phase, it applies
   * a final transformation to the e-graph, which does not involve rebasing.
   * @param extractor The extractor to use for extracting a tree from the e-graph.
   * @param recurrentPhase The strategy to apply in the recurrent phase, which is applied repeatedly until a fixpoint is
   *                       reached or the timeout is exceeded.
   * @param finalPhase The strategy to apply in the final phase, which is applied after the recurrent phase.
   * @param recurrentPhaseTimeout An optional timeout for the recurrent phase. If provided, the recurrent phase will
   *                              stop applying the recurrent strategy after the specified duration.
   * @param areEquivalent A function to check if two trees are equivalent. Defaults to structural equality.
   * @tparam NodeT The type of the nodes in the e-graph.
   * @tparam EGraphT The type of the e-graph, which must be a subtype of `EGraphLike` and `EGraph`.
   * @tparam Data The type of data that the recurrent phase operates on. This can be used to carry additional
   *              information during the transformation process.
   * @return A strategy that applies the recurrent phase and rebases until a fixpoint is reached, followed by the final
   *         phase.
   */
  def isaria[NodeT, EGraphT <: EGraphLike[NodeT, EGraphT] with EGraph[NodeT], Data](
      extractor: Extractor[NodeT, EGraphT],
      recurrentPhase: Strategy[EGraphWithRoot[NodeT, EGraphT], Data],
      finalPhase: Strategy[EGraphWithRoot[NodeT, EGraphT], Unit],
      recurrentPhaseTimeout: Option[Duration] = None,
      areEquivalent: (Tree[NodeT], Tree[NodeT]) => Boolean = (x: Tree[NodeT], y: Tree[NodeT]) => x == y): Strategy[EGraphWithRoot[NodeT, EGraphT], Unit] = {

    // Logic based on Figure 3 of the Isaria paper: Automatic Generation of Vectorizing Compilers for
    // Customizable Digital Signal Processors by Thomas and Bornholt.
    recurrentPhase
      .thenRebase(extractor, areEquivalent)
      .withTimeout(recurrentPhaseTimeout)
      .untilFixpoint
      .thenApply(finalPhase)
      .dropData
  }
}
