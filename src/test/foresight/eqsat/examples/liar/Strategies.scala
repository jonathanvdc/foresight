package foresight.eqsat.examples.liar

import foresight.eqsat.{EGraph, EGraphLike, Tree}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation.{BackoffRule, BackoffRuleApplication, EGraphWithRecordedApplications, EGraphWithRoot, MaximalRuleApplication, MaximalRuleApplicationWithCaching, RebasingStrategies, SearchAndApply, Strategy}

import scala.concurrent.duration.Duration

/**
 * A collection of pre-built BLAS idiom recognition strategies for the LIAR example.
 */
object Strategies {
  type Match = PatternMatch[ArrayIR]
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, Match, MetadataEGraph]

  private def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()
  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()
  private def blasIdiomRules: BlasIdiomRules[BaseEGraph] = BlasIdiomRules[BaseEGraph]()

  private val extractionAnalysis = TimeComplexity.analysis
  private def areEquivalent(left: Tree[ArrayIR], right: Tree[ArrayIR]): Boolean = {
    extractionAnalysis.cost(left) == extractionAnalysis.cost(right)
  }

  /**
   * A naive strategy that applies the expansion, simplification and idiom rules until a fixpoint is reached.
   * @param iterationLimit An optional limit on the number of iterations to perform.
   * @param timeout An optional timeout for the strategy.
   * @param rules A sequence of rules to apply. Defaults to all core, arithmetic, and BLAS idiom rules.
   * @return
   */
  def naive(iterationLimit: Option[Int] = None,
            timeout: Option[Duration] = None,
            rules: Seq[LiarRule] = coreRules.allWithConstArray ++ arithRules.all ++ blasIdiomRules.all): Strategy[BaseEGraph, Unit] = {
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .withTimeout(timeout)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(extractionAnalysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }

  /**
   * A strategy based on the Isaria system, which first applies cycles of expansion and simplification followed by
   * rebasing the e-graph, until a timeout is reached or a fixpoint is achieved. Then, it applies idiom rules.
   *
   * Based on the algorithm in Figure 3 of Thomas, Samuel, and James Bornholt. "Automatic generation of vectorizing
   * compilers for customizable digital signal processors." Proceedings of the 29th ACM International Conference on
   * Architectural Support for Programming Languages and Operating Systems, Volume 1. 2024.
   * @param timeout An optional timeout for the strategy.
   * @param phaseTimeout An optional timeout for each phase of the strategy.
   * @param expansionRules A sequence of rules to apply for expanding the e-graph, defaults to introducing constant
   *                       arrays and arithmetic introduction rules.
   * @param simplificationRules A sequence of rules to apply for simplifying the e-graph, defaults to core elimination rules
   *                            and arithmetic simplification rules.
   * @param idiomRules A sequence of rules to apply for recognizing idioms, defaults to all BLAS idiom rules.
   * @return A strategy that applies the Isaria approach to e-graph rewriting.
   */
  def isaria(timeout: Option[Duration] = None,
             phaseTimeout: Option[Duration] = None,
             expansionRules: Seq[LiarRule] = coreRules.introduceConstArray +: arithRules.introductionRules,
             simplificationRules: Seq[LiarRule] = coreRules.eliminationRules ++ arithRules.simplificationRules,
             idiomRules: Seq[LiarRule] = blasIdiomRules.all): Strategy[BaseEGraph, Unit] = {
    MaximalRuleApplicationWithCaching(expansionRules)
      .thenApply(MaximalRuleApplicationWithCaching(simplificationRules))
      .withTimeout(phaseTimeout)
      .untilFixpoint
      .closeRecording
      .thenRebase(extractionAnalysis.extractor, areEquivalent)
      .withTimeout(timeout)
      .untilFixpoint
      .thenApply(MaximalRuleApplication(idiomRules))
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(extractionAnalysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }

  /**
   * A strategy that applies a set of rules to an e-graph, rebases the e-graph, and extracts a tree.
   * It uses a backoff strategy for rule application and applies the rules until a fixpoint is reached.
   *
   * Based on the algorithm described in Section 4.4 of de Franca, Fabricio Olivetti, and Gabriel Kronberger. "Reducing
   * overparameterization of symbolic regression models with equality saturation." Proceedings of the Genetic and
   * Evolutionary Computation Conference. 2023.
   * @param iterationLimit An optional limit on the number of iterations to perform.
   * @param timeout An optional timeout for the strategy.
   * @param cycles The number of cycles to run the strategy for.
   * @param rules A sequence of rules to apply. Defaults to all core, arithmetic, and BLAS idiom rules.
   * @return A strategy that applies the rules, extracts a tree, and rebases the e-graph.
   */
  def sympy(iterationLimit: Option[Int] = None,
            timeout: Option[Duration] = None,
            cycles: Int = 2,
            rules: Seq[LiarRule] = coreRules.allWithConstArray ++ arithRules.all ++ blasIdiomRules.all): Strategy[BaseEGraph, Unit] = {

    val ruleApplicationLimit = 2500
    val ruleBanLength = 5

    val baseStrategy = BackoffRuleApplication(
      rules.map(rule =>
        BackoffRule[ArrayIR, Rule[ArrayIR, Match, MetadataEGraph], Match](
          rule, ruleApplicationLimit, ruleBanLength)),
      SearchAndApply.withCaching[ArrayIR, MetadataEGraph, Match])

    baseStrategy
      .withIterationLimit(iterationLimit)
      .withTimeout(timeout.map(_ / cycles))
      .untilFixpoint
      .closeRecording
      .thenRebase(extractionAnalysis.extractor)
      .withIterationLimit(cycles)
      .untilFixpoint
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(extractionAnalysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }
}
