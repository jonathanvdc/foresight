package foresight.eqsat.examples.liar

import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation._
import foresight.eqsat.Tree
import foresight.eqsat.immutable.{EGraphWithMetadata, EGraphWithRecordedApplications, EGraphWithRoot, EGraph}

import scala.concurrent.duration.Duration

/**
 * A collection of pre-built BLAS idiom recognition strategies for the LIAR example.
 */
object Strategies {
  type Match = PatternMatch[ArrayIR]
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type RecordedEGraph = EGraphWithRecordedApplications[ArrayIR, MetadataEGraph, Match]
  type LiarRule = Rule[ArrayIR, Match, MetadataEGraph]

  /**
   * Returns the core rules for the LIAR example.
   * @return A set of core rules for the LIAR example.
   */
  def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()

  /**
   * Returns the arithmetic rules for the LIAR example.
   * @return A set of arithmetic rules for the LIAR example.
   */
  def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()

  /**
   * Returns the BLAS idiom rules for the LIAR example.
   * @return A set of BLAS idiom rules for the LIAR example.
   */
  def blasIdiomRules: BlasIdiomRules[BaseEGraph] = BlasIdiomRules[BaseEGraph]()

  private val extractionAnalysis = TimeComplexity.analysis
  private def areEquivalent(left: Tree[ArrayIR], right: Tree[ArrayIR]): Boolean = {
    extractionAnalysis.cost(left) == extractionAnalysis.cost(right)
  }

  /**
   * A naive strategy that applies the expansion, simplification and idiom rules until a fixpoint is reached.
   * @param iterationLimit An optional limit on the number of iterations to perform.
   * @param timeout An optional timeout for the strategy.
   * @param rules A sequence of rules to apply. Defaults to all core, arithmetic, and BLAS idiom rules.
   * @param onChange A callback that is invoked whenever a change is made to the e-graph, providing the old and new
   *                 e-graphs. This can be used for logging or debugging purposes.
   * @return A strategy that applies the rules until a fixpoint is reached.
   */
  def naive(iterationLimit: Option[Int] = None,
            timeout: Option[Duration] = None,
            rules: Seq[LiarRule] = coreRules.allWithConstArray ++ arithRules.all ++ blasIdiomRules.all,
            onChange: (RecordedEGraph, RecordedEGraph) => Unit = (_, _) => ()): Strategy[BaseEGraph, Unit] = {
    MaximalRuleApplicationWithCaching(rules)
      .withChangeLogger(onChange)
      .withIterationLimit(iterationLimit)
      .withTimeout(timeout)
      .repeatUntilStable
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR], extractionAnalysis, TypeInferenceAnalysis)
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
   * @param expansionAndSimplificationTimeout An optional timeout for the strategy's expansion and simplification
   *                                          phases.
   * @param phaseTimeout An optional timeout for each phase of the strategy.
   * @param expansionRules A sequence of rules to apply for expanding the e-graph, defaults to introducing constant
   *                       arrays and arithmetic introduction rules.
   * @param simplificationRules A sequence of rules to apply for simplifying the e-graph, defaults to core elimination rules
   *                            and arithmetic simplification rules.
   * @param idiomRules A sequence of rules to apply for recognizing idioms, defaults to all BLAS idiom rules.
   * @param onChange A callback that is invoked whenever a change is made to the e-graph, providing the old and new
   *                 e-graphs. This can be used for logging or debugging purposes.
   * @return A strategy that applies the Isaria approach to e-graph rewriting.
   */
  def isaria(expansionAndSimplificationTimeout: Option[Duration] = None,
             phaseTimeout: Option[Duration] = None,
             expansionRules: Seq[LiarRule] = coreRules.introduceConstArray +: arithRules.introductionRules,
             simplificationRules: Seq[LiarRule] = coreRules.eliminationRules ++ arithRules.simplificationRules,
             idiomRules: Seq[LiarRule] = blasIdiomRules.all,
             onChange: (RecordedEGraph, RecordedEGraph) => Unit = (_, _) => ()): Strategy[BaseEGraph, Unit] = {

    def phase(rules: Seq[LiarRule]) = {
      MaximalRuleApplicationWithCaching(rules)
        .withChangeLogger(onChange)
        .withTimeout(phaseTimeout)
        .repeatUntilStable
    }

    phase(expansionRules)
      .thenApply(phase(simplificationRules))
      .withTimeout(expansionAndSimplificationTimeout)
      .thenRebase(extractionAnalysis.extractor, areEquivalent)
      .repeatUntilStable
      .thenApply(phase(idiomRules))
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR], extractionAnalysis, TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }

  /**
   * An improved version of the Isaria strategy that elides the last rebase after the expansion/simplification phase,
   * providing the idiom rules with a more fully expanded e-graph to work with.
   *
   * Based on the algorithm in Figure 3 of Thomas, Samuel, and James Bornholt. "Automatic generation of vectorizing
   * compilers for customizable digital signal processors." Proceedings of the 29th ACM International Conference on
   * Architectural Support for Programming Languages and Operating Systems, Volume 1. 2024.
   * @param expansionAndSimplificationTimeout An optional timeout for the strategy's expansion and simplification
   *                                          phases.
   * @param phaseTimeout An optional timeout for each phase of the strategy.
   * @param expansionRules A sequence of rules to apply for expanding the e-graph, defaults to introducing constant
   *                       arrays and arithmetic introduction rules.
   * @param simplificationRules A sequence of rules to apply for simplifying the e-graph, defaults to core elimination rules
   *                            and arithmetic simplification rules.
   * @param idiomRules A sequence of rules to apply for recognizing idioms, defaults to all BLAS idiom rules.
   * @param onChange A callback that is invoked whenever a change is made to the e-graph, providing the old and new
   *                 e-graphs. This can be used for logging or debugging purposes.
   * @return A strategy that applies the improved Isaria approach to e-graph rewriting.
   */
  def isariaImproved(expansionAndSimplificationTimeout: Option[Duration] = None,
                     phaseTimeout: Option[Duration] = None,
                     expansionRules: Seq[LiarRule] = coreRules.introduceConstArray +: arithRules.introductionRules,
                     simplificationRules: Seq[LiarRule] = coreRules.eliminationRules ++ arithRules.simplificationRules,
                     idiomRules: Seq[LiarRule] = blasIdiomRules.all,
                     onChange: (RecordedEGraph, RecordedEGraph) => Unit = (_, _) => ()): Strategy[BaseEGraph, Unit] = {

    def phase(rules: Seq[LiarRule]) = {
      MaximalRuleApplicationWithCaching(rules)
        .withChangeLogger(onChange)
        .withTimeout(phaseTimeout)
        .repeatUntilStable
    }

    phase(expansionRules)
      .thenApply(phase(simplificationRules))
      .withTimeout(expansionAndSimplificationTimeout)
      .rebaseBetweenIterations(extractionAnalysis.extractor, areEquivalent)
      .repeatUntilStable
      .thenApply(phase(idiomRules))
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR], extractionAnalysis, TypeInferenceAnalysis)
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
   * @param onChange A callback that is invoked whenever a change is made to the e-graph, providing the old and new
   *                 e-graphs. This can be used for logging or debugging purposes.
   * @return A strategy that applies the rules, extracts a tree, and rebases the e-graph.
   */
  def sympy(iterationLimit: Option[Int] = None,
            timeout: Option[Duration] = None,
            cycles: Int = 2,
            rules: Seq[LiarRule] = coreRules.allWithConstArray ++ arithRules.all ++ blasIdiomRules.all,
            onChange: (RecordedEGraph, RecordedEGraph) => Unit = (_, _) => ()): Strategy[BaseEGraph, Unit] = {

    val ruleApplicationLimit = 2500
    val ruleBanLength = 5

    val baseStrategy = BackoffRuleApplication(
      rules,
      ruleApplicationLimit,
      ruleBanLength,
      SearchAndApply.immutableWithCaching[ArrayIR, MetadataEGraph, Match])

    baseStrategy
      .withChangeLogger(onChange)
      .withIterationLimit(iterationLimit)
      .withTimeout(timeout.map(_ / cycles))
      .repeatUntilStable
      .closeRecording
      .thenRebase(extractionAnalysis.extractor)
      .withIterationLimit(cycles)
      .repeatUntilStable
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR], extractionAnalysis, TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }
}
