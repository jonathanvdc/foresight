package foresight.eqsat.examples.liar

import foresight.eqsat.{EGraph, Tree}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation.{EGraphWithRoot, MaximalRuleApplication, MaximalRuleApplicationWithCaching, Strategy}

import scala.concurrent.duration.Duration

/**
 * A collection of pre-built BLAS idiom recognition strategies for the LIAR example.
 */
object Strategies {
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  private def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()
  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()
  private def blasIdiomRules: BlasIdiomRules[BaseEGraph] = BlasIdiomRules[BaseEGraph]()

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
      .addAnalysis(TimeComplexity.analysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }

  /**
   * A strategy based on the Isaria system, which first applies cycles of expansion and simplification followed by
   * rebasing the e-graph, until a timeout is reached or a fixpoint is achieved. Then, it applies idiom rules.
   * @param timeout An optional timeout for the strategy.
   * @param expansionRules A sequence of rules to apply for expanding the e-graph, defaults to introducing constant
   *                       arrays and arithmetic introduction rules.
   * @param simplificationRules A sequence of rules to apply for simplifying the e-graph, defaults to core elimination rules
   *                            and arithmetic simplification rules.
   * @param idiomRules A sequence of rules to apply for recognizing idioms, defaults to all BLAS idiom rules.
   * @return A strategy that applies the Isaria approach to e-graph rewriting.
   */
  def isaria(timeout: Option[Duration],
             expansionRules: Seq[LiarRule] = coreRules.introduceConstArray +: arithRules.introductionRules,
             simplificationRules: Seq[LiarRule] = coreRules.eliminationRules ++ arithRules.simplificationRules,
             idiomRules: Seq[LiarRule] = blasIdiomRules.all): Strategy[BaseEGraph, Unit] = {
    val extractionAnalysis = TimeComplexity.analysis
    def areEquivalent(left: Tree[ArrayIR], right: Tree[ArrayIR]): Boolean = {
      extractionAnalysis.cost(left) == extractionAnalysis.cost(right)
    }

    MaximalRuleApplicationWithCaching(expansionRules)
      .thenApply(MaximalRuleApplicationWithCaching(simplificationRules))
      .thenRebase(extractionAnalysis.extractor, areEquivalent)
      .withTimeout(timeout)
      .untilFixpoint
      .closeRecording
      .thenApply(MaximalRuleApplication(idiomRules))
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(extractionAnalysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }
}
