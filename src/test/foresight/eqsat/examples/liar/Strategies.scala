package foresight.eqsat.examples.liar

import foresight.eqsat.{EGraph, Tree}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation.{EGraphWithRoot, MaximalRuleApplication, MaximalRuleApplicationWithCaching, Strategy}

import scala.concurrent.duration.Duration

object Strategies {
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  private def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()
  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()
  private def blasIdiomRules: BlasIdiomRules[BaseEGraph] = BlasIdiomRules[BaseEGraph]()


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
