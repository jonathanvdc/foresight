package foresight.eqsat.examples.sdql.tests

import foresight.eqsat.examples.sdql.SdqlRules.SdqlRule
import foresight.eqsat.EGraph
import foresight.eqsat.examples.sdql.{Kernels, KindAnalysis, SdqlIR, SdqlRules}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class KernelTest {
  @Test
  def saturateMmmSum1(): Unit = {
    val tree = Kernels.mmmSum1
    val (root, graph) = EGraph.empty[SdqlIR].add(tree)
    val saturated = strategy(5)(graph).get
  }

  @Test
  def saturateBatax2(): Unit = {
    val tree = Kernels.batax2
    val (root, graph) = EGraph.empty[SdqlIR].add(tree)
    val saturated = strategy(6)(graph).get
  }

  private def strategy(iterationLimit: Int,
                       rules: Seq[SdqlRule] = SdqlRules.allNew): Strategy[SdqlIR, EGraph[SdqlIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .repeatUntilStable
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[SdqlIR])
      .addAnalyses(KindAnalysis)
      .closeMetadata
      .dropData
}
