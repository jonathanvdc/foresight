package foresight.eqsat.examples.sdql.tests

import foresight.eqsat.examples.sdql.SdqlRules.SdqlRule
import foresight.eqsat.EGraph
import foresight.eqsat.examples.sdql.{Add, Get, Kernels, KindAnalysis, Lam, Mult, Num, SdqlIR, SdqlRules, Sing, Sub, SubArray, Sum, Var}
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
                       rules: Seq[SdqlRule] = SdqlRules.allNew): Strategy[EGraph[SdqlIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[SdqlIR])
      .addAnalysis(KindAnalysis)
      .closeMetadata
      .dropData
}
