package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.extraction.ExtractionAnalysis
import fixpoint.eqsat.parallel.ParallelMap
import fixpoint.eqsat.{EGraph, Slot}
import fixpoint.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class CoreRuleTests {
  private def strategy(iterationLimit: Int): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(CoreRules.introductionRules ++ CoreRules.eliminationRules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
//      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData


  @Test
  def introduceLambda(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val zero = ConstInt32(0).toTree
    val one = ConstInt32(1).toTree

    val (c1, egraph2) = egraph.add(zero)
    val (c2, egraph3) = egraph2.add(one)

    val strat = strategy(1)
    val egraph4 = strat(egraph3, strat.initialData, ParallelMap.sequential)._1.get

    val x = Slot.fresh()
    assert(egraph4.contains(Apply(Lambda(x, Int32Type.toTree, zero), one)))
    assert(egraph4.contains(Apply(Lambda(x, Int32Type.toTree, one), zero)))
    assert(egraph4.contains(Apply(Lambda(x, Int32Type.toTree, zero), zero)))
    assert(egraph4.contains(Apply(Lambda(x, Int32Type.toTree, one), one)))

    assert(egraph4.areSame(c1, egraph4.find(Apply(Lambda(x, Int32Type.toTree, zero), one)).get))
    assert(egraph4.areSame(c2, egraph4.find(Apply(Lambda(x, Int32Type.toTree, one), zero)).get))
    assert(egraph4.areSame(c1, egraph4.find(Apply(Lambda(x, Int32Type.toTree, zero), zero)).get))
    assert(egraph4.areSame(c2, egraph4.find(Apply(Lambda(x, Int32Type.toTree, one), one)).get))
  }
}
