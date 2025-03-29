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
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
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

    val egraph4 = strategy(1)(egraph3).get

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

  @Test
  def introduceIndexBuild(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val zero = ConstInt32(0).toTree
    val one = ConstInt32(1).toTree

    val (c1, egraph2) = egraph.add(zero)
    val (c2, egraph3) = egraph2.add(one)
    val (c3, egraph4) = egraph3.add(ArrayType(DoubleType.toTree, ConstIntType(100).toTree))

    val egraph5 = strategy(2)(egraph4).get

    val x = Slot.fresh()
    assert(egraph5.contains(IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, zero)), one)))
  }
}
