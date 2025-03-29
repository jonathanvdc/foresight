package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.extraction.ExtractionAnalysis
import fixpoint.eqsat.integration.liar.CoreRules.LiarRule
import fixpoint.eqsat.{EGraph, Slot}
import fixpoint.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class CoreRuleTests {
  private def strategy(iterationLimit: Int, rules: Seq[LiarRule] = CoreRules.all): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
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
    val indexedBuild = IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, zero)), one)

    assert(egraph5.contains(indexedBuild))
    assert(egraph5.areSame(c1, egraph5.find(indexedBuild).get))
  }

  @Test
  def eliminateIndexBuild(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val zero = ConstInt32(0).toTree
    val one = ConstInt32(1).toTree
    val x = Slot.fresh()
    val indexedBuild = IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, zero)), one)

    val (c1, egraph2) = egraph.add(indexedBuild)

    assert(!egraph2.areSame(c1, egraph2.find(zero).get))

    val egraph3 = strategy(2, rules = CoreRules.eliminationRules)(egraph2).get

    assert(egraph3.areSame(c1, egraph3.find(zero).get))
  }
}
