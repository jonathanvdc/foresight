package foresight.eqsat.examples.liar.tests

import foresight.eqsat.examples.liar.CoreRules.LiarRule
import foresight.eqsat.examples.liar._
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EGraph, Slot}
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
    assert(egraph5.areSame(c1, egraph5.find(IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, zero)), zero)).get))
    assert(egraph5.areSame(c2, egraph5.find(IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, one)), zero)).get))
    assert(egraph5.areSame(c2, egraph5.find(IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, one)), one)).get))
  }

  @Test
  def betaReduceIdentity(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val t = Int32Type.toTree
    val zero = ConstInt32(0).toTree
    val x = Slot.fresh()
    val identity = Lambda(x, t, Var(x, t))
    val application = Apply(identity, zero)

    val (c1, egraph2) = egraph.add(application)

    val egraph3 = strategy(1, rules = CoreRules.eliminationRules)(egraph2).get

    assert(egraph3.areSame(c1, egraph3.find(zero).get))
  }

  @Test
  def betaReduceIdentityWithExtraSlot(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val t = Int32Type.toTree
    val zero = ConstInt32(0).toTree
    val x = Slot.fresh()
    val y = Slot.fresh()
    val identity = Lambda(x, t, Apply(Lambda(y, t, Var(x, t)), Var(x, t)))
    val application = Apply(identity, zero)

    val (c1, egraph2) = egraph.add(application)

    val egraph3 = strategy(2, rules = CoreRules.eliminationRules)(egraph2).get

    assert(egraph3.areSame(c1, egraph3.find(zero).get))
  }

  @Test
  def betaReduceAdd(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val t = Int32Type.toTree
    val zero = ConstInt32(0).toTree
    val x = Slot.fresh()
    val y = Slot.fresh()
    val add = Lambda(x, t, Lambda(y, t, Add(Var(x, t), Var(y, t))))
    val application = Apply(Apply(add, zero), zero)

    val (c1, egraph2) = egraph.add(application)

    val egraph3 = strategy(2, rules = Seq(CoreRules.eliminateLambda))(egraph2, ParallelMap.sequential).get

    assert(egraph3.areSame(c1, egraph3.find(Add(zero, zero)).get))
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

  @Test
  def eliminateTupleFst(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val zero = ConstInt32(0).toTree
    val one = ConstInt32(1).toTree

    val tuple = Tuple(zero, one)
    val fst = Fst(tuple)

    val (c1, egraph2) = egraph.add(fst)

    assert(!egraph2.areSame(c1, egraph2.find(zero).get))

    val egraph3 = strategy(2, rules = CoreRules.eliminationRules)(egraph2).get

    assert(egraph3.areSame(c1, egraph3.find(zero).get))
  }

  @Test
  def eliminateTupleSnd(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val zero = ConstInt32(0).toTree
    val one = ConstInt32(1).toTree

    val tuple = Tuple(zero, one)
    val snd = Snd(tuple)

    val (c1, egraph2) = egraph.add(snd)

    assert(!egraph2.areSame(c1, egraph2.find(one).get))

    val egraph3 = strategy(2, rules = CoreRules.eliminationRules)(egraph2).get

    assert(egraph3.areSame(c1, egraph3.find(one).get))
  }
}
