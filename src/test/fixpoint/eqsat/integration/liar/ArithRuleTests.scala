package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{EGraph, Slot}
import fixpoint.eqsat.extraction.ExtractionAnalysis
import fixpoint.eqsat.integration.liar.CoreRules.LiarRule
import fixpoint.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class ArithRuleTests {
  private def strategy(iterationLimit: Int, rules: Seq[LiarRule] = ArithRules.all): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData

  @Test
  def simplifyAddZeroRight(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val x = Var(Slot.fresh(), DoubleType.toTree)
    val zero = ConstDouble(0.0).toTree
    val sum = Add(x, zero)

    val (_, egraph2) = egraph.add(sum)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(sum).get))
  }

  @Test
  def simplifyMulOneRight(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val x = Var(Slot.fresh(), DoubleType.toTree)
    val one = ConstDouble(1.0).toTree
    val product = Mul(x, one)

    val (_, egraph2) = egraph.add(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(product).get))
  }

  @Test
  def simplifyMulOneLeft(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val x = Var(Slot.fresh(), Int32Type.toTree)
    val one = ConstInt32(1).toTree
    val product = Mul(one, x)

    val (_, egraph2) = egraph.add(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(product).get))
  }

  @Test
  def simplifyMulZeroLeft(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val x = Var(Slot.fresh(), DoubleType.toTree)
    val zero = ConstDouble(0.0).toTree
    val product = Mul(zero, x)

    val (_, egraph2) = egraph.add(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(zero).get, egraph3.find(product).get))
  }
}
