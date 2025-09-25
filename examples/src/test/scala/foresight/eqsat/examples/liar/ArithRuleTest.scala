package foresight.eqsat.examples.liar

import foresight.eqsat.examples.liar._
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.Slot
import foresight.eqsat.immutable.{EGraph, EGraphWithMetadata, EGraphWithRoot}
import org.junit.Test

class ArithRuleTest {
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()

  private def strategy(iterationLimit: Int, rules: Seq[LiarRule] = arithRules.all): Strategy[ArrayIR, BaseEGraph, Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .repeatUntilStable
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalyses(TypeInferenceAnalysis)
      .closeMetadata
      .dropData

  @Test
  def simplifyAddZeroRight(): Unit = {
    val x = Var(Slot.fresh(), DoubleType.toTree)
    val zero = ConstDouble(0.0).toTree
    val sum = Add(x, zero)

    val (_, egraph2) = EGraphWithRoot.from(sum)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(sum).get))
  }

  @Test
  def simplifyMulOneRight(): Unit = {
    val x = Var(Slot.fresh(), DoubleType.toTree)
    val one = ConstDouble(1.0).toTree
    val product = Mul(x, one)

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(product).get))
  }

  @Test
  def simplifyMulOneLeft(): Unit = {
    val x = Var(Slot.fresh(), Int32Type.toTree)
    val one = ConstInt32(1).toTree
    val product = Mul(one, x)

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(x).get, egraph3.find(product).get))
  }

  @Test
  def simplifyMulZeroLeft(): Unit = {
    val x = Var(Slot.fresh(), DoubleType.toTree)
    val zero = ConstDouble(0.0).toTree
    val product = Mul(zero, x)

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(zero).get, egraph3.find(product).get))
  }

  @Test
  def introduceAddZero(): Unit = {
    val x = Var(Slot.fresh(), DoubleType.toTree)
    val sum = x

    val (_, egraph2) = EGraphWithRoot.from(sum)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(sum).get, egraph3.find(Add(x, ConstDouble(0.0).toTree)).get))
  }

  @Test
  def introduceMulOneLeft(): Unit = {
    val x = Var(Slot.fresh(), DoubleType.toTree)
    val product = x

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(product).get, egraph3.find(Mul(ConstDouble(1.0).toTree, x)).get))
  }

  @Test
  def introduceMulOneRight(): Unit = {
    val x = Var(Slot.fresh(), Int32Type.toTree)
    val product = x

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(product).get, egraph3.find(Mul(x, ConstInt32(1).toTree)).get))
  }

  @Test
  def mulCommutes(): Unit = {
    val x = Var(Slot.fresh(), Int32Type.toTree)
    val y = Var(Slot.fresh(), Int32Type.toTree)
    val product = Mul(x, y)

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(product).get, egraph3.find(Mul(y, x)).get))
  }

  @Test
  def mulAssociates(): Unit = {
    val x = Var(Slot.fresh(), Int32Type.toTree)
    val y = Var(Slot.fresh(), Int32Type.toTree)
    val z = Var(Slot.fresh(), Int32Type.toTree)
    val product = Mul(Mul(x, y), z)

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(product).get, egraph3.find(Mul(x, Mul(y, z))).get))
  }

  @Test
  def mulAssociates2(): Unit = {
    val x = Var(Slot.fresh(), Int32Type.toTree)
    val y = Var(Slot.fresh(), Int32Type.toTree)
    val z = Var(Slot.fresh(), Int32Type.toTree)
    val product = Mul(x, Mul(y, z))

    val (_, egraph2) = EGraphWithRoot.from(product)

    val egraph3 = strategy(1)(egraph2).get

    assert(egraph3.areSame(egraph3.find(product).get, egraph3.find(Mul(Mul(x, y), z)).get))
  }
}
