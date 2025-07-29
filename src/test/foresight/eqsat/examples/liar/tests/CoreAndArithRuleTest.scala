package foresight.eqsat.examples.liar.tests

import foresight.eqsat.examples.liar._
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EGraph, Slot}
import org.junit.Test

class CoreAndArithRuleTest {
  type BaseEGraph = EGraph[ArrayIR]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  private def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()
  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()

  private def strategy(iterationLimit: Int, rules: Seq[LiarRule] = coreRules.all ++ arithRules.all): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .repeatUntilStable
      .closeRecording
      .addAnalyses(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalyses(TypeInferenceAnalysis)
      .closeMetadata
      .dropData

  @Test
  def oneToVector(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val one = ConstDouble(1.0).toTree
    val zero = ConstInt32(0).toTree

    val x = Slot.fresh()
    val vector = IndexAt(Build(ConstIntType(100).toTree, Lambda(x, Int32Type.toTree, one)), zero)

    val (c, egraph2) = egraph.add(one)
    val (c2, egraph3) = egraph2.add(zero)
    val (c3, egraph4) = egraph3.add(ArrayType(DoubleType.toTree, ConstIntType(100).toTree))

    val egraph5 = strategy(2, rules = coreRules.all)(egraph4).get

    assert(egraph5.contains(vector))
    assert(egraph5.areSame(c, egraph5.find(vector).get))
  }

  @Test
  def vectorSumIsDotProduct(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val xs = Var(Slot.fresh(), ArrayType(DoubleType.toTree, ConstIntType(100).toTree))
    val vectorSum = {
      val i = Slot.fresh()
      val acc = Slot.fresh()
      Ifold(
        ConstIntType(100).toTree,
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            DoubleType.toTree,
            Add(
              Var(acc, DoubleType.toTree),
              IndexAt(xs, Var(i, Int32Type.toTree))))))
    }

    val ones = Build(ConstIntType(100).toTree, Lambda(Slot.fresh(), Int32Type.toTree, ConstDouble(1.0).toTree))
    val dotProduct = {
      val i = Slot.fresh()
      val acc = Slot.fresh()
      Ifold(
        ConstIntType(100).toTree,
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            DoubleType.toTree,
            Add(
              Var(acc, DoubleType.toTree),
              Mul(
                IndexAt(ones, Var(i, Int32Type.toTree)),
                IndexAt(xs, Var(i, Int32Type.toTree)))))))
    }

    val (c, egraph2) = egraph.add(vectorSum)
    val egraph3 = strategy(3)(egraph2).get

    assert(egraph3.contains(dotProduct))
    assert(egraph3.areSame(c, egraph3.find(dotProduct).get))
  }
}
