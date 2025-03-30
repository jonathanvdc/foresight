package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{EGraph, Slot}
import fixpoint.eqsat.extraction.ExtractionAnalysis
import fixpoint.eqsat.integration.liar.CoreRules.LiarRule
import fixpoint.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import org.junit.Test

class BlasIdiomRuleTests {
  private def strategy(iterationLimit: Int,
                       rules: Seq[LiarRule] = CoreRules.all ++ ArithRules.all ++ BlasIdiomRules.all): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData

  /**
   * Tests that the ddot rule fires when the pattern is present.
   */
  @Test
  def findDDot(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree

    val xs = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val ys = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))

    val dotProduct = {
      val i = Slot.fresh()
      val acc = Slot.fresh()
      IFold(
        N,
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            DoubleType.toTree,
            Add(
              Mul(
                IndexAt(xs, Var(i, Int32Type.toTree)),
                IndexAt(ys, Var(i, Int32Type.toTree))),
              Var(acc, DoubleType.toTree)))))
    }

    val ddot = BlasIdioms.DDot(xs, ys)

    val (c1, egraph2) = egraph.add(dotProduct)

    val egraph4 = strategy(2)(egraph2).get

    assert(egraph4.contains(ddot))
    assert(egraph4.areSame(c1, egraph4.find(ddot).get))
  }

  /**
   * Tests that the ddot rule does not fire when the pattern is not present due to ys not being independent of the
   * accumulator.
   */
  @Test
  def doNotFindDDot(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree

    val i = Slot.fresh()
    val acc = Slot.fresh()
    val xs = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val ys = Build(N, Lambda(Slot.fresh(), Int32Type.toTree, Var(acc, DoubleType.toTree)))

    val dotProduct = {
      IFold(
        N,
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            DoubleType.toTree,
            Add(
              Mul(
                IndexAt(xs, Var(i, Int32Type.toTree)),
                IndexAt(ys, Var(i, Int32Type.toTree))),
              Var(acc, DoubleType.toTree)))))
    }

    val ddot = BlasIdioms.DDot(xs, ys)

    val (_, egraph2) = egraph.add(dotProduct)

    val egraph4 = strategy(2)(egraph2).get

    assert(!egraph4.contains(ddot))
  }
}
