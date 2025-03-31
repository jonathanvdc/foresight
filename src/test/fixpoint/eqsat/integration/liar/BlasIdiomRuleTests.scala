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
      Ifold(
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

    val ddot = BlasIdioms.Dot(xs, ys)

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
      Ifold(
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

    val ddot = BlasIdioms.Dot(xs, ys)

    val (_, egraph2) = egraph.add(dotProduct)

    val egraph4 = strategy(2)(egraph2).get

    assert(!egraph4.contains(ddot))
  }

  /**
   * Tests that the axpy rule fires when the pattern is present.
   */
  @Test
  def findAxpy(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree

    val a = Var(Slot.fresh(), DoubleType.toTree)
    val xs = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val ys = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))

    val axpy = BlasIdioms.Axpy(a, xs, ys)

    val build = {
      val i = Slot.fresh()
      Build(
        N,
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              a,
              IndexAt(xs, Var(i, Int32Type.toTree))),
            IndexAt(ys, Var(i, Int32Type.toTree)))))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(2)(egraph2).get

    assert(egraph4.contains(axpy))
    assert(egraph4.areSame(c1, egraph4.find(axpy).get))
  }

  @Test
  def findGemv(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val K = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val x = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val y = Var(Slot.fresh(), ArrayType(DoubleType.toTree, K))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemv = BlasIdioms.Gemv(false)(alpha, a, x, beta, y)

    val build = {
      val i = Slot.fresh()
      Build(
        K,
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              alpha,
              BlasIdioms.Dot(IndexAt(a, Var(i, Int32Type.toTree)), x)),
            Mul(
              beta,
              IndexAt(y, Var(i, Int32Type.toTree))))))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(1, rules = Seq(BlasIdiomRules.detectGemv))(egraph2).get

    assert(egraph4.contains(gemv))
    assert(egraph4.areSame(c1, egraph4.find(gemv).get))
  }

  @Test
  def findGemvInMv(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val K = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val x = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val one = ConstDouble(1.0).toTree
    val zero = ConstDouble(0.0).toTree
    val zeroVec = Build(K, Lambda(Slot.fresh(), Int32Type.toTree, zero))

    val gemv = BlasIdioms.Gemv(false)(one, a, x, one, zeroVec)

    val build = {
      val i = Slot.fresh()
      Build(
        K,
        Lambda(
          i,
          Int32Type.toTree,
          BlasIdioms.Dot(IndexAt(a, Var(i, Int32Type.toTree)), x)))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(4)(egraph2).get

    assert(egraph4.contains(gemv))
    assert(egraph4.areSame(c1, egraph4.find(gemv).get))
  }
}
