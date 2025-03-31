package foresight.eqsat.examples.liar.tests

import foresight.eqsat.examples.liar.CoreRules.LiarRule
import foresight.eqsat.examples.liar._
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.saturation.{MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EGraph, Slot}
import org.junit.{Ignore, Test}

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

  /**
   * Tests that we can find gemv in a matrix-vector multiplication.
   */
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

  /**
   * Tests that the gemm rule fires when the pattern is present.
   */
  @Test
  def findGemm(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val M = ConstIntType(100).toTree
    val N = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val c = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), M))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemm = BlasIdioms.Gemm(false, true)(alpha, a, b, beta, c)

    val build = {
      val i = Slot.fresh()
      Build(
        M,
        Lambda(
          i,
          Int32Type.toTree,
          BlasIdioms.Gemv(false)(
            alpha,
            b,
            IndexAt(a, Var(i, Int32Type.toTree)),
            beta,
            IndexAt(c, Var(i, Int32Type.toTree)))))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(1, rules = Seq(BlasIdiomRules.detectGemm))(egraph2).get

    assert(egraph4.contains(gemm))
    assert(egraph4.areSame(c1, egraph4.find(gemm).get))
  }

  /**
   * Tests that we can find gemm in a matrix-matrix multiplication. This is a slow test, so it is ignored by default.
   */
  @Ignore("Finding gemm in a matrix-matrix multiplication is slow.")
  @Test
  def findGemmInMm(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val M = ConstIntType(100).toTree
    val N = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val one = ConstDouble(1.0).toTree
    val zero = ConstDouble(0.0).toTree
    val zeroMat = Build(M, Lambda(Slot.fresh(), Int32Type.toTree, Build(K, Lambda(Slot.fresh(), Int32Type.toTree, zero))))

    val gemm = BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(one, a, b, one, zeroMat)

    val build = {
      val i = Slot.fresh()
      val j = Slot.fresh()
      Build(
        M,
        Lambda(
          i,
          Int32Type.toTree,
          Build(
            K,
            Lambda(
              j,
              Int32Type.toTree,
              BlasIdioms.Dot(IndexAt(b, Var(j, Int32Type.toTree)), IndexAt(a, Var(i, Int32Type.toTree)))))))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(6)(egraph2).get

    assert(egraph4.contains(gemm))
    assert(egraph4.areSame(c1, egraph4.find(gemm).get))
  }
}
