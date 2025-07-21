package foresight.eqsat.examples.liar.tests

import foresight.eqsat.examples.liar.CoreRules.LiarRule
import foresight.eqsat.examples.liar._
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.saturation.{EGraphWithRoot, MaximalRuleApplicationWithCaching, RebasingStrategies, Strategy}
import foresight.eqsat.{EGraph, MixedTree, Slot, Tree}
import org.junit.{Ignore, Test}

class BlasIdiomRuleTest {
  private def strategy(iterationLimit: Int,
                       rules: Seq[LiarRule] = CoreRules.allWithConstArray ++ ArithRules.all ++ BlasIdiomRules.all): Strategy[EGraph[ArrayIR], Unit] =
    MaximalRuleApplicationWithCaching(rules)
      .withIterationLimit(iterationLimit)
      .untilFixpoint
      .closeRecording
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(TimeComplexity.analysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData

  private def isariaStrategy(rules: Seq[LiarRule] = CoreRules.allWithConstArray ++ ArithRules.all ++ BlasIdiomRules.all): Strategy[EGraphWithRoot[ArrayIR, EGraph[ArrayIR]], Unit] = {
    RebasingStrategies.isaria(
      TimeComplexity.analysis.extractor[EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]],
      ???,
      ???,
      ???,
      (left: Tree[ArrayIR], right: Tree[ArrayIR]) => TimeComplexity(left) == TimeComplexity(right))
      .addAnalysis(ExtractionAnalysis.smallest[ArrayIR])
      .addAnalysis(TimeComplexity.analysis)
      .addAnalysis(TypeInferenceAnalysis)
      .closeMetadata
      .dropData
  }

  @Test
  def findMemsetZero(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree

    val zeroArray = {
      val i = Slot.fresh()
      Build(
        N,
        Lambda(
          i,
          Int32Type.toTree,
          ConstDouble(0.0).toTree))
    }

    val memsetZero = BlasIdioms.Memset(N, ConstDouble(0.0).toTree)

    val (c1, egraph2) = egraph.add(zeroArray)

    val egraph4 = strategy(1)(egraph2).get

    assert(egraph4.contains(memsetZero))
    assert(egraph4.areSame(c1, egraph4.find(memsetZero).get))
  }

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

    val gemm = BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(alpha, a, b, beta, c)

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

  @Test
  def expandGemm(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val M = ConstIntType(100).toTree
    val N = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val c = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), M))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemm = BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(alpha, a, b, beta, c)

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

    val (c1, egraph2) = egraph.add(gemm)

    val egraph4 = strategy(1, rules = Seq(BlasIdiomRules.detectGemm.tryReverse.get))(egraph2).get

    assert(egraph4.contains(build))
    assert(egraph4.areSame(c1, egraph4.find(build).get))
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

  @Ignore("Finding idioms in the gemm kernel is slow.")
  @Test
  def findGemmInGemmKernel(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val M = ConstIntType(100).toTree
    val N = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), N))
    val c = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), M))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemm = BlasIdioms.Gemm(aTransposed = false, bTransposed = false)(alpha, a, b, beta, c)

    val build = Lib.matrixAdd(
      Lib.matrixMatrixProduct(
        Lib.matrixScalarProduct(a, alpha),
        b),
      Lib.matrixScalarProduct(c, beta))

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(5)(egraph2).get

    val best = MixedTree.fromTree(TimeComplexity.analysis(egraph4)(c1, egraph4).applied.toTree)

    assert(best == gemm, s"Best: $best, expected: $gemm")
    assert(egraph4.contains(gemm))
    assert(egraph4.areSame(c1, egraph4.find(gemm).get))
  }

  @Test
  def findTranspose(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val M = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))

    val transpose = BlasIdioms.Transpose(a)

    val build = {
      val i = Slot.fresh()
      val j = Slot.fresh()
      Build(
        N,
        Lambda(
          i,
          Int32Type.toTree,
          Build(
            M,
            Lambda(
              j,
              Int32Type.toTree,
              IndexAt(IndexAt(a, Var(j, Int32Type.toTree)), Var(i, Int32Type.toTree))))))
    }

    val (c1, egraph2) = egraph.add(build)

    val egraph4 = strategy(1)(egraph2).get

    assert(egraph4.contains(transpose))
    assert(egraph4.areSame(c1, egraph4.find(transpose).get))
  }

  @Test
  def hoistLhsMulFromDot(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree

    val xs = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val ys = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val a = Var(Slot.fresh(), DoubleType.toTree)

    val i = Slot.fresh()

    val dotProduct = BlasIdioms.Dot(
      Build(
        N,
        Lambda(
          i,
          Int32Type.toTree,
          Mul(
            a,
            IndexAt(xs, Var(i, Int32Type.toTree))))),
      ys)

    val dotProduct2 = Mul(a, BlasIdioms.Dot(xs, ys))

    val (c1, egraph2) = egraph.add(dotProduct)

    val egraph4 = strategy(1)(egraph2).get

    assert(egraph4.contains(dotProduct2))
    assert(egraph4.areSame(c1, egraph4.find(dotProduct2).get))
  }

  @Test
  def foldTransposeIntoGemvF(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val K = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), N))
    val x = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val y = Var(Slot.fresh(), ArrayType(DoubleType.toTree, K))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemvT = BlasIdioms.Gemv(true)(alpha, a, x, beta, y)
    val gemvF = BlasIdioms.Gemv(false)(alpha, BlasIdioms.Transpose(a), x, beta, y)

    val (c1, egraph2) = egraph.add(gemvF)

    val egraph4 = strategy(1)(egraph2).get

    assert(egraph4.contains(gemvT))
    assert(egraph4.areSame(c1, egraph4.find(gemvT).get))
  }

  @Test
  def foldTransposeIntoGemvT(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val K = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val x = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val y = Var(Slot.fresh(), ArrayType(DoubleType.toTree, K))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemvT = BlasIdioms.Gemv(true)(alpha, BlasIdioms.Transpose(a), x, beta, y)
    val gemvF = BlasIdioms.Gemv(false)(alpha, a, x, beta, y)

    val (c1, egraph2) = egraph.add(gemvT)

    val egraph4 = strategy(1)(egraph2).get

    assert(egraph4.contains(gemvF))
    assert(egraph4.areSame(c1, egraph4.find(gemvF).get))
  }

  @Test
  def foldTransposeABIntoGemmFF(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val M = ConstIntType(100).toTree
    val N = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, M), N))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val c = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), M))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemmFF = BlasIdioms.Gemm(aTransposed = false, bTransposed = false)(
      alpha, BlasIdioms.Transpose(a), BlasIdioms.Transpose(b), beta, c)

    val gemmFT = BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(
      alpha, BlasIdioms.Transpose(a), b, beta, c)

    val gemmTF = BlasIdioms.Gemm(aTransposed = true, bTransposed = false)(
      alpha, a, BlasIdioms.Transpose(b), beta, c)

    val gemmTT = BlasIdioms.Gemm(aTransposed = true, bTransposed = true)(alpha, a, b, beta, c)

    val (c1, egraph2) = egraph.add(gemmFF)

    val egraph3 = strategy(2)(egraph2).get

    assert(egraph3.contains(gemmFT))
    assert(egraph3.contains(gemmTF))
    assert(egraph3.areSame(c1, egraph3.find(gemmFT).get))
    assert(egraph3.areSame(c1, egraph3.find(gemmTF).get))

    assert(egraph3.contains(gemmTT))
    assert(egraph3.areSame(c1, egraph3.find(gemmTT).get))
  }

  /**
   * Tests that, even after saturating, gemv is the best form of the idiom.
   */
  @Test
  def gemvIsOptimal(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val K = ConstIntType(200).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), K))
    val x = Var(Slot.fresh(), ArrayType(DoubleType.toTree, N))
    val y = Var(Slot.fresh(), ArrayType(DoubleType.toTree, K))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemv = BlasIdioms.Gemv(aTransposed = false)(alpha, a, x, beta, y)

    val (c1, egraph2) = egraph.add(gemv)

    val egraph4 = strategy(7)(egraph2).get

    assert(MixedTree.fromTree(TimeComplexity.analysis(egraph4)(c1, egraph4).applied.toTree) == gemv)
  }

  /**
   * Tests that, even after saturating, gemm is the best form of the idiom.
   */
  @Test
  def gemmIsOptimal(): Unit = {
    val egraph = EGraph.empty[ArrayIR]

    val N = ConstIntType(100).toTree
    val M = ConstIntType(200).toTree
    val K = ConstIntType(300).toTree

    val a = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, N), M))
    val b = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), N))
    val c = Var(Slot.fresh(), ArrayType(ArrayType(DoubleType.toTree, K), M))
    val alpha = Var(Slot.fresh(), DoubleType.toTree)
    val beta = Var(Slot.fresh(), DoubleType.toTree)

    val gemm = BlasIdioms.Gemm(aTransposed = false, bTransposed = false)(alpha, a, b, beta, c)

    val (c1, egraph2) = egraph.add(gemm)

    val egraph4 = strategy(6)(egraph2).get

    assert(MixedTree.fromTree(TimeComplexity.analysis(egraph4)(c1, egraph4).applied.toTree) == gemm)
  }
}
