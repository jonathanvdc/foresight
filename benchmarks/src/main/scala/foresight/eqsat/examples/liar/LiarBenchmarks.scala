package foresight.eqsat.examples.liar

import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.{EGraph, Slot}
import foresight.eqsat.saturation.{EGraphWithRoot, Strategy}
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class LiarBenchmarks extends BenchmarksWithParallelMap {
  type BaseEGraph = EGraphWithRoot[ArrayIR, EGraph[ArrayIR]]
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  private def coreRules: CoreRules[BaseEGraph] = CoreRules[BaseEGraph]()
  private def arithRules: ArithRules[BaseEGraph] = ArithRules[BaseEGraph]()
  private def blasIdiomRules: BlasIdiomRules[BaseEGraph] = BlasIdiomRules[BaseEGraph]()

  private def strategy(iterationLimit: Int,
                       rules: Seq[LiarRule] = coreRules.allWithConstArray ++ arithRules.all ++ blasIdiomRules.all): Strategy[ArrayIR, BaseEGraph, Unit] = {
    Strategies.naive(iterationLimit = Some(iterationLimit), rules = rules)
  }

  @Benchmark
  def findGemvInMv(): BaseEGraph = {
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

    val (c1, egraph2) = EGraphWithRoot.from(build)

    val egraph4 = strategy(4)(egraph2, parallelMap).get

    assert(egraph4.contains(gemv))
    assert(egraph4.areSame(c1, egraph4.find(gemv).get))

    egraph4
  }

  @Benchmark
  def findGemmInMm(): BaseEGraph = {
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

    val (c1, egraph2) = EGraphWithRoot.from(build)

    val egraph4 = strategy(4)(egraph2).get

    assert(egraph4.contains(gemm))
    assert(egraph4.areSame(c1, egraph4.find(gemm).get))

    egraph4
  }
}
