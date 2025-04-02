package foresight.eqsat.examples.liar

import CoreRules.{LiarEGraph, LiarRule}
import SearcherOps.SearcherOfMetadataPatternMatchOps
import foresight.eqsat.{MixedTree, Slot}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.Pattern

object BlasIdiomRules {
  def all: Seq[LiarRule] = detectionRules ++ transformationRules

  def detectionRules: Seq[LiarRule] = Seq(
    detectMemsetZero,
    detectDot,
    detectAxpy,
    detectGemv,
    detectGemm,
    detectTranspose)

  def transformationRules: Seq[LiarRule] = Seq(
    hoistLhsMulFromDot,
    foldTransposeIntoGemvN,
    foldTransposeIntoGemvT)

  val detectMemsetZero: LiarRule = {
    // build N (λi. 0.0) -> memset N 0.0
    val N = Pattern.Var.fresh[ArrayIR]()

    val zero = ConstDouble(0.0).toTree
    val i = Slot.fresh()

    Rule(
      "build N (λi. 0.0) -> memset N 0",
      Build(
        MixedTree.Call(N),
        Lambda(
          i,
          Int32Type.toTree,
          ConstDouble(0.0).toTree))
        .toSearcher[LiarEGraph],
      BlasIdioms.Memset(MixedTree.Call(N), zero).toApplier)
  }

  val detectDot: LiarRule = {
    // ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys
    val N = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val xs = Pattern.Var.fresh[ArrayIR]()
    val ys = Pattern.Var.fresh[ArrayIR]()
    val accType = Pattern.Var.fresh[ArrayIR]()
    val acc = Slot.fresh()
    val i = Slot.fresh()

    Rule(
      "ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys",
      Ifold(
        MixedTree.Call(N),
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            MixedTree.Call(accType),
            Add(
              Mul(
                IndexAt(MixedTree.Call(xs), Var(i, Int32Type.toTree)),
                IndexAt(MixedTree.Call(ys), Var(i, Int32Type.toTree))),
              Var(acc, MixedTree.Call(accType))))))
        .toSearcher[LiarEGraph]
        .requireIndependent(xs, i, acc)
        .requireIndependent(ys, i, acc)
        .requireTypes(Map(
          xs -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          ys -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          accType -> MixedTree.Call(scalarType))),
      BlasIdioms.Dot(MixedTree.Call(xs), MixedTree.Call(ys)).toApplier)
  }

  val detectAxpy: LiarRule = {
    // build N (λi. a * xs[i] + ys[i]) -> axpy a xs ys
    val N = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val a = Pattern.Var.fresh[ArrayIR]()
    val xs = Pattern.Var.fresh[ArrayIR]()
    val ys = Pattern.Var.fresh[ArrayIR]()
    val i = Slot.fresh()

    Rule(
      "build N (λi. a * xs[i] + ys[i]) -> axpy a xs ys",
      Build(
        MixedTree.Call(N),
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              MixedTree.Call(a),
              IndexAt(MixedTree.Call(xs), Var(i, Int32Type.toTree))),
            IndexAt(MixedTree.Call(ys), Var(i, Int32Type.toTree)))))
        .toSearcher[LiarEGraph]
        .requireIndependent(a, i)
        .requireIndependent(xs, i)
        .requireIndependent(ys, i)
        .requireTypes(Map(
          a -> MixedTree.Call(scalarType),
          xs -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          ys -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)))),
      BlasIdioms.Axpy(MixedTree.Call(a), MixedTree.Call(xs), MixedTree.Call(ys)).toApplier)
  }

  val detectGemv: LiarRule = {
    // build N (λi. alpha * (dot a[i] x) + beta * y[i]) -> gemv alpha a x beta y
    val N = Pattern.Var.fresh[ArrayIR]()
    val K = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val alpha = Pattern.Var.fresh[ArrayIR]()
    val a = Pattern.Var.fresh[ArrayIR]()
    val x = Pattern.Var.fresh[ArrayIR]()
    val beta = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()

    val i = Slot.fresh()

    Rule(
      "build K (λi. alpha * (dot a[i] x) + beta * y[i]) -> gemv alpha a x beta y",
      Build(
        MixedTree.Call(K),
        Lambda(
          i,
          Int32Type.toTree,
          Add(
            Mul(
              MixedTree.Call(alpha),
              BlasIdioms.Dot(IndexAt(MixedTree.Call(a), Var(i, Int32Type.toTree)), MixedTree.Call(x))),
            Mul(
              MixedTree.Call(beta),
              IndexAt(MixedTree.Call(y), Var(i, Int32Type.toTree))))))
        .toSearcher[LiarEGraph]
        .requireIndependent(alpha, i)
        .requireIndependent(a, i)
        .requireIndependent(x, i)
        .requireIndependent(beta, i)
        .requireIndependent(y, i)
        .requireTypes(Map(
          alpha -> MixedTree.Call(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)), MixedTree.Call(K)),
          x -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          beta -> MixedTree.Call(scalarType),
          y -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(K)))),
      BlasIdioms.Gemv(false)(
        MixedTree.Call(alpha),
        MixedTree.Call(a),
        MixedTree.Call(x),
        MixedTree.Call(beta),
        MixedTree.Call(y)).toApplier)
  }

  val detectGemm: LiarRule = {
    // build M (λi. gemvN alpha b a[i] beta c[i]) -> gemmNT alpha a b beta c
    val M = Pattern.Var.fresh[ArrayIR]()
    val N = Pattern.Var.fresh[ArrayIR]()
    val K = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val alpha = Pattern.Var.fresh[ArrayIR]()
    val b = Pattern.Var.fresh[ArrayIR]()
    val a = Pattern.Var.fresh[ArrayIR]()
    val beta = Pattern.Var.fresh[ArrayIR]()
    val c = Pattern.Var.fresh[ArrayIR]()

    val i = Slot.fresh()

    Rule(
      "build M (λi. gemvN alpha b a[i] beta c[i]) -> gemmNT alpha a b beta c",
      Build(
        MixedTree.Call(M),
        Lambda(
          i,
          Int32Type.toTree,
          BlasIdioms.Gemv(false)(
            MixedTree.Call(alpha),
            MixedTree.Call(b),
            IndexAt(MixedTree.Call(a), Var(i, Int32Type.toTree)),
            MixedTree.Call(beta),
            IndexAt(MixedTree.Call(c), Var(i, Int32Type.toTree)))))
        .toSearcher[LiarEGraph]
        .requireIndependent(alpha, i)
        .requireIndependent(a, i)
        .requireIndependent(b, i)
        .requireIndependent(beta, i)
        .requireIndependent(c, i)
        .requireTypes(Map(
          alpha -> MixedTree.Call(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)), MixedTree.Call(M)),
          b -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)), MixedTree.Call(K)),
          beta -> MixedTree.Call(scalarType),
          c -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(K)), MixedTree.Call(M)))),
      BlasIdioms.Gemm(aTransposed = false, bTransposed = true)(
        MixedTree.Call(alpha),
        MixedTree.Call(a),
        MixedTree.Call(b),
        MixedTree.Call(beta),
        MixedTree.Call(c)).toApplier)
  }

  def detectTranspose: LiarRule = {
    // build N (λi. build M (λj. a[i][j])) -> transpose a

    val N = Pattern.Var.fresh[ArrayIR]()
    val M = Pattern.Var.fresh[ArrayIR]()

    val a = Pattern.Var.fresh[ArrayIR]()

    val i = Slot.fresh()
    val j = Slot.fresh()

    Rule(
      "build N (λi. build M (λj. a[i][j])) -> transpose a",
      Build(
        MixedTree.Call(N),
        Lambda(
          i,
          Int32Type.toTree,
          Build(
            MixedTree.Call(M),
            Lambda(
              j,
              Int32Type.toTree,
              IndexAt(IndexAt(MixedTree.Call(a), Var(i, Int32Type.toTree)), Var(j, Int32Type.toTree))))))
        .toSearcher[LiarEGraph]
        .requireIndependent(a, i, j)
        .requireTypes(Map(
          a -> ArrayType(ArrayType(DoubleType.toTree, MixedTree.Call(N)), MixedTree.Call(M)))),
      BlasIdioms.Transpose(MixedTree.Call(a)).toApplier)
  }

  def hoistLhsMulFromDot: LiarRule = {
    // dot (build N (λi. a * xs[i])) ys -> a * (dot xs ys)
    val N = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val xs = Pattern.Var.fresh[ArrayIR]()
    val ys = Pattern.Var.fresh[ArrayIR]()
    val a = Pattern.Var.fresh[ArrayIR]()

    val i = Slot.fresh()

    Rule(
      "dot (build N (λi. a * xs[i])) ys -> a * (dot xs ys)",
      BlasIdioms.Dot(
        Build(
          MixedTree.Call(N),
          Lambda(
            i,
            Int32Type.toTree,
            Mul(
              MixedTree.Call(a),
              IndexAt(MixedTree.Call(xs), Var(i, Int32Type.toTree))))),
        MixedTree.Call(ys))
        .toSearcher[LiarEGraph]
        .requireIndependent(a, i)
        .requireIndependent(xs, i)
        .requireTypes(Map(
          a -> MixedTree.Call(scalarType),
          xs -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          ys -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)))),
      Mul(MixedTree.Call(a), BlasIdioms.Dot(MixedTree.Call(xs), MixedTree.Call(ys))).toApplier)
  }

  def foldTransposeIntoGemvN: LiarRule = {
    // gemvN alpha (transpose a) x beta y -> gemvT alpha a x beta y
    val N = Pattern.Var.fresh[ArrayIR]()
    val K = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val alpha = Pattern.Var.fresh[ArrayIR]()
    val a = Pattern.Var.fresh[ArrayIR]()
    val x = Pattern.Var.fresh[ArrayIR]()
    val beta = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()

    Rule(
      "gemvN alpha (transpose a) x beta y -> gemvT alpha a x beta y",
      BlasIdioms.Gemv(false)(
        MixedTree.Call(alpha),
        BlasIdioms.Transpose(MixedTree.Call(a)),
        MixedTree.Call(x),
        MixedTree.Call(beta),
        MixedTree.Call(y))
        .toSearcher[LiarEGraph]
        .requireTypes(Map(
          alpha -> MixedTree.Call(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(K)), MixedTree.Call(N)),
          x -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          beta -> MixedTree.Call(scalarType),
          y -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(K)))),
      BlasIdioms.Gemv(true)(
        MixedTree.Call(alpha),
        MixedTree.Call(a),
        MixedTree.Call(x),
        MixedTree.Call(beta),
        MixedTree.Call(y)).toApplier)
  }

  def foldTransposeIntoGemvT: LiarRule = {
    // gemvT alpha (transpose a) x beta y -> gemvN alpha a x beta y
    val N = Pattern.Var.fresh[ArrayIR]()
    val K = Pattern.Var.fresh[ArrayIR]()
    val scalarType = Pattern.Var.fresh[ArrayIR]()

    val alpha = Pattern.Var.fresh[ArrayIR]()
    val a = Pattern.Var.fresh[ArrayIR]()
    val x = Pattern.Var.fresh[ArrayIR]()
    val beta = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()

    Rule(
      "gemvT alpha (transpose a) x beta y -> gemvN alpha a x beta y",
      BlasIdioms.Gemv(true)(
        MixedTree.Call(alpha),
        BlasIdioms.Transpose(MixedTree.Call(a)),
        MixedTree.Call(x),
        MixedTree.Call(beta),
        MixedTree.Call(y))
        .toSearcher[LiarEGraph]
        .requireTypes(Map(
          alpha -> MixedTree.Call(scalarType),
          a -> ArrayType(ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)), MixedTree.Call(K)),
          x -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(N)),
          beta -> MixedTree.Call(scalarType),
          y -> ArrayType(MixedTree.Call(scalarType), MixedTree.Call(K)))),
      BlasIdioms.Gemv(false)(
        MixedTree.Call(alpha),
        MixedTree.Call(a),
        MixedTree.Call(x),
        MixedTree.Call(beta),
        MixedTree.Call(y)).toApplier)
  }
}
