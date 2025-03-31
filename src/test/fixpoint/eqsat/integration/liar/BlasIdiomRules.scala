package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{MixedTree, Slot}
import fixpoint.eqsat.integration.liar.CoreRules.{LiarEGraph, LiarRule}
import fixpoint.eqsat.integration.liar.SearcherOps.SearcherOfMetadataPatternMatchOps
import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.Pattern

object BlasIdiomRules {
  def all: Seq[LiarRule] = Seq(
    detectDot,
    detectAxpy,
    detectGemv)

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
}
