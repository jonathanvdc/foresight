package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{MixedTree, Slot}
import fixpoint.eqsat.integration.liar.CoreRules.{LiarEGraph, LiarRule}
import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.Pattern

object BlasIdiomRules {
  def all: Seq[LiarRule] = Seq(
    detectDot,
    detectAxpy)

  val detectDot: LiarRule = {
    // ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys
    val N = Pattern.Var.fresh[ArrayIR]()
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
        .requireIndependent(ys, i, acc),
      BlasIdioms.Dot(MixedTree.Call(xs), MixedTree.Call(ys)).toApplier)
  }

  val detectAxpy: LiarRule = {
    // build N (λi. a * xs[i] + ys[i]) -> axpy a xs ys
    val N = Pattern.Var.fresh[ArrayIR]()
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
        .requireIndependent(ys, i),
      BlasIdioms.Axpy(MixedTree.Call(a), MixedTree.Call(xs), MixedTree.Call(ys)).toApplier)
  }
}
