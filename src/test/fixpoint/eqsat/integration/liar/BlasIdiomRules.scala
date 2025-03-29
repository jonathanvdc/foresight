package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.{MixedTree, Slot}
import fixpoint.eqsat.integration.liar.CoreRules.{LiarEGraph, LiarRule}
import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.Pattern

object BlasIdiomRules {
  def all: Seq[LiarRule] = Seq(detectDDot)

  val detectDDot: LiarRule = {
    // ifold N (λacc i. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys
    val N = Pattern.Var.fresh[ArrayIR]()
    val xs = Pattern.Var.fresh[ArrayIR]()
    val ys = Pattern.Var.fresh[ArrayIR]()
    val acc = Slot.fresh()
    val i = Slot.fresh()

    Rule(
      "ifold N (λi acc. xs[i] * ys[i] + acc) 0.0 -> ddot xs ys",
      IFold(
        MixedTree.Call(N),
        ConstDouble(0.0).toTree,
        Lambda(
          i,
          Int32Type.toTree,
          Lambda(
            acc,
            DoubleType.toTree,
            Add(
              Mul(
                IndexAt(MixedTree.Call(xs), Var(i, Int32Type.toTree)),
                IndexAt(MixedTree.Call(ys), Var(i, Int32Type.toTree))),
              Var(acc, DoubleType.toTree)))))
        .toSearcher[LiarEGraph]
        .requireIndependent(xs, i, acc)
        .requireIndependent(ys, i, acc),
      BlasIdioms.DDot(MixedTree.Call(xs), MixedTree.Call(ys)).toApplier)
  }
}
