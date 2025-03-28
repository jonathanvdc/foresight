package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.EGraph
import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.{Pattern, PatternMatch, SlotVar}
import fixpoint.eqsat.integration.liar.SearcherOps._

object CoreRules {
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], EGraph[ArrayIR]]

  def introductionRules: Seq[LiarRule] = Seq(
    introduceLambda,
    introduceIndexBuild
  )

  def eliminationRules: Seq[LiarRule] = Seq(
    eliminateIndexBuild,
    eliminateFstTuple,
    eliminateSndTuple
  )

  val introduceLambda: LiarRule = {
    // e -> (λx. e) y
    val e = Pattern.Var.fresh[ArrayIR]()
    val eType = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()
    val yType = Pattern.Var.fresh[ArrayIR]()
    val x = SlotVar.fresh()
    Rule(
      "e -> (λx. e) y",
      e.toSearcher
        .bindTypes(Map(e -> eType))
        .requireNonFunctionType(eType)
        .product(y.toSearcher.bindTypes(Map(y -> yType)).requireNonFunctionType(yType))
        .merge,
      Apply(Lambda(x, yType, e), y).toApplier)
  }

  val introduceIndexBuild: LiarRule = {
    // f i -> (build f N)[i]
    val f = Pattern.Var.fresh[ArrayIR]()
    val N = Pattern.Var.fresh[ArrayIR]()
    val i = Pattern.Var.fresh[ArrayIR]()
    val iType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "f i -> (build f N)[i]",
      Apply(f, i).toSearcher
        .bindTypes(Map(i -> iType))
        .requireInt32Type(iType)
        .product(ArrayType(Pattern.Var.fresh[ArrayIR](), N).toSearcher)
        .merge,
      IndexAt(Build(N, f), i).toApplier)
  }

  val eliminateIndexBuild: LiarRule = {
    // (build f N)[i] -> f i
    val f = Pattern.Var.fresh[ArrayIR]()
    val N = Pattern.Var.fresh[ArrayIR]()
    val i = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "(build f N)[i] -> f i",
      IndexAt(Build(N, f), i).toSearcher,
      Apply(f, i).toApplier)
  }

  val eliminateFstTuple: LiarRule = {
    // fst (tuple a b) -> a
    val a = Pattern.Var.fresh[ArrayIR]()
    val b = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "fst (tuple a b) -> a",
      Fst(Tuple(a, b)).toSearcher,
      a.toApplier)
  }

  val eliminateSndTuple: LiarRule = {
    // snd (tuple a b) -> b
    val a = Pattern.Var.fresh[ArrayIR]()
    val b = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "snd (tuple a b) -> b",
      Snd(Tuple(a, b)).toSearcher,
      b.toApplier)
  }
}
