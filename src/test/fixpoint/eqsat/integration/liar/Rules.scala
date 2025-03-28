package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.EGraph
import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.{Pattern, PatternMatch, SlotVar}
import fixpoint.eqsat.integration.liar.SearcherOps._

object Rules {
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

  def arithmeticRules: Seq[LiarRule] = Seq(
    simplifyAddZeroRight,
    simplifyMulOneRight,
    simplifyMulOneLeft,
    simplifyMulZeroLeft,
    introduceAddZero,
    introduceMulOneLeft,
    introduceMulOneRight,
    mulCommutativity,
    mulAssociativity1,
    mulAssociativity2
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

  val simplifyAddZeroRight: LiarRule = {
    // x + 0 -> x
    val x = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x + 0 -> x",
      Add(x, ConstDouble(0.0).toPattern).toSearcher,
      x.toApplier)
  }

  val simplifyMulOneRight: LiarRule = {
    // x * 1 -> x
    val x = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x * 1 -> x",
      Mul(x, ConstDouble(1.0).toPattern).toSearcher,
      x.toApplier)
  }

  val simplifyMulOneLeft: LiarRule = {
    // 1 * x -> x
    val x = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "1 * x -> x",
      Mul(ConstInt32(1).toPattern, x).toSearcher,
      x.toApplier)
  }

  val simplifyMulZeroLeft: LiarRule = {
    // 0 * x -> 0
    val x = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "0 * x -> 0",
      Mul(ConstDouble(0.0).toPattern, x).toSearcher,
      ConstDouble(0.0).toPattern.toApplier)
  }

  val introduceAddZero: LiarRule = {
    // x -> x + 0
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> x + 0",
      x.toSearcher.bindTypes(Map(x -> xType)).requireDoubleType(xType),
      Add(x, ConstDouble(0.0).toPattern).toApplier)
  }

  val introduceMulOneLeft: LiarRule = {
    // x -> 1 * x
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> 1 * x",
      x.toSearcher.bindTypes(Map(x -> xType)).requireDoubleType(xType),
      Mul(ConstDouble(1.0).toPattern, x).toApplier)
  }

  val introduceMulOneRight: LiarRule = {
    // x -> x * 1
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> x * 1",
      x.toSearcher.bindTypes(Map(x -> xType)).requireInt32Type(xType),
      Mul(x, ConstInt32(1).toPattern).toApplier)
  }

  val mulCommutativity: LiarRule = {
    // x * y -> y * x
    val x = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x * y -> y * x",
      Mul(x, y).toSearcher,
      Mul(y, x).toApplier)
  }

  val mulAssociativity1: LiarRule = {
    // x * (y * z) -> (x * y) * z
    val x = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()
    val z = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x * (y * z) -> (x * y) * z",
      Mul(x, Mul(y, z)).toSearcher,
      Mul(Mul(x, y), z).toApplier)
  }

  val mulAssociativity2: LiarRule = {
    // (x * y) * z -> x * (y * z)
    val x = Pattern.Var.fresh[ArrayIR]()
    val y = Pattern.Var.fresh[ArrayIR]()
    val z = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "(x * y) * z -> x * (y * z)",
      Mul(Mul(x, y), z).toSearcher,
      Mul(x, Mul(y, z)).toApplier)
  }
}
