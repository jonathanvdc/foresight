package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.rewriting.Rule
import fixpoint.eqsat.rewriting.patterns.Pattern
import fixpoint.eqsat.integration.liar.SearcherOps._

object ArithRules {
  import fixpoint.eqsat.integration.liar.CoreRules.LiarRule

  def allRules: Seq[LiarRule] = Seq(
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
