package foresight.eqsat.examples.liar

import SearcherOps._
import ApplierOps._
import foresight.eqsat.MixedTree
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.Pattern

object ArithRules {
  import CoreRules.{LiarRule,LiarEGraph}

  def all: Seq[LiarRule] = Seq(
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
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x + 0 -> x",
      Add(x, ConstDouble(0.0).toTree).toSearcher,
      x.toApplier[LiarEGraph].typeChecked)
  }

  val simplifyMulOneRight: LiarRule = {
    // x * 1 -> x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * 1 -> x",
      Mul(x, ConstDouble(1.0).toTree).toSearcher,
      x.toApplier[LiarEGraph].typeChecked)
  }

  val simplifyMulOneLeft: LiarRule = {
    // 1 * x -> x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "1 * x -> x",
      Mul(ConstInt32(1).toTree, x).toSearcher,
      x.toApplier[LiarEGraph].typeChecked)
  }

  val simplifyMulZeroLeft: LiarRule = {
    // 0 * x -> 0
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "0 * x -> 0",
      Mul(ConstDouble(0.0).toTree, x).toSearcher,
      MixedTree.fromTree(ConstDouble(0.0).toTree)
        .toApplier[LiarEGraph]
        .typeChecked)
  }

  val introduceAddZero: LiarRule = {
    // x -> x + 0
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> x + 0",
      MixedTree.Call[ArrayIR, Pattern[ArrayIR]](x)
        .toSearcher
        .requireMetadata
        .bindTypes(Map(x -> xType)).requireDoubleType(xType),
      Add(MixedTree.Call(x), ConstDouble(0.0).toTree)
        .toApplier[LiarEGraph]
        .typeChecked)
  }

  val introduceMulOneLeft: LiarRule = {
    // x -> 1 * x
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> 1 * x",
      MixedTree.Call[ArrayIR, Pattern[ArrayIR]](x)
        .toSearcher
        .requireMetadata
        .bindTypes(Map(x -> xType))
        .requireDoubleType(xType),
      Mul(ConstDouble(1.0).toTree, MixedTree.Call(x))
        .toApplier[LiarEGraph]
        .typeChecked)
  }

  val introduceMulOneRight: LiarRule = {
    // x -> x * 1
    val x = Pattern.Var.fresh[ArrayIR]()
    val xType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "x -> x * 1",
      MixedTree.Call[ArrayIR, Pattern[ArrayIR]](x)
        .toSearcher
        .requireMetadata
        .bindTypes(Map(x -> xType))
        .requireInt32Type(xType),
      Mul(MixedTree.Call(x), ConstInt32(1).toTree)
        .toApplier[LiarEGraph]
        .typeChecked)
  }

  val mulCommutativity: LiarRule = {
    // x * y -> y * x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * y -> y * x",
      Mul(x, y).toSearcher,
      Mul(y, x).toApplier[LiarEGraph].typeChecked)
  }

  val mulAssociativity1: LiarRule = {
    // x * (y * z) -> (x * y) * z
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * (y * z) -> (x * y) * z",
      Mul(x, Mul(y, z)).toSearcher,
      Mul(Mul(x, y), z).toApplier[LiarEGraph].typeChecked)
  }

  val mulAssociativity2: LiarRule = {
    // (x * y) * z -> x * (y * z)
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "(x * y) * z -> x * (y * z)",
      Mul(Mul(x, y), z).toSearcher,
      Mul(x, Mul(y, z)).toApplier[LiarEGraph].typeChecked)
  }
}
