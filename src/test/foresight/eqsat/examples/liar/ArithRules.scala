package foresight.eqsat.examples.liar

import SearcherOps._
import ApplierOps._
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.{EGraph, EGraphLike, MixedTree}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

final case class ArithRules[BaseEGraph <: EGraphLike[ArrayIR, BaseEGraph] with EGraph[ArrayIR]]() {
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  def all: Seq[LiarRule] = introductionRules ++ simplificationRules

  def introductionRules: Seq[LiarRule] = Seq(
    introduceAddZero,
    introduceMulOneLeft,
    introduceMulOneRight,
    mulCommutativity,
    mulAssociativity1,
    mulAssociativity2
  )

  def simplificationRules: Seq[LiarRule] = Seq(
    simplifyAddZeroRight,
    simplifyMulOneRight,
    simplifyMulOneLeft,
    simplifyMulZeroLeft
  )

  val simplifyAddZeroRight: LiarRule = {
    // x + 0 -> x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x + 0 -> x",
      Add(x, ConstDouble(0.0).toTree).toSearcher,
      x.toApplier[MetadataEGraph].typeChecked)
  }

  val simplifyMulOneRight: LiarRule = {
    // x * 1 -> x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * 1 -> x",
      Mul(x, ConstDouble(1.0).toTree).toSearcher,
      x.toApplier[MetadataEGraph].typeChecked)
  }

  val simplifyMulOneLeft: LiarRule = {
    // 1 * x -> x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "1 * x -> x",
      Mul(ConstInt32(1).toTree, x).toSearcher,
      x.toApplier[MetadataEGraph].typeChecked)
  }

  val simplifyMulZeroLeft: LiarRule = {
    // 0 * x -> 0
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "0 * x -> 0",
      Mul(ConstDouble(0.0).toTree, x).toSearcher,
      MixedTree.fromTree(ConstDouble(0.0).toTree)
        .toApplier[MetadataEGraph]
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
        .toApplier[MetadataEGraph]
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
        .toApplier[MetadataEGraph]
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
        .toApplier[MetadataEGraph]
        .typeChecked)
  }

  val mulCommutativity: LiarRule = {
    // x * y -> y * x
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * y -> y * x",
      Mul(x, y).toSearcher,
      Mul(y, x).toApplier[MetadataEGraph].typeChecked)
  }

  val mulAssociativity1: LiarRule = {
    // x * (y * z) -> (x * y) * z
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "x * (y * z) -> (x * y) * z",
      Mul(x, Mul(y, z)).toSearcher,
      Mul(Mul(x, y), z).toApplier[MetadataEGraph].typeChecked)
  }

  val mulAssociativity2: LiarRule = {
    // (x * y) * z -> x * (y * z)
    val x = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "(x * y) * z -> x * (y * z)",
      Mul(Mul(x, y), z).toSearcher,
      Mul(x, Mul(y, z)).toApplier[MetadataEGraph].typeChecked)
  }
}
