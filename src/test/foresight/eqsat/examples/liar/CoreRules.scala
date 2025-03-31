package foresight.eqsat.examples.liar

import ApplierOps.ApplierOfPatternMatchOps
import SearcherOps._
import foresight.eqsat.{EGraph, MixedTree, Slot}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

object CoreRules {
  type LiarEGraph = EGraphWithMetadata[ArrayIR, EGraph[ArrayIR]]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], LiarEGraph]

  def all: Seq[LiarRule] = introductionRules ++ eliminationRules

  def introductionRules: Seq[LiarRule] = Seq(
    introduceLambda,
    introduceIndexBuild
  )

  def eliminationRules: Seq[LiarRule] = Seq(
    eliminateLambda,
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
    val x = Slot.fresh()
    Rule(
      "e -> (λx. e) y",
      MixedTree.Call[ArrayIR, Pattern[ArrayIR]](e)
        .toSearcher[EGraph[ArrayIR]]
        .requireValues(e)
        .requireMetadata
        .bindTypes(Map(e -> eType))
        .requireNonFunctionType(eType)
        .product(
          MixedTree.Call[ArrayIR, Pattern[ArrayIR]](y)
            .toSearcher[EGraph[ArrayIR]]
            .requireValues(y)
            .requireMetadata
            .bindTypes(Map(y -> yType))
            .requireNonFunctionType(yType))
        .merge,
      Apply(Lambda(x, MixedTree.Call(yType), MixedTree.Call(e)), MixedTree.Call(y)).toApplier)
  }

  val introduceIndexBuild: LiarRule = {
    // f i -> (build f N)[i]
    val f = Pattern.Var.fresh[ArrayIR]()
    val N = Pattern.Var.fresh[ArrayIR]()
    val i = Pattern.Var.fresh[ArrayIR]()
    val iType = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "f i -> (build f N)[i]",
      Apply(MixedTree.Call(f), MixedTree.Call(i))
        .toSearcher[EGraph[ArrayIR]]
        .requireMetadata
        .bindTypes(Map(i -> iType))
        .requireInt32Type(iType)
        .product(
          ArrayType(MixedTree.Call(Pattern.Var.fresh[ArrayIR]()), MixedTree.Call(N))
            .toSearcher
            .requireMetadata)
        .merge,
      IndexAt(Build(MixedTree.Call(N), MixedTree.Call(f)), MixedTree.Call(i)).toApplier)
  }

  val eliminateLambda: LiarRule = {
    // (λx. e) y -> e[x/y]
    val e = Pattern.Var.fresh[ArrayIR]()
    val t = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val x = Slot.fresh()
    val y = Pattern.Var.fresh[ArrayIR]()
    Rule(
      "(λx. e) y -> e[x/y]",
      Apply(Lambda(x, t, MixedTree.Call(e)), MixedTree.Call(y)).toSearcher,
      MixedTree.Call[ArrayIR, Pattern[ArrayIR]](e)
        .toApplier[EGraphWithMetadata[ArrayIR, EGraph[ArrayIR]]]
        .substitute(e, x, y, e))
  }

  val eliminateIndexBuild: LiarRule = {
    // (build f N)[i] -> f i
    val f = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val N = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val i = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "(build f N)[i] -> f i",
      IndexAt(Build(N, f), i).toSearcher,
      Apply(f, i).toApplier)
  }

  val eliminateFstTuple: LiarRule = {
    // fst (tuple a b) -> a
    val a = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val b = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "fst (tuple a b) -> a",
      Fst(Tuple(a, b)).toSearcher,
      a.toApplier)
  }

  val eliminateSndTuple: LiarRule = {
    // snd (tuple a b) -> b
    val a = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    val b = MixedTree.Call(Pattern.Var.fresh[ArrayIR]())
    Rule(
      "snd (tuple a b) -> b",
      Snd(Tuple(a, b)).toSearcher,
      b.toApplier)
  }
}
