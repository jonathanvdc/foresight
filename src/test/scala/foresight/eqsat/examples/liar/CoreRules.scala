package foresight.eqsat.examples.liar

import ApplierOps._
import SearcherOps._
import foresight.eqsat.{EGraph, EGraphLike, MixedTree, Slot}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

final case class CoreRules[BaseEGraph <: EGraphLike[ArrayIR, BaseEGraph] with EGraph[ArrayIR]]() {
  type MetadataEGraph = EGraphWithMetadata[ArrayIR, BaseEGraph]
  type LiarRule = Rule[ArrayIR, PatternMatch[ArrayIR], MetadataEGraph]

  def all: Seq[LiarRule] = introductionRules ++ eliminationRules

  def allWithConstArray: Seq[LiarRule] = introduceConstArray +: eliminationRules

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

  val introduceConstArray: LiarRule = {
    // e -> (build (λi. e) N)[j]
    val e = Pattern.Var.fresh()
    val eType = Pattern.Var.fresh()
    val N = Pattern.Var.fresh()
    val j = Pattern.Var.fresh()

    val i = Slot.fresh()

    Rule(
      "e -> (build (λi. e) N)[j]",
      MixedTree.Atom[ArrayIR, Pattern.Var](e)
        .toSearcher[BaseEGraph]
        .requireValues(e)
        .requireMetadata
        .bindTypes(Map(e -> eType))
        .requireNonFunctionType(eType)
        .product(
          MixedTree.Atom[ArrayIR, Pattern.Var](j)
            .toSearcher[BaseEGraph]
            .requireValues(j)
            .requireMetadata
            .requireTypes(Map(j -> Int32Type.toTree)))
        .merge
        .product(
          ArrayType(MixedTree.Atom(Pattern.Var.fresh()), MixedTree.Atom(N))
            .asInstanceOf[MixedTree[ArrayIR, Pattern.Var]]
            .toSearcher
            .requireMetadata)
        .merge,
      IndexAt(Build(MixedTree.Atom(N), Lambda(i, Int32Type.toTree, MixedTree.Atom(e))), MixedTree.Atom(j))
        .toApplier[MetadataEGraph]
        .typeChecked)
  }

  val introduceLambda: LiarRule = {
    // e -> (λx. e) y
    val e = Pattern.Var.fresh()
    val eType = Pattern.Var.fresh()
    val y = Pattern.Var.fresh()
    val yType = Pattern.Var.fresh()
    val x = Slot.fresh()
    Rule(
      "e -> (λx. e) y",
      MixedTree.Atom[ArrayIR, Pattern.Var](e)
        .toSearcher[BaseEGraph]
        .requireValues(e)
        .requireMetadata
        .bindTypes(Map(e -> eType))
        .requireNonFunctionType(eType)
        .product(
          MixedTree.Atom[ArrayIR, Pattern.Var](y)
            .toSearcher[BaseEGraph]
            .requireValues(y)
            .requireMetadata
            .bindTypes(Map(y -> yType))
            .requireNonFunctionType(yType))
        .merge,
      Apply(Lambda(x, MixedTree.Atom(yType), MixedTree.Atom(e)), MixedTree.Atom(y))
        .toApplier[MetadataEGraph]
        .typeChecked)
  }

  val introduceIndexBuild: LiarRule = {
    // f i -> (build f N)[i]
    val f = Pattern.Var.fresh()
    val N = Pattern.Var.fresh()
    val i = Pattern.Var.fresh()
    val iType = Pattern.Var.fresh()
    Rule(
      "f i -> (build f N)[i]",
      Apply(MixedTree.Atom(f), MixedTree.Atom(i))
        .toSearcher[BaseEGraph]
        .requireMetadata
        .bindTypes(Map(i -> iType))
        .requireInt32Type(iType)
        .product(
          ArrayType(MixedTree.Atom(Pattern.Var.fresh()), MixedTree.Atom(N))
            .asInstanceOf[MixedTree[ArrayIR, Pattern.Var]]
            .toSearcher
            .requireMetadata)
        .merge,
      IndexAt(Build(MixedTree.Atom(N), MixedTree.Atom(f)), MixedTree.Atom(i))
        .toApplier[MetadataEGraph]
        .typeChecked)
  }

  val eliminateLambda: LiarRule = {
    // (λx. e) y -> e[x/y]
    val e = Pattern.Var.fresh()
    val t = MixedTree.Atom(Pattern.Var.fresh())
    val x = Slot.fresh()
    val y = Pattern.Var.fresh()
    Rule(
      "(λx. e) y -> e[x/y]",
      Apply(Lambda(x, t, MixedTree.Atom(e)), MixedTree.Atom(y)).toSearcher,
      MixedTree.Atom[ArrayIR, Pattern.Var](e)
        .toApplier[MetadataEGraph]
        .typeChecked
        .substitute(e, x, y, e))
  }

  val eliminateIndexBuild: LiarRule = {
    // (build f N)[i] -> f i
    val f = MixedTree.Atom(Pattern.Var.fresh())
    val N = MixedTree.Atom(Pattern.Var.fresh())
    val i = MixedTree.Atom(Pattern.Var.fresh())
    Rule(
      "(build f N)[i] -> f i",
      IndexAt(Build(N, f), i).toSearcher,
      Apply(f, i)
        .toApplier[MetadataEGraph]
        .typeChecked)
  }

  val eliminateFstTuple: LiarRule = {
    // fst (tuple a b) -> a
    val a = MixedTree.Atom[ArrayIR, Pattern.Var](Pattern.Var.fresh())
    val b = MixedTree.Atom[ArrayIR, Pattern.Var](Pattern.Var.fresh())
    Rule(
      "fst (tuple a b) -> a",
      Fst(Tuple(a, b)).toSearcher,
      a.toApplier[MetadataEGraph]
        .typeChecked)
  }

  val eliminateSndTuple: LiarRule = {
    // snd (tuple a b) -> b
    val a = MixedTree.Atom[ArrayIR, Pattern.Var](Pattern.Var.fresh())
    val b = MixedTree.Atom[ArrayIR, Pattern.Var](Pattern.Var.fresh())
    Rule(
      "snd (tuple a b) -> b",
      Snd(Tuple(a, b)).toSearcher,
      b.toApplier[MetadataEGraph]
        .typeChecked)
  }
}
