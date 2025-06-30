package foresight.eqsat.examples.arith

import ApplierOps._
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, MixedTree, Slot}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

/**
 * This object contains a collection of rules for rewriting arithmetic expressions.
 */
object Rules {
  type ArithEGraph = EGraphWithMetadata[ArithIR, EGraph[ArithIR]]
  type ArithRule = foresight.eqsat.rewriting.Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  /**
   * Returns all arithmetic rules.
   */
  def all: Seq[ArithRule] = Seq(
    beta,
    addCommutativity,
    addAssociativity1,
    addAssociativity2,
    mulCommutativity,
    mulAssociativity1,
    mulAssociativity2,
    distributivity1,
    distributivity2
  )

  val beta: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Slot.fresh()
    val b = Pattern.Var.fresh[ArithIR]()
    val t = Pattern.Var.fresh[ArithIR]()
    Rule(
      "beta",
      App(Lam(x, MixedTree.Call(b)), MixedTree.Call(t)).toSearcher,
      MixedTree.Call[ArithIR, Pattern[ArithIR]](b)
        .toApplier[ArithEGraph]
        .substitute(b, x, t, b)
    )
  }

  val etaExpansion: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Slot.fresh()
    val b = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "eta-expansion",
      b.toSearcher,
      Lam(x, App(b, Var(x))).toApplier
    )
  }

  val addCommutativity: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x + y = y + x",
      Add(x, y).toSearcher,
      Add(y, x).toApplier)
  }

  val addAssociativity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "(x + y) + z = x + (y + z)",
      Add(Add(x, y), z).toSearcher,
      Add(x, Add(y, z)).toApplier)
  }

  val addAssociativity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x + (y + z) = (x + y) + z",
      Add(x, Add(y, z)).toSearcher,
      Add(Add(x, y), z).toApplier)
  }

  val mulCommutativity: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * y = y * x",
      Mul(x, y).toSearcher,
      Mul(y, x).toApplier)
  }

  val mulAssociativity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "(x * y) * z = x * (y * z)",
      Mul(Mul(x, y), z).toSearcher,
      Mul(x, Mul(y, z)).toApplier)
  }

  val mulAssociativity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * (y * z) = (x * y) * z",
      Mul(x, Mul(y, z)).toSearcher,
      Mul(Mul(x, y), z).toApplier)
  }

  val distributivity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * (y + z) = x * y + x * z",
      Mul(x, Add(y, z)).toSearcher,
      Add(Mul(x, y), Mul(x, z)).toApplier)
  }

  val distributivity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val a = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val b = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val c = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "a * b + a * c = a * (b + c)",
      Add(Mul(a, b), Mul(a, c)).toSearcher,
      Mul(a, Add(b, c)).toApplier)
  }
}
