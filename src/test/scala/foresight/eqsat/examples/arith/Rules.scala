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
  type ArithRule = Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  /**
   * Returns all arithmetic rules.
   */
  def all: Seq[ArithRule] = Seq(
    beta,
    constantPropagation,
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
      App(Lam(x, MixedTree.Atom(b)), MixedTree.Atom(t)).toSearcher,
      MixedTree.Atom[ArithIR, Pattern[ArithIR]](b)
        .toApplier[ArithEGraph]
        .substitute(b, x, t, b)
    )
  }

  val etaExpansion: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Slot.fresh()
    val b = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "eta-expansion",
      b.toSearcher,
      Lam(x, App(b, Var(x))).toApplier
    )
  }

  val constantPropagation: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Pattern.Var.fresh[ArithIR]()
    Rule(
      "constant-propagation",
      MixedTree.Atom[ArithIR, Pattern[ArithIR]](x).toSearcher[ArithEGraph].flatMap({
        case (subst, egraph) =>
          // Use the ConstantAnalysis to find the constant value of the expression, if available.
          val result = ConstantAnalysis.get(egraph)(subst(x), egraph)
          result.toSeq.map { value =>
            // If a constant value is found, create a new Number node and bind it to the variable, overwriting the
            // original binding.
            subst.bind(x, Number(value))
          }
      }),
      MixedTree.Atom[ArithIR, Pattern[ArithIR]](x).toApplier
    )
  }

  val addCommutativity: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x + y = y + x",
      Add(x, y).toSearcher,
      Add(y, x).toApplier)
  }

  val addAssociativity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "(x + y) + z = x + (y + z)",
      Add(Add(x, y), z).toSearcher,
      Add(x, Add(y, z)).toApplier)
  }

  val addAssociativity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x + (y + z) = (x + y) + z",
      Add(x, Add(y, z)).toSearcher,
      Add(Add(x, y), z).toApplier)
  }

  val mulCommutativity: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * y = y * x",
      Mul(x, y).toSearcher,
      Mul(y, x).toApplier)
  }

  val mulAssociativity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "(x * y) * z = x * (y * z)",
      Mul(Mul(x, y), z).toSearcher,
      Mul(x, Mul(y, z)).toApplier)
  }

  val mulAssociativity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * (y * z) = (x * y) * z",
      Mul(x, Mul(y, z)).toSearcher,
      Mul(Mul(x, y), z).toApplier)
  }

  val distributivity1: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val y = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val z = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "x * (y + z) = x * y + x * z",
      Mul(x, Add(y, z)).toSearcher,
      Add(Mul(x, y), Mul(x, z)).toApplier)
  }

  val distributivity2: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val a = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val b = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    val c = MixedTree.Atom(Pattern.Var.fresh[ArithIR]())
    Rule(
      "a * b + a * c = a * (b + c)",
      Add(Mul(a, b), Mul(a, c)).toSearcher,
      Mul(a, Add(b, c)).toApplier)
  }
}
