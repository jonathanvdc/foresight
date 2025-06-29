package foresight.eqsat.examples.arith

import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, MixedTree, Slot}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

object Rules {
  type ArithEGraph = EGraph[ArithIR]
  type ArithRule = foresight.eqsat.rewriting.Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  def all: Seq[ArithRule] = Seq(
    beta,
    eta,
    myLetUnused,
    letVarSame,
    letApp,
    letLamDiff,
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
    val b = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    val t = MixedTree.Call(Pattern.Var.fresh[ArithIR]())
    Rule(
      "beta",
      App(Lam(x, b), t).toSearcher,
      Let(x, b, t).toApplier
    )
  }

  val eta: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Slot.fresh()
    val b = Pattern.Var.fresh[ArithIR]()
    Rule(
      "eta",
      Lam(x, App(MixedTree.Call(b), Var(x))).toSearcher,
      MixedTree.Call(b).toApplier.filter({
        case (subst, _) =>
          // Ensure that the body of the lambda does not contain the variable being applied to
          !subst(b).slotSet.contains(subst(x))
      })
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

  val myLetUnused: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val b = Pattern.Var.fresh[ArithIR]()
    val t = Pattern.Var.fresh[ArithIR]()
    val x = Slot.fresh()
    Rule(
      "my-let-unused",
      Let(x, MixedTree.Call(b), MixedTree.Call(t)).toSearcher,
      MixedTree.Call(b).toApplier.filter {
        case (subst, _) =>
          // Ensure that the body of the let does not contain the variable being bound
          !subst(b).slotSet.contains(subst(x))
      }
    )
  }

  val letVarSame: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val e = Pattern.Var.fresh[ArithIR]()
    val x = Slot.fresh()
    Rule(
      "let-var-same",
      Let(x, Var(x), MixedTree.Call[ArithIR, Pattern[ArithIR]](e)).toSearcher,
      MixedTree.Call(e).toApplier
    )
  }

  val letApp: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val a = Pattern.Var.fresh[ArithIR]()
    val b = Pattern.Var.fresh[ArithIR]()
    val e = Pattern.Var.fresh[ArithIR]()
    val x = Slot.fresh()
    Rule(
      "let-app",
      Let(x, App(MixedTree.Call(a), MixedTree.Call(b)), MixedTree.Call(e)).toSearcher,
      App(Let(x, MixedTree.Call(a), MixedTree.Call(e)), Let(x, MixedTree.Call(b), MixedTree.Call(e))).toApplier.filter {
        case (subst, _) =>
          // Ensure that the body of the lambda does not contain the variable being applied to
          !subst(a).slotSet.contains(subst(x)) && !subst(b).slotSet.contains(subst(x))
      }
    )
  }

  val letLamDiff: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val b = Pattern.Var.fresh[ArithIR]()
    val e = Pattern.Var.fresh[ArithIR]()
    val x = Slot.fresh()
    val y = Slot.fresh()
    Rule(
      "let-lam-diff",
      Let(x, Lam(y, MixedTree.Call(b)), MixedTree.Call(e)).toSearcher,
      Lam(y, Let(x, MixedTree.Call(b), MixedTree.Call(e))).toApplier.filter {
        case (subst, _) =>
          // Ensure that the body of the lambda does not contain the variable being applied to
          !subst(b).slotSet.contains(subst(x))
      }
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
