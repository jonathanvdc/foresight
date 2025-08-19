package foresight.eqsat.examples.arithWithLang

import scala.language.implicitConversions
import ApplierOps.*
import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.{EGraph, Slot}
import foresight.eqsat.rewriting.patterns.PatternMatch

/**
 * This object contains a collection of rules for rewriting arithmetic expressions.
 */
final case class Rules()(using L: Language[ArithExpr]) {
  type Op = LanguageOp[ArithExpr]
  type ArithEGraph = EGraphWithMetadata[ArithIR, EGraph[ArithIR]]
  type ArithRule = Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  import L.rule

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
    val b = PatternVar.fresh()
    val t = PatternVar.fresh()
    Rule(
      "beta",
      L.toSearcher[ArithEGraph](App(Lam(x, b), t)),
      L.toApplier[ArithEGraph](b)
        .substitute(b, x, t, b)
    )
  }

  val etaExpansion: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = Slot.fresh()
    val b = PatternVar.fresh()
    rule("eta-expansion", b, Lam(x, App(b, Var(x))))
  }

  val constantPropagation: Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph] = {
    val x = PatternVar.fresh()
    Rule(
      "constant-propagation",
      L.toSearcher(x).flatMap({
        case (subst, egraph) =>
          // Use the ConstantAnalysis to find the constant value of the expression, if available.
          val result = ConstantAnalysis.get(egraph)(subst(x.variable), egraph)
          result.toSeq.map { value =>
            // If a constant value is found, create a new Number node and bind it to the variable, overwriting the
            // original binding.
            subst.bind(x.variable, L.toTree(Number(value)))
          }
      }),
      L.toApplier(x)
    )
  }

  val addCommutativity: ArithRule =
    rule("add-commutativity") { (x, y) =>
      (x + y) -> (y + x)
    }

  val addAssociativity1: ArithRule =
    rule("add-associativity1") { (x, y, z) =>
      ((x + y) + z) -> (x + (y + z))
    }

  val addAssociativity2: ArithRule =
    rule("add-associativity2") { (x, y, z) =>
      (x + (y + z)) -> ((x + y) + z)
    }

  val mulCommutativity: ArithRule =
    rule("mul-commutativity") { (x, y) =>
      (x * y) -> (y * x)
    }

  val mulAssociativity1: ArithRule =
    rule("mul-associativity1") { (x, y, z) =>
      ((x * y) * z) -> (x * (y * z))
    }

  val mulAssociativity2: ArithRule =
    rule("mul-associativity2") { (x, y, z) =>
      (x * (y * z)) -> ((x * y) * z)
    }

  val distributivity1: ArithRule =
    rule("distributivity1") { (x, y, z) =>
      (x * (y + z)) -> ((x * y) + (x * z))
    }

  val distributivity2: ArithRule =
    rule("distributivity2") { (a, b, c) =>
      ((a * b) + (a * c)) -> (a * (b + c))
    }
}
