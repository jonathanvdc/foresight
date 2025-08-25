package foresight.eqsat.examples.poly

import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch
import foresight.eqsat.{EGraph, MixedTree}

import scala.language.implicitConversions

/**
 * This object contains a collection of rules for rewriting arithmetic expressions.
 */
final case class Rules()(using L: Language[ArithExpr]) {
  type Op = LanguageOp[ArithExpr]
  type ArithEGraph = EGraph[ArithIR]
  type ArithRule = Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  import L.rule

  val addCommutativity: ArithRule =
    rule("add-commutativity") { (x, y) =>
      (x + y) -> (y + x)
    }
  val mulCommutativity: ArithRule =
    rule("mul-commutativity") { (x, y) =>
      (x * y) -> (y * x)
    }
  val addAssociativity1: ArithRule =
    rule("add-associativity1") { (x, y, z) =>
      ((x + y) + z) -> (x + (y + z))
    }
  val addAssociativity2: ArithRule =
    rule("add-associativity2") { (x, y, z) =>
      (x + (y + z)) -> ((x + y) + z)
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
  val multiplicativeIdentity: ArithRule = {
    rule("multiplicative-identity") { (x) =>
      (x * Succ(Zero)) -> x
    }
  }
  val powerNull: ArithRule = {
    rule("power-null") { (x) =>
      (x ** Zero) -> Succ(Zero)
    }
  }
  val powerReduction: ArithRule = {
    rule("power-reduction") { (x, n) =>
      (x ** Succ(n)) -> (x * (x ** n))
    }
  }

  /**
   * Returns all arithmetic rules.
   */
  def all: Seq[ArithRule] = Seq(
    addCommutativity,
    mulCommutativity,
    addAssociativity1,
    addAssociativity2,
    mulAssociativity1,
    mulAssociativity2,
    distributivity1,
    distributivity2,
    multiplicativeIdentity,
    powerNull,
    powerReduction
  )
}
