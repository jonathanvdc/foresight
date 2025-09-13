package foresight.eqsat.examples.mm

import foresight.eqsat.EGraph
import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch

import scala.language.implicitConversions

/**
 * This object contains a collection of rules for rewriting linear algebra expressions.
 */
final case class Rules()(using L: Language[LinalgExpr]) {
  type Op = LanguageOp[LinalgExpr]
  type LinalgEGraph = EGraph[LinalgIR]
  type LinalgRule = Rule[LinalgIR, PatternMatch[LinalgIR], LinalgEGraph]

  import L.rule

  val matMulAssociativity1: LinalgRule =
    rule("mul-associativity1") { (x, y, z) =>
      ((x * y) * z) -> (x * (y * z))
    }

  val matMulAssociativity2: LinalgRule =
    rule("mul-associativity2") { (x, y, z) =>
      (x * (y * z)) -> ((x * y) * z)
    }

  /**
   * Returns all arithmetic rules.
   */
  def all: Seq[LinalgRule] = Seq(
    matMulAssociativity1,
    matMulAssociativity2
  )
}
