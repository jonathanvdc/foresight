package foresight.eqsat.examples.mm

import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.AbstractPatternMatch

import scala.language.implicitConversions

/**
 * This object contains a collection of rules for rewriting linear algebra expressions.
 */
final case class Rules()(using L: Language[LinalgExpr]) {
  type Op = LanguageOp[LinalgExpr]
  type LinalgEGraph = EGraph[LinalgIR]
  type LinalgRule = Rule[LinalgIR, AbstractPatternMatch[LinalgIR], LinalgEGraph]

  import L.borrowingRule

  val matMulAssociativity1: LinalgRule =
    borrowingRule("mul-associativity1") { (x, y, z) =>
      ((x * y) * z) -> (x * (y * z))
    }

  val matMulAssociativity2: LinalgRule =
    borrowingRule("mul-associativity2") { (x, y, z) =>
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
