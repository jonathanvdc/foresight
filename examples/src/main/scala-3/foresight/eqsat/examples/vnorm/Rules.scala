package foresight.eqsat.examples.vnorm

import foresight.eqsat.lang.{Language, LanguageOp}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.rewriting.patterns.PatternMatch

import scala.language.implicitConversions

/**
 * This object contains a collection of rules for rewriting arithmetic expressions.
 */
final case class Rules()(using L: Language[ArithExpr]) {
  type Op = LanguageOp[ArithExpr]
  type ArithEGraph = EGraph[ArithIR]
  type ArithRule = Rule[ArithIR, PatternMatch[ArithIR], ArithEGraph]

  import L.rule

  val recognizeFastInvSqrt: ArithRule =
    rule("recognize-fast-inv-sqrt") { x =>
      (FloatLiteral(1.0) / Sqrt(x)) -> FastInvSqrt(x)
    }

  /**
   * Returns all arithmetic rules.
   */
  def all: Seq[ArithRule] = Seq(
    recognizeFastInvSqrt
  )
}
