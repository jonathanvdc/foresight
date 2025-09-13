package foresight.eqsat.examples.mm

import foresight.eqsat.{EClassCall, MixedTree}
import foresight.eqsat.lang.{AnalysisBox, AtomDecoder, AtomEncoder, Box, Language, LanguageOp}
import foresight.eqsat.rewriting.patterns.Pattern

sealed trait LinalgExpr derives Language

/** Matrix */
final case class Mat(rows: Int, cols: Int) extends LinalgExpr

/** Matrix multiplication */
final case class Mul(lhs: LinalgExpr, rhs: LinalgExpr) extends LinalgExpr

/**
 * An explicit reference to an existing e-class in the e-graph.
 *
 * This is useful in examples/rules where we want to splice a matched subtree back
 * into another expression. Deriving [[Box]] ensures that matcher/applier treat `Ref`
 * as a leaf (no recursive matching into the referenced class).
 */
final case class Ref(eClass: EClassCall) extends LinalgExpr derives Box

/**
 * A pattern variable exposed at the surface AST level.
 *
 * `PatternVar` lets us write example rules in ordinary Scala by plugging these
 * placeholders into expressions. Because it derives [[Box]], it is also treated as
 * a leaf during matching. Use [[fresh]] to create a uniquely-named variable.
 */
final case class PatternVar(variable: Pattern.Var) extends LinalgExpr derives Box

object PatternVar {
  /**
   * Fresh pattern variable for building matchers in examples.
   *
   * {{{
   * val x = PatternVar.fresh(); val y = PatternVar.fresh()
   * Add(x, y)  // a pattern using surface syntax
   * }}}
   */
  def fresh(): PatternVar = PatternVar(Pattern.Var.fresh())
}

/**
 * Simple wrapper used by the analysis box.
 *
 * Analyses can produce `Fact[A]` nodes when convenient. For instance, constant-propagation
 * might compute an `Option[BigInt]` and rules can consult/box that information.
 */
final case class Fact[A](value: A) extends LinalgExpr

/**
 * Companion configures analysis boxing for this surface language.
 *
 * By implementing [[AnalysisBox]] we tell Foresight that analyses may embed results
 * as `Fact[A]` nodes inside this AST. This is optional but makes certain examples
 * and rules terser.
 */
object LinalgExpr {
  given AnalysisBox[LinalgExpr] with
    type Box[A] = Fact[A]

    def box[A](a: A): Fact[A] = Fact(a)
}

/** Infix operators for building trees concisely in rules/tests. */
extension (lhs: LinalgExpr)
  /** {{{ x * y }}} builds a [[Mul]] node. */
  infix def *(rhs: LinalgExpr): LinalgExpr = Mul(lhs, rhs)

type LinalgIR = LanguageOp[LinalgExpr]