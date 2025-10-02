package foresight.eqsat.examples.vector

import foresight.eqsat.lang._
import foresight.eqsat.rewriting.patterns.Pattern
import foresight.eqsat.{EClassCall, MixedTree}

sealed trait VectorArithExpr derives Language

/** Floating-point literal. */
final case class FloatLiteral(value: Double) extends VectorArithExpr

/** A variable of a given type. */
final case class Var(sym: String, t: Type) extends VectorArithExpr

/** Addition node. */
final case class Add(lhs: VectorArithExpr, rhs: VectorArithExpr) extends VectorArithExpr

/** Multiplication node. */
final case class Mul(lhs: VectorArithExpr, rhs: VectorArithExpr) extends VectorArithExpr

/** Division node. */
final case class Div(numer: VectorArithExpr, denom: VectorArithExpr) extends VectorArithExpr

/** Square root node. */
final case class Sqrt(arg: VectorArithExpr) extends VectorArithExpr

/** Fast inverse square root node. */
final case class FastInvSqrt(arg: VectorArithExpr) extends VectorArithExpr

/** 3D vector node. */
final case class Vector3(x: VectorArithExpr, y: VectorArithExpr, z: VectorArithExpr) extends VectorArithExpr

/** Extracts the vector element at a given index. */
final case class ElementAt(v: VectorArithExpr, index: Int) extends VectorArithExpr

/**
 * An explicit reference to an existing e-class in the e-graph.
 *
 * This is useful in examples/rules where we want to splice a matched subtree back
 * into another expression. Deriving [[Box]] ensures that matcher/applier treat `Ref`
 * as a leaf (no recursive matching into the referenced class).
 */
final case class Ref(eClass: EClassCall) extends VectorArithExpr derives Box

/**
 * A pattern variable exposed at the surface AST level.
 *
 * `PatternVar` lets us write example rules in ordinary Scala by plugging these
 * placeholders into expressions. Because it derives [[Box]], it is also treated as
 * a leaf during matching. Use [[fresh]] to create a uniquely-named variable.
 */
final case class PatternVar(variable: Pattern.Var) extends VectorArithExpr derives Box

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
final case class Fact[A](value: A) extends VectorArithExpr

/**
 * Companion configures analysis boxing for this surface language.
 *
 * By implementing [[AnalysisBox]] we tell Foresight that analyses may embed results
 * as `Fact[A]` nodes inside this AST. This is optional but makes certain examples
 * and rules terser.
 */
object VectorArithExpr {
  given AnalysisBox[VectorArithExpr] with
    type Box[A] = Fact[A]

    def box[A](a: A): Fact[A] = Fact(a)
}

/** Infix operators for building trees concisely in rules/tests. */
extension (lhs: VectorArithExpr)
  /** {{{ x + y }}} builds an [[Add]] node. */
  infix def +(rhs: VectorArithExpr): VectorArithExpr = Add(lhs, rhs)
  /** {{{ x * y }}} builds a [[Mul]] node. */
  infix def *(rhs: VectorArithExpr): VectorArithExpr = Mul(lhs, rhs)
  /** {{{ x / y }}} builds a [[Div]] node. */
  infix def /(rhs: VectorArithExpr): VectorArithExpr = Div(lhs, rhs)

type ArithIR = LanguageOp[VectorArithExpr]
