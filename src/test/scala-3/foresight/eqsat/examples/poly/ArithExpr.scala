package foresight.eqsat.examples.poly

import foresight.eqsat.lang.{AnalysisBox, Box, Language, LanguageOp}
import foresight.eqsat.rewriting.patterns.Pattern

sealed trait ArithExpr derives Language

/** Integer literal. */
//final case class Num(value: Int) extends ArithExpr

/** Peano numeral for zero. */
case object Zero extends ArithExpr

/** Peano numeral for successor. */
final case class Succ(pred: ArithExpr) extends ArithExpr

/** A variable. */
final case class Var(sym: String) extends ArithExpr

/** Addition node. */
final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr

/** Multiplication node. */
final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr

/** Power node. */
final case class Pow(base: ArithExpr, exponent: ArithExpr) extends ArithExpr

/**
 * A pattern variable exposed at the surface AST level.
 *
 * `PatternVar` lets us write example rules in ordinary Scala by plugging these
 * placeholders into expressions. Because it derives [[Box]], it is also treated as
 * a leaf during matching. Use [[fresh]] to create a uniquely-named variable.
 */
final case class PatternVar(variable: Pattern.Var) extends ArithExpr derives Box

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

/** Infix operators for building trees concisely in rules/tests. */
extension (lhs: ArithExpr)
  /** {{{ x + y }}} builds an [[Add]] node. */
  infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
  /** {{{ x * y }}} builds a [[Mul]] node. */
  infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)
  /** {{{ x ** y }}} builds a [[Pow]] node. */
  infix def **(rhs: ArithExpr): ArithExpr = Pow(lhs, rhs)

type ArithIR = LanguageOp[ArithExpr]