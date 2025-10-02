package foresight.eqsat.examples.vector

import foresight.eqsat.lang._
import foresight.eqsat.rewriting.patterns.Pattern
import foresight.eqsat.{EClassCall, MixedTree}

sealed trait VectorArithExpr derives Language {
  override def toString: String = {
    VectorArithPretty.render(this)
  }
}

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

/** Dot product of two vectors. */
final case class DotProduct(lhs: VectorArithExpr, rhs: VectorArithExpr) extends VectorArithExpr

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

/** Pretty-printer that renders VectorArithExpr as math with function calls.
  *
  * Examples:
  *   FastInvSqrt(Add(Add(x, y), z)) => "fast-inv-sqrt(x + y + z)"
  *   Mul(Add(x, y), z)              => "(x + y) * z"
  */
object VectorArithPretty:
  // Operator/construct precedence (higher = binds tighter)
  private enum Prec(val n: Int):
    case Lowest   extends Prec(0)   // outermost context
    case AddSub   extends Prec(60)  // +, - (we only have + here)
    case MulDiv   extends Prec(70)  // *, /
    case Unary    extends Prec(80)  // sqrt, fast-inv-sqrt
    case App      extends Prec(90)  // f(x), vec3(...), dot(...), element-at(...)
    case Atom     extends Prec(100) // literals, variables

  import Prec.*

  /** Public entry point. */
  def render(expr: VectorArithExpr): String = go(expr, Prec.Lowest)

  // ----- implementation -----
  private def go(e: VectorArithExpr, ctx: Prec): String =
    e match
      case FloatLiteral(v) => formatNumber(v)
      case Var(sym, _)     => sym

      case Add(l, r)       => bin(l, "+", r, AddSub, ctx)
      case Mul(l, r)       => bin(l, "*", r, MulDiv, ctx)
      case Div(n, d)       => bin(n, "/", d, MulDiv, ctx)

      case Sqrt(a)         => wrap(Unary, ctx, s"sqrt(${go(a, Lowest)})")
      case FastInvSqrt(a)  => wrap(Unary, ctx, s"fast-inv-sqrt(${go(a, Lowest)})")

      case Vector3(x, y, z) => s"vec3(${go(x, Lowest)}, ${go(y, Lowest)}, ${go(z, Lowest)})"
      case ElementAt(v, i)  => s"${go(v, App)}.$i"
      case DotProduct(l, r) => s"dot(${go(l, Lowest)}, ${go(r, Lowest)})"

      // For leaf-like wrappers, fall back to their toString (or a labeled form)
      case Ref(ec)          => s"ref(${ec.toString})"
      case PatternVar(pv)   => pv.toString
      case Fact(value)      => s"fact(${String.valueOf(value)})"

  private def bin(l: VectorArithExpr, op: String, r: VectorArithExpr, prec: Prec, ctx: Prec): String =
    val s = s"${go(l, prec)} $op ${go(r, prec)}"
    wrap(prec, ctx, s)

  private def wrap(cur: Prec, ctx: Prec, s: String): String =
    if cur.n < ctx.n then s"($s)" else s

  private def formatNumber(d: Double): String =
    val asLong = d.toLong
    if d.isNaN then "NaN"
    else if d.isPosInfinity then "+Infinity"
    else if d.isNegInfinity then "-Infinity"
    else if d == asLong.toDouble then asLong.toString
    else d.toString
