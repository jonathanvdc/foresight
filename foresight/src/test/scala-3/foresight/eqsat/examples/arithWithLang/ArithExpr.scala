package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.{EClassCall, Slot}
import foresight.eqsat.lang.{AnalysisBox, Box, Def, Language, LanguageOp, Use}
import foresight.eqsat.rewriting.patterns.Pattern

/**
 * Surface AST for the arithmetic-with-language example
 *
 * This file defines a tiny lambda-calculus with integers and `+`/`*`, written as a normal
 * sealed trait hierarchy. The interesting bit is that the root trait derives a
 * [[foresight.eqsat.lang.Language Language]] instance, which lets Foresight encode/decode
 * this *surface* AST to the internal e-graph representation automatically.
 *
 * We keep the model idiomatic and then show how Foresight attaches
 * rewrites and analyses.
 *
 * Design notes:
 * - **Binders vs uses:** parameter positions carry `Def[Slot]` (a binder), while variable
 *   references carry `Use[Slot]`. This makes binding structure explicit to the language
 *   derivation, so the e-graph can treat lambdas/apps hygienically.
 * - **Boxes when we really want leaves:** `Ref` and `PatternVar` "derive Atom" to tell the
 *   language that these become MixedTree atoms.
 * - **Fact as an analysis box:** `AnalysisBox` lets analyses temporarily embed results back
 *   into the surface AST (e.g., a discovered constant), which can be convenient for rules.
 */
sealed trait ArithExpr derives Language

/**
 * A variable reference to a previously bound slot.
 *
 * `Use[Slot]` marks that this node **reads** from a binder introduced elsewhere.
 * The slot identity is managed by the e-graph; we only carry the tag here.
 */
final case class Var(slot: Use[Slot]) extends ArithExpr

/**
 * A lambda abstraction with a single parameter and body.
 *
 * The parameter is a `Def[Slot]`, explicitly marking a **binder** site. By separating
 * binders/uses at the type level, the derived [[Language]] can reconstruct scopes
 * without bespoke encodings.
 */
final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr

/** Function application. */
final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr

/** Addition node. */
final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr

/** Multiplication node. */
final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr

/** Integer literal. */
final case class Number(value: BigInt) extends ArithExpr

/**
 * An explicit reference to an existing e-class in the e-graph.
 *
 * This is useful in examples/rules where we want to splice a matched subtree back
 * into another expression. Deriving [[Box]] ensures that matcher/applier treat `Ref`
 * as a leaf (no recursive matching into the referenced class).
 */
final case class Ref(eClass: EClassCall) extends ArithExpr derives Box

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

/**
 * Simple wrapper used by the analysis box.
 *
 * Analyses can produce `Fact[A]` nodes when convenient. For instance, constant-propagation
 * might compute an `Option[BigInt]` and rules can consult/box that information.
 */
final case class Fact[A](value: A) extends ArithExpr

/**
 * Companion configures analysis boxing for this surface language.
 *
 * By implementing [[AnalysisBox]] we tell Foresight that analyses may embed results
 * as `Fact[A]` nodes inside this AST. This is optional but makes certain examples
 * and rules terser.
 */
object ArithExpr {
  given AnalysisBox[ArithExpr] with
    type Box[A] = Fact[A]
    def box[A](a: A): Fact[A] = Fact(a)
}

// --- Convenience surface syntax (nice to read in examples) -------------------

/** Implicit conversion so integer literals write as `Number` in examples.
 *
 * {{{
 * Number(3) == (3: ArithExpr)
 * }}}
 */
given Conversion[Int, ArithExpr] with
  def apply(n: Int): ArithExpr = Number(BigInt(n))

/** Infix operators for building trees concisely in rules/tests. */
extension (lhs: ArithExpr)
  /** {{{ x + y }}} builds an [[Add]] node. */
  infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
  /** {{{ x * y }}} builds a [[Mul]] node. */
  infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

/**
 * Alias for the concrete e-graph node type of this language.
 *
 * Most APIs in Foresight are polymorphic in the node type. Naming it `ArithIR`
 * makes example code clearer.
 */
type ArithIR = LanguageOp[ArithExpr]
