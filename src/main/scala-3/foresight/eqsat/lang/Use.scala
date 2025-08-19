package foresight.eqsat.lang

import foresight.eqsat.Slot
import scala.Conversion
import scala.language.implicitConversions

/**
 * Labels a value as a use/read site in the AST or IR.
 *
 * In slotted e-graphs, binders and uses are often both represented as [[Slot]]s.
 * Wrapping them in `Use[A]` (and its counterpart `Def[A]`) makes intent explicit in type
 * signatures and constructors while remaining allocation-free at runtime (`AnyVal`).
 *
 * @tparam A the underlying payload (often [[Slot]])
 *
 * @example
 * {{{
 * sealed trait Expr derives Language
 * final case class Var(param: Use[Slot]) extends Expr
 *
 * val x = Slot.fresh()
 * Var(Use(x)) // a use of slot x
 * Var(x)      // also a use of slot x, but less explicit
 * }}}
 */
final case class Use[A](value: A) extends AnyVal

object Use:

  /**
   * Implicitly lift a raw [[Slot]] into a [[Use]].
   *
   * Keeps call sites concise where a `Use[Slot]` is expected but you have a `Slot`.
   * If both `Use[Slot]` and `Def[Slot]` are viable in the same position, disambiguate
   * explicitly with `Use(s)` or `Def(s)`.
   */
  given Conversion[Slot, Use[Slot]] with
    def apply(s: Slot): Use[Slot] = Use(s)
