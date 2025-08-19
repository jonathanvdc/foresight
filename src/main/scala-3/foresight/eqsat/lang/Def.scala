package foresight.eqsat.lang

import foresight.eqsat.Slot
import scala.Conversion
import scala.language.implicitConversions

/**
 * Labels a value as a binder/definition site in the AST or IR.
 *
 * In slotted e-graphs, both binders and uses are represented as raw [[Slot]]s. Wrapping
 * them as `Def[A]` (and its counterpart `Use[A]`) makes intent explicit in type
 * signatures and constructors, while staying allocation-free at runtime (`AnyVal`).
 *
 * @tparam A the underlying payload (commonly [[Slot]])
 *
 * @example
 * {{{
 * sealed trait Expr derives Language
 * final case class Lambda(param: Use[Slot], body: Expr) extends Expr
 * final case class Var(slot: Use[Slot]) extends Expr
 *
 * val x = Slot.fresh()
 * Lambda(Def(x), Var(Use(x))) // defines slot x
 * Lambda(x, Var(x))           // also defines slot x, but less explicit
 * }}}
 */
final case class Def[A](value: A) extends AnyVal

object Def:

  /**
   * Implicitly lift a raw [[Slot]] into a [[Def]][Slot].
   *
   * Keeps call sites concise where a `Def[Slot]` is expected but you have a `Slot`.
   * If both `Def[Slot]` and `Use[Slot]` are applicable at the same call site, disambiguate
   * explicitly with `Def(s)` or `Use(s)`.
   */
  given Conversion[Slot, Def[Slot]] with
    def apply(s: Slot): Def[Slot] = Def(s)
