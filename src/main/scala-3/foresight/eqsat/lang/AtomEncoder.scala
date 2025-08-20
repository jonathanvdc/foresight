package foresight.eqsat.lang

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

/**
 * Encoder from a surface value `E` to a leaf payload `A`.
 *
 * Conceptually, `AtomEncoder[E, A]` answers: “If `E` is a case whose atom payload is `A`,
 * extract that payload; otherwise, say `None`.”
 *
 * Its function shape is `(E, caseOrdinal) => Option[A]`, where `caseOrdinal` is the
 * ordinal of the constructor for `E` (as defined by `Mirror.SumOf[E]`). When the ordinal
 * matches a case that has an `AsAtom[Case, A]`, encoding succeeds with `Some(payload)`;
 * otherwise it returns `None`.
 *
 * You usually don’t pass ordinals yourself. Higher-level code (e.g., language encoders)
 * supplies the correct ordinal for the current constructor.
 *
 * Typical sources:
 *   - **Derived** for sealed sum types `E`: if a case `C <: E` has `AsAtom[C, A]`
 *     (e.g., via `derives Box` on a single-field case class), the derived encoder
 *     can extract the leaf payload `A` from values of that case.
 *   - **Manual** instances are rarely needed; prefer adding `derives Box` to the case
 *     and letting `AsAtom` + derivation do the work.
 *
 * ### Laws
 * For any value `e: E` of case `C` with ordinal `ordC`:
 *   - If there is `AsAtom[C, A]`, then `encode(enc, e, ordC)` must be `Some(a)` for the
 *     exact payload `a`, and `encode(enc, e, ord)` must be `None` for `ord != ordC`.
 *   - If no `AsAtom[C, A]` exists, encoding returns `None` for all ordinals.
 *
 * @example
 * {{{
 * sealed trait Expr derives Language
 * final case class Lit(i: Int) extends Expr derives Box
 * final case class Ref(id: EClassCall) extends Expr derives Box
 *
 * // Encoders exist for each payload type that a case exposes:
 * val encInt: AtomEncoder[Expr, Int] = summon
 * val encEClassCall: AtomEncoder[Expr, EClassCall] = summon
 *
 * val L = summon[Language[Expr]]
 * val lit = Lit(42)
 * val ref = Ref(eclass)
 * val litTree = L.toTree[Int](lit)(using encInt)
 * val refTree = L.toTree[EClassCall](ref)(using encEClassCall)
 * }}}
 */
opaque type AtomEncoder[E, A] = (E, Int) => Option[A]

/**
 * Companion for `AtomEncoder[E, A]`.
 *
 * Provides a derived instance for sum types, and a shorthand to run an encoder.
 */
object AtomEncoder:

  /**
   * Run an encoder on a value and a case ordinal.
   *
   * Shorthand to keep call sites uniform.
   */
  def encode[E, A](encoder: AtomEncoder[E, A], value: E, ord: Int): Option[A] =
    encoder(value, ord)

  /**
   * A no-op encoder that always returns `None`.
   */
  def noEncoding[E, A]: AtomEncoder[E, A] = (_: E, _: Int) => None

  /**
   * Derived encoder for a sum type `E`.
   *
   * The derived instance captures, at summon time, the `AsAtom` bridges for each case of `E`.
   * At runtime it dispatches by the provided `caseOrdinal` and attempts to encode only the
   * matching case; if that case has no `AsAtom` for `A`, it returns `None`.
   *
   * Priority: since this is a `given` in the companion, it is the default way to obtain
   * an `AtomEncoder[E, A]`. Provide an explicit instance if you need custom behavior.
   */
  inline given derived[E, A](using s: Mirror.SumOf[E]): AtomEncoder[E, A] =
    buildEncoder[E, A, s.MirroredElemTypes](start = 0)

  // For one case type `H`, either capture its AsAtom into a tiny function, or return a no-op.
  private inline def encoderForCase[E, A, H](caseIndex: Int): AtomEncoder[E, A] =
    summonFrom {
      case ev: AsAtom[H, A] =>
        // Capture the bridge once at summon time.
        (e: E, ord: Int) =>
          if ord == caseIndex then Some(ev.toAtom(e.asInstanceOf[H])) else None
      case _ =>
        // No bridge for this case => cannot encode to A.
        (_: E, _: Int) => None
    }

  // Fold all cases into a single function that tries head, then tail.
  private inline def buildEncoder[E, A, Elems <: Tuple](start: Int): AtomEncoder[E, A] =
    inline erasedValue[Elems] match
      case _: (h *: t) =>
        val head: AtomEncoder[E, A] = encoderForCase[E, A, h](start)
        val tail: AtomEncoder[E, A] = buildEncoder[E, A, t](start + 1)
        (e: E, ord: Int) => head(e, ord).orElse(tail(e, ord))
      case _: EmptyTuple =>
        (_: E, _: Int) => None
