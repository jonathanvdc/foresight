package foresight.eqsat.lang

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

/**
 * A decoder from leaf payloads `A` back to the surface language `E`.
 *
 * Conceptually, `AtomDecoder[E, A]` tries to interpret an atom (leaf) of type `A`
 * as an `E` term. It is *partial*—decoding may fail—and therefore returns `Option[E]`.
 *
 * Typical sources of decoders:
 *  - From an `Atom`/`AsAtom` bridge for a constructor `C <: E` whose payload type is `A`.
 *  - From an `AnalysisBox[E]` when the payload is `AnalysisFact[A]` (embed analysis results).
 *  - Via automatic derivation for sum types `E` (tries each case that has an `AsAtom` bridge).
 *
 * @example Single-field wrappers as atoms
 * {{{
 * sealed trait Expr derives Language
 * final case class Ref(id: EClassCall) extends Expr derives Box
 *
 * // Bridge exists:
 * val dec: AtomDecoder[Expr, EClassCall] = summon[AtomDecoder[Expr, EClassCall]]
 * dec(id)            // Some(Ref(id))
 * }}}
 *
 * @example Boxing analysis results
 * {{{
 * sealed trait Expr derives Language
 * final case class Fact[A](value: A) extends Expr
 *
 * given AnalysisBox[Expr] with
 *   type Box[X] = Fact[X]
 *   def box[X](x: X) = Fact(x)
 *
 * val decF: AtomDecoder[Expr, AnalysisFact[Int]] =
 *   summon[AtomDecoder[Expr, AnalysisFact[Int]]]
 *
 * decF(AnalysisFact(42)) // Some(Fact(42))
 * }}}
 *
 * @example Fallback/derivation for sums
 * {{{
 * sealed trait Expr derives Language
 * final case class Lit(i: Int) extends Expr derives Box
 * final case class Ref(id: EClassCall) extends Expr derives Box
 *
 * val dInt: AtomDecoder[Expr, Int]        = summon
 * val dRef: AtomDecoder[Expr, EClassCall] = summon
 * }}}
 */
opaque type AtomDecoder[E, A] = A => Option[E]

/**
 * Companion object for [[AtomDecoder]].
 *
 * Provides constructors, decoders, and instances.
 */
object AtomDecoder extends LowPriorityAtomDecoder:

  /**
   * Build a decoder from a function `A => Option[E]`.
   *
   * @param f Function implementing the decoding logic.
   * @tparam E Target surface type.
   * @tparam A Source atom payload type.
   * @return An `AtomDecoder[E, A]` wrapping `f`.
   */
  def apply[E, A](f: A => Option[E]): AtomDecoder[E, A] = f

  /**
   * Run a decoder on a value.
   *
   * @param decoder The decoder to run.
   * @param call The atom payload to decode.
   * @tparam E Target surface type.
   * @tparam A Source atom payload type.
   * @return `Some(e)` if decoding succeeded, `None` if it failed.
   */
  def decode[E, A](decoder: AtomDecoder[E, A], call: A): Option[E] =
    decoder(call)

  /**
   * A no-op decoder that always returns `None`.
   * @tparam E Target surface type.
   * @tparam A Source atom payload type.
   * @return An `AtomDecoder[E, A]` that does not decode anything.
   */
  def noDecoding[E, A]: AtomDecoder[E, A] = (_: A) => None

  /**
   * High-priority decoder for analysis facts:
   * if an `AnalysisBox[E]` is available, any `AnalysisFact[A]` can be embedded back into `E`.
   *
   * This lets generic code reconstruct surface terms from analysis views.
   */
  given analysisDecoder[E, A](using bx: AnalysisBox[E]): AtomDecoder[E, AnalysisFact[A]] =
  AtomDecoder(v => Some(bx.box(v.value)))

  /**
   * High-priority universal bridge:
   * if there is an `AsAtom[C, A]` and `C <: E`, we can decode `A` into the constructor `C`,
   * and thus into `E`.
   *
   * This is the usual path for single-field case classes declared with `derives Box`.
   */
  given atomDecoderFromAsAtom[E, C, A](using ev: C <:< E, aa: AsAtom[C, A]): AtomDecoder[E, A] =
  (a: A) => Some(ev(aa.fromAtom(a)))

/**
 * Low-priority derivation for sum types `E`.
 *
 * If `E` is a sealed trait/enum and some of its cases have an `AsAtom[Case, A]`,
 * the derived decoder tries each such case in order until one succeeds.
 *
 * Priority: these derived instances are lower priority than the specific bridges above,
 * so explicit instances or `AnalysisBox`/`AsAtom` bridges win when present.
 */
trait LowPriorityAtomDecoder:

  /**
   * Derive a decoder for sum type `E`, attempting each case that provides an `AsAtom`.
   *
   * Summoning `AtomDecoder[E, A]` will succeed if **any** case of `E` can decode from `A`.
   * If no cases match, decoding returns `None`.
   */
  inline given derived[E, A](using s: Mirror.SumOf[E]): AtomDecoder[E, A] =
    buildDecoder[E, A, s.MirroredElemTypes]

  /** Build a decoder for one case: succeeds iff an `AsAtom[H, A]` is available. */
  private inline def decoderForCase[E, A, H]: AtomDecoder[E, A] =
    summonFrom {
      case ev: AsAtom[H, A] =>
        AtomDecoder((a: A) => Some(ev.fromAtom(a).asInstanceOf[E]))
      case _ =>
        AtomDecoder((_: A) => None)
    }

  /** Fold all cases: try head, else tail. Returns `None` if no case can decode. */
  private inline def buildDecoder[E, A, Elems <: Tuple]: AtomDecoder[E, A] =
    inline erasedValue[Elems] match
      case _: (h *: t) =>
        val head: AtomDecoder[E, A] = decoderForCase[E, A, h]
        val tail: AtomDecoder[E, A] = buildDecoder[E, A, t]
        AtomDecoder((a: A) => AtomDecoder.decode(head, a).orElse(AtomDecoder.decode(tail, a)))
      case _: EmptyTuple =>
        AtomDecoder((_: A) => None)
