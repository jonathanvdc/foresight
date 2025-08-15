package foresight.eqsat.lang

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

opaque type AtomEncoder[E, A] = (E, Int) => Option[A]

object AtomEncoder:
  def encode[E, A](encoder: AtomEncoder[E, A], value: E, ord: Int): Option[A] =
    encoder(value, ord)

  // Build an encoder at summon time: capture AsAtom per case into the returned function.
  inline given derived[E, A](using s: Mirror.SumOf[E]): AtomEncoder[E, A] =
    buildEncoder[E, A, s.MirroredElemTypes](start = 0)

  // For one case type `H`, either capture its AsAtom into a tiny function, or return a no-op.
  private inline def encoderForCase[E, A, H](caseIndex: Int): AtomEncoder[E, A] =
    summonFrom {
      case ev: AsAtom[H, A] =>
        // capture `ev` here; no summoning will happen later
        (e: E, ord: Int) =>
          if ord == caseIndex then Some(ev.toAtom(e.asInstanceOf[H])) else None
      case _ =>
        // no AsAtom -> no-op for this case
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
