package foresight.eqsat.lang

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

opaque type AtomDecoder[E, A] = A => Option[E]

object AtomDecoder:
  def decode[E, A](decoder: AtomDecoder[E, A], call: A): Option[E] =
    decoder(call)
  
  // Build a decoder at summon time: capture AsAtom per case.
  inline given derived[E, A](using s: Mirror.SumOf[E]): AtomDecoder[E, A] =
    buildDecoder[E, A, s.MirroredElemTypes]

  private inline def decoderForCase[E, A, H]: AtomDecoder[E, A] =
    summonFrom {
      case ev: AsAtom[H, A] =>
        // capture `ev` here
        (a: A) => Some(ev.fromAtom(a).asInstanceOf[E])
      case _ =>
        (_: A) => None
    }

  private inline def buildDecoder[E, A, Elems <: Tuple]: AtomDecoder[E, A] =
    inline erasedValue[Elems] match
      case _: (h *: t) =>
        val head: AtomDecoder[E, A] = decoderForCase[E, A, h]
        val tail: AtomDecoder[E, A] = buildDecoder[E, A, t]
        (a: A) => head(a).orElse(tail(a))
      case _: EmptyTuple =>
        (_: A) => None