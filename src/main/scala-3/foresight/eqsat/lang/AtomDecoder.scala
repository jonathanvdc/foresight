package foresight.eqsat.lang

import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

opaque type AtomDecoder[E, A] = A => Option[E]

object AtomDecoder extends LowPriorityAtomDecoder:
  def apply[E, A](f: A => Option[E]): AtomDecoder[E, A] = f

  def decode[E, A](decoder: AtomDecoder[E, A], call: A): Option[E] =
    decoder(call)

  // High-priority decoder if AnalysisBox is available: AnalysisFact[A] => AtomDecoder[E, AnalysisFact[A]]
  given analysisDecoder[E, A](using bx: AnalysisBox[E]): AtomDecoder[E, AnalysisFact[A]] =
    AtomDecoder(v => Some(bx.box(v.value)))

  // High-priority universal bridge: AsAtom[C, A] => AtomDecoder[E, A] when C <: E
  given atomDecoderFromAsAtom[E, C, A](using ev: C <:< E, aa: AsAtom[C, A]): AtomDecoder[E, A] =
    (a: A) => Some(ev(aa.fromAtom(a)))

trait LowPriorityAtomDecoder:
  // Build a decoder at summon time: capture AsAtom per case.
  inline given derived[E, A](using s: Mirror.SumOf[E]): AtomDecoder[E, A] =
    buildDecoder[E, A, s.MirroredElemTypes]

  private inline def decoderForCase[E, A, H]: AtomDecoder[E, A] =
    summonFrom {
      case ev: AsAtom[H, A] =>
        // capture `ev` here
        AtomDecoder((a: A) => Some(ev.fromAtom(a).asInstanceOf[E]))
      case _ =>
        AtomDecoder((_: A) => None)
    }

  private inline def buildDecoder[E, A, Elems <: Tuple]: AtomDecoder[E, A] =
    inline erasedValue[Elems] match
      case _: (h *: t) =>
        val head: AtomDecoder[E, A] = decoderForCase[E, A, h]
        val tail: AtomDecoder[E, A] = buildDecoder[E, A, t]
        AtomDecoder((a: A) => AtomDecoder.decode(head, a).orElse(AtomDecoder.decode(tail, a)))
      case _: EmptyTuple =>
        AtomDecoder((_: A) => None)
