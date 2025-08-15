package foresight.eqsat.lang

import scala.deriving.*
import foresight.eqsat.{MixedTree, Slot}

import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}
import scala.util.NotGiven

// Mark Slot fields: use vs binder
final case class Use[A](value: A) extends AnyVal
final case class Defn[A](value: A) extends AnyVal

trait AsAtom[T, B]:
  def toAtom(t: T): B
  def fromAtom(b: B): T

object AsAtom:
  inline def apply[T, B](using ev: AsAtom[T, B]): AsAtom[T, B] = ev

  /** Helper to build a two-way codec. */
  def codec[T, B](to: T => B, from: B => T): AsAtom[T, B] =
    new AsAtom[T, B]:
      override def toAtom(t: T): B = to(t)
      override def fromAtom(b: B): T = from(b)

// ---------- Registries ----------
object Registries:
  opaque type AtomEncoder[E, A] = (E, Int) => Option[A]
  opaque type AtomDecoder[E, A] = A => Option[E]

  def encode[E, A](encoder: AtomEncoder[E, A], value: E, ord: Int): Option[A] =
    encoder(value, ord)
  def decode[E, A](decoder: AtomDecoder[E, A], call: A): Option[E] =
    decoder(call)

  object AtomEncoder:
    // Build a *closed* encoder at summon time: capture AsAtom per case into the returned function.
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

  object AtomDecoder:
    // Build a *closed* decoder at summon time: capture AsMixin per case.
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

trait Language[E]:
  /** Compact operator tag: (constructor ordinal, field schema, payloads). */
  final case class Op(ord: Int, schema: IArray[Byte], payload: IArray[Any])
  type Node[A] = MixedTree[Op, A]

  /** Encode surface AST into the core tree. */
  def toTree[A](e: E)(using enc: Registries.AtomEncoder[E, A]): Node[A]

  /** Decode core tree back to the surface AST. */
  def fromTree[A](n: Node[A])(using dec: Registries.AtomDecoder[E, A]): E

object Language:

  inline given derived[E](using m: Mirror.SumOf[E]): Language[E] =
    new Language[E]:

      // Build per-constructor builders: Product => E
      private val ctors: Array[Product => E] =
        ctorArray[E](using m)

      /** Encode one constructor instance. */
      private def encodeCase[A](ord: Int, e: E)(using enc: Registries.AtomEncoder[E, A]): Node[A] =
        val p = e.asInstanceOf[Product]
        val binders = scala.collection.mutable.ArrayBuffer.empty[Slot]
        val slots   = scala.collection.mutable.ArrayBuffer.empty[Slot]
        val kids    = scala.collection.mutable.ArrayBuffer.empty[Node[A]]
        val payload = scala.collection.mutable.ArrayBuffer.empty[Any]
        val schema  = scala.collection.mutable.ArrayBuffer.empty[Byte]

        var i = 0
        val n = p.productArity
        while i < n do
          p.productElement(i) match
            case Defn(s: Slot) =>
              binders += s; schema += 1
            case Use(s: Slot)  =>
              slots   += s; schema += 2
            case child: E =>
              kids    += toTree[A](child); schema += 3
            case other =>
              payload += other; schema += 4
          i += 1

        MixedTree.Node(
          Op(ord, IArray.from(schema), IArray.from(payload)),
          binders.toSeq,
          slots.toSeq,
          kids.toSeq
        )

      def toTree[A](e: E)(using enc: Registries.AtomEncoder[E, A]): Node[A] =
        Registries.encode(enc, e, m.ordinal(e)) match
          case Some(payload) => MixedTree.Atom(payload)
          case None => encodeCase[A](m.ordinal(e), e)(using enc)

      def fromTree[A](n: Node[A])(using dec: Registries.AtomDecoder[E, A]): E =
        n match
          case MixedTree.Atom(b) =>
            // Rebuild a concrete case C <: E from the call payload, if possible
            Registries.decode(dec, b).getOrElse {
              throw new IllegalArgumentException(
                s"fromTree: no decoder for call payload: $b"
              )
            }

          case MixedTree.Node(op, binders, slots, kids) =>
            val schema = op.schema
            val eb = binders.iterator
            val es = slots.iterator
            val ek = kids.iterator
            val ep = op.payload.iterator

            // Rebuild the product elements in original order
            val elems = new Array[Any](schema.length)
            var i = 0
            while i < schema.length do
              (schema(i): @annotation.switch) match
                case 1 => elems(i) = Defn(eb.next())
                case 2 => elems(i) = Use(es.next())
                case 3 => elems(i) = fromTree(ek.next())(using dec)  // recurse
                case 4 => elems(i) = ep.next()
              i += 1

            // Feed them to the right case constructor
            ctors(op.ord)(Tuple.fromArray(elems))

  // === helpers ===

  // ctor table
  private inline def ctorArray[E](using m: Mirror.SumOf[E]): Array[Product => E] =
    mirrorsToCtors[E](
      summonAll[Tuple.Map[m.MirroredElemTypes, [c] =>> Mirror.ProductOf[c]]]
    )

  private def mirrorsToCtors[E](ms: Tuple): Array[Product => E] =
    ms.productIterator
      .map { pc =>
        val pco = pc.asInstanceOf[Mirror.ProductOf[Any]]
        (p: Product) => pco.fromProduct(p).asInstanceOf[E]
      }
      .toArray
