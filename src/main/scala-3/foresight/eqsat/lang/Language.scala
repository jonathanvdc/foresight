package foresight.eqsat.lang

import scala.deriving.*
import foresight.eqsat.{MixedTree, Slot}

import scala.compiletime.{erasedValue, summonAll, summonInline}

// Mark Slot fields: use vs binder
final case class Use[A](value: A) extends AnyVal
final case class Defn[A](value: A) extends AnyVal

trait Language[E]:
  /** Compact operator tag: (constructor ordinal, field schema, payloads). */
  final case class Op(ord: Int, schema: IArray[Byte], payload: IArray[Any])
  type Node[A] = MixedTree[Op, A]

  /** Encode surface AST into the core tree. */
  def toTree[A](e: E): Node[A]

  /** Decode core tree back to the surface AST. */
  def fromTree[A](n: Node[A]): E

object Language:

  inline given derived[E](using m: Mirror.SumOf[E]): Language[E] =
    new Language[E]:

      // Build per-constructor builders: Product => E
      private val ctors: Array[Product => E] =
        ctorArray[E](using m)

      /** Encode one constructor instance. */
      private def encodeCase[A](ord: Int, e: E): Node[A] =
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

      def toTree[A](e: E): Node[A] =
        encodeCase(m.ordinal(e), e)

      def fromTree[A](n: Node[A]): E =
        n match
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
                case 3 => elems(i) = fromTree(ek.next())  // recurse
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
