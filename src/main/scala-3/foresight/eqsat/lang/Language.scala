package foresight.eqsat.lang

import foresight.eqsat.rewriting.{ReversibleSearcher, Rule}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}
import foresight.eqsat.{EGraph, EGraphLike, MixedTree, Slot}

import scala.deriving.*
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}

trait Language[E]:
  /** Compact operator tag: (constructor ordinal, field schema, payloads). */
  final case class Op(ord: Int, schema: Seq[Byte], payload: Seq[Any])
  type MTree[A] = MixedTree[Op, A]

  def opOrdering: Ordering[Op]

  /** Encode surface AST into the core tree. */
  def toTree[A](e: E)(using enc: AtomEncoder[E, A]): MTree[A]

  /** Decode core tree back to the surface AST. */
  def fromTree[A](n: MixedTree[Language[E]#Op, A])(using dec: AtomDecoder[E, A]): E

  def toSearcher[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]](e: E)(using enc: AtomEncoder[E, Pattern.Var]): ReversibleSearcher[Op, PatternMatch[Op], EGraphT] =
    toTree(e).toSearcher

  def toApplier[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]](e: E)(using enc: AtomEncoder[E, Pattern.Var]): PatternApplier[Op, EGraphT] =
    toTree(e).toApplier

  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String, lhs: E, rhs: E)
  (using enc: AtomEncoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] = {
    Rule(name, toSearcher[EGraphT](lhs), toApplier[EGraphT](rhs))
  }

  def fromAnalysisNode[A](node: Language[E]#Op, defs: Seq[Slot], uses: Seq[Slot], args: Seq[A])(using dec: AtomDecoder[E, AnalysisFact[A]]): E = {
    fromTree[AnalysisFact[A]](MixedTree.Node(node, defs, uses, args.map(AnalysisFact(_)).map(MixedTree.Atom(_))))(using dec)
  }


object Language:

  /** If a Language[E] is in scope, its Op ordering is summonable. */
  given opOrderingFor[E](using L: Language[E]): Ordering[L.Op] =
    L.opOrdering

  // Bridge for the projected type Language[E]#Op
  // Prefer the L.Op version above to avoid cross-instance mixing.
  given projectedOpOrdering[E](using L: Language[E]): Ordering[Language[E]#Op] =
  new Ordering[Language[E]#Op]:
    def compare(a: Language[E]#Op, b: Language[E]#Op): Int =
      // Unsafe if a,b come from different Language[E] instances.
      L.opOrdering.compare(a.asInstanceOf[L.Op], b.asInstanceOf[L.Op])

  inline given derived[E](using m: Mirror.SumOf[E]): Language[E] =
    new Language[E]:
      private val payComps: Array[Array[(Any, Any) => Int]] =
        payloadComparatorsFor[E](using m)

      given opOrdering: Ordering[Op] with
        def compare(a: Op, b: Op): Int =
          val c1 = Integer.compare(a.ord, b.ord)
          if c1 != 0 then return c1

          // compare schema lexicographically
          val as = a.schema;
          val bs = b.schema
          val n = math.min(as.length, bs.length)
          var i = 0
          while i < n do
            val c = java.lang.Byte.compare(as(i), bs(i))
            if c != 0 then return c
            i += 1
          val c2 = Integer.compare(as.length, bs.length)
          if c2 != 0 then return c2

          // compare payload with precomputed per-ordinal comparators
          val ap = a.payload;
          val bp = b.payload
          val cs = payComps(a.ord)
          val m = math.min(math.min(ap.length, bp.length), cs.length)
          var j = 0
          while j < m do
            val c = cs(j)(ap(j), bp(j))
            if c != 0 then return c
            j += 1
          Integer.compare(ap.length, bp.length)

      // Build per-constructor builders: Product => E
      private val ctors: Array[Product => E] =
        ctorArray[E](using m)

      /** Encode one constructor instance. */
      private def encodeCase[A](ord: Int, e: E)(using enc: AtomEncoder[E, A]): MTree[A] =
        val p = e.asInstanceOf[Product]
        val binders = scala.collection.mutable.ArrayBuffer.empty[Slot]
        val slots   = scala.collection.mutable.ArrayBuffer.empty[Slot]
        val kids    = scala.collection.mutable.ArrayBuffer.empty[MTree[A]]
        val payload = scala.collection.mutable.ArrayBuffer.empty[Any]
        val schema  = scala.collection.mutable.ArrayBuffer.empty[Byte]

        var i = 0
        val n = p.productArity
        while i < n do
          p.productElement(i) match
            case Def(s: Slot) =>
              binders += s; schema += 1
            case Use(s: Slot)  =>
              slots   += s; schema += 2
            case child: E =>
              kids    += toTree[A](child); schema += 3
            case other =>
              payload += other; schema += 4
          i += 1

        MixedTree.Node(
          Op(ord, Seq.from(schema), Seq.from(payload)),
          binders.toSeq,
          slots.toSeq,
          kids.toSeq
        )

      def toTree[A](e: E)(using enc: AtomEncoder[E, A]): MTree[A] =
        AtomEncoder.encode(enc, e, m.ordinal(e)) match
          case Some(payload) => MixedTree.Atom(payload)
          case None => encodeCase[A](m.ordinal(e), e)(using enc)

      def fromTree[A](n: MixedTree[Language[E]#Op, A])(using dec: AtomDecoder[E, A]): E =
        n match
          case MixedTree.Atom(b) =>
            // Rebuild a concrete case C <: E from the call payload, if possible
            AtomDecoder.decode(dec, b).getOrElse {
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
                case 1 => elems(i) = Def(eb.next())
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

  // -------- helpers to build per-constructor payload comparators --------

  private inline def summonComparator[T]: (Any, Any) => Int =
    summonFrom {
      case ord: Ordering[T] =>
        (a: Any, b: Any) => ord.compare(a.asInstanceOf[T], b.asInstanceOf[T])
      case _ =>
        (a: Any, b: Any) => a.toString.compareTo(b.toString) // total fallback
    }

  private inline def compsForElems[Elems <: Tuple, Rec]: List[(Any, Any) => Int] =
    inline erasedValue[Elems] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        val head: List[(Any, Any) => Int] =
          inline erasedValue[h] match
            case _: Def[?] => Nil
            case _: Use[?] => Nil
            case _: Rec => Nil // skip recursive fields of the same case type
            case _ => summonComparator[h] :: Nil
        head ::: compsForElems[t, Rec]

  private inline def compsForCase[C](using p: Mirror.ProductOf[C]): Array[(Any, Any) => Int] =
    compsForElems[p.MirroredElemTypes, C].toArray

  private inline def compsForCasesList[Cases <: Tuple]: List[Array[(Any, Any) => Int]] =
    inline erasedValue[Cases] match
      case _: EmptyTuple => Nil
      case _: (c *: cs) =>
        // Critically: summonInline here, pass as `using`, do NOT store in a val.
        compsForCase[c](using summonInline[Mirror.ProductOf[c]]) :: compsForCasesList[cs]

  inline def payloadComparatorsFor[E](using s: Mirror.SumOf[E]): Array[Array[(Any, Any) => Int]] =
    type Cases = s.MirroredElemTypes
    compsForCasesList[Cases].toArray