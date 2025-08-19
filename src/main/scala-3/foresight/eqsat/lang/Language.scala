package foresight.eqsat.lang

import foresight.eqsat.rewriting.{ReversibleSearcher, Rule}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}
import foresight.eqsat.{EGraph, EGraphLike, MixedTree, Slot}
import foresight.util.ordering.SeqOrdering

import scala.deriving.*
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}

/**
 * Type class describing how a surface AST `E` maps to the core e-graph language.
 *
 * A `Language[E]` provides:
 *  - a node type alias [[Op]] (backed by `LanguageOp[E]`) that the e-graph stores,
 *  - encoding/decoding between `E` and a core tree [[MTree]],
 *  - an ordering over nodes (for deterministic e-graph behavior),
 *  - convenience bridges from `E` values to matchers/appliers/rules,
 *  - and a helper to reconstruct `E` from an analysis-view of a node.
 *
 * @example Typical pattern
 * {{{
 * sealed trait ArithExpr derives Language
 * final case class Add(x: ArithExpr, y: ArithExpr) extends ArithExpr
 * // ...
 *
 * val Lang = summon[Language[ArithExpr]]
 * val e: ArithExpr = Add( /* ... */ )
 *
 * // Encode to a core tree
 * val t: Lang.MTree[Pattern.Var] = Lang.toTree[Pattern.Var](e)
 *
 * // Build a rewrite rule directly from surface syntax
 * val r = Lang.rule("comm-add", Add(x, y), Add(y, x))
 * }}}
 */
trait Language[E]:

  /** The node type stored in the e-graph for this surface language. */
  type Op = LanguageOp[E]

  /**
   * The core tree shape for this language.
   *
   * `MTree[A]` is a mixed tree whose internal nodes are [[Op]] and whose leaves are atoms of type `A`.
   * Different choices of `A` let the same surface syntax serve distinct purposes (e.g., patterns vs. concrete calls).
   */
  type MTree[A] = MixedTree[Op, A]

  /**
   * Total ordering on [[Op]].
   *
   * Used for deterministic hashing/canonicalization, stable printing, and for any data structures
   * that rely on an `Ordering` (e.g., priority queues). Implementations should be consistent with
   * structural equality of `Op`.
   */
  def opOrdering: Ordering[Op]

  /**
   * Encode a surface AST node `e: E` into the core tree representation [[MTree]].
   *
   * @tparam A    The atom payload type at the leaves (e.g., [[foresight.eqsat.rewriting.patterns.Pattern.Var]],
   *              [[EClassCall]], etc.).
   * @param e     A surface AST node.
   * @param enc   A given [[AtomEncoder]] that knows how to encode the leaf atoms for `A`.
   * @return      A mixed core tree equivalent to `e`.
   *
   * @example
   * {{{
   * val patTree: Lang.MTree[Pattern.Var] = Lang.toTree[Pattern.Var](surfaceExpr)
   * }}}
   */
  def toTree[A](e: E)(using enc: AtomEncoder[E, A]): MTree[A]

  /**
   * Decode a core tree back into a surface AST `E`.
   *
   * @tparam A    The atom payload type present at the leaves.
   * @param n     A mixed tree with [[Op]] internal nodes and `A` atoms.
   * @param dec   A given [[AtomDecoder]] that knows how to turn `A` atoms back into `E` fragments.
   * @return      The reconstructed surface expression.
   *
   * Note: decoding is typically partial in the presence of analysis-only atoms; see [[fromAnalysisNode]].
   */
  def fromTree[A](n: MixedTree[Op, A])(using dec: AtomDecoder[E, A]): E

  /**
   * Build a reversible searcher from a surface expression, by first encoding to a pattern tree.
   *
   * Requires an encoder for [[foresight.eqsat.rewriting.patterns.Pattern.Var]] leaves, i.e., a way to turn surface
   * leaves into pattern variables.
   *
   * @param e        Surface-side pattern.
   * @tparam EGraphT An e-graph type that supports this language.
   */
  def toSearcher[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]](e: E)
                                                                    (using enc: AtomEncoder[E, Pattern.Var]): ReversibleSearcher[Op, PatternMatch[Op], EGraphT] =
    toTree(e).toSearcher

  /**
   * Build a pattern applier from a surface expression, by first encoding to a pattern tree.
   *
   * This is commonly used for rule RHS construction.
   *
   * @param e        Surface-side template for rewriting.
   * @tparam EGraphT An e-graph type that supports this language.
   */
  def toApplier[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]](e: E)
                                                                   (using enc: AtomEncoder[E, Pattern.Var]): PatternApplier[Op, EGraphT] =
    toTree(e).toApplier

  /**
   * Construct a rewrite rule directly from surface syntax.
   *
   * @param name    Human-readable rule name (used in logs/diagnostics).
   * @param lhs     Surface pattern to match.
   * @param rhs     Surface template to build.
   * @param enc     Encoder for [[foresight.eqsat.rewriting.patterns.Pattern.Var]] leaves.
   * @tparam EGraphT An e-graph type that supports this language.
   *
   * @example
   * {{{
   * val r =
   *   Lang.rule("comm-add", Add(x, y), Add(y, x))
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String, lhs: E, rhs: E)
  (using enc: AtomEncoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    Rule(name, toSearcher[EGraphT](lhs), toApplier[EGraphT](rhs))

  /**
   * Reconstruct a surface expression `E` from an *analysis-view* of a single node.
   *
   * This helper wraps argument analysis results `args: Seq[A]` in [[AnalysisFact]] atoms and
   * builds a one-level [[MixedTree.Node]] with the given `node`, `defs`, and `uses`.
   * It then decodes the mixed tree using a decoder for `AnalysisFact[A]` atoms.
   *
   * This is useful when implementing analyses that want to "peek" back into the surface
   * language at a particular node boundary, while still operating within the core representation.
   *
   * @param node  The core node payload ([[Op]]).
   * @param defs  Slots defined by this node (binders).
   * @param uses  Slots used by this node (reads).
   * @param args  Analysis results for the node’s children, in order.
   * @param dec   A decoder from [[AnalysisFact]] atoms back to surface `E`.
   * @tparam A    The analysis result type.
   * @return      The surface expression reconstructed from this node’s analysis view.
   *
   * @example
   * {{{
   * val surf: E =
   *   Lang.fromAnalysisNode(resultNode, defs, uses, childFacts)(using analysisFactDecoder)
   * }}}
   */
  def fromAnalysisNode[A](node: Op, defs: Seq[Slot], uses: Seq[Slot], args: Seq[A])
                         (using dec: AtomDecoder[E, AnalysisFact[A]]): E =
    fromTree[AnalysisFact[A]](
      MixedTree.Node(
        node,
        defs,
        uses,
        args.map(AnalysisFact(_)).map(MixedTree.Atom(_))
      )
    )

/**
 * Companion object for [[Language]].
 */
object Language:
  inline given derived[E](using m: Mirror.SumOf[E]): Language[E] =
    new Language[E]:
      private val payComps: Array[Array[(Any, Any) => Int]] =
        payloadComparatorsFor[E](using m)

      def opOrdering: Ordering[Op] = new Ordering[Op]:
        def compare(a: Op, b: Op): Int =
          // 1) ord
          val c1 = java.lang.Integer.compare(a.ord, b.ord)
          if c1 != 0 then return c1

          // 2) schema (lexicographic)
          val c2 = SeqOrdering.lexOrdering.compare(a.schema, b.schema)
          if c2 != 0 then return c2

          // 3) payload via precomputed per-ordinal comparators
          val cs = payComps(a.ord)
          val len = math.min(math.min(a.payload.length, b.payload.length), cs.length)

          // find first non-zero comparison
          var i = 0
          while i < len do
            val c = cs(i)(a.payload(i), b.payload(i))
            if c != 0 then return c
            i += 1

          // tie-break on length
          java.lang.Integer.compare(a.payload.length, b.payload.length)

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
          LanguageOp[E](ord, Seq.from(schema), Seq.from(payload)),
          binders.toSeq,
          slots.toSeq,
          kids.toSeq
        )

      def toTree[A](e: E)(using enc: AtomEncoder[E, A]): MTree[A] =
        AtomEncoder.encode(enc, e, m.ordinal(e)) match
          case Some(payload) => MixedTree.Atom(payload)
          case None => encodeCase[A](m.ordinal(e), e)(using enc)

      def fromTree[A](n: MixedTree[Op, A])(using dec: AtomDecoder[E, A]): E =
        n match
          case MixedTree.Atom(b) =>
            // Rebuild a concrete case C <: E from the call payload, if possible
            AtomDecoder.decode(dec, b).getOrElse {
              throw new IllegalArgumentException(
                s"fromTree: no decoder for call payload: $b"
              )
            }

          case MixedTree.Node(op, binders, slots, kids) =>
            val op2: Op = op
            val schema = op2.schema
            val eb = binders.iterator
            val es = slots.iterator
            val ek = kids.iterator
            val ep = op2.payload.iterator

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
            ctors(op2.ord)(Tuple.fromArray(elems))

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
        compsForCase[c](using summonInline[Mirror.ProductOf[c]]) :: compsForCasesList[cs]

  private inline def payloadComparatorsFor[E](using s: Mirror.SumOf[E]): Array[Array[(Any, Any) => Int]] =
    type Cases = s.MirroredElemTypes
    compsForCasesList[Cases].toArray
