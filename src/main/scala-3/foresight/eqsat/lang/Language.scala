package foresight.eqsat.lang

import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.{ReversibleSearcher, Rule}
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}
import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Slot}
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
 *  - helpers to reconstruct `E` from analysis or extraction results.
 *
 * @example Encoding and rules at a glance
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
   * Build a language-level extractor from a core [[ExtractionAnalysis]].
   *
   * This adapter takes a core extractor produced by the given `analysis`
   * (which works over the e-graph's internal node type `LanguageOp[E]`)
   * and wraps it so that it returns surface AST values `E`.
   *
   * Internally it:
   *    - obtains the core extractor via `analysis.extractor[Repr]`, which
   *      returns a function `(EClassCall, Repr) => MixedTree[Op, EClassCall]`
   *    - invokes that extractor and then decodes the resulting core tree back
   *      to the surface AST using [[fromTree]]
   *
   * Use this when you want to perform extractor-based reconstruction
   * (e.g., cost-based extraction, witness construction) but interact with
   * your language `E` rather than the e-graph’s internal representation.
   *
   * This method does not alter the semantics or cost model of `analysis`;
   * it delegates extraction to the inner (core) extractor and then decodes
   * the resulting core tree to `E`.
   *
   * @tparam C
   *   The analysis result / cost type managed by `ExtractionAnalysis`.
   * @tparam Repr
   *   The concrete e-graph type. Must be an `EGraphLike[Op, Repr]` that is
   *   also an `EGraph[Op]`.
   * @param analysis
   *   The extraction analysis defined over `LanguageOp[E]` and cost type `C`.
   * @return
   *   A `LanguageExtractor[E, EGraphWithMetadata[Op, Repr]]` whose `apply`
   *   method takes an `EClassCall` and an `EGraphWithMetadata[Op, Repr]`
   *   and returns a surface AST value `E`.
   * @example
   * {{{
   * val lang: Language[E] = summon[Language[E]]
   * val analysis: ExtractionAnalysis[lang.Op, Cost] = ...
   *
   * val g: EGraphWithMetadata[lang.Op, Repr] = ...
   * val root: EClassCall = ...
   *
   * val extr = lang.extractor[Cost, Repr](analysis)
   * val program: E = extr(root, g)   // decodes to surface AST E
   * }}}
   */
  def extractor[C, Repr <: EGraphLike[Op, Repr] with EGraph[Op]](analysis: ExtractionAnalysis[LanguageOp[E], C]): LanguageExtractor[E, EGraphWithMetadata[Op, Repr]] = {
    val innerExtractor = analysis.extractor[Repr]
    def absurd[A](n: Nothing): A = n
    val dec: AtomDecoder[E, Nothing] = AtomDecoder[E, Nothing] { absurd }
    new LanguageExtractor[E, EGraphWithMetadata[Op, Repr]](using this) {
      def apply(call: EClassCall, egraph: EGraphWithMetadata[Op, Repr]): E =
        fromTree[Nothing](innerExtractor(call, egraph))(using dec)
    }
  }

  /**
   * Create a core [[ExtractionAnalysis]] from a cost function.
   *
   * This helper lifts a [[LanguageCostFunction]] over surface ASTs `E`
   * into an [[ExtractionAnalysis]] that operates over the e-graph's
   * internal node type `LanguageOp[E]`.
   *
   * Internally, it binds the provided cost function with the
   * ordering on costs and the ordering on language operators.
   *
   * @tparam C
   *   The cost type returned by the cost function.
   * @param name
   *   A human-readable name for the analysis (for logging/debugging).
   * @param costFunction
   *   A function assigning a cost to each surface AST `E`.
   * @param ord
   *   (using) Ordering on costs, required for extraction.
   * @return
   *   An [[ExtractionAnalysis]] defined over `LanguageOp[E]` and cost type `C`.
   * @example
   * {{{
   * val cf: LanguageCostFunction[Expr, Int] = ...
   * val analysis = lang.extractionAnalysis("my-cost", cf)
   * }}}
   */
  def extractionAnalysis[C](name: String, costFunction: LanguageCostFunction[E, C])
                           (using ord: Ordering[C]): ExtractionAnalysis[LanguageOp[E], C] = {
    ExtractionAnalysis(name, costFunction)(ord, opOrdering)
  }

  /**
   * Extract a surface expression from an e-graph given a cost function.
   *
   * This method performs a one-off version of the standard cost-based extraction process:
   *
   *   - Wrap the provided cost function in an [[ExtractionAnalysis]]
   *   - Enrich the input e-graph with that analysis
   *   - Run [[extractor]] to reconstruct the minimal-cost expression
   *     of type `E` rooted at the given [[EClassCall]]
   *
   * This does not mutate the input e-graph; instead it builds a
   * metadata-enriched wrapper and extracts from that.
   *
   * If multiple extraction rounds are needed, consider building a
   * [[foresight.eqsat.metadata.EGraphWithMetadata]]
   * configured with an extraction analysis obtained through [[extractionAnalysis]].
   * Concrete expressions can be extracted from that enriched e-graph
   * using the [[extractor]] method.
   *
   * @tparam C
   *   The cost type used to rank candidate extractions.
   * @param call
   *   The root e-class to extract from.
   * @param egraph
   *   The e-graph containing equivalences and metadata.
   * @param costFunction
   *   Function assigning costs to surface ASTs.
   * @param ord
   *   (using) Ordering on costs.
   * @return
   *   The extracted surface AST `E` of minimal cost according to the
   *   supplied cost function.
   * @example
   * {{{
   *   val cf: LanguageCostFunction[Expr, Int] = ...
   *   val program: Expr = lang.extract(rootCall, g, cf)
   * }}}
   */
  def extract[C](call: EClassCall, egraph: EGraph[Op], costFunction: LanguageCostFunction[E, C])
                (using ord: Ordering[C]): E = {
    val analysis = extractionAnalysis("extraction", costFunction)
    val withMetadata = egraph.withMetadata.addAnalysis(analysis)
    extractor(analysis)(call, withMetadata)
  }

  /**
   * Add an expression with e-class calls to the e-graph and immediately
   * extract its minimal-cost equivalent according to the given cost function.
   *
   * This is a convenience overload that:
   *
   *   - Inserts `exprWithCalls` into the e-graph
   *   - Delegates to [[extract(EClassCall,EGraph,LanguageCostFunction)]]
   *     on the resulting e-class
   *
   * @tparam C
   *   The cost type used to rank candidate extractions.
   *
   * @param exprWithCalls
   *   Surface expression to add and extract from.
   * @param egraph
   *   The e-graph to insert into.
   * @param costFunction
   *   Function assigning costs to surface ASTs.
   *
   * @param enc
   *   (using) Encoder from surface AST atoms to `EClassCall`.
   * @param ord
   *   (using) Ordering on costs.
   *
   * @return
   *   The extracted minimal-cost expression equivalent to
   *   `exprWithCalls`.
   *
   * @example
   *   {{{
   *   val cf: LanguageCostFunction[Expr, Int] = ...
   *   val program: Expr = lang.extract(myExpr, g, cf)
   *   }}}
   */
  def extract[C](exprWithCalls: E, egraph: EGraph[Op], costFunction: LanguageCostFunction[E, C])
                (using enc: AtomEncoder[E, EClassCall],
                 ord: Ordering[C]): E = {
    val (call, newGraph) = egraph.add(exprWithCalls)(using this, enc)
    extract(call, newGraph, costFunction)(using ord)
  }

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

  /** Generate a fresh [[foresight.eqsat.rewriting.patterns.Pattern.Var]] and decode it back to a surface `E` value. */
  private def freshPatternVar(using dec: AtomDecoder[E, Pattern.Var]): E =
    fromTree(MixedTree.Atom[Op, Pattern.Var](Pattern.Var.fresh()))

  /**
   * Create a rewrite rule by supplying a lambda that receives one fresh pattern variable.
   *
   * This overload is useful when the rule needs just a single variable and you prefer not to
   * predeclare `Pattern.Var`s or name variables explicitly.
   *
   * The implementation generates a fresh [[foresight.eqsat.rewriting.patterns.Pattern.Var]],
   * decodes it back into a surface `E` via the given [[AtomDecoder]], and passes that value to `f`.
   * The pair `(lhs, rhs)` returned by `f` is then compiled into a [[foresight.eqsat.rewriting.Rule]].
   *
   * @param name Human-readable rule name.
   * @param f    A function that receives one fresh `E` standing for a pattern variable,
   *             and returns `(lhs, rhs)` built in surface syntax.
   * @param enc  Encoder for `Pattern.Var` leaves (used to compile `lhs`/`rhs`).
   * @param dec  Decoder from `Pattern.Var` atoms back to surface `E` (used to supply the lambda arg).
   * @tparam EGraphT An e-graph type that supports this language.
   * @example
   * {{{
   * // β-reduction for a single-argument lambda, schematically: (λx. body)(x) → body
   * val beta =
   *   Lang.rule("beta") { x =>
   *     (App(Lam(Def(param), body = x), arg = x),
   *      x) // simplified example
   *   }
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String)
  (f: E => (E, E))
  (using enc: AtomEncoder[E, Pattern.Var],
   dec: AtomDecoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    val (lhs, rhs) = f(freshPatternVar)
    rule[EGraphT](name, lhs, rhs)

  /**
   * Create a rewrite rule by supplying a lambda that receives two fresh pattern variables.
   *
   * Use this when your rule naturally refers to two variables, without having to
   * allocate or manage them explicitly.
   *
   * Fresh variables are generated as [[foresight.eqsat.rewriting.patterns.Pattern.Var]]s
   * and decoded to surface `E` values using the given [[AtomDecoder]] before being
   * passed to `f`.
   *
   * @param name Human-readable rule name.
   * @param f    A function of two fresh variables that returns `(lhs, rhs)`.
   * @param enc  Encoder for `Pattern.Var` leaves.
   * @param dec  Decoder from `Pattern.Var` atoms back to surface `E`.
   * @tparam EGraphT An e-graph type that supports this language.
   * @example
   * {{{
   * // Commutativity: x + y → y + x
   * val addComm =
   *   Lang.rule("comm-add") { (x, y) =>
   *     (Add(x, y), Add(y, x))
   *   }
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String)
  (f: (E, E) => (E, E))
  (using enc: AtomEncoder[E, Pattern.Var],
   dec: AtomDecoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    val (lhs, rhs) = f(freshPatternVar, freshPatternVar)
    rule[EGraphT](name, lhs, rhs)

  /**
   * Create a rewrite rule by supplying a lambda that receives three fresh pattern variables.
   *
   * Choose this overload when your rule template references three variables.
   *
   * @param name Human-readable rule name.
   * @param f    A function of three fresh variables that returns `(lhs, rhs)`.
   * @param enc  Encoder for `Pattern.Var` leaves.
   * @param dec  Decoder from `Pattern.Var` atoms back to surface `E`.
   * @tparam EGraphT An e-graph type that supports this language.
   * @example
   * {{{
   * // Associativity (one direction): (x + y) + z → x + (y + z)
   * val addAssoc1 =
   *   Lang.rule("assoc-add-1") { (x, y, z) =>
   *     (Add(Add(x, y), z), Add(x, Add(y, z)))
   *   }
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String)
  (f: (E, E, E) => (E, E))
  (using enc: AtomEncoder[E, Pattern.Var],
   dec: AtomDecoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    val (lhs, rhs) = f(freshPatternVar, freshPatternVar, freshPatternVar)
    rule[EGraphT](name, lhs, rhs)

  /**
   * Create a rewrite rule by supplying a lambda that receives four fresh pattern variables.
   *
   * This is helpful for distributivity and similar rules with four bind sites.
   *
   * @param name Human-readable rule name.
   * @param f    A function of four fresh variables that returns `(lhs, rhs)`.
   * @param enc  Encoder for `Pattern.Var` leaves.
   * @param dec  Decoder from `Pattern.Var` atoms back to surface `E`.
   * @tparam EGraphT An e-graph type that supports this language.
   * @example
   * {{{
   * // Distributivity: x * (y + z) → (x * y) + (x * z)
   * val mulDistOverAdd =
   *   Lang.rule("mul-dist-add") { (x, y, z, w) =>
   *     // (example with four vars just to illustrate arity)
   *     (Mul(x, Add(y, z)), Add(Mul(x, y), Mul(x, z)))
   *   }
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String)
  (f: (E, E, E, E) => (E, E))
  (using enc: AtomEncoder[E, Pattern.Var],
   dec: AtomDecoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    val (lhs, rhs) = f(freshPatternVar, freshPatternVar, freshPatternVar, freshPatternVar)
    rule[EGraphT](name, lhs, rhs)

  /**
   * Create a rewrite rule by supplying a lambda that receives five fresh pattern variables.
   *
   * This highest-arity overload avoids manual variable pluming in complex rules.
   * If you need more than five variables, consider building helper tuples or
   * switching to the explicit `(name, lhs, rhs)` overload.
   *
   * @param name Human-readable rule name.
   * @param f    A function of five fresh variables that returns `(lhs, rhs)`.
   * @param enc  Encoder for `Pattern.Var` leaves.
   * @param dec  Decoder from `Pattern.Var` atoms back to surface `E`.
   * @tparam EGraphT An e-graph type that supports this language.
   * @example
   * {{{
   * val fancy =
   *   Lang.rule("five-var-demo") { (a, b, c, d, e) =>
   *     (F(a, b, c, d, e), F(e, d, c, b, a))
   *   }
   * }}}
   */
  def rule[EGraphT <: EGraphLike[Op, EGraphT] with EGraph[Op]]
  (name: String)
  (f: (E, E, E, E, E) => (E, E))
  (using enc: AtomEncoder[E, Pattern.Var],
   dec: AtomDecoder[E, Pattern.Var])
  : Rule[Op, PatternMatch[Op], EGraphT] =
    val (lhs, rhs) = f(
      freshPatternVar, freshPatternVar, freshPatternVar, freshPatternVar, freshPatternVar
    )
    rule[EGraphT](name, lhs, rhs)

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
