package foresight.eqsat.lang

/**
 * Canonical node representation for a surface language `E`.
 *
 * `LanguageOp[E]` is the internal node type used by e-graphs. It is not constructed
 * directly; instead, use the encoders and tree-conversion methods provided by
 * [[Language]]. This ensures that nodes are created consistently and can be
 * compared, ordered, and rewritten reliably.
 *
 * Key points:
 *  - Each surface language `E` has an associated `LanguageOp[E]` type.
 *  - You typically encounter it inside [[MixedTree]] or when working with an
 *    [[EGraph]] that has been parameterized by your language.
 *  - You do not construct or pattern match on it yourself. Go through the
 *    `Language[E]` instance instead.
 *
 * Example:
 * {{{
 * sealed trait ArithExpr derives Language
 * final case class Add(x: ArithExpr, y: ArithExpr) extends ArithExpr
 *
 * val Lang = summon[Language[ArithExpr]]
 * val expr: ArithExpr = Add(x, y)
 *
 * // Core representation: tree of LanguageOp[ArithExpr] + atoms
 * val tree: Lang.MTree[Pattern.Var] = Lang.toTree(expr)
 * }}}
 *
 * @tparam E the surface language this node belongs to.
 */
final class LanguageOp[E] private[lang](private[lang] val ord: Int,
                                        private[lang] val schema: Seq[Byte],
                                        private[lang] val payload: Seq[Any]) {

  override def equals(obj: Any): Boolean = obj match {
    case that: LanguageOp[_] =>
      (this eq that) || (this.ord == that.ord && this.payload == that.payload)

    case _ => false
  }

  private val _hash: Int = {
    var h = ord.##
    h = 31 * h + payload.##
    h
  }

  override def hashCode: Int = _hash
}

/**
 * Companion object for [[LanguageOp]].
 *
 * Provides utilities and implicits related to `LanguageOp`.
 */
object LanguageOp {
  /**
   * Constructs a new `LanguageOp`.
   *
   * This is primarily intended for use by `Language` implementations.
   *
   * @param ord     The unique ordinal of the operation within its language.
   * @param schema  The schema bytes describing the structure of the payload.
   * @param payload The actual data of the operation.
   * @tparam E The surface language this node belongs to.
   * @return A new `LanguageOp` instance.
   */
  def apply[E](ord: Int, schema: Seq[Byte], payload: Seq[Any]): LanguageOp[E] =
    new LanguageOp[E](ord, schema, payload)

  /**
   * Provides an ordering over nodes whenever a [[Language]] is in scope.
   *
   * This ordering comes from the language definition itself, ensuring that
   * comparison and canonicalization of nodes is deterministic.
   *
   * Example:
   * {{{
   * given Language[ArithExpr] = summon[Language[ArithExpr]]
   *
   * val ord: Ordering[LanguageOp[ArithExpr]] =
   *   summon[Ordering[LanguageOp[ArithExpr]]]
   * }}}
   */
  given opOrderingFor[E](using L: Language[E]): Ordering[LanguageOp[E]] =
    L.opOrdering
}
