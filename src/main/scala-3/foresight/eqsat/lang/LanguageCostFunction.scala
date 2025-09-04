package foresight.eqsat.lang

import foresight.eqsat.Slot
import foresight.eqsat.extraction.{CostFunction, ExtractionTreeCall}

/**
 * Defines extraction cost functions in terms of a surface AST `E` rather than raw e-graph nodes.
 *
 * Instead of implementing a low-level [[CostFunction]] over `LanguageOp[E]`, you implement:
 *
 * {{{
 * def apply(expr: E): C
 * }}}
 *
 * The framework will:
 *
 *   1. Reconstruct a surface expression `E` from the e-graph analysis view
 *      (operator, binders/uses, and child costs).
 *   2. Instantiate children using your language-specific `AnalysisBox[E]`. Concretely,
 *      if you provide a given `AnalysisBox[E] { type Box[A] = X[A] }`, then each child
 *      is wrapped as `X[C]` (e.g., for `ArithExpr`, children appear as `Fact[C]`).
 *   3. Delegate to your `apply(expr: E)` to compute the cost.
 *
 * This lets you write compositional cost functions over your own AST while leveraging the
 * extraction engine’s incremental computation of child costs.
 *
 * @tparam C The cost type, which can be a number, tuple, or any custom structure
 *           that represents the cost of the expression.
 * @param L Encodes/decodes `E` to/from the core e-graph language and
 *          can rebuild surface nodes from analysis views.
 * @param dec Decoder for your boxed child costs. If you use a language-specific
 *           `AnalysisBox[E]` with `type Box[A] = X[A]`, then you need an
 *           `AtomDecoder[E, X[C]]` so the reconstruction can embed child costs in
 *           your box type.
 * @example For a language like:
 *
 * {{{
 * sealed trait ArithExpr derives Language
 *
 * final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
 * final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
 * final case class Number(value: BigInt)               extends ArithExpr
 * final case class Fact[A](value: A)                   extends ArithExpr
 *
 * object ArithExpr:
 *   given AnalysisBox[ArithExpr] with
 *     type Box[A] = Fact[A]
 *     def box[A](a: A): Fact[A] = Fact(a)
 * }}}
 *
 * a surface-level cost function can access child costs via the language box (`Fact[C]`):
 *
 * {{{
 * object SizeCost extends LanguageCostFunction[ArithExpr, Int]:
 *   def apply(expr: ArithExpr): Int = expr match
 *     case Number(_)        => 1
 *     case Add(Fact(l: Int), Fact(r: Int)) => 1 + l + r // child costs from Fact[Int]
 *     case Mul(Fact(l: Int), Fact(r: Int)) => 1 + l + r
 * }}}
 *
 * The key point is that children are not raw subtrees: they are the same surface
 * constructors, but where child positions encode precomputed costs using your
 * `AnalysisBox[E]` (e.g., `Fact[C]`).
 */
trait LanguageCostFunction[E, C](using
                                 L: Language[E],
                                 dec: AtomDecoder[E, AnalysisFact[C]]
                                ) extends CostFunction[LanguageOp[E], C] {

  /**
   * Compute the cost for a reconstructed surface expression.
   *
   * Child positions may contain your language’s analysis box (via [[AnalysisBox]]) carrying
   * the already-computed subtree cost. Use this to define costs compositionally without
   * recomputing child costs.
   *
   * @param expr
   *   The reconstructed surface expression, whose child positions are populated
   *   using the language-specific analysis box with precomputed costs.
   * @return
   *   The cost of `expr`.
   */
  def apply(expr: E): C

  final def apply(
                   nodeType: LanguageOp[E],
                   definitions: Seq[Slot],
                   uses: Seq[Slot],
                   args: Seq[C]
                 ): C =
    apply(L.fromAnalysisNode(nodeType, definitions, uses, args))
}
