package foresight.eqsat.lang

/**
 * Witness describing how a language `E` boxes analysis results into its AST.
 *
 * In equality saturation, analyses compute values `A` (constants, types, costs, …)
 * associated with nodes. To expose these results uniformly at the surface language level,
 * the language provides a wrapper type `Box[A]` (a case class or ADT constructor) that embeds
 * an `A` back into the AST.
 *
 * `AnalysisBox[E]` is the evidence that such a wrapper exists. It tells the system:
 *  - given a result type `A`,
 *  - how to construct a node `Box[A]` that is itself an `E`.
 *
 * ### Members
 *  - [[Box]] — a type constructor describing the wrapper (e.g. `AnalysisFact[A]`).
 *  - [[box]] — the function that actually wraps a value into `E`.
 *
 * @tparam E Surface language whose AST can embed analysis facts.
 * @example
 * Suppose your surface language `ArithExpr` has a case class to hold analysis results:
 * {{{
 * sealed trait ArithExpr derives Language
 * final case class Const(value: BigInt) extends ArithExpr
 * final case class Fact[A](value: A) extends ArithExpr
 *
 * given AnalysisBox[ArithExpr] with
 *   type Box[A] = Fact[A]
 *   def box[A](x: A): Fact[X] = Fact(x)
 *
 * // Now any analysis result can be boxed back into the AST:
 * val L = summon[Language[ArithExpr]]
 * val expr: ArithExpr = ab.fromAnalysisNode(...)
 * }}}
 *
 * This makes it possible to reconstruct surface `E` terms from analysis nodes in a generic way
 * (see [[Language.fromAnalysisNode]]).
 */
trait AnalysisBox[E]:
  /** A type constructor that, for any result type `A`, yields an `E` node wrapping `A`. */
  type Box[_] <: E

  /** Wrap an analysis result `a` into the surface AST as a `Box[A]`. */
  def box[A](a: A): Box[A]

object AnalysisBox:

  /**
   * Summon an implicit [[AnalysisBox]] for a given language `E`.
   *
   * Shorthand for `summon[AnalysisBox[E]]`.
   */
  def apply[E](using ev: AnalysisBox[E]): AnalysisBox[E] = ev
