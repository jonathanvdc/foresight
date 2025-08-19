package foresight.eqsat.lang

import foresight.eqsat.Slot
import foresight.eqsat.metadata.Analysis

/** A helper trait for defining analyses over a Language with an expression type E.
  *
  * This trait leverages the Language's ability to construct an expression E from
  * its analysis nodes, allowing you to write your analysis logic directly
  * in terms of E rather than the raw analysis node structure.
  *
  * To use this trait, implement the `make(expr: E): A` method with your
  * analysis logic. The `make` method that takes the analysis node and
  * its arguments is provided for you and will construct the view E
  * before delegating to your implementation.
  *
  * @tparam E The view type provided by the Language.
  * @tparam A The result type of the analysis.
  */
trait LanguageAnalysis[E, A](using Language[E], AtomDecoder[E, AnalysisFact[A]]) extends Analysis[LanguageOp[E], A] {
  private inline def Lang: Language[E] = summon[Language[E]]

  /**
   * User-defined `make`: constructs the analysis result from the expression view.
   * @param expr The expression constructed from the analysis node and its arguments.
   * @return The analysis result of type A.
   */
  def make(expr: E): A

  final def make(node: LanguageOp[E], defs: Seq[Slot], uses: Seq[Slot], args: Seq[A]): A =
    make(Lang.fromAnalysisNode[A](node, defs, uses, args))
}
