package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.SlotMap
import foresight.eqsat.lang.LanguageAnalysis

/**
 * This analysis tries to compute a compile-time constant for each node in the
 * arithmetic-with-lang example. It is intentionally minimal and pedagogical:
 * we keep the lattice as `Option[BigInt]` and only propagate through
 * `Number`, `Add`, and `Mul`.
 *
 * Because we extend [[foresight.eqsat.lang.LanguageAnalysis LanguageAnalysis]] with
 * the surface type `ArithExpr`, `make` receives *decoded* AST nodes (not e-graph
 * tuples). That keeps the implementation idiomatic Scala pattern matching.
 *
 * ## Child-facts invariant
 * When `make` is called on a node, its **immediate children are already wrapped as
 * `Fact[A]`** where `A = Option[BigInt]` for this analysis. In other words, the
 * sub-results of the analysis have been placed into the tree before your case
 * analysis runs. This means pattern matches can look directly for
 * `Fact(Some(v))` on the children. The parent node itself (the `expr` you match on)
 * remains the normal surface constructor (e.g., `Add(lhs, rhs)`).
 *
 * ## How this plugs into the example
 *   - The companion of [[ArithExpr]] provides an [[foresight.eqsat.lang.AnalysisBox AnalysisBox]]
 *     using `Fact[A]`.
 *   - In `make`, we specifically match on `Fact(Some(v))` subtrees: this leverages the
 *     child-facts invariant described above.
 *   - Tests wire this analysis alongside `ExtractionAnalysis.smallest` to enable rules
 *     that consult constants mid-rewrite.
 */
object ConstantAnalysis extends LanguageAnalysis[ArithExpr, Option[BigInt]] {
  /** A human-readable handle for the analysis registry. */
  def name: String = "ConstantAnalysis"

  /**
   * Renaming is a no-op here because constants do not mention slots.
   *
   * Some analyses may need to remap references after e-classes merge; our lattice
   * carries only values, so we simply pass them through.
   */
  def rename(result: Option[BigInt], renaming: SlotMap): Option[BigInt] = result

  /**
   * Local transfer function: compute the fact for a single node.
   *
   * The [[LanguageAnalysis]] parent decodes the current node to `ArithExpr` and
   * hands it to us. **Each child is already a `Fact[A]`** containing that child's
   * analysis result (`A = Option[BigInt]`). We propagate constants through a small
   * set of constructors, recognizing when both operands are known.
   *
   * {{{
   * // Examples of what this recognizes
   * make(Number(3))                                       // => Some(3)
   * make(Add(Fact(Some(2)),  Fact(Some(4))))              // => Some(6)
   * make(Mul(Fact(Some(7)),  Fact(Some(6))))              // => Some(42)
   * }}}
   */
  def make(expr: ArithExpr): Option[BigInt] = {
    expr match {
      case Add(Fact(Some(left: BigInt)), Fact(Some(right: BigInt))) => Some(left + right)
      case Mul(Fact(Some(left: BigInt)), Fact(Some(right: BigInt))) => Some(left * right)
      case Number(value) => Some(value)
      case _ => None
    }
  }

  /**
   * Join for the lattice `Option[BigInt]`.
   *
   * We treat `Some(v)` as the most precise fact; `None` means "unknown". When both
   * sides agree on a value, we keep it. If they disagree, we throw an exception
   * to signal a contradiction. This is a simple lattice where `None` is the bottom
   * and `Some(v)` are the top elements.
   */
  def join(left: Option[BigInt], right: Option[BigInt]): Option[BigInt] = {
    (left, right) match {
      case (Some(l), Some(r)) if l == r => Some(l)
      case (Some(_), Some(_))           => throw new IllegalArgumentException("Cannot join different constants")
      case (Some(l), None)              => Some(l)
      case (None, Some(r))              => Some(r)
      case (None, None)                 => None
    }
  }
}
