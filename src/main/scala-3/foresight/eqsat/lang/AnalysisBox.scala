package foresight.eqsat.lang

// A witness: how does this language box an A into its AST?
trait AnalysisBox[E]:
  type Box[_] <: E
  def box[A](a: A): Box[A]

object AnalysisBox:
  // syntax helper
  def apply[E](using ev: AnalysisBox[E]): AnalysisBox[E] = ev
