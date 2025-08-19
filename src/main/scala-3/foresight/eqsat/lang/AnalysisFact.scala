package foresight.eqsat.lang

/**
 * A wrapper for a value of type A that represents a fact in the context of
 * an analysis. It is used to store information that can be used during
 * the analysis phase of an e-graph.
 * @param value The value of the analysis fact.
 * @tparam A The type of the value stored in the analysis fact.
 */
final case class AnalysisFact[A](value: A) extends AnyVal
