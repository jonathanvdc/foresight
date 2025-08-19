package foresight.eqsat.lang

import foresight.eqsat.{EClassCall, EGraph, EGraphLike}

extension [E, This <: EGraphLike[LanguageOp[E], This] & EGraph[LanguageOp[E]]](g: EGraphLike[LanguageOp[E], This])
  /**
   * Adds an expression to the EGraph, converting it to an `EClassCall` using the provided language and encoder.
   * @param expr The expression to add.
   * @param L Implicit language instance for converting the expression to a tree.
   * @param enc Implicit encoder for converting the expression to an `EClassCall`.
   * @return A tuple containing the `EClassCall` created from the expression and the updated EGraph.
   */
  def add(expr: E)(using L: Language[E], enc: AtomEncoder[E, EClassCall]): (EClassCall, This) =
    g.add(L.toTree[EClassCall](expr))
