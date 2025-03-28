package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.rewriting.Searcher
import fixpoint.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import fixpoint.eqsat.{EGraph, EGraphLike}

object SearcherOps {
  private def functionTypePattern = {
    FunctionType(Pattern.Var.fresh[ArrayIR](), Pattern.Var.fresh[ArrayIR]()).compiled
  }

  private def int32TypePattern = {
    Pattern.Node[ArrayIR](Int32Type, Seq.empty, Seq.empty, Seq.empty).compiled
  }

  implicit class SearcherOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](val searcher: Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT])
    extends AnyVal {

    /**
     * Updates a match to also include the types of already-bound values.
     * @param types A mapping of already-bound values, referred to by their variables, to their yet unbound types.
     * @return The searcher that binds the types to the variables.
     */
    def bindTypes(types: Map[Pattern.Var[ArrayIR], Pattern.Var[ArrayIR]]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT] = {
      searcher.mapWithEGraph((m, egraph) => {
        val newVarMapping = m.varMapping ++ types.map {
          case (value, t) =>
            t -> egraph.nodes(m.varMapping(value)).head.args.head
        }
        PatternMatch(m.root, newVarMapping, m.slotMapping)
      })
    }

    /**
     * Requires that the given variable is not a function type. Matches that do not satisfy this condition are filtered
     * out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is a function type.
     */
    def requireNonFunctionType(t: Pattern.Var[ArrayIR]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT] = {
      searcher.filterWithEGraph((m, egraph) => {
        !functionTypePattern.matches(m(t), egraph)
      })
    }

    /**
     * Requires that the given variable is an int32 type. Matches that do not satisfy this condition are filtered out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is not an int32 type.
     */
    def requireInt32Type(t: Pattern.Var[ArrayIR]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT] = {
      searcher.filterWithEGraph((m, egraph) => {
        int32TypePattern.matches(m(t), egraph)
      })
    }
  }
}
