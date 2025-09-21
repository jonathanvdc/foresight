package foresight.eqsat.examples.liar

import foresight.eqsat.examples.liar.TypeRequirements.RequirementsSearcherContinuationBuilder
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Pattern, PatternMatch}
import foresight.eqsat.rewriting.{Applier, ReversibleSearcher, Searcher}
import foresight.eqsat.{EGraph, EGraphLike, MixedTree}

object SearcherOps {
  implicit class SearcherOfMetadataPatternMatchOps[
    EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]
  ](private val searcher: Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]])
    extends AnyVal {

    /**
     * Updates a match to also include the types of already-bound values.
     *
     * @param types A mapping of already-bound values, referred to by their variables, to their yet unbound types.
     * @return The searcher that binds the types to the variables.
     */
    def bindTypes(types: Map[Pattern.Var, Pattern.Var]): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      searcher.map((m, egraph) => {
        val newVarMapping = m.varMapping ++ types.map {
          case (value, t) =>
            val (call, newEGraph) = egraph.add(m(value))
            t -> TypeInferenceAnalysis.get(newEGraph)(call, newEGraph)
        }
        PatternMatch(m.root, newVarMapping, m.slotMapping)
      })
    }

    /**
     * Requires that the given variables are bound to types that adhere to the given patterns, with variables shared by
     * the patterns bound to the same type. Matches that do not satisfy this condition are filtered out.
     * @param types A mapping of variables to type patterns.
     * @return The searcher that filters out matches where the variables are not bound to the given type patterns.
     */
    def requireTypes(types: Map[Pattern.Var, MixedTree[ArrayIR, Pattern.Var]]): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      searcher.andThen(RequirementsSearcherContinuationBuilder(types))
    }
  }

  implicit class SearcherOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val searcher: Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphT])
    extends AnyVal {

    /**
     * Requires that the given variables are values. Matches that do not satisfy this condition are filtered out.
     * @param values The variables to check.
     * @return The searcher that filters out matches where the variables are not values.
     */
    def requireValues(values: Pattern.Var*): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphT] = {
      searcher.filter((m, egraph) => {
        values.forall(v => {
          m(v) match {
            case MixedTree.Atom(c) => egraph.nodes(c).head.nodeType.isInstanceOf[Value]
            case MixedTree.Node(nodeType, _, _, _) => nodeType.isInstanceOf[Value]
          }
        })
      })
    }

    /**
     * Requires that the given variable is not a function type. Matches that do not satisfy this condition are filtered
     * out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is a function type.
     */
    def requireNonFunctionType(t: Pattern.Var): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphT] = {
      searcher.filter((m, egraph) => {
        m(t) match {
          case MixedTree.Atom(c) => egraph.nodes(c).head.nodeType != FunctionType
          case MixedTree.Node(nodeType, _, _, _) => nodeType != FunctionType
        }
      })
    }

    /**
     * Requires that the given variable is an int32 type. Matches that do not satisfy this condition are filtered out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is not an int32 type.
     */
    def requireInt32Type(t: Pattern.Var): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphT] = {
      searcher.filter((m, egraph) => {
        m(t) match {
          case MixedTree.Atom(c) => egraph.nodes(c).head.nodeType == Int32Type
          case MixedTree.Node(nodeType, _, _, _) => nodeType == Int32Type
        }
      })
    }

    /**
     * Requires that the given variable is a double type. Matches that do not satisfy this condition are filtered out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is not a double type.
     */
    def requireDoubleType(t: Pattern.Var): Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphT] = {
      searcher.filter((m, egraph) => {
        m(t) match {
          case MixedTree.Atom(c) => egraph.nodes(c).head.nodeType == DoubleType
          case MixedTree.Node(nodeType, _, _, _) => nodeType == DoubleType
        }
      })
    }
  }
}
