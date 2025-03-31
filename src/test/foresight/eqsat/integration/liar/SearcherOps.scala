package foresight.eqsat.integration.liar

import foresight.eqsat.{EGraph, EGraphLike, MixedTree}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Searcher
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Pattern, PatternMatch}

object SearcherOps {
  private def functionTypePattern = {
    FunctionType(
      MixedTree.Call(Pattern.Var.fresh[ArrayIR]()),
      MixedTree.Call(Pattern.Var.fresh[ArrayIR]())).compiled[EGraph[ArrayIR]]
  }

  private def int32TypePattern = {
    CompiledPattern[ArrayIR, EGraph[ArrayIR]](Int32Type.toTree)
  }

  private def doubleTypePattern = {
    CompiledPattern[ArrayIR, EGraph[ArrayIR]](DoubleType.toTree)
  }

  implicit class SearcherOfMetadataPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val searcher: Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphWithMetadata[ArrayIR, EGraphT]])
    extends AnyVal {

    /**
     * Updates a match to also include the types of already-bound values.
     *
     * @param types A mapping of already-bound values, referred to by their variables, to their yet unbound types.
     * @return The searcher that binds the types to the variables.
     */
    def bindTypes(types: Map[Pattern.Var[ArrayIR], Pattern.Var[ArrayIR]]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      searcher.mapWithEGraph((m, egraph) => {
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
    def requireTypes(types: Map[Pattern.Var[ArrayIR], MixedTree[ArrayIR, Pattern[ArrayIR]]]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      // Precompile the patterns.
      val compiledPatterns = types.map {
        case (v, t) => v -> t.compiled[EGraph[ArrayIR]]
      }

      def tryMatchVariableToTypePattern(variable: Pattern.Var[ArrayIR],
                                        pattern: CompiledPattern[ArrayIR, EGraph[ArrayIR]],
                                        m: PatternMatch[ArrayIR],
                                        egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Option[PatternMatch[ArrayIR]] = {
        val (value, egraphWithValue) = egraph.add(m(variable))
        val t = TypeInferenceAnalysis.get(egraphWithValue)(value, egraphWithValue)

        val (typeInGraph, egraphWithType) = egraphWithValue.add(t)
        pattern.search(typeInGraph, egraphWithType).toStream.flatMap(m.tryMerge).headOption
      }

      // For each potential match, try to iteratively construct a match that binds the variables in the type patterns.
      // If such a match is found, then the original match is kept. Otherwise, it is filtered out.
      searcher.filterWithEGraph((m, egraph) => {
        compiledPatterns.foldLeft(Option(m)) {
          case (Some(newMatch), (variable, pattern)) =>
            tryMatchVariableToTypePattern(variable, pattern, newMatch, egraph)

          case (None, _) => None
        }.isDefined
      })
    }
  }

  implicit class SearcherOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val searcher: Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT])
    extends AnyVal {

    /**
     * Requires that the given variables are values. Matches that do not satisfy this condition are filtered out.
     * @param values The variables to check.
     * @return The searcher that filters out matches where the variables are not values.
     */
    def requireValues(values: Pattern.Var[ArrayIR]*): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT] = {
      searcher.filterWithEGraph((m, egraph) => {
        values.forall(v => {
          m(v) match {
            case MixedTree.Call(c) => egraph.nodes(c).head.nodeType.isInstanceOf[Value]
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

    /**
     * Requires that the given variable is a double type. Matches that do not satisfy this condition are filtered out.
     * @param t The variable to check.
     * @return The searcher that filters out matches where the variable is not a double type.
     */
    def requireDoubleType(t: Pattern.Var[ArrayIR]): Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphT] = {
      searcher.filterWithEGraph((m, egraph) => {
        doubleTypePattern.matches(m(t), egraph)
      })
    }
  }
}
