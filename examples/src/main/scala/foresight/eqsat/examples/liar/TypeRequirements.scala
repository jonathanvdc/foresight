package foresight.eqsat.examples.liar

import foresight.eqsat.commands.CommandScheduleBuilder
import foresight.eqsat.rewriting.SearcherContinuation.Continuation
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Pattern, PatternMatch}
import foresight.eqsat.rewriting.{Applier, ReversibleApplier, Searcher, SearcherContinuation}
import foresight.eqsat.MixedTree
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithMetadata}

object TypeRequirements {
  private def tryMatchVariableToTypePattern[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](variable: Pattern.Var,
                                                                                                          pattern: CompiledPattern[ArrayIR, EGraph[ArrayIR]],
                                                                                                          m: PatternMatch[ArrayIR],
                                                                                                          egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Option[PatternMatch[ArrayIR]] = {
    val (value, egraphWithValue) = egraph.add(m(variable))
    val t = TypeInferenceAnalysis.get(egraphWithValue)(value, egraphWithValue)

    val (typeInGraph, egraphWithType) = egraphWithValue.add(t)
    pattern.search(typeInGraph, egraphWithType).toStream.flatMap(m.tryMerge).headOption
  }

  private def checkMatch[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](m: PatternMatch[ArrayIR],
                                                                                       compiledPatterns: Map[Pattern.Var, CompiledPattern[ArrayIR, EGraph[ArrayIR]]],
                                                                                       egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Option[PatternMatch[ArrayIR]] = {
    compiledPatterns.foldLeft(Option(m)) {
      case (Some(newMatch), (variable, pattern)) =>
        tryMatchVariableToTypePattern(variable, pattern, newMatch, egraph)

      case (None, _) => None
    }
  }

  final case class RequirementsSearcherContinuationBuilder[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](types: Map[Pattern.Var, MixedTree[ArrayIR, Pattern.Var]])
    extends SearcherContinuation.ContinuationBuilder[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {

    // Precompile the patterns.
    private val compiledPatterns = types.map {
      case (v, t) => v -> t.compiled[EGraph[ArrayIR]]
    }

    override def apply(downstream: Continuation[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]): Continuation[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      (m, egraph) => {
        checkMatch(m, compiledPatterns, egraph) match {
          case Some(m2) => downstream(m2, egraph)
          case None => true
        }
      }
    }

    override def tryReverse(applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]): Option[Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]] = {
      Some(ApplierWithRequirements(applier, types))
    }
  }

  final case class ApplierWithRequirements[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]],
                                                                                                         types: Map[Pattern.Var, MixedTree[ArrayIR, Pattern.Var]])
    extends ReversibleApplier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {

    // Precompile the patterns.
    private val compiledPatterns = types.map {
      case (v, t) => v -> t.compiled[EGraph[ArrayIR]]
    }

    override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT], builder: CommandScheduleBuilder[ArrayIR]): Unit = {
      applier.flatMap((m2: PatternMatch[ArrayIR], egraph2: EGraphWithMetadata[ArrayIR, EGraphT]) => checkMatch(m2, compiledPatterns, egraph2)).apply(m, egraph, builder)
    }

    override def tryReverse: Option[Searcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]] = {
      applier match {
        case applier: ReversibleApplier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] =>
          applier.tryReverse.map(_.andThen(RequirementsSearcherContinuationBuilder(types)))

        case _ => None
      }
    }
  }
}
