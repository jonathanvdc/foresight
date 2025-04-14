package foresight.eqsat.examples.liar

import foresight.eqsat.commands.Command
import foresight.eqsat.{EGraph, EGraphLike, MixedTree}
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.rewriting.{Applier, ReversibleApplier, ReversibleSearcher, Searcher}
import foresight.eqsat.rewriting.patterns.{CompiledPattern, Pattern, PatternMatch}

object TypeRequirements {
  private def tryMatchVariableToTypePattern[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](variable: Pattern.Var[ArrayIR],
                                                                                                          pattern: CompiledPattern[ArrayIR, EGraph[ArrayIR]],
                                                                                                          m: PatternMatch[ArrayIR],
                                                                                                          egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Option[PatternMatch[ArrayIR]] = {
    val (value, egraphWithValue) = egraph.add(m(variable))
    val t = TypeInferenceAnalysis.get(egraphWithValue)(value, egraphWithValue)

    val (typeInGraph, egraphWithType) = egraphWithValue.add(t)
    pattern.search(typeInGraph, egraphWithType).toStream.flatMap(m.tryMerge).headOption
  }

  private def checkMatch[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](m: PatternMatch[ArrayIR],
                                                                                       compiledPatterns: Map[Pattern.Var[ArrayIR], CompiledPattern[ArrayIR, EGraph[ArrayIR]]],
                                                                                       egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Option[PatternMatch[ArrayIR]] = {
    compiledPatterns.foldLeft(Option(m)) {
      case (Some(newMatch), (variable, pattern)) =>
        tryMatchVariableToTypePattern(variable, pattern, newMatch, egraph)

      case (None, _) => None
    }
  }

  final case class SearcherWithRequirements[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](searcher: Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphWithMetadata[ArrayIR, EGraphT]],
                                                                                                          types: Map[Pattern.Var[ArrayIR], MixedTree[ArrayIR, Pattern[ArrayIR]]])
    extends ReversibleSearcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {

    // Precompile the patterns.
    private val compiledPatterns = types.map {
      case (v, t) => v -> t.compiled[EGraph[ArrayIR]]
    }

    override def search(egraph: EGraphWithMetadata[ArrayIR, EGraphT], parallelize: ParallelMap): Seq[PatternMatch[ArrayIR]] = {
      searcher.flatMap(checkMatch(_, compiledPatterns, _)).search(egraph, parallelize)
    }

    override def tryReverse: Option[Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]] = {
      searcher match {
        case searcher: ReversibleSearcher[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] =>
          searcher.tryReverse.map(ApplierWithRequirements(_, types))

        case _ => None
      }
    }
  }

  final case class ApplierWithRequirements[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]],
                                                                                                          types: Map[Pattern.Var[ArrayIR], MixedTree[ArrayIR, Pattern[ArrayIR]]])
    extends ReversibleApplier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {

    // Precompile the patterns.
    private val compiledPatterns = types.map {
      case (v, t) => v -> t.compiled[EGraph[ArrayIR]]
    }

    override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Command[ArrayIR] = {
      applier.flatMap((m2: PatternMatch[ArrayIR], egraph2: EGraphWithMetadata[ArrayIR, EGraphT]) => checkMatch(m2, compiledPatterns, egraph2)).apply(m, egraph)
    }

    override def tryReverse: Option[Searcher[ArrayIR, Seq[PatternMatch[ArrayIR]], EGraphWithMetadata[ArrayIR, EGraphT]]] = {
      applier match {
        case applier: ReversibleApplier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] =>
          applier.tryReverse.map(SearcherWithRequirements(_, types))

        case _ => None
      }
    }
  }
}
