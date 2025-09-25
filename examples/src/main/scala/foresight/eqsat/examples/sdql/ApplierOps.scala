package foresight.eqsat.examples.sdql

import foresight.eqsat.commands.Command
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import foresight.eqsat.immutable.{EGraphLike, EGraphWithMetadata, EGraph}
import foresight.eqsat._

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[SdqlIR, EGraphT] with EGraph[SdqlIR]](private val applier: Applier[SdqlIR, PatternMatch[SdqlIR], EGraphWithMetadata[SdqlIR, EGraphT]]) extends AnyVal {

    /**
     * Substitutes a variable in a pattern match with another variable.
     * @param source The source pattern variable to perform the substitution on.
     * @param from The variable to be substituted.
     * @param to The variable to substitute with.
     * @param destination A variable to store the substituted result in within the pattern match.
     * @return An applier that performs the substitution.
     */
    def substitute(source: Pattern.Var,
                   from: Slot,
                   to: Pattern.Var,
                   destination: Pattern.Var): Applier[SdqlIR, PatternMatch[SdqlIR], EGraphWithMetadata[SdqlIR, EGraphT]] = {

      new Applier[SdqlIR, PatternMatch[SdqlIR], EGraphWithMetadata[SdqlIR, EGraphT]] {
        override def apply(m: PatternMatch[SdqlIR], egraph: EGraphWithMetadata[SdqlIR, EGraphT]): Command[SdqlIR] = {
          val extracted = ExtractionAnalysis.smallest[SdqlIR].extractor(m(source), egraph)

          def subst(tree: Tree[SdqlIR]): MixedTree[SdqlIR, EClassCall] = {
            tree match {
              case Tree(Var, Seq(), Seq(use), Seq()) if use == m(from) => m(to)
              case Tree(nodeType, defs, uses, args) =>
                MixedTree.Node(nodeType, defs, uses, args.map(subst))
            }
          }

          val substituted = subst(extracted)
          val newMatch = m.copy(varMapping = m.varMapping + (destination -> substituted))
          applier.apply(newMatch, egraph)
        }
      }
    }
  }
}
