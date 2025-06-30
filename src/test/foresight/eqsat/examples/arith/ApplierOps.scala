package foresight.eqsat.examples.arith

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Slot, Tree}
import foresight.eqsat.commands.Command
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[ArithIR, EGraphT] with EGraph[ArithIR]](private val applier: Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]]) extends AnyVal {

    /**
     * Substitutes a variable in a pattern match with another variable.
     * @param source The source pattern variable to perform the substitution on.
     * @param from The variable to be substituted.
     * @param to The variable to substitute with.
     * @param destination A variable to store the substituted result in within the pattern match.
     * @return An applier that performs the substitution.
     */
    def substitute(source: Pattern.Var[ArithIR],
                   from: Slot,
                   to: Pattern.Var[ArithIR],
                   destination: Pattern.Var[ArithIR]): Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]] = {

      new Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]] {
        override def apply(m: PatternMatch[ArithIR], egraph: EGraphWithMetadata[ArithIR, EGraphT]): Command[ArithIR] = {
          val extracted = ExtractionAnalysis.smallest[ArithIR].extractor(m(source), egraph)

          def subst(tree: Tree[ArithIR]): MixedTree[ArithIR, EClassCall] = {
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
