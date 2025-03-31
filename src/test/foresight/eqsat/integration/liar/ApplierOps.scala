package foresight.eqsat.integration.liar

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Slot, Tree}
import foresight.eqsat.commands.Command
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]) extends AnyVal {

    def substitute(source: Pattern.Var[ArrayIR],
                   from: Slot,
                   to: Pattern.Var[ArrayIR],
                   destination: Pattern.Var[ArrayIR]): Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {

      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Command[ArrayIR] = {
          def subst(tree: Tree[ArrayIR]): MixedTree[ArrayIR, EClassCall] = {
            tree match {
              case Tree(Var, Seq(), Seq(use), Seq(_)) if use == m(from) => m(to)
              case Tree(nodeType, defs, uses, args) =>
                MixedTree.Node(nodeType, defs, uses, args.map(subst))
            }
          }

          val extracted = ExtractionAnalysis.smallest[ArrayIR].extractor(m(source), egraph)
          val substituted = subst(extracted)
          val newMatch = m.copy(varMapping = m.varMapping + (destination -> substituted))
          applier.apply(newMatch, egraph)
        }
      }
    }
  }
}
