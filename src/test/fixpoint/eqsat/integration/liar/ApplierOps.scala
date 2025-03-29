package fixpoint.eqsat.integration.liar

import fixpoint.eqsat.commands.Command
import fixpoint.eqsat.extraction.ExtractionAnalysis
import fixpoint.eqsat.metadata.EGraphWithMetadata
import fixpoint.eqsat.rewriting.Applier
import fixpoint.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import fixpoint.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Slot, Tree}

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](val applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]) extends AnyVal {
    def substitute(source: Pattern.Var[ArrayIR],
                   from: Slot,
                   to: Pattern.Var[ArrayIR],
                   destination: Pattern.Var[ArrayIR]): Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {

      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Command[ArrayIR] = {
          def subst(tree: Tree[ArrayIR]): MixedTree[ArrayIR, EClassCall] = {
            tree match {
              case Tree(Var, Seq(), Seq(use), Seq()) if use == from => m(to)
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
