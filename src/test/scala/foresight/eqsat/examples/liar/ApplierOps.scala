
package foresight.eqsat.examples.liar

import foresight.eqsat.{EClassCall, EGraph, EGraphLike, MixedTree, Slot, Tree}
import foresight.eqsat.commands.{Command, EClassSymbol}
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.metadata.EGraphWithMetadata
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val applier: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]]) extends AnyVal {

    /**
     * Substitutes a variable in a pattern match with another variable.
     * @param source The source pattern variable to perform the substitution on.
     * @param from The variable to be substituted.
     * @param to The variable to substitute with.
     * @param destination A variable to store the substituted result in within the pattern match.
     * @return An applier that performs the substitution.
     */
    def substitute(source: Pattern.Var[ArrayIR],
                   from: Slot,
                   to: Pattern.Var[ArrayIR],
                   destination: Pattern.Var[ArrayIR]): Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {

      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Command[ArrayIR] = {
          val extracted = ExtractionAnalysis.smallest[ArrayIR].extractor(m(source), egraph)

          def typeOf(tree: MixedTree[ArrayIR, EClassCall]): MixedTree[Type, EClassCall] = {
            val (toInGraph, newGraph) = egraph.add(tree)
            TypeInferenceAnalysis.get(newGraph)(toInGraph, newGraph)
          }

          def subst(tree: Tree[ArrayIR]): MixedTree[ArrayIR, EClassCall] = {
            tree match {
              case Tree(Var, Seq(), Seq(use), Seq(fromType))
                if use == m(from) && typeOf(m(to)) == MixedTree.fromTree(fromType) =>

                m(to)
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

  implicit class PatternApplierOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val applier: PatternApplier[ArrayIR, EGraphWithMetadata[ArrayIR, EGraphT]]) extends AnyVal {
    private def inferType(tree: MixedTree[ArrayIR, EClassCall], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): MixedTree[ArrayIR, EClassCall] = {
      val (call, newEGraph) = egraph.add(tree)
      TypeInferenceAnalysis.get(newEGraph)(call, newEGraph)
    }

    /**
     * Checks the types of the instantiated pattern before adding it to the e-graph.
     * @return An applier that checks the types of the instantiated pattern.
     */
    def typeChecked: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): Command[ArrayIR] = {
          val tree = applier.instantiate(m)
          inferType(tree.mapAtoms(_.asInstanceOf[EClassSymbol.Real].call), egraph)
          Command.addEquivalentTree(EClassSymbol.real(m.root), tree)
        }
      }
    }
  }
}
