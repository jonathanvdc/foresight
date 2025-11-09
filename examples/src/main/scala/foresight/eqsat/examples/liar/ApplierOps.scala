
package foresight.eqsat.examples.liar

import foresight.eqsat.commands.CommandScheduleBuilder
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithMetadata}
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.{Pattern, PatternApplier, PatternMatch}
import foresight.eqsat._

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
    def substitute(source: Pattern.Var,
                   from: Slot,
                   to: Pattern.Var,
                   destination: Pattern.Var): Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {

      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT], builder: CommandScheduleBuilder[ArrayIR]): Unit = {
          val extracted = ExtractionAnalysis.smallest[ArrayIR].extractor[EGraphT](m(source), egraph)

          def typeOf(tree: MixedTree[ArrayIR, EClassCall]): MixedTree[Type, EClassCall] = {
            TypeInferenceAnalysis.get(egraph)(tree, egraph)
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
          val newMatch = m.bind(destination, substituted)
          applier.apply(newMatch, egraph, builder)
        }
      }
    }
  }

  implicit class PatternApplierOps[EGraphT <: EGraphLike[ArrayIR, EGraphT] with EGraph[ArrayIR]](private val applier: PatternApplier[ArrayIR, EGraphWithMetadata[ArrayIR, EGraphT]]) extends AnyVal {
    private def inferType(tree: MixedTree[ArrayIR, EClassCall], egraph: EGraphWithMetadata[ArrayIR, EGraphT]): MixedTree[ArrayIR, EClassCall] = {
      TypeInferenceAnalysis.get(egraph)(tree, egraph)
    }

    /**
     * Checks the types of the instantiated pattern before adding it to the e-graph.
     * @return An applier that checks the types of the instantiated pattern.
     */
    def typeChecked: Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] = {
      new Applier[ArrayIR, PatternMatch[ArrayIR], EGraphWithMetadata[ArrayIR, EGraphT]] {
        override def apply(m: PatternMatch[ArrayIR], egraph: EGraphWithMetadata[ArrayIR, EGraphT], builder: CommandScheduleBuilder[ArrayIR]): Unit = {
          val tree = applier.instantiate(m)
          val realTree = tree.mapAtoms(_.asInstanceOf[EClassCall])
          inferType(realTree, egraph)
          val c = builder.addSimplifiedReal(realTree, egraph)
          builder.unionSimplified(EClassSymbol.real(m.root), c, egraph)
        }
      }
    }
  }
}
