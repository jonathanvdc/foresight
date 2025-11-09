package foresight.eqsat.examples.arithWithLang

import foresight.eqsat.{EClassCall, MixedTree, Slot}
import foresight.eqsat.commands.CommandScheduleBuilder
import foresight.eqsat.extraction.ExtractionAnalysis
import foresight.eqsat.lang.Language
import foresight.eqsat.immutable.{EGraph, EGraphLike, EGraphWithMetadata}
import foresight.eqsat.rewriting.Applier
import foresight.eqsat.rewriting.patterns.PatternMatch

object ApplierOps {
  implicit class ApplierOfPatternMatchOps[EGraphT <: EGraphLike[ArithIR, EGraphT] with EGraph[ArithIR]](private val applier: Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]])
                                                                                                       (using L: Language[ArithExpr]) {

    /**
     * Substitutes a variable in a pattern match with another variable.
     * @param source The source pattern variable to perform the substitution on.
     * @param from The variable to be substituted.
     * @param to The variable to substitute with.
     * @param destination A variable to store the substituted result in within the pattern match.
     * @return An applier that performs the substitution.
     */
    def substitute(source: PatternVar,
                   from: Slot,
                   to: PatternVar,
                   destination: PatternVar): Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]] = {

      new Applier[ArithIR, PatternMatch[ArithIR], EGraphWithMetadata[ArithIR, EGraphT]] {
        override def apply(m: PatternMatch[ArithIR], egraph: EGraphWithMetadata[ArithIR, EGraphT], builder: CommandScheduleBuilder[ArithIR]): Unit = {
          val extractedTree = ExtractionAnalysis.smallest[ArithIR].extractor(m(source.variable), egraph)
          val extractedExpr = L.fromTree[EClassCall](extractedTree)

          def subst(tree: ArithExpr): ArithExpr = {
            tree match {
              case Var(slot) if slot == m(from) => L.fromTree[EClassCall](m(to.variable))
              case Var(slot) => Var(slot)
              case Lam(param, body) => Lam(param, subst(body))
              case App(fun, arg) => App(subst(fun), subst(arg))
              case Add(lhs, rhs) => Add(subst(lhs), subst(rhs))
              case Mul(lhs, rhs) => Mul(subst(lhs), subst(rhs))
              case Number(value) => Number(value)
              case Ref(eClass) => Ref(eClass)
              case PatternVar(_) | Fact(_) =>
                throw new IllegalArgumentException(s"Unexpected node in expression: $tree")
            }
          }

          val substituted = subst(extractedExpr)
          val newMatch = m.copy(varMapping = m.varMapping + (destination.variable -> L.toTree[EClassCall](substituted)))
          applier.apply(newMatch, egraph, builder)
        }
      }
    }
  }
}
