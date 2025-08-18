package foresight.eqsat.lang

import scala.language.implicitConversions
import foresight.eqsat.rewriting.patterns.{Pattern, PatternMatch}
import foresight.eqsat.{EClassCall, EGraph, Slot}
import foresight.eqsat.rewriting.Rule
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import org.junit.Test

class PatternTest {
  sealed trait ArithExpr derives Language
  final case class Var(slot: Use[Slot]) extends ArithExpr
  final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr
  final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
  final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Number(value: BigInt) extends ArithExpr

  final case class Ref(eClass: EClassCall) extends ArithExpr derives Atom
  final case class PatternVar(variable: Pattern.Var) extends ArithExpr derives Atom
  object PatternVar {
    def fresh(): PatternVar = PatternVar(Pattern.Var.fresh())
  }

  // Implicit conversion: Int => ArithExpr (Number)
  given Conversion[Int, ArithExpr] with
    def apply(n: Int): ArithExpr = Number(BigInt(n))

  // Operator overloads on ArithExpr
  extension (lhs: ArithExpr)
    infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
    infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

  val Lang: Language[ArithExpr] = summon[Language[ArithExpr]]
  type ArithIR = Lang.Op

  def associativityRule: Rule[ArithIR, PatternMatch[ArithIR], EGraph[ArithIR]] = {
    val x = PatternVar.fresh()
    val y = PatternVar.fresh()
    Lang.rule("assoc", x + y, y + x)
  }

  def strategies: Seq[Strategy[ArithIR, EGraph[ArithIR], Unit]] = {
    Seq(
      MaximalRuleApplication(Seq(associativityRule))
        .withIterationLimit(12)
        .repeatUntilStable
    )
  }

  /**
   * Test that addition is associative.
   */
  @Test
  def additionIsAssociative(): Unit = {
    // x + y = y + x
    val x = Var(Slot.fresh())
    val y = Var(Slot.fresh())

    val egraph = EGraph.empty[ArithIR]
    val (c1, egraph2) = egraph.add(Lang.toTree(x + y))
    val (c2, egraph3) = egraph2.add(Lang.toTree(y + x))

    assert(!egraph3.areSame(c1, c2))

    for (strategy <- strategies) {
      val Some(egraph4) = strategy(egraph3)
      assert(egraph4.areSame(c1, c2))
    }
  }
}
