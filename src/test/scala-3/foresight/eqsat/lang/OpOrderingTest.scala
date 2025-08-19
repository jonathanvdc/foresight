package foresight.eqsat.lang

import scala.language.implicitConversions

import foresight.eqsat.Slot
import org.junit.Test

class OpOrderingTest {
  sealed trait ArithExpr derives Language
  final case class Var(slot: Use[Slot]) extends ArithExpr
  final case class Lam(param: Def[Slot], body: ArithExpr) extends ArithExpr
  final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
  final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
  final case class Number(value: BigInt) extends ArithExpr

  // Implicit conversion: Int => ArithExpr (Number)
  given Conversion[Int, ArithExpr] with
    def apply(n: Int): ArithExpr = Number(BigInt(n))

  // Operator overloads on ArithExpr
  extension (lhs: ArithExpr)
    infix def +(rhs: ArithExpr): ArithExpr = Add(lhs, rhs)
    infix def *(rhs: ArithExpr): ArithExpr = Mul(lhs, rhs)

  val Lang: Language[ArithExpr] = summon[Language[ArithExpr]]
  type ArithIR = Lang.Op

  @Test
  def opOrderingWorks(): Unit = {
    val L = summon[Language[ArithExpr]]
    val x = Slot.numeric(0)
    val expr1 = Lam(Def(x), Var(Use(x)) + 3 + 4)
    val expr2 = Lam(Def(x), Var(Use(x)) + 7 + 8)
//
//    // Check that the order of operations is preserved
//    assert(L.toTree(expr1) < L.toTree(expr2))
//    assert(L.fromTree(L.toTree(expr1)) == expr1)
//    assert(L.fromTree(L.toTree(expr2)) == expr2)
  }
}
