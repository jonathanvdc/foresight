package foresight.eqsat.lang

import scala.language.implicitConversions
import foresight.eqsat.Slot
import org.junit.Test

import scala.math.Ordered.orderingToOrdered

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

  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  type ArithIR = L.Op

  @Test
  def opOrderingWorks(): Unit = {
    val x = Slot.numeric(0)
    val expr1 = Lam(Def(x), Var(Use(x)) + 3 + 4)
    val expr2 = Lam(Def(x), Var(Use(x)) + 7 + 8)

    assert(L.toTree(expr1) < L.toTree(expr2))
    assert(L.fromTree(L.toTree(expr1)) == expr1)
    assert(L.fromTree(L.toTree(expr2)) == expr2)
  }

  @Test
  def numberOrderingWorks(): Unit = {
    val numbers = Seq.range(0, 200).map(Number(_))

    for (i <- 0 until numbers.length - 1) {
      for (j <- 0 until i) {
        val a = numbers(i)
        val b = numbers(j)
        assert(L.toTree(a) > L.toTree(b), s"Expected $a > $b but got ${L.toTree(a)} > ${L.toTree(b)}")
      }
      for (j <- i + 1 until numbers.length) {
        val a = numbers(i)
        val b = numbers(j)
        assert(L.toTree(a) < L.toTree(b), s"Expected $a < $b but got ${L.toTree(a)} < ${L.toTree(b)}")
      }
    }
  }
}
