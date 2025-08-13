package foresight.eqsat.examples.arith2

import foresight.eqsat.Slot
import foresight.eqsat.lang.*
import org.junit.Test

sealed trait ArithExpr derives Language

final case class Var(slot: Use[Slot]) extends ArithExpr
final case class Lam(param: Defn[Slot], body: ArithExpr) extends ArithExpr
final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr

final case class Number(value: BigInt) extends ArithExpr

def example[A](using L: Language[ArithExpr]): (L.Node[A], ArithExpr) =
  val x = Slot.numeric(0)
  val expr = Lam(Defn(x), Add(Var(Use(x)), Number(3)))
  (L.toTree(expr), L.fromTree(L.toTree(expr)))


class ArithTest {
  @Test
  def testExample(): Unit = {
    val (tree, expr) = example[Int]
    println(tree)
    println(expr)
  }
}
