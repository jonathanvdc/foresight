package foresight.eqsat.examples.arith2

import foresight.eqsat.{EClassCall, EGraph, MixedTree, Slot}
import foresight.eqsat.lang.*
import org.junit.Test

sealed trait ArithExpr derives Language

final case class Var(slot: Use[Slot]) extends ArithExpr
final case class Lam(param: Defn[Slot], body: ArithExpr) extends ArithExpr
final case class App(fun: ArithExpr, arg: ArithExpr) extends ArithExpr
final case class Add(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
final case class Mul(lhs: ArithExpr, rhs: ArithExpr) extends ArithExpr
final case class Number(value: BigInt) extends ArithExpr

final case class Ref(eClass: EClassCall) extends ArithExpr
object Ref:
  given AsMixin[Ref, EClassCall] = AsMixin.codec(to = _.eClass, from = Ref(_))

def example(using L: Language[ArithExpr]): (L.Node[EClassCall], ArithExpr) =
  val (c, _) = EGraph.from[Int](MixedTree.unslotted(0, Seq()))
  val x = Slot.numeric(0)
  val expr = Lam(Defn(x), Add(Add(Var(Use(x)), Number(3)), Ref(c)))
  (L.toTree[EClassCall](expr), L.fromTree[EClassCall](L.toTree[EClassCall](expr)))

class ArithTest {
  @Test
  def testExample(): Unit = {
    val (c, _) = EGraph.from[Int](MixedTree.unslotted(0, Seq()))
    println(Registries.decode(summon[Registries.CallDecoder[ArithExpr, EClassCall]], c))
    println(summon[Language[ArithExpr]].toTree[EClassCall](Ref(c)))
    println(summon[AsMixin[Ref, EClassCall]].toMixin(Ref(c)))
    println(summon[AsMixin[Ref, EClassCall]].fromMixin(c))
    println(summon[Language[ArithExpr]].fromTree[EClassCall](MixedTree.Call(c)))

    val (tree, expr) = example
    println(tree)
    println(expr)
  }
}
