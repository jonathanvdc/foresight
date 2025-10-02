package foresight.eqsat.examples.vnorm

import foresight.eqsat.lang.{Language, LanguageCostFunction}
import foresight.eqsat.mutable.EGraph
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import org.junit.Test

class VectorNormTest {
  type ArithRule = R.ArithRule
  val L: Language[ArithExpr] = summon[Language[ArithExpr]]
  val R: Rules = Rules()(using L)

  val simpleStrategy: Strategy[EGraph[ArithIR], Unit] = MaximalRuleApplication.mutable(R.all).repeatUntilStable

  val costFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
    override def apply(expr: ArithExpr): Int = {
      expr match {
        case Add(lhs, rhs) => apply(lhs) + apply(rhs) + 10
        case Mul(lhs, rhs) => apply(lhs) + apply(rhs) + 50
        case Div(lhs, rhs) => apply(lhs) + apply(rhs) + 200
        case Sqrt(arg) => apply(arg) + 1000
        case FastInvSqrt(arg) => apply(arg) + 1100
        case Vector3(x, y, z) => apply(x) + apply(y) + apply(z)
        case FloatLiteral(value) => 1
        case Var(name) => 1
        case Fact(cost: Int) => cost
      }
    }
  }

  @Test
  def vectorNormSaturates(): Unit = {
    // vector normalization:
    // norm(v) = sqrt(x^2 + y^2 + z^2)
    // (x, y, z) / norm(v)

    val x = Var("x")
    val y = Var("y")
    val z = Var("z")

    val normalized = {
      val norm = Sqrt(x * x + y * y + z * z)
      val invNorm = FloatLiteral(1.0) / norm
      Vector3(x * invNorm, y * invNorm, z * invNorm)
    }

    val fastNormalized = {
      val invNormFast = FastInvSqrt(x * x + y * y + z * z)
      Vector3(x * invNormFast, y * invNormFast, z * invNormFast)
    }

    val egraph = EGraph.empty[ArithIR]
    val root = egraph.add(L.toTree(normalized))

    simpleStrategy(egraph)

    assert(L.extract(root, egraph, costFunction) == fastNormalized)
  }
}
