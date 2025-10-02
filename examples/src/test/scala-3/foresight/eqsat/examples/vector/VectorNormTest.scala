package foresight.eqsat.examples.vector

import foresight.eqsat.lang.Language
import foresight.eqsat.mutable.EGraph
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import org.junit.Test

class VectorNormTest {
  type ArithRule = R.ArithRule
  val L: Language[VectorArithExpr] = summon[Language[VectorArithExpr]]
  val R: Rules = Rules()(using L)

  val simpleStrategy: Strategy[EGraph[ArithIR], Unit] = MaximalRuleApplication.mutable(R.all).repeatUntilStable

  @Test
  def vectorNormSaturates(): Unit = {
    // vector normalization:
    // norm(v) = sqrt(x^2 + y^2 + z^2)
    // (x, y, z) / norm(v)

    val v = Var("v", Type.Vector3Type(Type.FloatType))

    val x = ElementAt(v, 0)
    val y = ElementAt(v, 1)
    val z = ElementAt(v, 2)

    val normalized = {
      val norm = Sqrt(x * x + y * y + z * z)
      val invNorm = FloatLiteral(1.0) / norm
      Vector3(x * invNorm, y * invNorm, z * invNorm)
    }

    val fastNormalized = {
      val invNormFast = FastInvSqrt(x * x + y * y + z * z)
      Vector3(x * invNormFast, y * invNormFast, z * invNormFast)
    }

    val fastNormalizedCSE = {
      val invNormFast = FastInvSqrt(x * x + y * y + z * z)
      Vector3(x, y, z) * invNormFast
    }

    val egraph = EGraph.empty[ArithIR]
    val root = egraph.add(L.toTree(normalized))

    simpleStrategy(egraph)

    val fastNormalizedClass = egraph.add(L.toTree(fastNormalized))
    assert(egraph.areSame(root, fastNormalizedClass))
    assert(egraph.areSame(root, egraph.add(L.toTree(fastNormalizedCSE))))

    // assert(L.extract(root, egraph, CostModel) == fastNormalizedCSE)
  }
}
