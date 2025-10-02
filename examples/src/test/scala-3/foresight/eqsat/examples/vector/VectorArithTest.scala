package foresight.eqsat.examples.vector

import foresight.eqsat.lang.Language
import foresight.eqsat.mutable.EGraph
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import org.junit.Test

class VectorArithTest {
  type ArithRule = R.ArithRule
  val Lang: Language[VectorArithExpr] = summon[Language[VectorArithExpr]]
  val R: Rules = Rules()(using Lang)

  val simpleStrategy: Strategy[EGraph[ArithIR], Unit] = MaximalRuleApplication.mutable(R.all).repeatUntilStable

  private def dot(a: VectorArithExpr, b: VectorArithExpr) = {
    ElementAt(a, 0) * ElementAt(b, 0) + ElementAt(a, 1) * ElementAt(b, 1) + ElementAt(a, 2) * ElementAt(b, 2)
  }

  private def invNorm(v: VectorArithExpr) = {
    FloatLiteral(1.0) / Sqrt(dot(v, v))
  }

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
    val root = egraph.add(Lang.toTree(normalized))

    simpleStrategy(egraph)

    val fastNormalizedClass = egraph.add(Lang.toTree(fastNormalized))
    assert(egraph.areSame(root, fastNormalizedClass))
    assert(egraph.areSame(root, egraph.add(Lang.toTree(fastNormalizedCSE))))

    // assert(L.extract(root, egraph, CostModel) == fastNormalizedCSE)
  }

  @Test
  def reflectionSaturates(): Unit = {
    val I = Var("I", Type.Vector3Type(Type.FloatType))
    val N = Var("N", Type.Vector3Type(Type.FloatType))

    val invNn = invNorm(N)
    val Nn = N * invNn

    val two = FloatLiteral(2.0)
    val Ni = dot(Nn, I)
    val R = I + (FloatLiteral(-1.0) * (Nn * (two * Ni)))

    // optional: normalize R to get Rn
    val invR = invNorm(R)
    val Rn = R * invR

    val egraph = EGraph.empty[ArithIR]
    val root = egraph.add(Lang.toTree(Rn))

    simpleStrategy(egraph)

//    println(Lang.extract(root, egraph, CostModel))
  }

  @Test
  def gramSchmidtSaturates(): Unit = {
    val v1 = Var("v1", Type.Vector3Type(Type.FloatType))
    val v2 = Var("v2", Type.Vector3Type(Type.FloatType))
    val v3 = Var("v3", Type.Vector3Type(Type.FloatType))

    val invN1 = invNorm(v1)
    val u1 = v1 * invN1

    val a2 = dot(v2, u1) // scalar
    val proj2 = u1 * a2 // vector
    val w2 = v2 + (FloatLiteral(-1.0) * proj2)

    val invN2 = invNorm(w2)
    val u2 = w2 * invN2

    val a3a = dot(v3, u1)
    val a3b = dot(v3, u2)
    val proj3 = (u1 * a3a) + (u2 * a3b)
    val w3 = v3 + (FloatLiteral(-1.0) * proj3)

    val invN3 = invNorm(w3)
    val u3 = w3 * invN3

    // Result: (u1, u2, u3)
    val expression = Vector3(u1, u2, u3)

    val egraph = EGraph.empty[ArithIR]
    val root = egraph.add(Lang.toTree(expression))

    simpleStrategy(egraph)

//    println(Lang.extract(root, egraph, CostModel))
  }
}
