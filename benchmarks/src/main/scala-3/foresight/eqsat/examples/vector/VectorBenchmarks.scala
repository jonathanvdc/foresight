package foresight.eqsat.examples.vector

import foresight.eqsat.lang.Language
import foresight.eqsat.mutable.EGraph as MutableEGraph
import foresight.eqsat.immutable.EGraph as ImmutableEGraph
import foresight.eqsat.saturation.MaximalRuleApplication
import foresight.util.BenchmarksWithParallelMap
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, State}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class VectorBenchmarks extends BenchmarksWithParallelMap {
  @Param(Array("true", "false"))
  var mutableEGraph: Boolean = _

  @Benchmark
  def vectorNormalization(): VectorArithExpr = {
    bench(Expressions.vectorNormalization)
  }

  @Benchmark
  def reflection(): VectorArithExpr = {
    bench(Expressions.reflection)
  }

  @Benchmark
  def gramSchmidt(): VectorArithExpr = {
    bench(Expressions.gramSchmidt)
  }

  @Benchmark
  def blinnPhong(): VectorArithExpr = {
    bench(Expressions.blinnPhong)
  }

  private def bench(expr: VectorArithExpr): VectorArithExpr = {
    if (mutableEGraph) {
      val egraph = MutableEGraph.empty[ArithIR]
      val root = egraph.add(Lang.toTree(expr))
      MaximalRuleApplication.mutable(R.all).repeatUntilStable(egraph, parallelMap)
      Lang.extract(root, egraph, CostModel)
    } else {
      val (root, egraph) = Lang.toEGraph(expr)
      val Some(saturated) = MaximalRuleApplication(R.all).repeatUntilStable(egraph, parallelMap)
      Lang.extract(root, saturated, CostModel)
    }
  }

  private type ArithRule = R.ArithRule
  private val Lang: Language[VectorArithExpr] = summon[Language[VectorArithExpr]]
  private val R: Rules = Rules()(using Lang)

  private object Expressions {
    private def dot(a: VectorArithExpr, b: VectorArithExpr) = {
      ElementAt(a, 0) * ElementAt(b, 0) + ElementAt(a, 1) * ElementAt(b, 1) + ElementAt(a, 2) * ElementAt(b, 2)
    }

    private def invNorm(v: VectorArithExpr) = {
      FloatLiteral(1.0) / Sqrt(dot(v, v))
    }

    private def vec3(identifier: String) = Var(identifier, Type.Vector3Type(Type.FloatType))

    def vectorNormalization: VectorArithExpr = {
      val v = vec3("v")
      val x = ElementAt(v, 0)
      val y = ElementAt(v, 1)
      val z = ElementAt(v, 2)

      val factor = invNorm(v)
      Vector3(x * factor, y * factor, z * factor)
    }

    /**
     * Reflection vector computation.
     * This benchmark computes the reflection of an incident vector across a surface normal, a core operation in
     * graphics and physics-based simulation. It combines normalization, a dot product, and vector scaling/subtraction
     * in a compact yet algebraically rich form. Reflection vectors are used in ray tracing, environment mapping, and
     * geometric optics.
     */
    def reflection: VectorArithExpr = {
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

      Rn
    }

    /**
     * Gram–Schmidt (Orthonormalization).
     * Gram–Schmidt is a standard linear algebra routine underlying QR decomposition and appears in scientific
     * computing, simulation, and geometry processing.
     * This benchmark constructs an orthonormal basis from three input vectors using the classical Gram–Schmidt
     * procedure. It involves multiple dot products, vector projections, and repeated normalizations, stressing
     * algebraic simplification and common-subexpression elimination.
     */
    def gramSchmidt: VectorArithExpr = {
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
      Vector3(u1, u2, u3)
    }

    /**
     * Blinn–Phong shading model.
     * The Blinn–Phong reflection model is a modification of the Phong reflection model that is widely used in
     * computer graphics for its simplicity and efficiency. It approximates the way light reflects off surfaces,
     * particularly shiny surfaces, to create realistic highlights and shading effects.
     *
     * This benchmark implements the Blinn–Phong shading model using vector arithmetic operations, including
     * normalization, dot products, and scalar multiplications. It involves multiple steps to compute the final
     * color based on surface normals, light direction, view direction, and material properties.
     */
    def blinnPhong: VectorArithExpr = {
      val N = Var("N", Type.Vector3Type(Type.FloatType)) // surface normal
      val L = Var("L", Type.Vector3Type(Type.FloatType)) // light direction
      val V = Var("V", Type.Vector3Type(Type.FloatType)) // view direction
      val kd = Var("kd", Type.Vector3Type(Type.FloatType)) // diffuse coeff (scalar)
      val ks = Var("ks", Type.Vector3Type(Type.FloatType)) // specular coeff (scalar)

      // normalize N, L, and H = normalize(L+V)
      val invNn = invNorm(N)
      val Nn = N * invNn
      val invLn = invNorm(L)
      val Ln = L * invLn

      val H = Ln + V
      val invH = invNorm(H)
      val Hn = H * invH

      val NdL = dot(Nn, Ln) // scalar
      val NdH = dot(Nn, Hn) // scalar

      // cheap specular exponent ≈ 4 via squaring:
      val spec = (NdH * NdH) * (NdH * NdH)

      // final color (grayscale scalar here; or multiply a base color vector):
      val color = (kd * NdL) + (ks * spec)

      color
    }
  }
}
