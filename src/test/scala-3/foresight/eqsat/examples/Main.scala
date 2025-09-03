package foresight.eqsat.examples

import foresight.eqsat.examples.mm.*
import foresight.eqsat.examples.poly.*
import foresight.eqsat.lang.*
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.saturation.{MaximalRuleApplication, Strategy}
import foresight.eqsat.{EClassCall, EGraph}

import scala.collection.mutable.ArrayBuffer

object Main {

  final case class DimAndCost(nrows: Int, ncols: Int, cost: Int)
  given Ordering[DimAndCost] = Ordering.by(_.cost)

  var benchStartTime = 0L;

  val linalgCostFunction: LanguageCostFunction[LinalgExpr, DimAndCost] = new LanguageCostFunction[LinalgExpr, DimAndCost]() {
    override def apply(expr: LinalgExpr): DimAndCost = {
      expr match {
        case mm.Mat(rows, cols) => DimAndCost(rows, cols, 0)
        case mm.Fact(dimAndCost: DimAndCost) => dimAndCost
        case mm.Mul(lhs, rhs) =>
          val DimAndCost(lrows, lcols, lcost) = apply(lhs)
          val DimAndCost(rrows, rcols, rcost) = apply(rhs)
          if (lcols != rrows) {
            throw new RuntimeException(s"Dimension mismatch: $lcols != $rrows")
          } else {
            DimAndCost(lrows, rcols, lrows * lcols * rcols + lcost + rcost)
          }
        case _ => DimAndCost(-1, -1, 0)
      }
    }
  }

  val arithCostFunction: LanguageCostFunction[ArithExpr, Int] = new LanguageCostFunction[ArithExpr, Int]() {
    override def apply(expr: ArithExpr): Int = {
      expr match {
        case poly.Add(lhs, rhs) => apply(lhs) + apply(rhs) + 10
        case poly.Mul(lhs, rhs) => apply(lhs) + apply(rhs) + 100
        case poly.Pow(lhs, rhs) => apply(lhs) + apply(rhs) + 1000
        case poly.Fact(cost: Int) => cost
        case _ => 1
      }
    }
  }

  def nmm(n: Int): LinalgExpr = {
    if (n == 0) {
      mm.Mat(10, 10)
    } else {
      mm.Mul(nmm(n - 1), Mat(10, 10))
    }
  }

  def testNmmBench(n: Int, map: ParallelMap): (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Long]) = {
    benchStartTime = System.nanoTime()
    val L: Language[LinalgExpr] = summon[Language[LinalgExpr]]
    val R: mm.Rules = mm.Rules()(using L)
    type LinalgRule = R.LinalgRule

    val nEclasses: ArrayBuffer[Int] = ArrayBuffer.empty
    val nEnodes: ArrayBuffer[Int] = ArrayBuffer.empty
    val times: ArrayBuffer[Long] = ArrayBuffer.empty

    val simpleStrategy: Strategy[LinalgIR, EGraph[LinalgIR], Unit] = MaximalRuleApplication(R.all)
      .withChangeLogger((oldEGraph, newEGraph) => {
//        println(s"\t\tEGraph changed: ${oldEGraph.classCount} -> ${newEGraph.classCount} eclasses, ${oldEGraph.nodeCount} -> ${newEGraph.nodeCount} enodes, Time since start of this benchmark: ${(System.nanoTime() - benchStartTime) / 1e6} ms")
        val dt = System.nanoTime() - benchStartTime

        if (nEclasses.isEmpty) {
          nEclasses += oldEGraph.classCount
          nEnodes += oldEGraph.nodeCount
          times += 0L
        }

        nEclasses += newEGraph.classCount
        nEnodes += newEGraph.nodeCount
        times += dt
      })
      .repeatUntilStable

    val expr = nmm(n)

    val (root, egraph) = L.toEGraph(expr)
    val egraph2 = simpleStrategy(egraph, map).get

    val extracted = egraph2.extract(root, linalgCostFunction)

    return (nEclasses, nEnodes, times)
  }

  def benchNmm(n: Int, map: ParallelMap, str: String): Unit = {
    println(s"## Benchmarking nmm with n=$n for 60 seconds.")
    val time = 60_000_000_000L
    val start = System.nanoTime()
    var times: List[Long] = List()
    var iterations = 0

    while (System.nanoTime() - start < time) {
//      println("\tStarting iteration " + (iterations + 1))
      val testStart = System.nanoTime()
      testNmmBench(n, map)
      val testEnd = System.nanoTime()
      val duration = testEnd - testStart
      //      println(s"Iteration $iterations took $duration ms")
      times = duration :: times
      iterations += 1
    }

    val (nEclasses, nEnodes, dts) = testNmmBench(n, map)

    println(s"Completed $iterations iterations in 60 seconds")
    val avgTime = if (times.nonEmpty) times.sum / times.length else 0
    println(s"Average time per iteration: ${avgTime / 1e6} ms")

    // save nEclasses, nEnodes, dts to csv
    val csvLines = new StringBuilder
    csvLines.append("Iteration,Time(ns),NumEClasses,NumENodes\n")
    for (i <- dts.indices) {
      csvLines.append(s"${i},${dts(i)},${nEclasses(i)},${nEnodes(i)}\n")
    }
    val filename = s"bench/${n}mm$str.csv"
    import java.io._
    val pw = new PrintWriter(new File(filename))
    pw.write(csvLines.toString())
    pw.close()
  }

  def benchMM(map: ParallelMap, str: String): Unit = {
    val n = List(3, 5, 10, 20, 40, 80)
    for (i <- n) {
      benchNmm(i, map, str)
    }
  }

  def testPoly5Bench(map: ParallelMap) : (ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Long]) = {
    benchStartTime = System.nanoTime()
    val L: Language[ArithExpr] = summon[Language[ArithExpr]]
    val R: poly.Rules = poly.Rules()(using L)
    type ArithRule = R.ArithRule

    val nEclasses: ArrayBuffer[Int] = ArrayBuffer.empty
    val nEnodes: ArrayBuffer[Int] = ArrayBuffer.empty
    val times: ArrayBuffer[Long] = ArrayBuffer.empty

    val simpleStrategy: Strategy[ArithIR, EGraph[ArithIR], Unit] = MaximalRuleApplication(R.all)
      .withChangeLogger((oldEGraph, newEGraph) => {
//        println(s"\t\tEGraph changed: ${oldEGraph.classCount} -> ${newEGraph.classCount} eclasses, ${oldEGraph.nodeCount} -> ${newEGraph.nodeCount} enodes, Time since start of this benchmark: ${(System.nanoTime() - benchStartTime) / 1e6} ms")
        val dt = System.nanoTime() - benchStartTime

        if (nEclasses.isEmpty) {
          nEclasses += oldEGraph.classCount
          nEnodes += oldEGraph.nodeCount
          times += 0L
        }

        nEclasses += newEGraph.classCount
        nEnodes += newEGraph.nodeCount
        times += dt
      })
      .repeatUntilStable

    // polynomial of degree 5: ax^5 + bx^4 + cx^3 + dx^2 + ex + f
    val c0 = Zero // 0
    val c1 = Succ(c0) // 1
    val c2 = Succ(c1) // 2
    val c3 = Succ(c2) // 3
    val c4 = Succ(c3) // 4
    val c5 = Succ(c4) // 5

    val x = Var("x")
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val d = Var("d")
    val e = Var("e")
    val f = Var("f")

    val poly5 = a * (x ** c5) + b * (x ** c4) + c * (x ** c3) + d * (x ** c2) + e * x + f
    val poly5Simplified = f + x * (e + x * (d + x * (c + x * (b + a * x))))

    val (root, egraph) = L.toEGraph(poly5)
    val egraph2 = simpleStrategy(egraph, map).get

    val extracted = egraph2.extract(root, arithCostFunction)

    return (nEclasses, nEnodes, times)
  }

  def benchPoly5(map: ParallelMap, str: String): Unit = {
    println("## Benchmarking poly5 for 60 seconds.")
    val time = 60_000_000_000L
    val start = System.nanoTime()
    var times: List[Long] = List()
    var iterations = 0
    while (System.nanoTime() - start < time) {
//      println("\tStarting iteration " + (iterations + 1))
      val testStart = System.nanoTime()
      testPoly5Bench(map)
      val testEnd = System.nanoTime()
      val duration = testEnd - testStart
      //      println(s"Iteration $iterations took $duration ms")
      times = duration :: times
      iterations += 1
    }
    println(s"Completed $iterations iterations in 60 seconds")
    val avgTime = if (times.nonEmpty) times.sum / times.length else 0
    println(s"Average time per iteration: ${avgTime / 1e6} ms")

    val (nEclasses, nEnodes, dts) = testPoly5Bench(map)

    // save nEclasses, nEnodes, dts to csv
    val csvLines = new StringBuilder
    csvLines.append("Iteration,Time(ns),NumEClasses,NumENodes\n")
    for (i <- dts.indices) {
      csvLines.append(s"${i},${dts(i)},${nEclasses(i)},${nEnodes(i)}\n")
    }
    val filename = s"bench/poly_$str.csv"
    import java.io._
    val pw = new PrintWriter(new File(filename))
    pw.write(csvLines.toString())
    pw.close()
  }

  def main(args: Array[String]): Unit = {
    {
      val map = ParallelMap.default.timed
      println("# ================Using default parallel map================")
      benchPoly5(map, "default")
      println(map.report)
      benchMM(map, "default")
      println(map.report)
    }

    {
      val map = ParallelMap.sequential.timed
      println("# ================Using sequential map================")
      benchPoly5(map, "sequential")
      println(map.report)
      benchMM(map, "sequential")
      println(map.report)
    }

    for (i <- 1 to 10) {
      val map = ParallelMap.fixedThreadParallel(i).timed
      println(s"# ================Using fixed parallel map with $i threads================")
      benchPoly5(map, "fixedThreadParallel" + i)
      println(map.report)
      benchMM(map, "fixedThreadParallel" + i)
      println(map.report)
    }
  }

}