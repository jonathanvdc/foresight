package foresight.eqsat.examples.mm.tests

import foresight.eqsat.examples.mm.*
import foresight.eqsat.lang.*
import foresight.eqsat.saturation.{MaximalRuleApplication, MaximalRuleApplicationWithCaching, Strategy}
import foresight.eqsat.{EClassCall, EGraph}
import org.junit.Test

import scala.language.implicitConversions

class MMTest {
  type LinalgRule = R.LinalgRule
  val L: Language[LinalgExpr] = summon[Language[LinalgExpr]]
  val R: Rules = Rules()(using L)

  val simpleStrategy: Strategy[LinalgIR, EGraph[LinalgIR], Unit] = MaximalRuleApplication(R.all).repeatUntilStable

  final case class DimAndCost(nrows: Int, ncols: Int, cost: Int)
  given Ordering[DimAndCost] = Ordering.by(_.cost)

  val costFunction: LanguageCostFunction[LinalgExpr, DimAndCost] = new LanguageCostFunction[LinalgExpr, DimAndCost]() {
    override def apply(expr: LinalgExpr): DimAndCost = {
      expr match {
        case Mat(rows, cols) => DimAndCost(rows, cols, 0)
        case Fact(dimAndCost: DimAndCost) => dimAndCost
        case Mul(lhs, rhs) =>
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


  @Test
  def test2MMSaturation(): Unit = {
    val X = Mat(100, 10)
    val Y = Mat(10, 150)
    val Z = Mat(150, 8)

    val expr1 = (X * Y) * Z
    val expr2 = X * (Y * Z)

    val egraph = EGraph.empty[LinalgIR]
    val (class1, egraph2) = egraph.add(expr1)
    val (class2, egraph3) = egraph2.add(expr2)

    assert(!egraph3.areSame(class1, class2))

    val Some(egraph4) = simpleStrategy(egraph3)
    assert(egraph4.areSame(class1, class2))
  }

  @Test
  def test2MMExtraction(): Unit = {
    val X = Mat(100, 10)
    val Y = Mat(10, 150)
    val Z = Mat(150, 8)

    val expr1 = (X * Y) * Z
    val expr2 = X * (Y * Z)

    val expr1Cost = costFunction(expr1)
    val expr2Cost = costFunction(expr2)

    assert(expr1Cost.cost == 100*10*150 + 100*150*8, s"Got ${expr1Cost.cost}")
    assert(expr2Cost.cost == 10*150*8 + 100*10*8, s"Got ${expr2Cost.cost}")
    assert(expr1Cost.cost > expr2Cost.cost)

    val (root, egraph) = L.toEGraph(expr1)
    val egraph2 = simpleStrategy(egraph).get

    val extracted = egraph2.extract(root, costFunction)
    assert(extracted == expr2, s"Expected $expr2 but got $extracted")
  }

  @Test
  def test3MMSaturation(): Unit = {
    val X = Mat(200, 175)
    val Y = Mat(175, 250)
    val Z = Mat(250, 150)
    val W = Mat(150, 10)

    val expr1 = ((X * Y) * Z) * W
    val expr2 = (X * (Y * Z)) * W
    val expr3 = X * ((Y * Z) * W)
    val expr4 = X * (Y * (Z * W))
    val expr5 = (X * Y) * (Z * W)

    val egraph = EGraph.empty[LinalgIR]
    val (class1, egraph2) = egraph.add(expr1)
    val (class2, egraph3) = egraph2.add(expr2)
    val (class3, egraph4) = egraph3.add(expr3)
    val (class4, egraph5) = egraph4.add(expr4)
    val (class5, egraph6) = egraph5.add(expr5)

    assert(!egraph6.areSame(class1, class2))
    assert(!egraph6.areSame(class1, class3))
    assert(!egraph6.areSame(class1, class4))
    assert(!egraph6.areSame(class1, class5))
    assert(!egraph6.areSame(class2, class3))
    assert(!egraph6.areSame(class2, class4))
    assert(!egraph6.areSame(class2, class5))
    assert(!egraph6.areSame(class3, class4))
    assert(!egraph6.areSame(class3, class5))
    assert(!egraph6.areSame(class4, class5))

    val Some(egraph7) = simpleStrategy(egraph6)
    assert(egraph7.areSame(class1, class2))
    assert(egraph7.areSame(class1, class3))
    assert(egraph7.areSame(class1, class4))
    assert(egraph7.areSame(class1, class5))
    assert(egraph7.areSame(class2, class3))
    assert(egraph7.areSame(class2, class4))
    assert(egraph7.areSame(class2, class5))
    assert(egraph7.areSame(class3, class4))
    assert(egraph7.areSame(class3, class5))
    assert(egraph7.areSame(class4, class5))
  }

  @Test
  def test3MMExtraction(): Unit = {
    val X = Mat(200, 175)
    val Y = Mat(175, 250)
    val Z = Mat(250, 150)
    val W = Mat(150, 10)

    val expr1 = ((X * Y) * Z) * W
    val expr2 = (X * (Y * Z)) * W
    val expr3 = X * ((Y * Z) * W)
    val expr4 = X * (Y * (Z * W))
    val expr5 = (X * Y) * (Z * W)

    //Expr: Mul(Mul(Mul(Mat(200,175),Mat(175,250)),Mat(250,150)),Mat(150,10)), Cost: 16550000
    //Expr: Mul(Mul(Mat(200,175),Mul(Mat(175,250),Mat(250,150))),Mat(150,10)), Cost: 12112500
    //Expr: Mul(Mat(200,175),Mul(Mul(Mat(175,250),Mat(250,150)),Mat(150,10))), Cost: 7175000
    //Expr: Mul(Mat(200,175),Mul(Mat(175,250),Mul(Mat(250,150),Mat(150,10)))), Cost: 1162500
    //Expr: Mul(Mul(Mat(200,175),Mat(175,250)),Mul(Mat(250,150),Mat(150,10))), Cost: 9625000

    val expr1Cost = costFunction(expr1)
    val expr2Cost = costFunction(expr2)
    val expr3Cost = costFunction(expr3)
    val expr4Cost = costFunction(expr4)
    val expr5Cost = costFunction(expr5)

    assert(expr1Cost.cost == 200*175*250 + 200*250*150 + 200*150*10, s"Got ${expr1Cost.cost}")
    assert(expr2Cost.cost == 175*250*150 + 200*175*150 + 200*150*10, s"Got ${expr2Cost.cost}")
    assert(expr3Cost.cost == 175*250*150 + 175*150*10 + 200*175*10, s"Got ${expr3Cost.cost}")
    assert(expr4Cost.cost == 250*150*10 + 175*250*10 + 200*175*10, s"Got ${expr4Cost.cost}")
    assert(expr5Cost.cost == 200*175*250 + 250*150*10 + 200*250*10, s"Got ${expr5Cost.cost}")

    assert(expr4Cost.cost < expr1Cost.cost)
    assert(expr4Cost.cost < expr2Cost.cost)
    assert(expr4Cost.cost < expr3Cost.cost)
    assert(expr4Cost.cost < expr5Cost.cost)

    val (root, egraph) = L.toEGraph(expr1)
    val egraph2 = simpleStrategy(egraph).get

    val extracted = egraph2.extract(root, costFunction)
    assert(extracted == expr4, s"Expected $expr4 but got $extracted")
  }

  def nmm(n: Int): LinalgExpr = {
    if (n == 0) {
      Mat(10, 10)
    } else {
      Mul(nmm(n - 1), Mat(10, 10))
    }
  }

  def testNmmBench(n: Int): Unit = {
    type LinalgRule = R.LinalgRule
    val L: Language[LinalgExpr] = summon[Language[LinalgExpr]]
    val R: Rules = Rules()(using L)

    val simpleStrategy: Strategy[LinalgIR, EGraph[LinalgIR], Unit] = MaximalRuleApplication(R.all).repeatUntilStable

    val costFunction: LanguageCostFunction[LinalgExpr, DimAndCost] = new LanguageCostFunction[LinalgExpr, DimAndCost]() {
      override def apply(expr: LinalgExpr): DimAndCost = {
        expr match {
          case Mat(rows, cols) => DimAndCost(rows, cols, 0)
          case Fact(dimAndCost: DimAndCost) => dimAndCost
          case Mul(lhs, rhs) =>
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

    val expr = nmm(n)

    val (root, egraph) = L.toEGraph(expr)
    val egraph2 = simpleStrategy(egraph).get

    val extracted = egraph2.extract(root, costFunction)
  }

  def benchNmm(n: Int): Unit = {
    val time = 60_000_000_000L
    val start = System.nanoTime()
    var times: List[Long] = List()
    var iterations = 0
    while (System.nanoTime() - start < time) {
      val testStart = System.nanoTime()
      testNmmBench(n)
      val testEnd = System.nanoTime()
      val duration = testEnd - testStart
      //      println(s"Iteration $iterations took $duration ms")
      times = duration :: times
      iterations += 1
    }
    println(s"Completed $iterations iterations in 60 seconds")
    val avgTime = if (times.nonEmpty) times.sum / times.length else 0
    println(s"Average time per iteration: ${avgTime/1e6} ms")
  }

//  @Test
//  def benchMM(): Unit = {
//    val n = List(3, 5, 10, 20, 40, 80)
//    for (i <- n) {
//      println(s"Benchmarking n=$i")
//      benchNmm(i)
//    }
//  }
}
