package foresight.eqsat.parallel

import org.junit.Test

class TimingReportTest {
  /**
   * Tests that leaf [[TimingReport]] instances can be created and merged correctly.
   */
  @Test
  def mergeLeafReports(): Unit = {
    val report1 = TimingReport("task", 100, Seq.empty)
    val report2 = TimingReport("task", 200, Seq.empty)

    val merged = TimingReport.merge("task", Seq(report1, report2))

    assert(merged.name == "task")
    assert(merged.nanos == 300)
    assert(merged.children.isEmpty)

    assert(report1.totalDuration + report2.totalDuration == merged.totalDuration)
  }

  /**
   * Tests that [[TimingReport]] instances with children can be created and merged correctly.
   */
  @Test
  def mergeChildReports(): Unit = {
    val report1 = TimingReport("task", 100, Seq(TimingReport("child", 50, Seq.empty)))
    val report2 = TimingReport("task", 200, Seq(TimingReport("child", 100, Seq.empty)))

    val merged = TimingReport.merge("task", Seq(report1, report2))

    assert(merged.name == "task")
    assert(merged.nanos == 300)
    assert(merged.children.size == 1)
    assert(merged.children.head.name == "child")
    assert(merged.children.head.nanos == 150)

    assert(report1.totalDuration + report2.totalDuration == merged.totalDuration)
  }

  /**
   * Tests that [[TimingReport]] instances with asymmetric children can be merged correctly.
   */
  @Test
  def mergeAsymmetricChildReports(): Unit = {
    val report1 = TimingReport("task", 100, Seq(TimingReport("child1", 50, Seq.empty), TimingReport("child3", 500, Seq.empty)))
    val report2 = TimingReport("task", 200, Seq(TimingReport("child1", 100, Seq.empty), TimingReport("child2", 1000, Seq.empty)))

    val merged = TimingReport.merge("task", Seq(report1, report2))

    assert(merged.name == "task")
    assert(merged.nanos == 300)
    assert(merged.children.size == 3)
    assert(merged.children.exists(_.name == "child1"))
    assert(merged.children.exists(_.name == "child2"))
    assert(merged.children.exists(_.name == "child3"))
    assert(merged.children.find(_.name == "child1").get.nanos == 150)
    assert(merged.children.find(_.name == "child2").get.nanos == 1000)
    assert(merged.children.find(_.name == "child3").get.nanos == 500)

    assert(report1.totalDuration + report2.totalDuration == merged.totalDuration)
  }

  /**
   * Tests that [[TimingReport]] instances can be simplified correctly.
   */
  @Test
  def simplifyReport(): Unit = {
    val report = TimingReport("task", 100, Seq(TimingReport("child", 50, Seq.empty), TimingReport("child", 200, Seq.empty)))

    val simplified = TimingReport.simplify(report)

    assert(simplified.name == "task")
    assert(simplified.nanos == 100)
    assert(simplified.children.size == 1)
    assert(simplified.children.head.name == "child")
    assert(simplified.children.head.nanos == 250)

    assert(report.totalDuration == simplified.totalDuration)
  }
}
