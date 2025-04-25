package foresight.eqsat.parallel

import scala.concurrent.duration.Duration

/**
 * A report of the time taken for a hierarchy of operations.
 *
 * @param name The name of the operation.
 * @param nanos The time taken for this operation in nanoseconds.
 * @param children The timing reports of child operations.
 */
final case class TimingReport(name: String, nanos: Long, children: Seq[TimingReport]) {
  /**
   * The total time taken for this operation and all child operations in nanoseconds.
   * @return The total time taken in nanoseconds.
   */
  lazy val totalNanos: Long = nanos + children.map(_.totalNanos).sum

  /**
   * The time taken for this operation, excluding child operations, as a Duration.
   * @return The time taken as a Duration.
   */
  def duration: Duration = Duration(nanos, "nanoseconds")

  /**
   * The total time taken for this operation and all child operations as a Duration.
   * @return The total time taken as a Duration.
   */
  def totalDuration: Duration = Duration(totalNanos, "nanoseconds")

  private def toLines: Seq[String] = {
    val childLines = children.flatMap(_.toLines).map("  " + _)
    val ownLine = s"$name: $duration (total: $totalDuration)"
    Seq(ownLine) ++ childLines
  }

  /**
   * Converts the timing report to a string representation.
   * @return The string representation of the timing report.
   */
  override def toString: String = {
    toLines.mkString("\n")
  }
}

/**
 * A companion object for the [[TimingReport]] class.
 */
object TimingReport {
  /**
   * Merges a sequence of timing reports into a single report.
   * @param name The name of the merged report.
   * @param reports The sequence of timing reports to merge.
   * @return The merged timing report.
   */
  def merge(name: String, reports: Seq[TimingReport]): TimingReport = {
    val ownNanos = reports.map(_.nanos).sum
    val childReports = reports.flatMap(_.children).groupBy(_.name).map {
      case (name, reports) => merge(name, reports)
    }.toSeq.sortBy(_.name)
    TimingReport(name, ownNanos, childReports)
  }

  /**
   * Simplifies a timing report by merging any of its children that have the same name.
   * @param report The timing report to simplify.
   * @return The simplified timing report.
   */
  def simplify(report: TimingReport): TimingReport = merge(report.name, Seq(report))
}
