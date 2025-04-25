package foresight.eqsat.parallel

import scala.concurrent.duration.Duration

/**
 * A report of the time taken for a hierarchy of operations.
 *
 * @param name The name of the operation.
 * @param nanos The wall clock time taken for this operation in nanoseconds, including child operations.
 * @param children The timing reports of child operations.
 */
final case class TimingReport(name: String, nanos: Long, children: Seq[TimingReport]) {
  /**
   * The time taken for this operation, including child operations, as a Duration.
   * @return The time taken as a Duration.
   */
  def duration: Duration = Duration.fromNanos(nanos)

  private def toLines(total: Long): Seq[String] = {
    val childLines = children.flatMap(_.toLines(total)).map("  " + _)
    val percentage = "%.2f".format(100 * nanos.toDouble / total.toDouble)
    val ownLine = s"$name: $duration - $duration ($percentage%)"
    Seq(ownLine) ++ childLines
  }

  /**
   * Converts the timing report to a string representation.
   * @return The string representation of the timing report.
   */
  override def toString: String = {
    toLines(nanos).mkString("\n")
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
    require(reports.nonEmpty, "Cannot merge empty reports")

    val totalNanos = reports.map(_.nanos).sum
    val childReports = reports.flatMap(_.children).groupBy(_.name).map {
      case (name, reports) => merge(name, reports)
    }.toSeq.sortBy(_.name)
    TimingReport(name, totalNanos, childReports)
  }

  /**
   * Simplifies a timing report by merging any of its children that have the same name.
   * @param report The timing report to simplify.
   * @return The simplified timing report.
   */
  def simplify(report: TimingReport): TimingReport = merge(report.name, Seq(report))
}
