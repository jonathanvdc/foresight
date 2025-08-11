package foresight.eqsat.parallel

import scala.concurrent.duration.Duration

/**
 * Hierarchical, wall-clock timing summary for a tree of operations.
 *
 * Each node reports:
 *  - [[name]]: logical label (e.g., a phase or rule group)
 *  - [[nanos]]: elapsed wall-clock time for this node **including all descendants**
 *  - [[children]]: sub-reports for nested phases
 *
 * Percentages shown by [[toString]] are computed relative to the *root* node passed to the
 * pretty-printer of that tree. If you call `root.toString`, every line’s percentage is relative
 * to `root.nanos`. If you print a subtree, percentages will be relative to that subtree’s `nanos`.
 *
 * Typical sources of `TimingReport` are [[TimedParallelMap.report]] trees.
 *
 * {{{
 * val report: TimingReport = timedPM.report
 * println(report)    // Multiline hierarchy with durations and percentages
 * }}}
 *
 * @param name     Logical label for this timing node.
 * @param nanos    Wall-clock time in nanoseconds for this node **and all of its descendants**.
 * @param children Child timing reports (each child is a subtree).
 */
final case class TimingReport(name: String, nanos: Long, children: Seq[TimingReport]) {

  /**
   * Elapsed wall-clock time as a Scala [[scala.concurrent.duration.Duration]].
   */
  def duration: Duration = Duration.fromNanos(nanos)

  // Builds an indented, multiline representation. Percentages are relative to `total`,
  // which should be the root node’s nanos for consistent percentages across the whole tree.
  private def toLines(total: Long): Seq[String] = {
    val childLines = children.flatMap(_.toLines(total)).map("  " + _)
    val percentage = "%.2f".format(100 * nanos.toDouble / total.toDouble)
    val ownLine = s"$name: $duration ($percentage%)"
    Seq(ownLine) ++ childLines
  }

  /**
   * Pretty, indented, multiline report.
   *
   * Lines are ordered as in [[children]] (depth-first). Each line shows:
   * `name: <duration> (<percentage%>)`, where the percentage is relative to this node’s
   * total if printed standalone, or to the root if called on the root.
   */
  override def toString: String = toLines(nanos).mkString("\n")
}

/**
 * Utilities for constructing and normalizing [[TimingReport]] trees.
 */
object TimingReport {

  /**
   * Merges multiple reports with the same conceptual root into a single tree.
   *
   * Semantics:
   *   - Root nanos becomes the sum of all root nanos.
   *   - Children are grouped by [[TimingReport.name]]; groups are merged recursively.
   *   - Sibling groups are sorted by name for stable output.
   *
   * Use this to coalesce timing from repeated runs or distributed shards into a single report.
   *
   * @param name    Name for the merged root.
   * @param reports Non-empty sequence of root reports to merge.
   * @return A single, merged [[TimingReport]] tree.
   * @throws IllegalArgumentException if `reports` is empty.
   */
  def merge(name: String, reports: Seq[TimingReport]): TimingReport = {
    require(reports.nonEmpty, "Cannot merge empty reports")

    val totalNanos = reports.map(_.nanos).sum
    val childReports = reports
      .flatMap(_.children)
      .groupBy(_.name)
      .map { case (n, rs) => merge(n, rs) }
      .toSeq
      .sortBy(_.name)

    TimingReport(name, totalNanos, childReports)
  }

  /**
   * Normalizes a single report by merging child subtrees that share the same name.
   *
   * Equivalent to calling [[merge]] on a singleton sequence of `report`.
   * Handy for collapsing repeated, same-named phases under a node.
   */
  def simplify(report: TimingReport): TimingReport =
    merge(report.name, Seq(report))
}
