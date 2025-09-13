package foresight.eqsat.parallel

import java.util.concurrent.locks.Lock
import scala.collection.mutable.ArrayBuffer

/**
 * A [[ParallelMap]] decorator that records wall-clock timing for mapping and ad-hoc tasks.
 *
 * `TimedParallelMap` captures elapsed time for both this node and its **entire subtree** of child
 * strategies created via [[child]]. Timing is computed as:
 *
 *  - **Start**: the earliest `System.nanoTime()` observed among this node and all of its children
 *    during any call to [[apply]] or [[run]].
 *  - **End**:   the latest `System.nanoTime()` observed among this node and all of its children.
 *  - [[nanos]] returns `End - Start`. Overlapping child work is **not double-counted**; this is
 *    wall-clock duration, not a sum of per-task CPU times.
 *
 * ## Typical usage
 * {{{
 * val pm: ParallelMap = ParallelMap.parallel
 * val timed = new TimedParallelMap("root", pm)
 *
 * val child = timed.child("phase-1")
 * val out   = child(Seq(1,2,3), _ * 2)   // work to be timed under "phase-1"
 *
 * val report: TimingReport = timed.report
 * println(report.pretty)                 // or however you surface it
 * }}}
 *
 * ## Thread safety
 * All timing state and the child list are protected by an internal [[java.util.concurrent.locks.ReentrantLock]].
 * You may call [[apply]], [[run]], [[child]], [[nanos]], and [[report]] concurrently from multiple threads.
 *
 * ## Overhead
 * Timing introduces one `System.nanoTime()` pair per [[apply]]/[[run]] call and a few lock acquisitions.
 * For extremely fine-grained tasks, prefer timing fewer, larger phases.
 *
 * @param name  Logical name for this timing node (used in [[report]]).
 * @param inner The underlying strategy to which work is delegated.
 */
final class TimedParallelMap(val name: String, inner: ParallelMap) extends ParallelMap {
  private val namedChildren: ArrayBuffer[TimedParallelMap] = ArrayBuffer()
  private val lock: Lock = new java.util.concurrent.locks.ReentrantLock()
  private var startTime: Option[Long] = None
  private var endTime: Option[Long] = None

  /**
   * The immediate child timing nodes created from this strategy via [[child]].
   *
   * Returned as an immutable snapshot.
   */
  def children: Seq[TimedParallelMap] = locked(namedChildren.toVector)

  /**
   * Wall-clock elapsed time (in nanoseconds) covered by this node **and all of its descendants**.
   *
   * Defined as `max(endTimes) - min(startTimes)` across the subtree. If no work has been recorded
   * yet (no [[apply]] or [[run]] was called on this node or its children), returns `0`.
   */
  def nanos: Long = locked {
    (maxEndTime, minStartTime) match {
      case (Some(end), Some(start)) => end - start
      case _ => 0
    }
  }

  private def minStartTime: Option[Long] = {
    (startTime ++ namedChildren.flatMap(_.minStartTime)).toList match {
      case Nil => None
      case times => Some(times.min)
    }
  }

  private def maxEndTime: Option[Long] = {
    (endTime ++ namedChildren.flatMap(_.maxEndTime)).toList match {
      case Nil => None
      case times => Some(times.max)
    }
  }

  /**
   * Builds a hierarchical [[TimingReport]] for this node and its children.
   *
   * The report captures each node's name and elapsed wall-clock time (via [[nanos]]) and then
   * runs a simplification pass (see [[TimingReport.simplify]]) to collapse trivial structure.
   */
  def report: TimingReport = locked {
    val childReports = namedChildren.map(_.report)
    TimingReport.simplify(TimingReport(name, nanos, childReports.toSeq))
  }

  private def locked[A](f: => A): A = {
    lock.lock()
    try {
      f
    } finally {
      lock.unlock()
    }
  }

  /**
   * Creates a named child timing node that delegates work to the same underlying strategy.
   *
   * The child contributes to this node's overall timing window (see [[nanos]]).
   */
  override def child(name: String): ParallelMap = {
    val c = new TimedParallelMap(name, inner)
    locked {
      namedChildren.append(c)
    }
    c
  }

  private def timed[A](f: => A): A = {
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    locked {
      startTime = startTime.orElse(Some(start)) // preserve earliest start
      endTime = Some(end) // track latest end
    }
    result
  }

  /**
   * Delegates to the inner strategy while recording a timing window for this node.
   */
  override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = timed {
    inner.apply(inputs, f)
  }

  /**
   * Delegates to the inner strategy while recording a timing window for this node.
   */
  override def run[A](f: => A): A = timed {
    inner.run(f)
  }
}
