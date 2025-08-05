package foresight.eqsat.parallel

import java.util.concurrent.locks.Lock
import scala.collection.mutable.ArrayBuffer

/**
 * A parallel map that measures the time taken to process elements.
 * @param name The name of the parallel mapping strategy.
 * @param inner The inner parallel mapping strategy to use.
 */
final class TimedParallelMap(val name: String, inner: ParallelMap) extends ParallelMap {
  private val namedChildren: ArrayBuffer[TimedParallelMap] = ArrayBuffer()
  private val lock: Lock = new java.util.concurrent.locks.ReentrantLock()
  private var startTime: Option[Long] = None
  private var endTime: Option[Long] = None

  /**
   * The name of the parallel mapping strategy.
   * @return The name of the parallel mapping strategy.
   */
  def children: Seq[TimedParallelMap] = locked(namedChildren.view.force.toSeq)

  /**
   * The wall clock time taken to process elements in this parallel map, including the time taken by child parallel maps.
   * @return The wall clock time in nanoseconds.
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
   * A timing report of the operations performed by this parallel map and its children.
   * @return The timing report.
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
      startTime = startTime.orElse(Some(start))
      endTime = Some(end)
    }
    result
  }

  override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = timed {
    inner.apply(inputs, f)
  }

  override def run[A](f: => A): A = timed {
    inner.run(f)
  }
}
