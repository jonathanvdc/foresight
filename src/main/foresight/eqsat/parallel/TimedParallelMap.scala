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
  private var ownNanos: Long = 0

  /**
   * The name of the parallel mapping strategy.
   * @return The name of the parallel mapping strategy.
   */
  def children: Seq[TimedParallelMap] = locked(namedChildren.view.force)

  /**
   * The total time taken to process elements in this parallel map, excluding the time taken by child parallel maps.
   * @return The time taken in nanoseconds.
   */
  def nanos: Long = locked(ownNanos)

  /**
   * The total time taken to process elements in this parallel map, including the time taken by child parallel maps.
   * @return The time taken in nanoseconds.
   */
  def totalNanos: Long = locked {
    ownNanos + namedChildren.map(_.totalNanos).sum
  }

  /**
   * A timing report of the operations performed by this parallel map and its children.
   * @return The timing report.
   */
  def report: TimingReport = locked {
    TimingReport.simplify(TimingReport(name, ownNanos, namedChildren.map(_.report)))
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

  override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
    val startTime = System.nanoTime()
    val result = inner(inputs, f)
    val endTime = System.nanoTime()
    locked {
      ownNanos += endTime - startTime
    }
    result
  }
}
