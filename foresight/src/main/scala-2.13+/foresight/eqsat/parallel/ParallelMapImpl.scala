package foresight.eqsat.parallel

import java.util.concurrent.{ConcurrentLinkedQueue, ForkJoinPool}
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._

/**
 * An implementation of the [[ParallelMap]] trait that provides parallel processing capabilities. This implementation
 * uses the Scala parallel collections library for parallel processing and is designed for Scala 2.13 and later.
 */
private[parallel] object ParallelMapImpl {
  /**
   * A parallel map that processes elements in parallel using the Scala parallel collections library.
   */
  val parallel: ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = this
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.par.map(f).seq
    override def run[A](f: => A): A = f
  }

  /**
   * A parallel map that processes elements in parallel using a fixed number of threads.
   * @param n The number of threads to use for parallel processing.
   * @return The parallel map that uses a fixed number of threads.
   */
  def fixedThreadParallel(n: Int): ParallelMap = {
    new ParallelMap {
      private val taskSupport = new scala.collection.parallel.ForkJoinTaskSupport(new ForkJoinPool(n))

      override def child(name: String): ParallelMap = this

      override def run[A](f: => A): A = f

      override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
        val parInputs = inputs.par
        parInputs.tasksupport = taskSupport
        parInputs.map(f).seq
      }
    }
  }

  /**
   * Collects elements produced by a callback function into a sequence.
   * This method is thread-safe and can be used in parallel contexts.
   * @param body The callback function that produces elements.
   * @tparam A The type of elements produced by the callback function.
   * @return A sequence of collected elements.
   */
  def collectFrom[A](body: (A => Unit) => Unit): Seq[A] = {
    val queue = new ConcurrentLinkedQueue[A]()
    body(a => queue.add(a))
    queue.asScala.toSeq
  }
}
