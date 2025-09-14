package foresight.eqsat.parallel

/**
 * An implementation of the [[ParallelMap]] trait that provides parallel processing capabilities. This implementation
 * is designed for Scala 2.11.
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
      private val taskSupport = new scala.collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(n))

      override def child(name: String): ParallelMap = this

      override def run[A](f: => A): A = f

      override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
        val parInputs = inputs.par
        parInputs.tasksupport = taskSupport
        parInputs.map(f).seq
      }
    }
  }
}
