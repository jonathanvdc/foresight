package foresight.eqsat.parallel

/**
 * The core parallelism abstraction in Foresight, `ParallelMap` is a trait for applying functions to collections,
 * supporting both sequential and parallel execution. It serves as the foundation for parallel computation throughout
 * the library, enabling scalable data processing and concurrent algorithms.
 *
 * `ParallelMap` provides flexible mapping strategies, allowing users to control how computations are distributed and
 * executed. It supports hierarchical parallelism through child strategies, cancellation via tokens, and timing features
 * for performance measurement.
 *
 * Implementations of this trait can vary in their parallelism model, resource usage, and cancellation semantics,
 * making it suitable for custom execution models and advanced parallel processing scenarios.
 */
trait ParallelMap {
  /**
   * Creates a child parallel mapping strategy, tagged with a name.
   * @param name The name of the child parallel mapping strategy.
   * @return The child parallel mapping strategy.
   */
  def child(name: String): ParallelMap

  /**
   * Applies a function to each element of an iterable.
   * @param inputs The iterable to apply the function to.
   * @param f The function to apply to each element of the iterable.
   * @tparam A The type of the elements of the iterable.
   * @tparam B The type of the result of applying the function to the elements of the iterable.
   * @return The result of applying the function to each element of the iterable.
   */
  def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B]

  /**
   * Creates a new parallel mapping strategy that cancels the operation if a cancellation token is canceled.
   * @param token The cancellation token to use.
   * @return The new parallel mapping strategy.
   */
  @throws[OperationCanceledException]
  final def cancelable(token: CancellationToken): ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = ParallelMap.this.child(name).cancelable(token)

    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
      if (token.isCanceled) {
        throw OperationCanceledException(token)
      }
      ParallelMap.this.apply(inputs, (a: A) => {
        if (token.isCanceled) {
          throw OperationCanceledException(token)
        }
        f(a)
      })
    }

    override def run[A](f: => A): A = {
      if (token.isCanceled) {
        throw OperationCanceledException(token)
      }
      ParallelMap.this.run(f)
    }
  }

  /**
   * Runs a function and returns the result.
   * @param f The function to run.
   * @tparam A The type of the result of the function.
   * @return The result of the function.
   */
  def run[A](f: => A): A = apply[Int, A](Seq(0), _ => f).head

  /**
   * Creates a new parallel mapping strategy that measures the time taken to process elements.
   * @return The new parallel mapping strategy.
   */
  final def timed: TimedParallelMap = new TimedParallelMap("root", this)
}

/**
 * A companion object for the `ParallelMap` trait.
 */
object ParallelMap {
  /**
   * A parallel map that processes elements sequentially.
   */
  val sequential: ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = this
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.map(f)
  }

  /**
   * A parallel map that processes elements in parallel.
   */
  val parallel: ParallelMap = ParallelMapImpl.parallel

  /**
   * A parallel map that processes elements in parallel using a fixed number of threads.
   * @param n The number of threads to use for parallel processing.
   * @return The parallel map that uses a fixed number of threads.
   */
  def fixedThreadParallel(n: Int): ParallelMap = {
    if (n == 1) {
      sequential
    } else {
      ParallelMapImpl.fixedThreadParallel(n)
    }
  }

  /**
   * The default parallel mapping strategy.
   */
  val default: ParallelMap = parallel
}
