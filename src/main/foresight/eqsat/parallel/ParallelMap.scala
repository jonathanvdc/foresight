package foresight.eqsat.parallel

/**
 * A trait that provides a method to apply a function to each element of an iterable.
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
  @throws[OperationCanceledException.type]
  final def cancelable(token: CancellationToken): ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = ParallelMap.this.child(name).cancelable(token)

    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
      if (token.isCanceled) {
        throw OperationCanceledException
      }
      ParallelMap.this.apply(inputs, (a: A) => {
        if (token.isCanceled) {
          throw OperationCanceledException
        }
        f(a)
      })
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
   * A parallel map that processes elements sequentially using the Scala collections library.
   */
  val sequential: ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = this
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.map(f)
  }

  /**
   * A parallel map that processes elements in parallel using the Scala parallel collections library.
   */
  val parallel: ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = this
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.par.map(f).seq
  }

  /**
   * The default parallel mapping strategy.
   */
  val default: ParallelMap = parallel
}
