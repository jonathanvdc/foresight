package foresight.eqsat.parallel

/**
 * A trait that provides a method to apply a function to each element of an iterable.
 */
trait ParallelMap {
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
}

/**
 * A companion object for the `ParallelMap` trait.
 */
object ParallelMap {
  /**
   * A parallel map that processes elements sequentially using the Scala collections library.
   */
  val sequential: ParallelMap = new ParallelMap {
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.map(f)
  }

  /**
   * A parallel map that processes elements in parallel using the Scala parallel collections library.
   */
  val parallel: ParallelMap = new ParallelMap {
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = inputs.par.map(f).seq
  }

  /**
   * The default parallel mapping strategy.
   */
  val default: ParallelMap = parallel
}
