package fixpoint.eqsat.parallel

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
}
