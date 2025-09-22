package foresight.eqsat.parallel

/**
 * Core parallelism abstraction in Foresight.
 *
 * `ParallelMap` defines a uniform interface for applying functions to collections,
 * supporting both sequential and parallel execution. It is the foundation for scalable
 * data processing and concurrent algorithms throughout the library.
 *
 * ## Key Features
 *   - **Flexible execution**: Implementations may run tasks sequentially, in parallel,
 *     or with custom scheduling and resource usage.
 *   - **Hierarchical strategies**: Create named child strategies via [[child]],
 *     useful for subdividing work in complex algorithms.
 *   - **Cancellation support**: Wrap an existing strategy with [[cancelable]] to stop
 *     computation early when a [[CancellationToken]] is triggered.
 *   - **Performance measurement**: Use [[timed]] to obtain a [[TimedParallelMap]]
 *     for measuring execution times.
 *   - **Arbitrary task execution**: Run standalone computations with [[run]].
 *
 * ## Usage
 * {{{
 * val pm: ParallelMap = ParallelMap.parallel
 *
 * // Parallel mapping over a collection
 * val results = pm(Seq(1, 2, 3), (x: Int) => x * x)
 *
 * // With cancellation
 * val token = new CancellationToken
 * val cancelablePM = pm.cancelable(token)
 *
 * // Timed execution
 * val timedPM = pm.timed
 * }}}
 *
 * ## Implementation Notes
 * Implementations may differ in:
 *   - Degree of parallelism and thread scheduling
 *   - Overhead for small collections
 *   - Cancellation behavior (when checks are performed)
 *
 * @see [[ParallelMap.sequential]], [[ParallelMap.parallel]], [[ParallelMap.fixedThreadParallel]]
 */
trait ParallelMap {
  /**
   * Creates a named child parallel mapping strategy.
   *
   * Child strategies can inherit resource pools or scheduling from their parent,
   * but can also have independent timing, cancellation, or logging.
   *
   * @param name Identifier for the child strategy (for debugging or metrics).
   * @return A new [[ParallelMap]] scoped as a child of this strategy.
   */
  def child(name: String): ParallelMap

  /**
   * Applies a function to each element of a collection.
   *
   * Execution may be sequential or parallel depending on the implementation.
   *
   * @param inputs Input collection to process.
   * @param f Function to apply to each element.
   * @tparam A Element type of `inputs`.
   * @tparam B Result type of `f`.
   * @return Collection of results, in the same order as the input.
   */
  def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B]

  /**
   * Returns a view of this strategy that cooperatively cancels work using the given token.
   *
   * Semantics:
   *   - If the token is canceled before work begins, no work is performed and an
   *     [[OperationCanceledException]] is thrown immediately.
   *   - While mapping, cancellation is observed at wrapper checkpoints (see below). When detected,
   *     an [[OperationCanceledException]] is thrown and remaining work is skipped.
   *
   * Checkpoints added by this wrapper:
   *   - One check before the mapping starts.
   *   - One check before processing each input element.
   *
   * Important:
   *   - The wrapper does not interrupt user code while running a single element function `f`.
   *     If `f` can run for a long time, pass the token into `f` and poll it inside `f`
   *     to achieve finer-grained cancellation.
   *
   * Example:
   * {{{
   * val token = new CancellationToken
   * token.cancelAfter(30.seconds)                 // common "set and forget" timeout
   *
   * val pm = ParallelMap.parallel.cancelable(token)
   * val out = pm(items, { a =>
   *   // Optional finer-grained check inside user code
   *   if (token.isCanceled) throw OperationCanceledException(token)
   *   compute(a)
   * })
   * }}}
   *
   * Composition:
   *   - Children created via [[child]] inherit cancellation by wrapping the parent's child.
   *   - Can be combined with [[timed]]; timing still reflects the work up to cancellation.
   *
   * @param token Cancellation trigger to observe.
   * @throws OperationCanceledException if the token is canceled before start or at any checkpoint.
   * @return A new [[ParallelMap]] that observes the token at the described checkpoints.
   */

  final def cancelable(token: CancellationToken): ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap =
      ParallelMap.this.child(name).cancelable(token)

    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] = {
      if (token.isCanceled) throw OperationCanceledException(token)
      ParallelMap.this.apply(inputs, (a: A) => {
        if (token.isCanceled) throw OperationCanceledException(token)
        f(a)
      })
    }

    override def run[A](f: => A): A = {
      if (token.isCanceled) throw OperationCanceledException(token)
      ParallelMap.this.run(f)
    }
  }

  /**
   * Executes an arbitrary computation within this parallel mapping strategy.
   *
   * Internally implemented using [[apply]] over a single dummy element.
   *
   * @param f The computation to execute.
   * @tparam A The result type.
   * @return The result of the computation.
   */
  def run[A](f: => A): A =
    apply[Int, A](Seq(0), _ => f).head

  /**
   * Collects elements produced by a callback function into a sequence.
   * This method is thread-safe wrt parallelism created by this parallel map.
   * @param body The callback function that produces elements.
   * @tparam A The type of elements produced by the callback function.
   * @return A sequence of collected elements.
   */
  def collectFrom[A](body: (A => Unit) => Unit): Seq[A] = {
    run { ParallelMapImpl.collectFrom(body) }
  }

  /**
   * Wraps this strategy to record execution times.
   *
   * Useful for profiling or monitoring performance. Timing starts at the root
   * and can be nested for child strategies.
   *
   * @return A [[TimedParallelMap]] that records elapsed times.
   */
  final def timed: TimedParallelMap =
    new TimedParallelMap("root", this)
}

/**
 * Factory and predefined [[ParallelMap]] strategies.
 */
object ParallelMap {

  /**
   * Sequential execution strategy.
   *
   * Processes elements in input order, on the calling thread,
   * without any concurrency overhead.
   *
   * Best for:
   *   - Small collections
   *   - Deterministic debugging
   *   - Environments where parallelism is unavailable or undesired
   */
  val sequential: ParallelMap = new ParallelMap {
    override def child(name: String): ParallelMap = this
    override def apply[A, B](inputs: Iterable[A], f: A => B): Iterable[B] =
      inputs.map(f)
  }

  /**
   * Default parallel execution strategy.
   *
   * Uses the library's standard parallelism settings (e.g., thread pool).
   * Suitable for most parallel workloads.
   */
  val parallel: ParallelMap =
    ParallelMapImpl.parallel

  /**
   * Parallel execution with a fixed number of threads.
   *
   * @param n Number of threads to use.
   * @return A [[ParallelMap]] backed by a fixed-size thread pool.
   *         If `n == 1`, returns [[sequential]].
   */
  def fixedThreadParallel(n: Int): ParallelMap =
    if (n == 1) sequential else ParallelMapImpl.fixedThreadParallel(n)

  /**
   * The library-wide default mapping strategy.
   *
   * Currently equal to [[parallel]].
   */
  val default: ParallelMap =
    parallel
}
