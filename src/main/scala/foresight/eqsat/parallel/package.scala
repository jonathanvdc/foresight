package foresight.eqsat

/**
 * Parallelism utilities for Foresight.
 *
 * This package provides a minimal, composable abstraction for executing collection transforms
 * and ad-hoc tasks with optional cancellation and timing. The design favors clear checkpoints
 * over implicit thread interruption, so that callers control how cooperative cancellation
 * is observed inside their own code.
 *
 * Core types:
 *   - [[ParallelMap]]: strategy interface for applying a function over a collection,
 *     with sequential and parallel implementations, child scoping, and wrappers for cancellation and timing.
 *   - [[TimedParallelMap]]: decorator that records wall-clock timing for a strategy and its child hierarchy,
 *     producing a [[TimingReport]].
 *   - [[CancellationToken]]: thread-safe, cooperative cancellation flag with optional timeouts via `cancelAfter`.
 *   - [[OperationCanceledException]]: exception thrown when cancellation is observed at a checkpoint.
 *   - [[TimingReport]]: immutable, hierarchical timing summary with merge and simplify helpers.
 *
 * Choosing a strategy:
 *   - Use [[ParallelMap.sequential]] for tiny inputs, deterministic debugging, or environments without threads.
 *   - Use [[ParallelMap.parallel]] for general-purpose parallelism with library defaults.
 *   - Use [[ParallelMap.fixedThreadParallel]] when you need an explicit thread cap.
 *
 * Cancellation model:
 *   - Wrap a strategy with [[ParallelMap.cancelable]] and pass a [[CancellationToken]].
 *   - The wrapper performs checks before starting and before each element.
 *   - Long-running element functions should also poll the token inside `f` to observe cancellation promptly.
 *   - Typical usage is to arm a timeout with `token.cancelAfter(d)` and not disarm it.
 *
 * Timing and reporting:
 *   - Call [[ParallelMap.timed]] or wrap an existing strategy in [[TimedParallelMap]] to collect timings.
 *   - Elapsed time is wall-clock across the subtree, not a sum of per-task CPU times.
 *   - Use [[TimingReport.simplify]] to collapse repeated, same-named child nodes; use [[TimingReport.merge]]
 *     to combine reports from multiple runs or shards.
 *
 * Threading considerations:
 *   - [[CancellationToken]] uses an AtomicBoolean for safe cross-thread visibility without locks.
 *   - [[TimedParallelMap]] protects its internal state with a lock and is safe to use concurrently.
 *   - Implementations of [[ParallelMap]] may differ in scheduling and resource usage; choose according to workload.
 *
 * Examples:
 *
 * Simple parallel map with timeout
 * {{{
 * import scala.concurrent.duration._
 *
 * val token = new CancellationToken
 * token.cancelAfter(30.seconds)                           // set-and-forget cancellation
 *
 * val pm = ParallelMap.parallel.cancelable(token)
 * val out = pm(Seq(1, 2, 3), x => x * x)
 * }}}
 *
 * Finer-grained cancellation inside a long-running function
 * {{{
 * def compute(x: Int, token: CancellationToken): Int = {
 *   if (token.isCanceled) throw OperationCanceledException(token)
 *   // ... long step ...
 *   if (token.isCanceled) throw OperationCanceledException(token)
 *   // ... next step ...
 *   x * x
 * }
 *
 * val out = ParallelMap.parallel.cancelable(token)(items, x => compute(x, token))
 * }}}
 *
 * Timing a nested workflow
 * {{{
 * val timed = ParallelMap.parallel.timed
 * val phase1 = timed.child("phase-1")
 * val phase2 = timed.child("phase-2")
 *
 * phase1(Seq.fill(100)(()), _ => doWork1())
 * phase2.run(doWork2())
 *
 * println(timed.report)                                   // hierarchical durations with percentages
 * }}}
 */
package object parallel