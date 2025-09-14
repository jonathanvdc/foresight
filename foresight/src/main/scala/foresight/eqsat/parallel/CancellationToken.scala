package foresight.eqsat.parallel

import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.Duration

/**
 * Cooperative cancellation signal for long-running or batched operations.
 *
 * A `CancellationToken` is a simple, thread-safe flag that can be polled by running code
 * to decide whether to abort early. It does not forcibly interrupt threads. Instead,
 * libraries (for example, [[ParallelMap.cancelable]]) periodically check the token and abort
 * work cooperatively (often by throwing an [[OperationCanceledException]]).
 *
 * Common usage:
 *   - Create a token for a job.
 *   - Optionally arm a timeout with [[cancelAfter]].
 *   - Pass the token into your work and periodically check it (or rely on library checkpoints).
 *   - Do not disarm; if the timeout elapses, the job cancels.
 *
 * Example:
 * {{{
 * val token = new CancellationToken
 * token.cancelAfter(30.seconds)                // let the job auto-cancel after 30s
 *
 * val pm = ParallelMap.parallel.cancelable(token)
 * val out = pm(Seq(1, 2, 3), { x =>
 *   expensiveComputation(x)
 * })
 * }}}
 *
 * Advanced:
 *   - If you need to suppress or extend a timeout while the task is running,
 *     keep the control token returned by [[cancelAfter]] and cancel it to disarm the scheduled cancel,
 *     or cancel and re-arm to extend the deadline. Most code does not need this.
 *   - One example is a "heartbeat": extend the timeout periodically while progress is observed.
 *
 * Example: heartbeat to keep the job alive while progress is made
 * {{{
 * def process(items: Seq[Item], token: CancellationToken): Unit = {
 *   var ctl = token.cancelAfter(10.seconds)      // initial deadline
 *   for ((item, i) <- items.zipWithIndex) {
 *     step(item, token)                          // check token inside
 *     if (i % 100 == 0) {                        // progress checkpoint
 *       ctl.cancel()                             // disarm old deadline
 *       ctl = token.cancelAfter(10.seconds)      // renew deadline
 *     }
 *   }
 * }
 * }}}
 *
 * Thread safety:
 *   - Uses a Java AtomicBoolean for internal state.
 *   - Changes to the cancellation flag are visible to all threads immediately.
 *   - Both reads and writes are non-blocking and lock-free.
 *
 * @see [[ParallelMap.cancelable]] for integrating cancellation with mapping.
 */
final class CancellationToken {
  private val canceled = new AtomicBoolean(false)

  /**
   * Marks this token as canceled.
   *
   * Any code polling [[isCanceled]] will observe the change immediately on all threads.
   * Safe to call multiple times.
   */
  def cancel(): Unit = canceled.set(true)

  /**
   * Returns whether this token has been canceled.
   *
   * This is a non-blocking, thread-safe read.
   */
  def isCanceled: Boolean = canceled.get()

  /**
   * Arms a timeout that will cancel this token after the given duration.
   *
   * This method is non-blocking. It returns a new [[CancellationToken]] (the control token)
   * that can be used to disarm the scheduled cancellation if needed.
   *
   * Typical pattern is to ignore the returned control token entirely; the timeout will fire and
   * cancel this token if the job is still running.
   *
   * @param timeout Duration to wait before canceling this token.
   * @return A separate token that, when canceled, prevents the scheduled timeout from firing.
   */
  def cancelAfter(timeout: Duration): CancellationToken = {
    val controlToken = new CancellationToken
    if (isCanceled) {
      controlToken
    } else {
      CancellationToken.cancellationTimer.schedule(new TimerTask {
        override def run(): Unit = {
          if (!controlToken.isCanceled) {
            CancellationToken.this.cancel()
          }
        }
      }, timeout.toMillis)
      controlToken
    }
  }
}

/**
 * Internal helpers for [[CancellationToken]].
 */
private object CancellationToken {
  /** Shared daemon timer for scheduling timeouts across tokens. */
  private lazy val cancellationTimer = new Timer(true)
}
