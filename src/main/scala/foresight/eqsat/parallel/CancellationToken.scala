package foresight.eqsat.parallel

import java.util.{Timer, TimerTask}
import scala.concurrent.duration.Duration

/**
 * A token that can be used to cancel an operation.
 */
final class CancellationToken {
  private var canceled = false

  /**
   * Cancels the operation.
   */
  def cancel(): Unit = {
    canceled = true
  }

  /**
   * Returns `true` if the operation has been cancelled, `false` otherwise.
   */
  def isCanceled: Boolean = canceled

  /**
   * Cancels the operation after a timeout. This operation is non-blocking and returns a new `CancellationToken` that
   * can be used to cancel the cancellation.
   * @param timeout The timeout after which the operation will be cancelled.
   * @return A new `CancellationToken` that can be used to cancel the cancellation.
   */
  def cancelAfter(timeout: Duration): CancellationToken = {
    val token = new CancellationToken
    if (isCanceled) {
      token
    } else {
      CancellationToken.cancellationTimer.schedule(new TimerTask {
        override def run(): Unit = {
          if (!token.isCanceled) {
            CancellationToken.this.cancel()
          }
        }
      }, timeout.toMillis)
      token
    }
  }
}

/**
 * A companion object for [[CancellationToken]].
 */
private object CancellationToken {
  /**
   * A timer that is used to schedule cancellation tasks.
   */
  private lazy val cancellationTimer = new Timer(true)
}