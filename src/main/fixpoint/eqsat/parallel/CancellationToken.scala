package fixpoint.eqsat.parallel

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
}
