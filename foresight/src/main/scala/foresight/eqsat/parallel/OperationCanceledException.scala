package foresight.eqsat.parallel

/**
 * Exception signaling that an operation was canceled via a [[CancellationToken]].
 *
 * This is the mechanism used by cooperative cancellation in Foresight. Code that supports
 * cancellation (such as [[ParallelMap.cancelable]]) will periodically check a token's
 * [[CancellationToken.isCanceled]] flag and, if set, abort by throwing this exception.
 *
 * Usage:
 *   - Constructed and thrown internally by APIs that accept a [[CancellationToken]].
 *   - You can also throw it manually if you check the token yourself inside long-running logic.
 *   - Typically caught at a high level to stop the operation cleanly.
 *
 * Example:
 * {{{
 * val token = new CancellationToken
 * token.cancelAfter(5.seconds)
 *
 * try {
 *   val pm = ParallelMap.parallel.cancelable(token)
 *   pm(myLargeDataset, processItem)
 * } catch {
 *   case OperationCanceledException(t) if t == token =>
 *     println("Work was canceled due to timeout.")
 * }
 * }}}
 *
 * @param token The [[CancellationToken]] that triggered the cancellation.
 */
final case class OperationCanceledException(token: CancellationToken)
  extends RuntimeException {

  override def getMessage: String =
    "Operation was canceled by cancellation token."
}
