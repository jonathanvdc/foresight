package foresight.eqsat.parallel

/**
 * An exception that is thrown when an operation is cancelled.
 * @param token The cancellation token that was used to cancel the operation.
 */
final case class OperationCanceledException(token: CancellationToken) extends RuntimeException {
  override def getMessage: String = "Operation was canceled by cancellation token."
}
