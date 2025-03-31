package foresight.eqsat.parallel

/**
 * An exception that is thrown when an operation is cancelled.
 */
object OperationCanceledException extends RuntimeException {
  override def getMessage: String = "Operation was canceled by cancellation token."
}
