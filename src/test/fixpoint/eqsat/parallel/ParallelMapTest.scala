package fixpoint.eqsat.parallel

import org.junit.Test

/**
 * Tests for parallel maps and their cancellation.
 */
class ParallelMapTest {
  private val implementations = Seq(ParallelMap.sequential, ParallelMap.parallel)

  /**
   * Tests that the parallel map preserves the order of the elements.
   */
  @Test
  def preservesOrder(): Unit = {
    for (impl <- implementations) {
      val inputs = 0 until 100
      val outputs = impl.apply[Int, Int](inputs, identity)
      assert(outputs == inputs)
    }
  }

  /**
   * Tests that parallel maps can be cancelled ahead of time.
   */
  @Test
  def cancelAheadOfTime(): Unit = {
    for (impl <- implementations) {
      val token = new CancellationToken
      token.cancel()
      try {
        impl.cancelable(token).apply[Int, Int](0 until 100, identity)
        assert(false)
      } catch {
        case OperationCanceledException =>
      }
    }
  }

  /**
   * Tests that parallel maps can be cancelled during an operation.
   */
  @Test
  def cancelDuringOperation(): Unit = {
    for (impl <- implementations) {
      val token = new CancellationToken
      try {
        impl.cancelable(token).apply[Int, Int](0 until 1000, i => {
          if (i == 0) {
            token.cancel()
          }
          Thread.sleep(10)
          i
        })
        assert(false)
      } catch {
        case OperationCanceledException =>
      }
    }
  }
}
