package foresight.eqsat.parallel

import org.junit.Test

/**
 * Tests for parallel maps and their cancellation.
 */
class ParallelMapTest {
  private val implementations = Seq(ParallelMap.sequential, ParallelMap.parallel, ParallelMap.fixedThreadParallel(2))

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
        case OperationCanceledException(_) =>
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
        impl.cancelable(token).apply[Int, Int](0 until 100, i => {
          if (i == 0) {
            token.cancel()
          }
          Thread.sleep(10)
          i
        })
        assert(false)
      } catch {
        case OperationCanceledException(_) =>
      }
    }
  }

  /**
   * Tests that parallel maps can be cancelled from a separate thread.
   */
  @Test
  def cancelFromSeparateThread(): Unit = {
    for (impl <- implementations) {
      val token = new CancellationToken
      val thread = new Thread(new Runnable {
        override def run(): Unit = {
          Thread.sleep(10)
          token.cancel()
        }
      })
      thread.start()
      try {
        impl.cancelable(token).apply[Int, Int](0 until 100, i => {
          Thread.sleep(20)
          i
        })
        assert(false)
      } catch {
        case OperationCanceledException(_) =>
      }
    }
  }

  /**
   * Tests that a sequential task can be timed.
   */
  @Test
  def timeSequentialTask(): Unit = {
    val impl = ParallelMap.sequential.timed
    val inputs = 0 until 10
    val outputs = impl.apply[Int, Int](inputs, i => {
      Thread.sleep(1)
      i
    })
    assert(outputs == inputs)
    assert(impl.nanos > 0)
    assert(impl.children.isEmpty)
  }

  /**
   * Tests that a parallel task can be timed.
   */
  @Test
  def timeParallelTask(): Unit = {
    val impl = ParallelMap.parallel.timed
    val inputs = 0 until 10
    val outputs = impl.apply[Int, Int](inputs, i => {
      Thread.sleep(1)
      i
    })
    assert(outputs == inputs)
    assert(impl.nanos > 0)
    assert(impl.children.isEmpty)
  }

  /**
   * Tests that a child task can be timed.
   */
  @Test
  def timeChildTask(): Unit = {
    val impl = ParallelMap.parallel.timed
    val inputs = 0 until 10
    val outputs = impl.child("child").apply[Int, Int](inputs, i => {
      Thread.sleep(1)
      i
    })
    assert(outputs == inputs)
    assert(impl.children.size == 1)
    assert(impl.nanos == impl.children.head.nanos)
  }

  /**
   * Tests that child tasks can be timed, even if they are generated in parallel from within the parent task.
   */
  @Test
  def timeParallelChildTasks(): Unit = {
    val impl = ParallelMap.parallel.timed
    val inputs = 0 until 10
    val outputs = impl.apply[Int, Seq[Int]](inputs, i => {
      Thread.sleep(1)
      impl.child(s"child$i").apply[Int, Int](inputs, j => i * j).toSeq
    }).flatten
    assert(outputs == inputs.flatMap(i => inputs.map(j => i * j)))
    assert(impl.nanos > 0)
    assert(impl.children.size == inputs.size)
    assert(impl.children.forall(_.nanos > 0))
  }
}
