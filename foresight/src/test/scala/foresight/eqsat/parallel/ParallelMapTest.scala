package foresight.eqsat.parallel

import org.junit.Test
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.atomic.AtomicInteger

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

  /**
   * processBlocks: empty inputs do not invoke f.
   */
  @Test
  def processBlocksEmptyInputs(): Unit = {
    for (impl <- implementations) {
      val inputs = ArraySeq.empty[Int]
      val counter = new AtomicInteger(0)
      impl.processBlocks[Int](inputs, 4, _ => counter.incrementAndGet())
      assert(counter.get() == 0)
    }
  }

  /**
   * processBlocks: single block path (blockSize >= length) processes sequentially and preserves order.
   */
  @Test
  def processBlocksSingleBlockSequentialPath(): Unit = {
    for (impl <- implementations) {
      for (n <- 1 to 20) {
        val inputs = ArraySeq.unsafeWrapArray((0 until n).toArray)
        val seen = new ArrayBuffer[Int]()
        impl.processBlocks[Int](inputs, blockSize = n + 1, i => seen.synchronized {
          seen += i
        })
        // Exact order since the single-block path is sequential.
        assert(seen.toSeq == inputs.toSeq)
      }
    }
  }

  /**
   * processBlocks: multiple blocks process every element exactly once (order across blocks unspecified).
   */
  @Test
  def processBlocksMultipleBlocksAllElementsProcessed(): Unit = {
    for (impl <- implementations) {
      val n = 32
      val inputs = ArraySeq.unsafeWrapArray((0 until n).toArray)
      val seen = new ArrayBuffer[Int]()
      impl.processBlocks[Int](inputs, blockSize = 4, i => seen.synchronized { seen += i })
      // Same multiset of elements
      assert(seen.sorted == inputs.toSeq)
      // No duplicates / omissions
      assert(seen.distinct.size == n)
    }
  }

  /**
   * processBlocks: within each block, element order is preserved even if blocks interleave.
   */
  @Test
  def processBlocksPerBlockOrderPreserved(): Unit = {
    for (impl <- implementations) {
      val n = 25
      val blockSize = 6 // creates 5 blocks: [0..5], [6..11], [12..17], [18..23], [24]
      val inputs = ArraySeq.unsafeWrapArray((0 until n).toArray)
      val seen = new ArrayBuffer[Int]()
      impl.processBlocks[Int](inputs, blockSize, i => seen.synchronized { seen += i })

      // Check: for every block, the subsequence of seen that belongs to the block is increasing.
      val numBlocks = (n + blockSize - 1) / blockSize
      for (b <- 0 until numBlocks) {
        val start = b * blockSize
        val end = math.min(start + blockSize, n)
        val subseq = seen.filter(i => i >= start && i < end)
        assert(subseq == subseq.sorted, s"Elements within block $b not in order: $subseq")
      }
    }
  }

  /**
   * processBlocks: blockSize == 1 is valid and processes all elements.
   */
  @Test
  def processBlocksBlockSizeOne(): Unit = {
    for (impl <- implementations) {
      val n = 17
      val inputs = ArraySeq.unsafeWrapArray((0 until n).toArray)
      val counter = new AtomicInteger(0)
      impl.processBlocks[Int](inputs, blockSize = 1, _ => counter.incrementAndGet())
      assert(counter.get() == n)
    }
  }

  /**
   * processBlocks: cancellation stops work in the parallel path.
   * Note: cancellation may only be observed between blocks.
   */
  @Test
  def processBlocksCancellationDuringProcessing(): Unit = {
    for (impl0 <- implementations) {
      val token = new CancellationToken
      val impl = impl0.cancelable(token)
      val n = 100
      val blockSize = 5
      val inputs = ArraySeq.unsafeWrapArray((0 until n).toArray)

      try {
        impl.processBlocks[Int](inputs, blockSize, i => {
          if (i == 0) token.cancel() // cancel early
          Thread.sleep(2) // give scheduler a chance to observe the token
          ()
        })
        assert(false, "Expected OperationCanceledException was not thrown")
      } catch {
        case OperationCanceledException(_) => // expected
      }
    }
  }

  /**
   * processBlocks: blockSize must be positive; a zero blockSize raises an exception.
   */
  @Test
  def processBlocksBlockSizeZeroThrows(): Unit = {
    for (impl <- implementations) {
      val inputs = ArraySeq.unsafeWrapArray((0 until 10).toArray)
      var threw = false
      try {
        impl.processBlocks[Int](inputs, 0, _ => ())
      } catch {
        case _: IllegalArgumentException => threw = true
      }
      assert(threw, "Expected an IllegalArgumentException when blockSize == 0")
    }
  }

  /**
   * processBlocks: negative blockSize raises an exception.
   */
  @Test
  def processBlocksNegativeBlockSizeThrows(): Unit = {
    for (impl <- implementations) {
      val inputs = ArraySeq.unsafeWrapArray((0 until 10).toArray)
      var threw = false
      try {
        impl.processBlocks[Int](inputs, -1, _ => ())
      } catch {
        case _: IllegalArgumentException => threw = true
      }
      assert(threw, "Expected an IllegalArgumentException when blockSize < 0")
    }
  }
}
