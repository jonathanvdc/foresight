package foresight.eqsat.commands

private[eqsat] final class IntRef(var elem: Int)

object IntRef {
  /**
   * Pool of reusable [[IntRef]] instances.
   *
   * Usage:
   *   val r = IntRef.acquire()    // from the default thread-local pool
   *   r.elem = 42
   *   IntRef.release(r)           // return to pool
   */
  final class Pool {
    // LIFO stack to maximize cache locality
    private val free = new java.util.ArrayDeque[IntRef]()

    /** Acquire an IntRef, initializing its value. */
    @inline def acquire(initial: Int): IntRef = {
      val ref = free.pollFirst()
      if (ref eq null) new IntRef(initial)
      else { ref.elem = initial; ref }
    }

    /** Return an IntRef to the pool for reuse. */
    @inline def release(ref: IntRef): Unit = {
      // no double-free tracking for performance; callers ensure discipline
      free.addFirst(ref)
    }

    /** Number of currently stored reusable objects. */
    @inline def size: Int = free.size()

    /** Drop all cached objects. */
    def clear(): Unit = free.clear()
  }

  // Default thread-local pool.
  private val threadLocal: ThreadLocal[Pool] = new ThreadLocal[Pool] {
    override def initialValue(): Pool = new Pool
  }

  /** Access the default thread-local pool for the current thread. */
  @inline def defaultPool: Pool = threadLocal.get()
}