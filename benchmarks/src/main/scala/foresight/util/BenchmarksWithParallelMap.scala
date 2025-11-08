package foresight.util


import foresight.eqsat.parallel.ParallelMap
import org.openjdk.jmh.annotations.{Param, Scope, State}

/**
  * Shared JMH state that exposes a configurable [[foresight.eqsat.parallel.ParallelMap]] for benchmarks.
  *
  * This class lets you control the level of parallelism used by benchmarks via a JMH parameter.
  * JMH will instantiate this state once per benchmark (see [[org.openjdk.jmh.annotations.Scope.Benchmark]]),
  * set the `threadCount` field from the `-p`/`--param` value, and then your benchmark can call
  * [[parallelMap]] to obtain a `ParallelMap` configured with that thread count.
  *
  * Typical usage in a benchmark:
  * {{{
  *   @State(Scope.Thread)
  *   class MyState { /* benchmark inputs */ }
  *
  *   @Benchmark
  *   def run(s: MyState, pm: BenchmarksWithParallelMap): Unit = {
  *     val p = pm.parallelMap
  *     // use `p` to run parallel work within the benchmark
  *   }
  * }}}
  *
  * To select the thread count at runtime, pass a JMH param, e.g.:
  * {{{
  *   sbt 'benchmarks/jmh:run -p threadCount=1'     // single-threaded
  *   sbt 'benchmarks/jmh:run -p threadCount=2'     // two worker threads
  *   sbt 'benchmarks/jmh:run -p threadCount=1,2,4' // sweep
  * }}}
  */
@State(Scope.Benchmark)
class BenchmarksWithParallelMap {
  /**
    * Number of worker threads used by the created [[foresight.eqsat.parallel.ParallelMap]].
    *
    * This field is populated by JMH from the `-p threadCount=...` command-line parameter.
    * Provide a single value (e.g., `1`) or a comma-separated list (e.g., `1,2,4`) to run sweeps.
    */
  @Param(Array("1", "2"))
  var threadCount: Int = _

  /**
    * Builds a fixed-size [[foresight.eqsat.parallel.ParallelMap]] using the configured `threadCount`.
    *
    * @return a `ParallelMap` that schedules work on exactly `threadCount` threads.
    */
  lazy val parallelMap: ParallelMap = ParallelMap.fixedThreadParallel(threadCount)
}
