package foresight.util

import foresight.eqsat.parallel.ParallelMap
import org.openjdk.jmh.annotations.{Param, Scope, State}

@State(Scope.Benchmark)
class BenchmarksWithParallelMap {
  @Param(Array("1", "2"))
  var threadCount: Int = _

  def parallelMap: ParallelMap = ParallelMap.fixedThreadParallel(threadCount)
}
