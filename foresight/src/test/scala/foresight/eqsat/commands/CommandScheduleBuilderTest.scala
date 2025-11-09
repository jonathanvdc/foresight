package foresight.eqsat.commands

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassSymbol, ENode, ENodeSymbol}
import foresight.eqsat.immutable.EGraph
import org.junit.Test

import scala.collection.compat.immutable.ArraySeq

class CommandScheduleBuilderTest {
  /**
   * An empty schedule is a no-op.
   */
  @Test
  def emptyScheduleDoesNothing(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val egraph = EGraph.empty[Int]

    val schedule = builder.result()
    assert(schedule.additions.isEmpty)
    assert(schedule.unions.isEmpty)
    assert(schedule.applyImmutable(egraph, ParallelMap.sequential).isEmpty)
  }

  /**
   * Adding a concrete node in batch 0 inserts one class.
   */
  @Test
  def addConcreteNodeInBatchZero(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val egraph = EGraph.empty[Int]

    val enode = ENode(0, Seq.empty, Seq.empty, Seq.empty)
    val sym: EClassSymbol.Virtual = builder.add(enode, 0)

    val schedule = builder.result()
    // Batch structure
    assert(schedule.additions.size == 1)
    val (symbols0, nodes0) = schedule.additions.head
    assert(nodes0.nonEmpty)
    assert(schedule.batchZero._2.head == enode)
    assert(schedule.batchZero._1.head == sym)

    val e2 = schedule.applyImmutable(egraph, ParallelMap.sequential)
    assert(e2.nonEmpty)
    val g2 = e2.get
    assert(g2.classes.size == 1)
  }

  /**
   * Add symbolic node to a later batch that depends on a batch-0 symbol.
   * Ensures reification is wired and batch order respected.
   */
  @Test
  def addDependentSymbolicNodeInBatchOne(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val egraph = EGraph.empty[Int]

    val a = builder.add(ENode(0, Seq.empty, Seq.empty, Seq.empty), 0)
    val n1 = ENodeSymbol(1, SlotSeq.empty, SlotSeq.empty, ArraySeq(a))
    val b = builder.add(n1, 1)

    val schedule = builder.result()
    // Two batches: 0 and 1
    assert(schedule.additions.size == 2)
    assert(schedule.batchZero._1.contains(a))
    assert(schedule.otherBatches.head._1.contains(b))

    val g2 = schedule.applyImmutable(egraph, ParallelMap.sequential).get
    assert(g2.classes.size == 2)
  }

  /**
   * Unions execute after all additions; no-op when already same.
   */
  @Test
  def unionsExecuteAndNoOpWhenSame(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val e0 = EGraph.empty[Int]

    val a = builder.add(ENode(0, Seq.empty, Seq.empty, Seq.empty), 0)
    val b = builder.add(ENode(1, Seq.empty, Seq.empty, Seq.empty), 0)

    builder.union(a, b)

    val schedule = builder.result()
    val g1 = schedule.applyImmutable(e0, ParallelMap.sequential).get
    assert(g1.classes.size == 1)

    // Apply the same schedule again: already united → no changes
    assert(schedule.applyImmutable(g1, ParallelMap.sequential).isEmpty)
  }

  /**
   * Batch index correctness: all direct parents must have strictly higher batch index than their children.
   */
  @Test
  def batchIndexParentsGreaterThanChildren(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]

    // Build a small diamond: a (batch 0) -> b,c (batch 1) -> d (batch 2)
    val a = builder.add(ENode(10, Seq.empty, Seq.empty, Seq.empty), 0)
    val b = builder.add(ENodeSymbol(20, SlotSeq.empty, SlotSeq.empty, ArraySeq(a)), 1)
    val c = builder.add(ENodeSymbol(21, SlotSeq.empty, SlotSeq.empty, ArraySeq(a)), 1)
    val d = builder.add(ENodeSymbol(30, SlotSeq.empty, SlotSeq.empty, ArraySeq(b, c)), 2)

    val schedule = builder.result()

    // Validate batches and membership
    assert(schedule.additions.size == 3)
    assert(schedule.batchZero._1 == ArraySeq(a))
    assert(schedule.otherBatches(0)._1 == ArraySeq(b, c))
    assert(schedule.otherBatches(1)._1 == ArraySeq(d))
  }

  /**
   * Mixed batches: verify that adding the same ENode twice yields no change on the second application,
   * and that batch indices are preserved.
   */
  @Test
  def idempotentApplyAndStableBatches(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val g0 = EGraph.empty[Int]

    val a = builder.add(ENode(1, Seq.empty, Seq.empty, Seq.empty), 0)
    val b = builder.add(ENodeSymbol(2, SlotSeq.empty, SlotSeq.empty, ArraySeq(a)), 1)

    val schedule = builder.result()

    val g1 = schedule.applyImmutable(g0, ParallelMap.sequential)
    assert(g1.nonEmpty)

    // Re-apply -> no changes
    val g2 = schedule.applyImmutable(g1.get, ParallelMap.sequential)
    assert(g2.isEmpty)

    // Batches unchanged
    assert(schedule.batchZero._1 == ArraySeq(a))
    assert(schedule.otherBatches.head._1 == ArraySeq(b))
  }

  /**
   * When using unions with a symbolic and a concrete reference, ensure the union is scheduled and applied.
   */
  @Test
  def unionWithVirtualAndReal(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val g0 = EGraph.empty[Int]

    val a = builder.add(ENode(7, Seq.empty, Seq.empty, Seq.empty), 0)
    val scheduleBeforeUnion = builder.result()
    val g1 = scheduleBeforeUnion.applyImmutable(g0, ParallelMap.sequential).get

    // Real call obtained from g1
    val realA: EClassCall = g1.canonicalize(g1.classes.head)
    val realSym: EClassSymbol = EClassSymbol.real(realA)

    // Add a new virtual and union it with the real one
    val b = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val v = b.add(ENode(8, Seq.empty, Seq.empty, Seq.empty), 0)
    b.union(v, realSym)

    val sched2 = b.result()
    val g2 = sched2.applyImmutable(g1, ParallelMap.sequential).get

    assert(g2.classes.size == 1)
  }
}
