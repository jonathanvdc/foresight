package foresight.eqsat.commands

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassCall, EClassSymbol, ENode, ENodeSymbol, MixedTree}
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

  /**
   * addSimplifiedReal: Atom path should return the same real symbol and schedule nothing.
   */
  @Test
  def addSimplifiedRealAtomNoop(): Unit = {
    val baseBuilder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val g0 = EGraph.empty[Int]

    // Create one real class in the graph so we can obtain a real EClassCall.
    val seed = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val v = seed.add(ENode(42, Seq.empty, Seq.empty, Seq.empty), 0)
    val sched = seed.result()
    val g1 = sched.applyImmutable(g0, ParallelMap.sequential).get

    // Get a real call referring to the inserted class.
    val realCall: EClassCall = g1.canonicalize(g1.classes.head)

    // Use addSimplifiedReal on an Atom (real call).
    val sym = baseBuilder.addSimplifiedReal(
      MixedTree.Atom[Int, EClassCall](realCall),
      g1
    )

    // It should return the same real symbol and schedule nothing.
    assert(sym == EClassSymbol.real(realCall))
    val out = baseBuilder.result()
    assert(out.additions.isEmpty)
    assert(out.unions.isEmpty)
  }

  /**
   * addSimplifiedReal: Single node with no children should schedule exactly one addition.
   */
  @Test
  def addSimplifiedRealSingleNodeAddsOne(): Unit = {
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val g0 = EGraph.empty[Int]

    // Build a trivial 1-node tree: Node(7, [], [], [])
    val tree = MixedTree.Node[Int, EClassCall](7, SlotSeq.empty, SlotSeq.empty, ArraySeq.empty[MixedTree[Int, EClassCall]])
    val sym = builder.addSimplifiedReal(tree, g0)

    // We only care that exactly one node gets scheduled and applying the schedule grows the graph by 1.
    val sched = builder.result()
    assert(sched.additions.size == 1)

    val g1opt = sched.applyImmutable(g0, ParallelMap.sequential)
    assert(g1opt.nonEmpty)
    val g1 = g1opt.get
    assert(g1.classes.nonEmpty)

    // The returned symbol should be virtual and present in the first (and only) batch.
    val (syms0, nodes0) = sched.additions.head
    assert(syms0.contains(sym))
    assert(nodes0.nonEmpty)
  }

  /**
   * addSimplifiedReal: Nested tree should create children first (earlier batch), then parent (later batch).
   */
  @Test
  def addSimplifiedRealNestedBatchesIncrease(): Unit = {
    val g0 = EGraph.empty[Int]
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]

    // Seed a real leaf in the graph to use as an Atom in the child.
    val seed = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val leafV = seed.add(ENode(1, Seq.empty, Seq.empty, Seq.empty), 0)
    val seedSched = seed.result()
    val g1 = seedSched.applyImmutable(g0, ParallelMap.sequential).get
    val realLeaf: EClassCall = g1.canonicalize(g1.classes.head)

    // child = Node(10, [], [], [ Atom(realLeaf) ])
    val child = MixedTree.Node[Int, EClassCall](
      10,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(MixedTree.Atom[Int, EClassCall](realLeaf))
    )
    // parent = Node(20, [], [], [ child ])
    val parent = MixedTree.Node[Int, EClassCall](
      20,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(child)
    )

    // Instantiate child first to get its symbol and ensure batch ordering is visible.
    val childSym = builder.addSimplifiedReal(child, g1)
    val parentSym = builder.addSimplifiedReal(parent, g1)

    val sched = builder.result()
    // Expect two distinct non-empty batches for additions beyond batch 0.
    assert(sched.additions.size == 2)
    assert(sched.otherBatches.length == 1)

    // First batch should contain the child, second batch the parent.
    assert(sched.batchZero._1.contains(childSym))
    assert(sched.otherBatches(0)._1.contains(parentSym))

    // Applying the schedule should add exactly two new classes.
    val g2 = sched.applyImmutable(g1, ParallelMap.sequential).get
    assert(g2.classes.size == g1.classes.size + 2)
  }


  /**
   * addSimplifiedReal: Complex tree should preserve strict batch layering across depths.
   * Level 1 (children of real atoms) -> Level 2 (parents of level 1) -> Level 3 (root).
   */
  @Test
  def addSimplifiedRealComplexTreeBatchesStable(): Unit = {
    val g0 = EGraph.empty[Int]
    val builder = CommandScheduleBuilder.newConcurrentBuilder[Int]

    // Seed two real leaves in the graph to use as Atoms.
    val seed = CommandScheduleBuilder.newConcurrentBuilder[Int]
    val leafA = seed.add(ENode(101, Seq.empty, Seq.empty, Seq.empty), 0)
    val leafB = seed.add(ENode(102, Seq.empty, Seq.empty, Seq.empty), 0)
    val seedSched = seed.result()
    val g1 = seedSched.applyImmutable(g0, ParallelMap.sequential).get
    val realA: EClassCall = g1.canonicalize(g1.classes.head)
    val realB: EClassCall = {
      // the second class; order is not guaranteed, so find the one that's not realA
      val calls = g1.classes.map(g1.canonicalize)
      calls.find(_ != realA).get
    }

    // Level 1 nodes (directly depend on real atoms)
    val child1 = MixedTree.Node[Int, EClassCall](
      201,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(MixedTree.Atom[Int, EClassCall](realA))
    )
    val child2 = MixedTree.Node[Int, EClassCall](
      202,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(MixedTree.Atom[Int, EClassCall](realA), MixedTree.Atom[Int, EClassCall](realB))
    )
    val childSym1 = builder.addSimplifiedReal(child1, g1)
    val childSym2 = builder.addSimplifiedReal(child2, g1)

    // Level 2 nodes (depend on Level 1)
    val parent1 = MixedTree.Node[Int, EClassCall](
      301,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(child1, MixedTree.Atom[Int, EClassCall](realB))
    )
    val parent2 = MixedTree.Node[Int, EClassCall](
      302,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(child2)
    )
    val parentSym1 = builder.addSimplifiedReal(parent1, g1)
    val parentSym2 = builder.addSimplifiedReal(parent2, g1)

    // Level 3 node (root depends on both Level 2 nodes)
    val root = MixedTree.Node[Int, EClassCall](
      401,
      SlotSeq.empty,
      SlotSeq.empty,
      ArraySeq(parent1, parent2)
    )
    val rootSym = builder.addSimplifiedReal(root, g1)

    val sched = builder.result()

    // Expect three distinct non-empty batches for additions: L1, L2, L3
    assert(sched.additions.size == 3)
    assert(sched.otherBatches.length == 2)

    // Level 1 batch (batchZero) should contain both children
    assert(sched.batchZero._1.contains(childSym1))
    assert(sched.batchZero._1.contains(childSym2))

    // Level 2 batch should contain both parents
    assert(sched.otherBatches(0)._1.contains(parentSym1))
    assert(sched.otherBatches(0)._1.contains(parentSym2))

    // Level 3 batch should contain the root
    assert(sched.otherBatches(1)._1.contains(rootSym))

    // Applying the schedule should add exactly 5 new classes (2 L1 + 2 L2 + 1 L3)
    val g2 = sched.applyImmutable(g1, ParallelMap.sequential).get
    assert(g2.classes.size == g1.classes.size + 5)
  }
}
