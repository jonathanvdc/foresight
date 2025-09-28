package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EClassSymbol, ENode, MixedTree, Slot}
import foresight.eqsat.immutable.EGraph
import org.junit.Test

class CommandQueueBuilderTest {
  /**
   * An empty queue does nothing.
   */
  @Test
  def emptyQueueDoesNothing(): Unit = {
    val builder = new CommandQueueBuilder[Int]

    val egraph = EGraph.empty[Int]

    assert(builder.result().applyImmutable(egraph, Map.empty, ParallelMap.sequential)._1.isEmpty)
  }

  /**
   * Adding a node to the queue adds it to the e-graph.
   */
  @Test
  def addNode(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]

    val node = ENodeSymbol(0, Seq.empty, Seq.empty, Seq.empty)
    builder.add(node)

    val queue = builder.result()
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[AddManyCommand[Int]])
    assert(queue.commands.head.asInstanceOf[AddManyCommand[Int]].nodes.head._2 == node)

    val (Some(egraph2), _) = builder.result().applyImmutable(egraph, Map.empty, ParallelMap.sequential)
    assert(egraph2.classes.size == 1)
    assert(egraph2.nodes(egraph2.canonicalize(egraph2.classes.head)).head == node.reify(Map.empty))
  }

  /**
   * Adding a tree to the queue adds it to the e-graph.
   */
  @Test
  def addSingleNodeTree(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]

    val tree = MixedTree.Node[Int, EClassSymbol](0, Seq.empty[Slot], Seq.empty[Slot], Seq.empty[MixedTree[Int, EClassSymbol]])
    builder.add(tree)

    val queue = builder.result()
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[AddManyCommand[Int]])

    val (Some(egraph2), _) = builder.result().applyImmutable(egraph, Map.empty, ParallelMap.sequential)
    assert(egraph2.classes.size == 1)
  }

  /**
   * Adding a tree with a child to the queue adds it to the e-graph.
   */
  @Test
  def addTreeWithChild(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]

    val child = MixedTree.unslotted(1, Seq.empty[MixedTree[Int, EClassSymbol]])
    val tree = MixedTree.unslotted(0, Seq(child))
    builder.add(tree)

    val queue = builder.result()
    assert(queue.commands.size == 2)
    assert(queue.commands.head.isInstanceOf[AddManyCommand[Int]])
    assert(queue.commands(1).isInstanceOf[AddManyCommand[Int]])

    val (Some(egraph2), _) = builder.result().applyImmutable(egraph, Map.empty, ParallelMap.sequential)
    assert(egraph2.classes.size == 2)
  }

  /**
   * Adding a tree with a call to the queue adds it to the e-graph.
   */
  @Test
  def addTreeWithCall(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]
    val (call, egraph2) = egraph.add(ENode(0, Seq.empty, Seq.empty, Seq.empty))

    val tree = MixedTree.Atom[Int, EClassSymbol](EClassSymbol.real(call))
    builder.add(tree)

    val queue = builder.result()
    assert(queue.commands.isEmpty)

    val (None, _) = builder.result().applyImmutable(egraph2, Map.empty, ParallelMap.sequential)
  }

  /**
   * Unions two e-classes in the e-graph.
   */
  @Test
  def union(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]
    val (a, egraph2) = egraph.add(ENode(0, Seq.empty, Seq.empty, Seq.empty))
    val (b, egraph3) = egraph2.add(ENode(1, Seq.empty, Seq.empty, Seq.empty))

    builder.union(EClassSymbol.real(a), EClassSymbol.real(b))

    val queue = builder.result()
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[UnionManyCommand[Int]])

    val (Some(egraph4), _) = builder.result().applyImmutable(egraph3, Map.empty, ParallelMap.sequential)
    assert(egraph4.classes.size == 1)
    assert(egraph4.areSame(a, b))
  }

  /**
   * Unions are combined into a single command by CommandQueue.optimized.
   */
  @Test
  def optimizedUnions(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]
    val (a, egraph2) = egraph.add(ENode(0, Seq.empty, Seq.empty, Seq.empty))
    val (b, egraph3) = egraph2.add(ENode(1, Seq.empty, Seq.empty, Seq.empty))
    val (c, egraph4) = egraph3.add(ENode(2, Seq.empty, Seq.empty, Seq.empty))

    builder.union(EClassSymbol.real(a), EClassSymbol.real(b))
    builder.union(EClassSymbol.real(b), EClassSymbol.real(c))

    val naiveQueue = builder.result()
    assert(naiveQueue.commands.size == 2)
    assert(naiveQueue.commands.head.isInstanceOf[UnionManyCommand[Int]])
    assert(naiveQueue.commands(1).isInstanceOf[UnionManyCommand[Int]])

    val optimizedQueue = builder.result().optimized
    assert(optimizedQueue.commands.size == 1)
    assert(optimizedQueue.commands.head.isInstanceOf[UnionManyCommand[Int]])

    val (Some(egraph5), _) = optimizedQueue.applyImmutable(egraph4, Map.empty, ParallelMap.sequential)
    assert(egraph5.classes.size == 1)
    assert(egraph5.areSame(a, b))
    assert(egraph5.areSame(b, c))
  }

  @Test
  def xyUnionYxProducesPermutation(): Unit = {
    val builder = new CommandQueueBuilder[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val node1 = ENodeSymbol(0, Seq.empty, Seq(x, y), Seq.empty)
    val node2 = ENodeSymbol(0, Seq.empty, Seq(y, x), Seq.empty)

    val a = builder.add(node1)
    val b = builder.add(node2)

    builder.union(a, b)

    val node3 = ENodeSymbol(1, Seq(x, y), Seq.empty, Seq(a))
    val node4 = ENodeSymbol(1, Seq(x, y), Seq.empty, Seq(b))

    val c = builder.add(node3)
    val d = builder.add(node4)

    for (queue <- Seq(builder.result(), builder.result().optimized)) {
      val (Some(egraph), reification) = queue.applyImmutable(EGraph.empty[Int], Map.empty, ParallelMap.sequential)

      assert(egraph.classes.size == 2)
      assert(egraph.areSame(a.reify(reification), b.reify(reification)))
      assert(egraph.areSame(c.reify(reification), d.reify(reification)))
    }
  }

  @Test
  def constructTreeWithSlots(): Unit = {
    val builder = new CommandQueueBuilder[Int]

    val x = Slot.fresh()
    val y = Slot.fresh()

    val tree1 = builder.add(MixedTree.Node(2, Seq.empty, Seq(y), Seq.empty))
    val tree2 = builder.add(
      MixedTree.Node(0, Seq(x), Seq.empty, Seq(MixedTree.Node(1, Seq.empty, Seq(x), Seq(MixedTree.Atom(tree1))))))

    for (queue <- Seq(builder.result(), builder.result().optimized)) {
      val (Some(egraph), reification) = queue.applyImmutable(EGraph.empty[Int], Map.empty, ParallelMap.sequential)

      assert(egraph.classes.size == 3)
      assert(tree1.reify(reification).args.valueSet == Set(y))
      assert(tree2.reify(reification).args.valueSet == Set(y))

      assert(!egraph.contains(
        MixedTree.Node(0, Seq(x), Seq.empty, Seq(
          MixedTree.Node(1, Seq.empty, Seq(y), Seq(MixedTree.Atom(tree1.reify(reification))))))))
    }
  }
}

