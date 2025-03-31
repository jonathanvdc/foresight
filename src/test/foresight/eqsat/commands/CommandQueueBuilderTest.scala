package foresight.eqsat.commands

import foresight.eqsat.parallel.ParallelMap
import foresight.eqsat.{EGraph, ENode, MixedTree, Slot}
import org.junit.Test

class CommandQueueBuilderTest {
  /**
   * An empty queue does nothing.
   */
  @Test
  def emptyQueueDoesNothing(): Unit = {
    val builder = new CommandQueueBuilder[Int]

    val egraph = EGraph.empty[Int]

    assert(builder.queue(egraph, Map.empty, ParallelMap.sequential)._1.isEmpty)
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

    val queue = builder.queue
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[AddCommand[Int]])
    assert(queue.commands.head.asInstanceOf[AddCommand[Int]].node == node)

    val (Some(egraph2), _) = builder.queue(egraph, Map.empty, ParallelMap.sequential)
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

    val tree = MixedTree.Node[Int, EClassSymbol](0, Seq.empty, Seq.empty, Seq.empty)
    builder.add(tree)

    val queue = builder.queue
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[AddCommand[Int]])

    val (Some(egraph2), _) = builder.queue(egraph, Map.empty, ParallelMap.sequential)
    assert(egraph2.classes.size == 1)
  }

  /**
   * Adding a tree with a child to the queue adds it to the e-graph.
   */
  @Test
  def addTreeWithChild(): Unit = {
    val builder = new CommandQueueBuilder[Int]
    val egraph = EGraph.empty[Int]

    val child = MixedTree.Node[Int, EClassSymbol](1, Seq.empty, Seq.empty, Seq.empty)
    val tree = MixedTree.Node[Int, EClassSymbol](0, Seq.empty, Seq.empty, Seq(child))
    builder.add(tree)

    val queue = builder.queue
    assert(queue.commands.size == 2)
    assert(queue.commands.head.isInstanceOf[AddCommand[Int]])
    assert(queue.commands(1).isInstanceOf[AddCommand[Int]])

    val (Some(egraph2), _) = builder.queue(egraph, Map.empty, ParallelMap.sequential)
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

    val tree = MixedTree.Call[Int, EClassSymbol](EClassSymbol.real(call))
    builder.add(tree)

    val queue = builder.queue
    assert(queue.commands.isEmpty)

    val (None, _) = builder.queue(egraph2, Map.empty, ParallelMap.sequential)
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

    val queue = builder.queue
    assert(queue.commands.size == 1)
    assert(queue.commands.head.isInstanceOf[UnionManyCommand[Int]])

    val (Some(egraph4), _) = builder.queue(egraph3, Map.empty, ParallelMap.sequential)
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

    val naiveQueue = builder.queue
    assert(naiveQueue.commands.size == 2)
    assert(naiveQueue.commands.head.isInstanceOf[UnionManyCommand[Int]])
    assert(naiveQueue.commands(1).isInstanceOf[UnionManyCommand[Int]])

    val optimizedQueue = builder.queue.optimized
    assert(optimizedQueue.commands.size == 1)
    assert(optimizedQueue.commands.head.isInstanceOf[UnionManyCommand[Int]])

    val (Some(egraph5), _) = optimizedQueue(egraph4, Map.empty, ParallelMap.sequential)
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

    for (queue <- Seq(builder.queue, builder.queue.optimized)) {
      val (Some(egraph), reification) = queue(EGraph.empty[Int], Map.empty, ParallelMap.sequential)

      assert(egraph.classes.size == 2)
      assert(egraph.areSame(a.reify(reification), b.reify(reification)))
      assert(egraph.areSame(c.reify(reification), d.reify(reification)))
    }
  }
}
