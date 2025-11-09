package foresight.eqsat.commands

import foresight.eqsat.{EClassSymbol, ENode, ENodeSymbol}
import foresight.util.collections.UnsafeSeqFromArray

import java.util.function.Function
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.collection.compat.immutable.ArraySeq
import scala.reflect.ClassTag

private[commands] class ConcurrentCommandScheduleBuilder[NodeT] extends CommandScheduleBuilder[NodeT] {
  private val batchZeroAdds = new ConcurrentLinkedQueue[(EClassSymbol.Virtual, ENode[NodeT])]()
  private val otherBatchAdds = new ConcurrentHashMap[Int, ConcurrentLinkedQueue[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]]()
  private val unions: ConcurrentLinkedQueue[(EClassSymbol, EClassSymbol)] = new ConcurrentLinkedQueue()

  override def add(symbol: EClassSymbol.Virtual, node: ENodeSymbol[NodeT], batch: Int): Unit = {
    if (batch == 0) {
      node match {
        case eNode: ENode[NodeT] => batchZeroAdds.add((symbol, eNode))
        case _ => throw new IllegalArgumentException("Only ENode instances are allowed in batch 0")
      }
    } else {
      val queue = otherBatchAdds.computeIfAbsent(batch, new Function[Int, ConcurrentLinkedQueue[(EClassSymbol.Virtual, ENodeSymbol[NodeT])]] {
        override def apply(t: Int): ConcurrentLinkedQueue[(EClassSymbol.Virtual, ENodeSymbol[NodeT])] = {
          new ConcurrentLinkedQueue()
        }
      })
      queue.add((symbol, node))
    }
  }

  override def union(a: EClassSymbol, b: EClassSymbol): Unit = {
    unions.add((a, b))
  }

  private def _highestBatchIndex: Int = {
    var highestBatchIndex = 0
    val batchIndexIterator = otherBatchAdds.keySet.iterator()
    while (batchIndexIterator.hasNext) {
      val batchIndex = batchIndexIterator.next()
      if (batchIndex > highestBatchIndex) {
        highestBatchIndex = batchIndex
      }
    }
    highestBatchIndex
  }

  private def linkedQueueToSplitArrays[A: ClassTag](queue: ConcurrentLinkedQueue[(EClassSymbol.Virtual, A)]): (ArraySeq[EClassSymbol.Virtual], ArraySeq[A]) = {
    val size = queue.size()
    val symbols = new Array[EClassSymbol.Virtual](size)
    val nodes = new Array[A](size)

    val iterator = queue.iterator()
    var index = 0
    while (iterator.hasNext) {
      val (first, second) = iterator.next()
      symbols(index) = first
      nodes(index) = second
      index += 1
    }

    (UnsafeSeqFromArray(symbols), UnsafeSeqFromArray(nodes))
  }

  override def result(): CommandSchedule[NodeT] = {
    val batchZero = linkedQueueToSplitArrays[ENode[NodeT]](batchZeroAdds)

    val highestBatchIndex = _highestBatchIndex
    val batches = new Array[(ArraySeq[EClassSymbol.Virtual], ArraySeq[ENodeSymbol[NodeT]])](highestBatchIndex)
    for (batchIndex <- 1 to highestBatchIndex) {
      val queue = otherBatchAdds.get(batchIndex)
      if (queue != null) {
        batches(batchIndex - 1) = linkedQueueToSplitArrays[ENodeSymbol[NodeT]](queue)
      } else {
        batches(batchIndex - 1) = (ArraySeq.empty, ArraySeq.empty)
      }
    }

    val unionArraySeq = UnsafeSeqFromArray(unions.toArray(new Array[(EClassSymbol, EClassSymbol)](0)))
    CommandSchedule(batchZero, batches.toSeq, unionArraySeq)
  }
}
