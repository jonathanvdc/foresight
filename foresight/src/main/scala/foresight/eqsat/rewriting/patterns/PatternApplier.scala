package foresight.eqsat.rewriting.patterns

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.commands.{CommandScheduleBuilder, IntRef}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.{ReversibleApplier, Searcher}
import foresight.eqsat.{EClassSymbol, ENode, MixedTree, Slot}

import scala.collection.compat.immutable.ArraySeq
import java.util.ArrayDeque

/**
 * An applier that applies a pattern match to an e-graph.
 *
 * @param pattern The pattern to apply.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the applier applies the match to.
 */
final case class PatternApplier[NodeT, EGraphT <: EGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var])
  extends ReversibleApplier[NodeT, PatternMatch[NodeT], EGraphT] {

  override def apply(m: PatternMatch[NodeT], egraph: EGraphT, builder: CommandScheduleBuilder[NodeT]): Unit = {
    val symbol = instantiateAsSimplifiedAddCommand(pattern, m, egraph, builder)
    builder.unionSimplified(EClassSymbol.real(m.root), symbol, egraph)
  }

  override def tryReverse: Option[Searcher[NodeT, PatternMatch[NodeT], EGraphT]] = {
    Some(pattern.toSearcher)
  }

  /**
   * Instantiates the pattern with the given match.
   *
   * @param m The match to use for instantiation.
   * @return The instantiated pattern.
   */
  def instantiate(m: PatternMatch[NodeT]): MixedTree[NodeT, EClassSymbol] = instantiate(pattern, m)

  /**
   * Instantiates the pattern with the given match.
   *
   * @param pattern The pattern to instantiate.
   * @param m The match to use for instantiation.
   * @return The instantiated pattern.
   */
  private def instantiate(pattern: MixedTree[NodeT, Pattern.Var],
                          m: PatternMatch[NodeT]): MixedTree[NodeT, EClassSymbol] = {
    pattern match {
      case MixedTree.Atom(p) => p match {
        case v: Pattern.Var => m(v).mapAtoms(EClassSymbol.real)
      }

      case MixedTree.Node(t, Seq(), uses, args) =>
        // No definitions, so we can reuse the PatternMatch and its original slot mapping
        MixedTree.Node[NodeT, EClassSymbol](t, Seq(), uses.map(m.apply: Slot => Slot), args.map(instantiate(_, m)))

      case MixedTree.Node(t, defs, uses, args) =>
        val defSlots = defs.map { (s: Slot) =>
          m.slotMapping.get(s) match {
            case Some(v) => v
            case None => Slot.fresh()
          }
        }
        val newMatch = m.copy(slotMapping = m.slotMapping ++ defs.zip(defSlots))
        MixedTree.Node[NodeT, EClassSymbol](t, defSlots, uses.map(newMatch.apply: Slot => Slot), args.map(instantiate(_, newMatch)))
    }
  }

  private final class SimplifiedAddCommandInstantiator {
    // Mutable state to allow pooling; initialized via init() before use.
    private var m: PatternMatch[NodeT] = _
    private var egraph: EGraphT = _
    private var builder: CommandScheduleBuilder[NodeT] = _
    private var nodePool: ENode.Pool = _
    private var refPool: IntRef.Pool = _

    def init(m0: PatternMatch[NodeT],
             egraph0: EGraphT,
             builder0: CommandScheduleBuilder[NodeT],
             nodePool0: ENode.Pool,
             refPool0: IntRef.Pool): Unit = {
      this.m = m0
      this.egraph = egraph0
      this.builder = builder0
      this.nodePool = nodePool0
      this.refPool = refPool0
    }

    def clear(): Unit = {
      // release references to help GC when pooled instances sit around
      this.m = null
      this.egraph = null.asInstanceOf[EGraphT]
      this.builder = null
      this.nodePool = null
      this.refPool = null
    }

    def instantiate(pattern: MixedTree[NodeT, Pattern.Var], maxBatch: IntRef): EClassSymbol = {
      pattern match {
        case MixedTree.Atom(p) => builder.addSimplifiedReal(m(p), egraph)
        case MixedTree.Node(t, defs@Seq(), uses, args) =>
          // No definitions, so we can reuse the PatternMatch and its original slot mapping
          addSimplifiedNode(t, defs, uses, args, maxBatch)

        case MixedTree.Node(t, defs, uses, args) =>
          val defSlots = defs.map { (s: Slot) =>
            m.slotMapping.get(s) match {
              case Some(v) => v
              case None => Slot.fresh()
            }
          }
          val newMatch = m.copy(slotMapping = m.slotMapping ++ defs.zip(defSlots))

          // Acquire a nested instantiator.
          val nested = SimplifiedAddCommandInstantiator.acquire()
          nested.init(newMatch, egraph, builder, nodePool, refPool)
          try {
            nested.addSimplifiedNode(t, defSlots, uses, args, maxBatch)
          } finally {
            nested.clear()
            SimplifiedAddCommandInstantiator.release(nested)
          }
      }
    }

    private def addSimplifiedNode(nodeType: NodeT,
                                  definitions: SlotSeq,
                                  uses: SlotSeq,
                                  args: ArraySeq[MixedTree[NodeT, Pattern.Var]],
                                  maxBatch: IntRef): EClassSymbol = {
      val argMaxBatch = refPool.acquire(0)
      val argSymbols = CommandScheduleBuilder.symbolArrayFrom(args, argMaxBatch, nodePool, instantiate)
      val useSymbols = uses.map(m.apply: Slot => Slot)
      val result = builder.addSimplifiedNode(nodeType, definitions, useSymbols, argSymbols, argMaxBatch, egraph, nodePool)
      if (argMaxBatch.elem > maxBatch.elem) {
        maxBatch.elem = argMaxBatch.elem
      }
      refPool.release(argMaxBatch)
      result
    }
  }

  private object SimplifiedAddCommandInstantiator {
    // Per-thread pool to avoid contention; LIFO to improve cache locality.
    private val local: ThreadLocal[ArrayDeque[SimplifiedAddCommandInstantiator]] =
      new ThreadLocal[ArrayDeque[SimplifiedAddCommandInstantiator]]() {
        override def initialValue(): ArrayDeque[SimplifiedAddCommandInstantiator] =
          new ArrayDeque[SimplifiedAddCommandInstantiator]()
      }

    def acquire(): SimplifiedAddCommandInstantiator = {
      val dq = local.get()
      val inst = dq.pollFirst()
      if (inst != null) inst else new SimplifiedAddCommandInstantiator
    }

    def release(inst: SimplifiedAddCommandInstantiator): Unit = {
      local.get().offerFirst(inst)
    }
  }

  private def instantiateAsSimplifiedAddCommand(pattern: MixedTree[NodeT, Pattern.Var],
                                                m: PatternMatch[NodeT],
                                                egraph: EGraphT,
                                                builder: CommandScheduleBuilder[NodeT]): EClassSymbol = {

    val refPool = IntRef.defaultPool
    val ref = refPool.acquire(0)
    val inst = SimplifiedAddCommandInstantiator.acquire()
    inst.init(m, egraph, builder, ENode.defaultPool, refPool)
    val result =
      try {
        inst.instantiate(pattern, ref)
      } finally {
        inst.clear()
        SimplifiedAddCommandInstantiator.release(inst)
      }
    refPool.release(ref)
    result
  }
}
