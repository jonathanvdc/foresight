package foresight.eqsat.rewriting.patterns

import foresight.eqsat.collections.SlotSeq
import foresight.eqsat.commands.{CommandScheduleBuilder, IntRef}
import foresight.eqsat.readonly.EGraph
import foresight.eqsat.rewriting.{ReversibleApplier, Searcher}
import foresight.eqsat.{EClassSymbol, ENode, MixedTree, Slot}

import scala.collection.compat.immutable.ArraySeq

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

  private final class SimplifiedAddCommandInstantiator(m: PatternMatch[NodeT],
                                                       egraph: EGraphT,
                                                       builder: CommandScheduleBuilder[NodeT],
                                                       nodePool: ENode.Pool,
                                                       refPool: IntRef.Pool) {
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
          new SimplifiedAddCommandInstantiator(newMatch, egraph, builder, nodePool, refPool)
            .addSimplifiedNode(t, defSlots, uses, args, maxBatch)
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

  private def instantiateAsSimplifiedAddCommand(pattern: MixedTree[NodeT, Pattern.Var],
                                                m: PatternMatch[NodeT],
                                                egraph: EGraphT,
                                                builder: CommandScheduleBuilder[NodeT]): EClassSymbol = {

    val refPool = IntRef.defaultPool
    val ref = refPool.acquire(0)
    val result = new SimplifiedAddCommandInstantiator(m, egraph, builder, ENode.defaultPool, refPool).instantiate(pattern, ref)
    refPool.release(ref)
    result
  }
}
