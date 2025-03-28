package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.{EGraph, MixedTree, Slot}
import fixpoint.eqsat.commands.{Command, CommandQueueBuilder, EClassSymbol}
import fixpoint.eqsat.rewriting.Applier

/**
 * An applier that applies a pattern match to an e-graph.
 * @param pattern The pattern to apply.
 * @tparam NodeT The type of the nodes in the e-graph.
 */
final case class PatternApplier[NodeT](pattern: Pattern[NodeT]) extends Applier[NodeT, PatternMatch[NodeT], EGraph[NodeT]] {
  override def apply(m: PatternMatch[NodeT], egraph: EGraph[NodeT]): Command[NodeT] = {
    val tree = instantiate(pattern, m)
    val builder = new CommandQueueBuilder[NodeT]
    val c = builder.add(tree)
    builder.union(EClassSymbol.real(m.root), c)
    builder.queue
  }

  private def instantiate(pattern: Pattern[NodeT], m: PatternMatch[NodeT]): MixedTree[NodeT, EClassSymbol] = {
    pattern match {
      case v: Pattern.Var[NodeT] => MixedTree.Call[NodeT, EClassSymbol](EClassSymbol.real(m(v)))

      case Pattern.Node(t, defs, uses, args) =>
        val defSlots = defs.map { s =>
          m.slotMapping.get(s) match {
            case Some(v) => v
            case None => Slot.fresh()
          }
        }
        val newMatch = m.copy(slotMapping = m.slotMapping ++ defs.zip(defSlots))
        MixedTree.Node[NodeT, EClassSymbol](t, defSlots, uses.map(newMatch(_)), args.map(instantiate(_, newMatch)))
    }
  }
}
