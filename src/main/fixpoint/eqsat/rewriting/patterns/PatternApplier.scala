package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.{EClassCall, EGraph, MixedTree}
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
    val c = builder.add(tree.mapCalls(EClassSymbol.real))
    builder.union(EClassSymbol.real(m.root), c)
    builder.queue
  }

  private def instantiate(pattern: Pattern[NodeT], m: PatternMatch[NodeT]): MixedTree[NodeT, EClassCall] = {
    pattern match {
      case v: Pattern.Var[NodeT] => MixedTree.Call(m.varMapping(v))

      case Pattern.Node(t, defs, uses, args) =>
        MixedTree.Node(t, defs.map(m.slotMapping), uses.map(m.slotMapping), args.map(instantiate(_, m)))
    }
  }
}
