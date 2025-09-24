package foresight.eqsat.rewriting.patterns

import foresight.eqsat.commands.{Command, CommandQueueBuilder}
import foresight.eqsat.rewriting.{ReversibleApplier, Searcher}
import foresight.eqsat.{EClassSymbol, MixedTree, ReadOnlyEGraph, Slot}

/**
 * An applier that applies a pattern match to an e-graph.
 *
 * @param pattern The pattern to apply.
 * @tparam NodeT The type of the nodes in the e-graph.
 * @tparam EGraphT The type of the e-graph that the applier applies the match to.
 */
final case class PatternApplier[NodeT, EGraphT <: ReadOnlyEGraph[NodeT]](pattern: MixedTree[NodeT, Pattern.Var])
  extends ReversibleApplier[NodeT, PatternMatch[NodeT], EGraphT] {

  override def apply(m: PatternMatch[NodeT], egraph: EGraphT): Command[NodeT] = {
    val builder = new CommandQueueBuilder[NodeT]()
    val symbol = instantiateAsSimplifiedAddCommand(pattern, m, egraph, builder)
    builder.unionSimplified(EClassSymbol.real(m.root), symbol, egraph)
    builder.result()
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

  private def instantiateAsSimplifiedAddCommand(pattern: MixedTree[NodeT, Pattern.Var],
                                                m: PatternMatch[NodeT],
                                                egraph: EGraphT,
                                                builder: CommandQueueBuilder[NodeT]): EClassSymbol = {

    pattern match {
      case MixedTree.Atom(p) => builder.addSimplifiedReal(m(p), egraph)
      case MixedTree.Node(t, Seq(), uses, args) =>
        // No definitions, so we can reuse the PatternMatch and its original slot mapping
        val argSymbols = args.map(instantiateAsSimplifiedAddCommand(_, m, egraph, builder))
        val useSymbols = uses.map(m.apply: Slot => Slot)
        builder.addSimplifiedNode(t, Seq(), useSymbols, argSymbols, egraph)

      case MixedTree.Node(t, defs, uses, args) =>
        val defSlots = defs.map { (s: Slot) =>
          m.slotMapping.get(s) match {
            case Some(v) => v
            case None => Slot.fresh()
          }
        }
        val newMatch = m.copy(slotMapping = m.slotMapping ++ defs.zip(defSlots))
        val argSymbols = args.map(instantiateAsSimplifiedAddCommand(_, newMatch, egraph, builder))
        builder.addSimplifiedNode(t, defSlots, uses.map(newMatch.apply: Slot => Slot), argSymbols, egraph)
    }
  }
}
