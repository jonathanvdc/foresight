package foresight.eqsat

import foresight.util.Debug

/**
 * A node in a slotted e-graph.
 *
 * An e-node represents one operator application together with its slot usage and child e-class applications.
 * Slots are partitioned into:
 *   - `definitions`: slots introduced locally by this node (binder-internal slots that are not visible outside)
 *   - `uses`: slots referenced by this node that come from its surrounding context (free with respect to this node)
 * Child expressions are referenced via `args` as [[EClassCall]]s, which carry their own parameter-to-argument
 * slot maps.
 *
 * Node types (`NodeT`) supply the operator and any non-structural payload. Slots and arguments are provided here.
 *
 * @param nodeType     Operator or symbol for this node.
 * @param definitions  Slots introduced by this node that are scoped locally and invisible to parents. These are
 *                     redundant by construction at the boundary of this node and exist to model binders such as
 *                     lambda-abstraction or let.
 * @param uses         Slots referenced by this node that are visible to its parent and must be satisfied by the
 *                     surrounding e-class application.
 * @param args         Child e-class applications, each with its own parameter-to-argument [[SlotMap]].
 * @tparam NodeT       The domain-specific node type. It defines operator identity and payload but not slots/children.
 */
final case class ENode[+NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[EClassCall]) {

  /**
   * All slots that appear syntactically in this node: local definitions, free uses, and all argument slots of children.
   * Order preserves definitions, then uses, then the values of each child's argument map in child order.
   *
   * @return An ordered sequence of slots used by this node.
   */
  def slots: Seq[Slot] = definitions ++ uses ++ args.flatMap(_.args.values)

  /**
   * The set of all distinct slots occurring in this node: definitions, uses, and children’s argument slots.
   *
   * @return A set of slots used by this node.
   */
  def slotSet: Set[Slot] = definitions.toSet ++ uses ++ args.flatMap(_.args.valueSet)

  /**
   * Whether this node contains any slots at all: definitions, uses, or children’s argument slots.
   *
   * @return True if this node has any slots; false if it is completely ground.
   */
  def hasSlots: Boolean = definitions.nonEmpty || uses.nonEmpty || args.exists(!_.args.isEmpty)

  private def containsSlot(slot: Slot): Boolean = {
    definitions.contains(slot) || uses.contains(slot) || args.exists(_.args.valueSet.contains(slot))
  }

  /**
   * Renames every occurrence of the given slots throughout this node.
   *
   * The renaming applies uniformly to:
   *   - definitions and uses
   *   - each child's argument-slot map, composed via `composeRetain` so unmapped child entries are preserved
   *
   * All keys of `renaming` must occur in this node; extra keys are allowed but ignored.
   *
   * @param renaming Mapping from old to new slots.
   * @return A node with slots renamed.
   */
  def rename(renaming: SlotMap): ENode[NodeT] = {
    if (Debug.isEnabled) {
      require(renaming.keySet.forall(containsSlot), "All slots in the renaming must be present in the e-node.")
    }

    val newDefinitions = definitions.map(renaming.apply)
    val newUses = uses.map(renaming.apply)
    val newArgs = args.map(call => call.copy(args = call.args.composeRetain(renaming)))
    copy(definitions = newDefinitions, uses = newUses, args = newArgs)
  }

  /**
   * Returns the canonical shape of this node together with the inverse renaming from canonical to original slots.
   *
   * Canonicalization replaces every slot that appears in this node with a numeric slot in lexicographic order
   * (e.g., `$0`, `$1`, ...) producing a name-independent normal form. The accompanying [[SlotMap]] maps the canonical
   * numeric slots back to the original slots. This decomposition underlies hash-consing and equality modulo renaming.
   *
   * @return A [[ShapeCall]] whose shape is this node in canonical slot form, and whose renaming maps canonical
   *         slots back to the original slots of this node.
   */
  def asShapeCall: ShapeCall[NodeT] = {
    val renamedSlots = SlotMap(slots.distinct.zipWithIndex.map(p => p._1 -> Slot.numeric(p._2)).toMap)
    ShapeCall(rename(renamedSlots), renamedSlots.inverse)
  }

  /**
   * Checks whether this node is already in canonical shape-normal form.
   *
   * @return True if equal to `asShapeCall.shape`; false otherwise.
   */
  def isShape: Boolean = this == asShapeCall.shape
}

/**
 * Constructors and helpers for [[ENode]].
 */
object ENode {

  /**
   * Builds an e-node that declares no local or free slots.
   *
   * Useful for ground nodes in languages without variables or when slot usage is expressed entirely in children.
   *
   * @param nodeType Operator or symbol.
   * @param args     Child e-class applications.
   * @tparam NodeT   Domain-specific node type.
   * @return A slotless node with the given operator and children.
   *
   * @example
   * {{{
   * val n: ENode[Op] = ENode.unslotted(Add, Seq(leftCall, rightCall))
   * }}}
   */
  def unslotted[NodeT](nodeType: NodeT, args: Seq[EClassCall]): ENode[NodeT] = {
    ENode(nodeType, Seq.empty, Seq.empty, args)
  }
}
