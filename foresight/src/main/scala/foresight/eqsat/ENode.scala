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
 * @tparam NodeT       The domain-specific node type. It defines operator identity and payload but not slots/children.
 */
final class ENode[+NodeT] private (
  val nodeType: NodeT,
  private val _definitions: Array[Slot],
  private val _uses: Array[Slot],
  private val _args: Array[EClassCall]
) {
  import scala.collection.immutable.ArraySeq

  /**
   * Slots introduced by this node that are scoped locally and invisible to parents. These are
   * redundant by construction at the boundary of this node and exist to model binders such as
   * lambda-abstraction or let.
   *
   * @return Sequence of definition slots.
   */
  def definitions: ArraySeq[Slot] = ArraySeq.unsafeWrapArray(_definitions)

  /**
   * Slots referenced by this node that are visible to its parent and must be satisfied by the
   * surrounding e-class application.
   *
   * @return Sequence of use slots.
   */
  def uses: ArraySeq[Slot] = ArraySeq.unsafeWrapArray(_uses)

  /**
   * Child e-class applications, each with its own parameter-to-argument [[SlotMap]].
   *
   * @return Sequence of child e-class calls.
   */
  def args: ArraySeq[EClassCall]  = ArraySeq.unsafeWrapArray(_args)

  /**
   * All slots that appear syntactically in this node: local definitions, free uses, and all argument slots of children.
   * Order preserves definitions, then uses, then the values of each child's argument map in child order.
   *
   * @return An ordered sequence of slots used by this node.
   */
  def slots: Seq[Slot] = {
    // tiny sequences: build a compact ArrayBuffer then expose as ArraySeq
    val buf = new scala.collection.mutable.ArrayBuffer[Slot](_definitions.length + _uses.length + _args.length * 2)
    buf ++= _definitions
    buf ++= _uses
    var i = 0
    while (i < _args.length) {
      buf ++= _args(i).args.values
      i += 1
    }
    ArraySeq.unsafeWrapArray(buf.toArray)
  }

  /**
   * The set of all distinct slots occurring in this node: definitions, uses, and children’s argument slots.
   *
   * @return A set of slots used by this node.
   */
  def slotSet: Set[Slot] = {
    var s: Set[Slot] = Set.empty
    var i = 0
    while (i < _definitions.length) { s = s + _definitions(i); i += 1 }
    i = 0
    while (i < _uses.length) { s = s + _uses(i); i += 1 }
    i = 0
    while (i < _args.length) { s = s ++ _args(i).args.valueSet; i += 1 }
    s
  }

  /**
   * Whether this node contains any slots at all: definitions, uses, or children’s argument slots.
   *
   * @return True if this node has any slots; false if it is completely ground.
   */
  def hasSlots: Boolean = _definitions.nonEmpty || _uses.nonEmpty || _args.exists(!_.args.isEmpty)

  private def containsSlot(slot: Slot): Boolean = {
    _definitions.contains(slot) ||
    _uses.contains(slot) ||
    _args.exists(_.args.valueSet.contains(slot))
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
    if (!hasSlots) return this
    if (Debug.isEnabled) {
      require(renaming.keySet.forall(containsSlot), "All slots in the renaming must be present in the e-node.")
    }

    val newDefs = ENode.renameSlotsOrNull(_definitions, renaming)
    val newUses = ENode.renameSlotsOrNull(_uses, renaming)

    var newArgs: Array[EClassCall] = null
    var i = 0
    while (i < _args.length) {
      val renamedArg = _args(i).renameRetain(renaming)
      if (newArgs == null && renamedArg != _args(i)) {
        newArgs = new Array[EClassCall](_args.length)
        java.util.Arrays.copyOf(_args, i) // create a prefix copy
        // We must actually copy the prefix if we allocated newArgs
        Array.copy(_args, 0, newArgs, 0, i)
      }
      if (newArgs != null) newArgs(i) = renamedArg
      i += 1
    }

    // If nothing changed, return this; otherwise, construct with new-or-old arrays
    if ((newDefs eq null) && (newUses eq null) && (newArgs eq null)) this
    else new ENode(
      nodeType,
      if (newDefs eq null) _definitions else newDefs,
      if (newUses eq null) _uses else newUses,
      if (newArgs eq null) _args else newArgs
    )
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
    val distinctSlots = {
      val seen = scala.collection.mutable.LinkedHashSet[Slot]()
      var i = 0
      while (i < _definitions.length) { seen += _definitions(i); i += 1 }
      i = 0
      while (i < _uses.length) { seen += _uses(i); i += 1 }
      i = 0
      while (i < _args.length) { seen ++= _args(i).args.values; i += 1 }
      seen.toVector
    }
    val renamedSlots = SlotMap.fromPairs(distinctSlots.zipWithIndex.map { case (s, idx) => s -> Slot.numeric(idx) })
    ShapeCall(this.rename(renamedSlots), renamedSlots.inverse)
  }

  /**
   * Checks whether this node is already in canonical shape-normal form.
   *
   * @return True if equal to `asShapeCall.shape`; false otherwise.
   */
  def isShape: Boolean = this == asShapeCall.shape

  /**
   * Applies a function to each argument e-class call, returning a new node if any argument changes.
   *
   * If the function does not change any arguments, returns this node unchanged.
   *
   * @param f Function to apply to each argument e-class call.
   * @return A node identical to this one but with each argument replaced by `f(arg)`.
   */
  def mapArgs(f: EClassCall => EClassCall): ENode[NodeT] = {
    var newArgs: Array[EClassCall] = null
    var i = 0
    while (i < _args.length) {
      val newArg = f(_args(i))
      if (newArgs == null && newArg != _args(i)) {
        newArgs = new Array[EClassCall](_args.length)
        Array.copy(_args, 0, newArgs, 0, i)
      }
      if (newArgs != null) newArgs(i) = newArg
      i += 1
    }
    if (newArgs == null) return this

    new ENode(nodeType, _definitions, _uses, newArgs)
  }

  /**
   * Returns a copy of this node with the given argument e-class calls.
   *
   * If the new arguments are identical to the current ones, returns this node unchanged.
   *
   * @param newArgs New argument e-class calls.
   * @return A node identical to this one but with `newArgs` as its arguments.
   */
  def withArgs(newArgs: Seq[EClassCall]): ENode[NodeT] = {
    if (newArgs == args) return this

//    newArgs match {
//      case calls: ArraySeq[EClassCall] =>
//        new ENode(nodeType, _definitions, _uses, calls.unsafeArray.asInstanceOf[Array[EClassCall]])
//
//      case _ =>
//        new ENode(nodeType, _definitions, _uses, newArgs.toArray)
//    }
    new ENode(nodeType, _definitions, _uses, newArgs.toArray)
  }

  // --- case-class-like API preservation ---
  override def toString: String = s"ENode($nodeType, $definitions, $uses, $args)"

  override def equals(other: Any): Boolean = other match {
    case that: ENode[_] =>
      (this.nodeType == that.nodeType) &&
      this.definitions == that.definitions &&
      this.uses == that.uses &&
      this.args == that.args
    case _ => false
  }

  private lazy val _hash: Int = {
    var h = 1
    h = 31 * h + (if (nodeType == null) 0 else nodeType.hashCode)
    h = 31 * h + java.util.Arrays.hashCode(_definitions.asInstanceOf[Array[AnyRef]])
    h = 31 * h + java.util.Arrays.hashCode(_uses.asInstanceOf[Array[AnyRef]])
    h = 31 * h + java.util.Arrays.hashCode(_args.asInstanceOf[Array[AnyRef]])
    h
  }
  override def hashCode(): Int = _hash
}

/**
 * Constructors and helpers for [[ENode]].
 */
object ENode {
  def apply[NodeT](nodeType: NodeT, definitions: Seq[Slot], uses: Seq[Slot], args: Seq[EClassCall]): ENode[NodeT] = {
    new ENode(nodeType, definitions.toArray, uses.toArray, args.toArray)
  }

  def unapply[NodeT](n: ENode[NodeT]): Option[(NodeT, Seq[Slot], Seq[Slot], Seq[EClassCall])] =
    Some((n.nodeType, n.definitions, n.uses, n.args))

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
  def unslotted[NodeT](nodeType: NodeT, args: Seq[EClassCall]): ENode[NodeT] =
    new ENode(nodeType, Array.empty, Array.empty, args.toArray)

  private[eqsat] def renameSlotsOrNull(orig: Array[Slot], renaming: SlotMap): Array[Slot] = {
    var newArr: Array[Slot] = null
    var i = 0
    while (i < orig.length) {
      val mapped = renaming.apply(orig(i))
      if (newArr == null) {
        if (mapped != orig(i)) {
          newArr = java.util.Arrays.copyOf(orig, orig.length)
          newArr(i) = mapped
        }
      } else {
        newArr(i) = mapped
      }
      i += 1
    }
    newArr
  }
}
