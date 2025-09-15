package foresight.eqsat

/**
 * A canonicalized e-node paired with a renaming from its canonical slots back to the original slots.
 *
 * A shape is the name-independent normal form of an [[ENode]]: all slots that occur in the node are replaced
 * by numbered slots in lexicographic order (`$0`, `$1`, ...). The `renaming` then maps those canonical slots back
 * to the original (caller-context) slots so that applying it to `shape` reconstructs the original node.
 *
 * Typical construction is via `ENode.asShapeCall`, which guarantees that:
 *   - `shape` is in canonical slot form (all slots numeric, in order).
 *   - `renaming` maps each canonical numeric slot to the original slot it replaced.
 *
 * This decomposition underpins hash-consing and equality modulo renaming: equal-up-to-renaming nodes share
 * the same `shape` and differ only in the `renaming`.
 *
 * @param shape    The canonical e-node (slots are numeric and ordered).
 * @param renaming A map from the shape's numeric slots to the original slots. Applying it to `shape` yields the
 *                 original node.
 * @tparam NodeT   The domain-specific node type.
 *
 * @example
 * {{{
 * // Start with a non-canonical node `n` that uses arbitrary slots:
 * val n: ENode[Op] = ...
 *
 * // Decompose into canonical shape and a renaming back to the original slots:
 * val sc: ShapeCall[Op] = n.asShapeCall
 *
 * // `sc.shape` has numeric slots (`$0`, `$1`, ...), and applying `sc.renaming` reconstructs `n`:
 * assert(sc.asNode == n)
 *
 * // To retarget the original slots to fresh ones:
 * val freshen = SlotMap.bijectionFromSetToFresh(sc.asNode.slotSet)
 * val scFresh = sc.rename(freshen)
 * val nFresh  = scFresh.asNode
 * }}}
 */
final case class ShapeCall[+NodeT](shape: ENode[NodeT], renaming: SlotMap) {

  /** The node type carried by the shape. */
  def nodeType: NodeT = shape.nodeType

  /**
   * The shape interpreted back into the original slot space using `renaming`.
   *
   * Equivalent to `shape.rename(renaming)`.
   */
  def asNode: ENode[NodeT] = shape.rename(renaming)

  /** Definitions of `asNode` (after applying the renaming). */
  def definitions: Seq[Slot] = asNode.definitions

  /** Uses of `asNode` (after applying the renaming). */
  def uses: Seq[Slot] = asNode.uses

  /** Arguments of `asNode` (after applying the renaming). */
  def args: Seq[EClassCall] = asNode.args

  /**
   * Post-compose the current renaming with another mapping.
   *
   * Composition follows `other ∘ current`: first apply the existing `renaming`, then the provided mapping.
   * Concretely, this updates the mapping from the shape's numeric slots to a new target slot space.
   *
   * @param renaming The additional renaming to apply after the current one.
   * @return A `ShapeCall` with `this.renaming` composed with the provided mapping.
   */
  def rename(renaming: SlotMap): ShapeCall[NodeT] =
    ShapeCall(shape, this.renaming.compose(renaming))

  /**
   * Partial post-composition of the current renaming with another mapping.
   *
   * Like `rename`, but only applies to entries whose image appears as a key in the provided mapping; others are kept.
   * Useful for projecting into a smaller slot space.
   *
   * @param renaming The partial mapping to apply after the current one.
   * @return A `ShapeCall` with partially composed renaming.
   */
  def renamePartial(renaming: SlotMap): ShapeCall[NodeT] = {
    if (this.renaming.isEmpty) return this

    ShapeCall(shape, this.renaming.composePartial(renaming))
  }
}
