package foresight.eqsat.extraction

import foresight.eqsat.{Slot, SlotMap}

/**
 * A cost-annotated extraction tree together with a total slot renaming.
 *
 * This pairs an [[ExtractionTree]] with a [[SlotMap]] that must map every slot
 * appearing in the tree (`tree.slotSet`) to a (possibly different) slot. The
 * renaming encodes alpha-renaming needed to place the tree in a caller's context.
 *
 * Invariants:
 *   - `renaming` is total over `tree.slotSet` (checked by the constructor assertion).
 *   - Renaming preserves structure: only slot identities change, not shape, cost, size, or depth.
 *
 * Immutability:
 *   - Methods return new values; neither the tree nor the renaming are modified in place.
 *
 * @param tree     The underlying, immutable extraction tree.
 * @param renaming A total mapping for all slots in `tree`.
 * @tparam NodeT   Node/operator type.
 * @tparam C       Cost type.
 */
final case class ExtractionTreeCall[+NodeT, C](tree: ExtractionTree[NodeT, C], renaming: SlotMap) {
  assert(tree.slotSet.forall(renaming.contains))

  /**
   * Cost of the subtree (independent of renaming).
   */
  def cost: C = tree.cost

  /**
   * Total node count (independent of renaming).
   */
  def size: Int = tree.size

  /**
   * Maximum depth (independent of renaming).
   */
  def depth: Int = tree.depth

  /**
   * All slots of the tree, in traversal order, after applying `renaming`.
   *
   * This sequence may contain duplicates if the same slot appears multiple times.
   */
  def slots: Seq[Slot] = tree.slots.map(renaming(_))

  /**
   * The deduplicated set of slots of the tree after applying `renaming`.
   */
  val slotSet: Set[Slot] = {
    tree.slotSet.map(renaming(_))
  }

  /**
   * Composes this call's renaming with an additional renaming defined on its image.
   *
   * If `this.renaming` maps original slots to intermediate slots, `rename(r)` expects
   * `r` to be defined for every intermediate slot (`this.renaming.valueSet`), ensuring
   * the composed mapping remains total. Composition is performed with
   * `composePartial`, which applies `r` where defined while retaining existing
   * mappings elsewhere.
   *
   * @param renaming A renaming defined on `this.renaming`'s image.
   * @return A new call with `this.renaming ∘ renaming` (applied to the image).
   */
  def rename(renaming: SlotMap): ExtractionTreeCall[NodeT, C] = {
    assert(this.renaming.valueSet.forall(renaming.contains))
    ExtractionTreeCall(tree, this.renaming.composePartial(renaming))
  }

  /**
   * Applies the current renaming to produce a plain renamed [[ExtractionTree]].
   *
   * This:
   *   - Rewrites the root's `definitions` and `uses`.
   *   - Composes the same renaming into each child call, leaving their costs intact.
   *   - Preserves `cost`, `size`, and `depth`.
   *
   * @return A structurally identical extraction tree with slots rewritten.
   */
  def applied: ExtractionTree[NodeT, C] = {
    val newDefinitions = tree.definitions.map(renaming.apply)
    val newUses = tree.uses.map(renaming.apply)
    val newArgs = tree.args.map(_.rename(renaming))
    ExtractionTree(tree.cost, tree.nodeType, newDefinitions, newUses, newArgs)
  }
}
