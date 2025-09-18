package foresight.eqsat.extraction

import foresight.eqsat.collections.SlotSet
import foresight.eqsat.{Slot, Tree}

/**
 * Immutable, cost-annotated tree used by extraction analyses.
 *
 * An `ExtractionTree` represents a concrete, fully expanded expression
 * rooted at `nodeType`, annotated with:
 *   - `cost` — precomputed total cost for the subtree, according to a
 *     [[CostFunction]].
 *   - `definitions` — slots bound/defined locally at this node.
 *   - `uses` — slots referenced at this node but defined elsewhere.
 *   - `args` — already-costed children, each an [[ExtractionTreeCall]].
 *
 * The cost type `C` is abstract; it may be numeric, tuple-based, or any
 * other ordered type usable for extraction ordering.
 *
 * @param cost         Total cost of this subtree (root + all descendants).
 * @param nodeType     Operator/type of the root node.
 * @param definitions  Slots introduced by this node.
 * @param uses         Slots consumed by this node that are defined externally.
 * @param args         Costed children, in evaluation order.
 * @tparam NodeT       The node/operator type.
 * @tparam C           The cost type.
 */
final case class ExtractionTree[+NodeT, C](
                                            cost: C,
                                            nodeType: NodeT,
                                            definitions: Seq[Slot],
                                            uses: Seq[Slot],
                                            args: Seq[ExtractionTreeCall[NodeT, C]]
                                          ) {

  /**
   * Total number of nodes in this tree, including the root.
   *
   * Computed as: `1 + sum(child sizes)`.
   */
  val size: Int = args.foldLeft(1)((acc, child) => acc + child.size)

  /**
   * Maximum depth of this tree (root has depth 1).
   *
   * Computed as: `1 + max(child depths)`. If there are no children,
   * the depth is 1.
   */
  val depth: Int = args.foldLeft(0)((maxDepth, child) => math.max(maxDepth, child.depth)) + 1

  /**
   * All slots appearing in this tree, in traversal order:
   * definitions first, then uses, then recursively from children.
   *
   * This sequence does not deduplicate slots.
   */
  def slots: Seq[Slot] =
    definitions ++ uses ++ args.flatMap(_.slots)

  private def slottedComponentCount: Int = {
    var count = 0
    if (definitions.nonEmpty) count += 1
    if (uses.nonEmpty) count += 1
    for (child <- args) {
      if (child.slotSet.nonEmpty) count += 1
    }
    count
  }

  private def buildSlotSet: SlotSet = {
    // First count the number of components with non-empty slot sets.
    val count = slottedComponentCount

    // Common cases: no slots in components or only one component has slots.
    if (count == 0) {
      SlotSet.empty
    } else if (count == 1) {
      if (definitions.nonEmpty) {
        SlotSet.from(definitions)
      } else if (uses.nonEmpty) {
        SlotSet.from(uses)
      } else {
        // Find the single child with a non-empty slot set.
        for (child <- args) {
          if (child.slotSet.nonEmpty) return child.slotSet
        }

        throw new IllegalStateException("Invariant violation: counted one slotted component but found none")
      }
    } else {
      // General case: build a set that unions all components.
      val results = Array.newBuilder[Slot]
      results ++= definitions
      results ++= uses
      for (child <- args) {
        results ++= child.slotSet.unsafeArray
      }
      SlotSet.fromUnsortedMutableArrayUnsafe(results.result())
    }
  }

  /**
   * All unique slots appearing in this tree (deduplicated, as a set).
   *
   * Includes slots defined locally and in descendants.
   */
  val slotSet: SlotSet = buildSlotSet

  /**
   * Converts this cost-annotated tree into a plain [[Tree]] by:
   *   - Recursively applying each child's renaming.
   *   - Discarding cost annotations.
   *
   * @return A plain expression tree with the same structure and slots.
   */
  def toTree: Tree[NodeT] = {
    val newArgs = args.map(_.applied.toTree)
    Tree(nodeType, definitions, uses, newArgs)
  }
}
