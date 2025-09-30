package foresight.eqsat.hashCons.mutable

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable
import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.hashCons.{AbstractEClassData, PermutationGroup}
import foresight.eqsat.{EClassCall, ENode, ShapeCall}

/**
 * Mutable variant of EClassData with thread-safe, lazily cached projections.
 */
private[eqsat] final class MutableEClassData[NodeT](
                                                     initialSlots: SlotSet,
                                                     initialPermutations: PermutationGroup[SlotMap]
                                                   ) extends AbstractEClassData[NodeT] {

  // ----------------- Core mutable state (encapsulated) -----------------

  @volatile private var _slots: SlotSet = initialSlots
  @volatile private var _permutations: PermutationGroup[SlotMap] = initialPermutations

  private val _nodes: mutable.HashMap[ENode[NodeT], SlotMap] = mutable.HashMap.empty
  private val _users: mutable.HashSet[ENode[NodeT]] = mutable.HashSet.empty

  // Counter that remembers how many nodes with slots have been added.
  private var nodeWithSlotCounter: Int = 0

  // These hold the cached, materialized views
  private val appliedNodesCache =
    new AtomicReference[Seq[ShapeCall[NodeT]]](null)

  private val appliedNodesWithIdCache =
    new AtomicReference[Seq[ENode[NodeT]]](null)

  // ----------------- Public accessors -----------------

  /**
   * Get the slots of the e-class.
   * @return The slots of the e-class.
   */
  def slots: SlotSet = _slots

  /**
   * Get the e-class' slot permutations.
   * @return The slot permutations of the e-class.
   */
  def permutations: PermutationGroup[SlotMap] = _permutations

  /**
   * Get the nodes of the e-class, along with their renamings.
   * @return The nodes of the e-class and their renamings.
   */
  def nodes: collection.Map[ENode[NodeT], SlotMap] = _nodes

  /**
   * Get the e-nodes that take the e-class as an argument.
   * @return The e-nodes that take the e-class as an argument.
   */
  def users: collection.Set[ENode[NodeT]] = _users

  /**
   * Get the nodes of the e-class, along with their renamings.
   * @return The nodes of the e-class and their renamings.
   */
  def hasSlots: Boolean = {
    nodeWithSlotCounter > 0
  }

  // ----------------- Mutations (always bump version) -----------------

  /** Replace the slot set. */
  def setSlots(newSlots: SlotSet): Unit = {
    _slots = newSlots
    bumpVersion()
  }

  /** Replace permutations. */
  def setPermutations(newPerms: PermutationGroup[SlotMap]): Unit = {
    _permutations = newPerms
    bumpVersion()
  }

  /** Insert a node -> renaming mapping. */
  def addNode(node: ENode[NodeT], renaming: SlotMap): Unit = {
    _nodes.put(node, renaming) match {
      case Some(oldRen) if !oldRen.isEmpty =>
        // Replacing an existing node with slots: decrement the number of nodes with slots
        nodeWithSlotCounter -= 1
      case _ => // do nothing
    }
    if (!renaming.isEmpty) {
      // Adding a node with slots: increment the number of nodes with slots
      nodeWithSlotCounter += 1
    }
    bumpVersion()
  }

  /** Remove a node. */
  def removeNode(node: ENode[NodeT]): Unit = {
    _nodes.remove(node) match {
      case Some(ren) if !ren.isEmpty =>
        nodeWithSlotCounter -= 1
        bumpVersion()
      case _ => // do nothing
    }
  }

  /** Add a user e-node. */
  def addUser(enode: ENode[NodeT]): Unit = {
    // No need to bump version, users are not part of projections
    _users.add(enode)
  }

  /** Remove a user e-node. */
  def removeUser(enode: ENode[NodeT]): Unit = {
    // No need to bump version, users are not part of projections
    _users.remove(enode)
  }

  // ----------------- Lazily cached projections -----------------

  /**
   * Get the applied nodes.
   * @return The applied nodes.
   */
  def appliedNodes: Seq[ShapeCall[NodeT]] = {
    val cached = appliedNodesCache.get()
    if (cached ne null) return cached

    val computed = _nodes.map { case (node, ren) => ShapeCall(node, ren) }.toSeq

    appliedNodesCache.compareAndSet(null, computed)
    computed
  }

  /**
   * Get the applied nodes, but with identity renaming.
   * @return The applied nodes with identity renaming.
   */
  def appliedNodesWithIdentity: Seq[ENode[NodeT]] = {
    val cached = appliedNodesWithIdCache.get()
    if (cached ne null) return cached

    val mapping = SlotMap.identity(slots)
    val computed = appliedNodes.map(_.renamePartial(mapping).asNode)

    appliedNodesWithIdCache.compareAndSet(null, computed)
    computed
  }

  // ----------------- Internals -----------------

  private def bumpVersion(): Unit = {
    appliedNodesCache.set(null)
    appliedNodesWithIdCache.set(null)
  }

  /**
   * Get the applied nodes for a given canonical e-class call to this e-class.
   *
   * @param canonicalApp The canonical e-class call.
   * @return The applied nodes for the given e-class call.
   */
  override def appliedNodes(canonicalApp: EClassCall): Iterable[ENode[NodeT]] = {
    assert(canonicalApp.args.size == slots.size)

    if (hasSlots) {
      if (canonicalApp.args.isIdentity) {
        // Common case: the e-class call's arguments are the identity mapping.
        // We can return a precomputed set of applied nodes with identity renaming.
        appliedNodesWithIdentity
      } else {
        // E-class has slots and the e-class call's arguments are not the identity mapping.
        // We rename all applied nodes by the e-class call's arguments.
        appliedNodes.map(_.renamePartial(canonicalApp.args).asNode)
      }
    } else {
      // E-class has no slots: all nodes are the same regardless of the e-class call's arguments.
      nodes.keys
    }
  }

  /**
   * Get the applied nodes for a given canonical e-class call to this e-class, returning nodes of a specific type.
   *
   * @param canonicalApp The canonical e-class call.
   * @param nodeType     The type of nodes to return.
   * @return The applied nodes of type nodeType for the given e-class call.
   */
  override def appliedNodes(canonicalApp: EClassCall, nodeType: NodeT): Iterable[ENode[NodeT]] = {
    assert(canonicalApp.args.size == slots.size)

    if (hasSlots) {
      if (canonicalApp.args.isIdentity) {
        // Common case: the e-class call's arguments are the identity mapping.
        // We can return a precomputed set of applied nodes with identity renaming.
        appliedNodesWithIdentity.view.filter(_.nodeType == nodeType)
      } else {
        // E-class has slots and the e-class call's arguments are not the identity mapping.
        // We rename all applied nodes by the e-class call's arguments.
        appliedNodes.view.filter(_.shape.nodeType == nodeType).map(_.renamePartial(canonicalApp.args).asNode)
      }
    } else {
      // E-class has no slots: all nodes are the same regardless of the e-class call's arguments.
      nodes.keys.view.filter(_.nodeType == nodeType)
    }
  }
}
