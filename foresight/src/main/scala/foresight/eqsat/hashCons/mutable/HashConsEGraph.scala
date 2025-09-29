package foresight.eqsat.hashCons.mutable

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassCall, EClassRef, ENode, ShapeCall}
import foresight.eqsat.hashCons.{AbstractMutableHashConsEGraph, EClassData, PermutationGroup}
import foresight.util.Debug

import scala.collection.mutable

private[eqsat] final class HashConsEGraph[NodeT] extends AbstractMutableHashConsEGraph[NodeT] {
  protected override val unionFind: SlottedUnionFind = new SlottedUnionFind()
  private val hashCons: mutable.HashMap[ENode[NodeT], EClassRef] = mutable.HashMap.empty
  private val classData: mutable.HashMap[EClassRef, EClassData[NodeT]] = mutable.HashMap.empty

  protected override def updateClassPermutations(ref: EClassRef, permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData(ref)
    classData.update(ref, data.copy(permutations = permutations))
  }

  protected override def updateClassSlotsAndPermutations(ref: EClassRef,
                                                         slots: SlotSet,
                                                         permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData(ref)
    classData.update(ref, data.copy(slots = slots, permutations = permutations))
  }

  override def dataForClass(ref: EClassRef): EClassData[NodeT] = classData(ref)
  override def isCanonical(ref: EClassRef): Boolean = unionFind.isCanonical(ref)
  override def canonicalizeOrNull(ref: EClassRef): EClassCall = unionFind.findAndCompressOrNull(ref)
  override def classes: Iterable[EClassRef] = classData.keys
  protected override def shapes: Iterable[ENode[NodeT]] = hashCons.keys
  override def nodeToRefOrElse(node: ENode[NodeT], default: => EClassRef): EClassRef = hashCons.getOrElse(node, default)

  protected override def createEmptyClass(slots: SlotSet): EClassRef = {
    val ref = new EClassRef()
    unionFind.add(ref, slots)

    val data = EClassData[NodeT](slots, Map.empty, PermutationGroup.identity(SlotMap.identity(slots)), Set.empty)
    classData.put(ref, data)

    ref
  }

  /**
   * Adds a node to an e-class. The node is added to the hash cons, the class data, and the argument e-classes' users.
   *
   * @param ref  The reference to the e-class.
   * @param node The node to add.
   */
  protected override def addNodeToClass(ref: EClassRef, node: ShapeCall[NodeT]): Unit = {
    // Set the node in the hash cons, update the class data and add the node to the argument e-classes' users.
    val data = classData(ref)
    hashCons.put(node.shape, ref)
    classData.update(ref, data.copy(nodes = data.nodes + (node.shape -> node.renaming)))
    classData ++= node.shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users + node.shape)
    })
  }

  /**
   * Removes a node from an e-class. The node is removed from the hash cons, the class data, and the argument e-classes'
   * users.
   *
   * @param ref   The reference to the e-class.
   * @param shape The node to remove.
   */
  protected override def removeNodeFromClass(ref: EClassRef, shape: ENode[NodeT]): Unit = {
    if (Debug.isEnabled) {
      assert(shape.isShape)
    }

    val data = classData(ref)
    hashCons.remove(shape)
    classData.update(ref, data.copy(nodes = data.nodes - shape))
    classData ++= shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users - shape)
    })
  }

  /**
   * Unlinks all empty e-classes from the class data map. An e-class is considered empty if it has no nodes.
   */
  protected override def unlinkEmptyClasses(): Unit = {
    val emptyClasses = classData.collect { case (ref, data) if data.nodes.isEmpty => ref }
    emptyClasses.foreach(ref => classData.remove(ref))
  }

  override def emptied: this.type = {
    new HashConsEGraph[NodeT]().asInstanceOf[this.type]
  }
}

private[eqsat] object HashConsEGraph {
  def empty[NodeT]: HashConsEGraph[NodeT] = new HashConsEGraph[NodeT]()
}
