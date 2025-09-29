package foresight.eqsat.hashCons.immutable

import foresight.eqsat.{EClassCall, EClassRef, ENode, ShapeCall}
import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.hashCons.{AbstractMutableHashConsEGraph, EClassData, PermutationGroup}
import foresight.util.Debug

private final class HashConsEGraphBuilder[NodeT](protected override val unionFind: SlottedUnionFindBuilder,
                                                 private var hashCons: Map[ENode[NodeT], EClassRef],
                                                 private var classData: Map[EClassRef, EClassData[NodeT]])
  extends AbstractMutableHashConsEGraph[NodeT] {

  def result(): HashConsEGraph[NodeT] = {
    new HashConsEGraph(unionFind.toImmutable, hashCons, classData)
  }

  override def classes: Iterable[EClassRef] = classData.keys

  protected override def shapes: Iterable[ENode[NodeT]] = hashCons.keys

  override def canonicalizeOrNull(ref: EClassRef): EClassCall = {
    unionFind.findAndCompressOrNull(ref)
  }

  override def isCanonical(ref: EClassRef): Boolean = {
    unionFind.isCanonical(ref)
  }

  override def nodeToRefOrElse(node: ENode[NodeT], default: => EClassRef): EClassRef = {
    hashCons.getOrElse(node, default)
  }

  override def dataForClass(ref: EClassRef): EClassData[NodeT] = {
    classData(ref)
  }

  protected override def updateClassPermutations(ref: EClassRef, permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData(ref)
    classData = classData + (ref -> data.copy(permutations = permutations))
  }

  protected override def updateClassSlotsAndPermutations(ref: EClassRef,
                                                         slots: SlotSet,
                                                         permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData(ref)
    classData = classData + (ref -> data.copy(slots = slots, permutations = permutations))
  }

  /**
   * Creates an empty e-class with the given slots. The e-class is added to the union-find and the class data map.
   * @param slots The slots of the e-class.
   * @return The reference to the new e-class.
   */
  protected override def createEmptyClass(slots: SlotSet): EClassRef = {
    val ref = new EClassRef()
    unionFind.add(ref, slots)

    val data = EClassData[NodeT](slots, Map.empty, PermutationGroup.identity(SlotMap.identity(slots)), Set.empty)
    classData = classData + (ref -> data)

    ref
  }

  protected override def addNodeToClass(ref: EClassRef, node: ShapeCall[NodeT]): Unit = {
    // Set the node in the hash cons, update the class data and add the node to the argument e-classes' users.
    val data = classData(ref)
    hashCons = hashCons + (node.shape -> ref)
    classData = classData + (ref -> data.copy(nodes = data.nodes + (node.shape -> node.renaming)))
    classData = classData ++ node.shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users + node.shape)
    })
  }

  /**
   * Removes a node from an e-class. The node is removed from the hash cons, the class data, and the argument e-classes'
   * users.
   * @param ref The reference to the e-class.
   * @param shape The node to remove.
   */
  protected override def removeNodeFromClass(ref: EClassRef, shape: ENode[NodeT]): Unit = {
    if (Debug.isEnabled) {
      assert(shape.isShape)
    }

    // Remove the node from the hash cons, update the class data and remove the node from the argument e-classes' users.
    val data = classData(ref)
    hashCons = hashCons - shape
    classData = classData + (ref -> data.copy(nodes = data.nodes - shape))
    classData = classData ++ shape.args.map(_.ref).distinct.map(c => {
      val argData = classData(c)
      c -> argData.copy(users = argData.users - shape)
    })
  }

  protected override def unlinkEmptyClasses(): Unit = {
    classData = classData.filterNot(_._2.nodes.isEmpty)
  }

  override def emptied: this.type = {
    new HashConsEGraphBuilder[NodeT](
      new SlottedUnionFindBuilder(Map.empty),
      Map.empty,
      Map.empty).asInstanceOf[this.type]
  }
}
