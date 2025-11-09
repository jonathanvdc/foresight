package foresight.eqsat.hashCons.mutable

import foresight.eqsat.collections.{SlotMap, SlotSet}
import foresight.eqsat.{EClassRef, ENode}
import foresight.eqsat.hashCons.{AbstractMutableHashConsEGraph, PermutationGroup}
import foresight.util.Debug
import it.unimi.dsi.fastutil.Hash
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap
import it.unimi.dsi.fastutil.ints.{Int2ObjectOpenHashMap, IntArrayList}

private[eqsat] final class HashConsEGraph[NodeT] extends AbstractMutableHashConsEGraph[NodeT] {
  type ClassData = MutableEClassData[NodeT]
  type UnionFind = SlottedUnionFind

  protected override val unionFind: SlottedUnionFind = new SlottedUnionFind()
  private val hashCons = new Object2IntOpenHashMap[ENode[NodeT]](16, Hash.VERY_FAST_LOAD_FACTOR)
  private val classData = new Int2ObjectOpenHashMap[MutableEClassData[NodeT]]()
  hashCons.defaultReturnValue(EClassRef.Invalid.id)

  protected override def updateClassPermutations(ref: EClassRef, permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData.get(ref.id)
    data.setPermutations(permutations)
  }

  protected override def updateClassSlotsAndPermutations(ref: EClassRef,
                                                         slots: SlotSet,
                                                         permutations: PermutationGroup[SlotMap]): Unit = {
    val data = classData.get(ref.id)
    data.setSlots(slots)
    data.setPermutations(permutations)
  }

  override def dataForClass(ref: EClassRef): MutableEClassData[NodeT] = classData.get(ref.id)
  override def classes: Iterable[EClassRef] = new Iterable[EClassRef] {
    override def iterator: Iterator[EClassRef] = new Iterator[EClassRef] {
      private val it = classData.keySet().iterator()
      override def hasNext: Boolean = it.hasNext
      override def next(): EClassRef = EClassRef(it.nextInt())
    }
  }

  protected override def shapes: Iterable[ENode[NodeT]] = new Iterable[ENode[NodeT]] {
    override def iterator: Iterator[ENode[NodeT]] = new Iterator[ENode[NodeT]] {
      private val it = hashCons.keySet().iterator()
      override def hasNext: Boolean = it.hasNext
      override def next(): ENode[NodeT] = it.next()
    }
  }

  override def nodeToRefOrElse(node: ENode[NodeT], default: => EClassRef): EClassRef = {
    val id = hashCons.getInt(node)
    if (id != EClassRef.Invalid.id) EClassRef(id) else default
  }

  protected override def createEmptyClass(slots: SlotSet): EClassRef = {
    val ref = unionFind.add(slots)
    val data = new MutableEClassData[NodeT](slots, PermutationGroup.identity(SlotMap.identity(slots)))
    classData.put(ref.id, data)
    ref
  }

  protected override def addNodeToClass(ref: EClassRef, shape: ENode[NodeT], renaming: SlotMap): Unit = {
    // Set the node in the hash cons, update the class data and add the node to the argument e-classes' users.
    val data = classData.get(ref.id)
    hashCons.put(shape, ref.id)
    data.addNode(shape, renaming)

    val argsArray = shape.unsafeArgsArray
    var i = 0
    while (i < argsArray.length) {
      val arg = argsArray(i)
      val argData = classData.get(arg.ref.id)
      argData.addUser(shape)
      i += 1
    }
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

    val data = classData.get(ref.id)
    hashCons.removeInt(shape)
    data.removeNode(shape)

    val argsArray = shape.unsafeArgsArray
    var i = 0
    while (i < argsArray.length) {
      val arg = argsArray(i)
      val argData = classData.get(arg.ref.id)
      argData.removeUser(shape)
      i += 1
    }
  }

  /**
   * Unlinks all empty e-classes from the class data map. An e-class is considered empty if it has no nodes.
   */
  protected override def unlinkEmptyClasses(): Unit = {
    val it = classData.int2ObjectEntrySet().fastIterator()
    val toRemove = new IntArrayList()
    while (it.hasNext) {
      val e = it.next()
      if (e.getValue.nodes.isEmpty) toRemove.add(e.getIntKey)
    }
    val rit = toRemove.iterator()
    while (rit.hasNext) classData.remove(rit.nextInt())
  }

  override def emptied: this.type = {
    new HashConsEGraph[NodeT]().asInstanceOf[this.type]
  }
}

private[eqsat] object HashConsEGraph {
  def empty[NodeT]: HashConsEGraph[NodeT] = new HashConsEGraph[NodeT]()
}
