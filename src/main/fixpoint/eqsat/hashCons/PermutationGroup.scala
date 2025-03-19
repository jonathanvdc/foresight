package fixpoint.eqsat.hashCons

import fixpoint.eqsat.Slot

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * A group of permutations.
 * @param identity The identity permutation.
 * @param next The next permutation in the group.
 * @tparam P The type of the permutations in the group.
 */
final case class PermutationGroup[P <: Permutation[P]](identity: P, next: Option[NextPermutation[P]]) {
  /**
   * Check if the group is trivial. A group is trivial if it only contains the identity permutation.
   * @return True if the group is trivial.
   */
  def isTrivial: Boolean = next.isEmpty

  def orbit(s: Slot): Set[Slot] = {
    PermutationGroup.buildOt(s, identity, generators).keys.toSet
  }

  private def generatorsImpl: Set[P] = next match {
    case None => Set.empty
    case Some(n) => n.ot.values.toSet ++ n.g.generatorsImpl
  }

  def generators: Set[P] = {
    generatorsImpl - identity
  }

  def allPerms: Set[P] = next match {
    case None => Set(identity)
    case Some(n) =>
      val left = n.ot.values.toSet
      val right = n.g.allPerms
      val out = for {
        l <- left
        r <- right
      } yield r.compose(l)
      out
  }

  @tailrec
  def contains(p: Permutation[P]): Boolean = next match {
    case None => p.iterator.forall { case (x, y) => x == y }
    case Some(n) =>
      n.ot.get(p(n.stab)) match {
        case None => false
        case Some(part) => n.g.contains(p.compose(part.inverse))
      }
  }

  def add(p: P): PermutationGroup[P] = tryAdd(p).getOrElse(this)

  def tryAdd(p: P): Option[PermutationGroup[P]] = tryAddSet(Set(p))

  def tryAddSet(perms: Set[P]): Option[PermutationGroup[P]] = {
    val newPerms = perms.filterNot(p => contains(p))
    if (newPerms.nonEmpty) {
      val newGroup = PermutationGroup(identity, generators ++ newPerms)
      Some(this.copy(identity = newGroup.identity, next = newGroup.next))
    } else {
      None
    }
  }

  def count: Int = next match {
    case None => 1
    case Some(n) => n.ot.size * n.g.count
  }
}

final case class NextPermutation[P <: Permutation[P]](stab: Slot, ot: Map[Slot, P], g: PermutationGroup[P])

object NextPermutation {
  def apply[P <: Permutation[P]](stab: Slot, identity: P, generators: Set[P]): NextPermutation[P] = {
    val ot = PermutationGroup.buildOt(stab, identity, generators)
    val newGenerators = PermutationGroup.schreiersLemma(stab, ot, generators)
    val g = PermutationGroup(identity, newGenerators)
    NextPermutation(stab, ot, g)
  }
}

object PermutationGroup {
  def apply[P <: Permutation[P]](identity: P, generators: Set[P]): PermutationGroup[P] = {
    this(identity, PermutationGroup.findLowestNonstab(generators).map(s => NextPermutation(s, identity, generators)))
  }

  def identity[P <: Permutation[P]](identity: P): PermutationGroup[P] = PermutationGroup(identity, None)

  private[hashCons] def buildOt[P <: Permutation[P]](stab: Slot, identity: P, generators: Set[P]): Map[Slot, P] = {
    val ot = mutable.Map(stab -> identity)
    var len = 0
    while (len != ot.size) {
      len = ot.size
      for {
        g <- generators
        (_, v) <- ot.clone()
      } {
        val newPerm = v.compose(g)
        val target = newPerm(stab)
        if (!ot.contains(target)) {
          ot(target) = newPerm
        }
      }
    }
    ot.toMap
  }

  private[hashCons] def schreiersLemma[P <: Permutation[P]](stab: Slot, ot: Map[Slot, P], generators: Set[P]): Set[P] = {
    val out = mutable.Set[P]()
    for {
      (_, r) <- ot
      s <- generators
    } {
      val rs = r.compose(s)
      val rs2Inv = ot(rs(stab)).inverse
      out += rs.compose(rs2Inv)
    }
    out.toSet
  }

  private[hashCons] def findLowestNonstab[P <: Permutation[P]](generators: Set[P]): Option[Slot] = {
    val found = generators.flatMap(_.iterator.collect { case (x, y) if x != y => x })
    if (found.isEmpty) {
      None
    } else {
      Some(found.min)
    }
  }
}
