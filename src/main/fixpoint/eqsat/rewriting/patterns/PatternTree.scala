package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.Slot

/**
 * A tree that represents a pattern.
 * @tparam NodeT The type of the nodes.
 */
sealed trait PatternTree[NodeT]

/**
 * A node in a pattern tree.
 * @param nodeType The type of the node.
 * @param definitions The slots that are defined directly by the node.
 * @param uses The slots that are used directly by the node.
 * @param children The children of the node.
 * @tparam NodeT The type of the node.
 */
final case class PatternNode[NodeT](nodeType: NodeT,
                                    definitions: Seq[Slot],
                                    uses: Seq[Slot],
                                    children: Seq[PatternTree[NodeT]]) extends PatternTree[NodeT]

/**
 * A variable in a pattern tree.
 * @tparam NodeT The type of the nodes.
 */
final class PatternVar[NodeT] extends PatternTree[NodeT]

/**
 * A substitution in a pattern tree.
 * @param body The body of the substitution.
 * @param from The variable that is substituted.
 * @param to The tree that is substituted for the variable.
 * @tparam NodeT The type of the nodes.
 */
final case class PatternSubstitution[NodeT](body: PatternTree[NodeT],
                                            from: PatternVar[NodeT],
                                            to: PatternTree[NodeT]) extends PatternTree[NodeT]
