package fixpoint.eqsat.rewriting.patterns

import fixpoint.eqsat.Slot

/**
 * A pattern that represents a tree of nodes, variables, and substitutions. The pattern is used to match against
 * e-classes in e-graphs.
 * @tparam NodeT The type of the nodes in the pattern.
 */
sealed trait Pattern[NodeT]

/**
 * A node in a pattern.
 * @param nodeType The type of the node.
 * @param definitions The slots that are defined directly by the node.
 * @param uses The slots that are used directly by the node.
 * @param children The children of the node.
 * @tparam NodeT The type of the node.
 */
final case class PatternNode[NodeT](nodeType: NodeT,
                                    definitions: Seq[Slot],
                                    uses: Seq[Slot],
                                    children: Seq[Pattern[NodeT]]) extends Pattern[NodeT]

/**
 * A variable in a pattern.
 * @tparam NodeT The type of the nodes.
 */
final class PatternVar[NodeT] extends Pattern[NodeT]

/**
 * A substitution in a pattern.
 * @param body The body of the substitution.
 * @param from The variable that is substituted.
 * @param to The tree that is substituted for the variable.
 * @tparam NodeT The type of the nodes.
 */
final case class PatternSubstitution[NodeT](body: Pattern[NodeT],
                                            from: PatternVar[NodeT],
                                            to: Pattern[NodeT]) extends Pattern[NodeT]
