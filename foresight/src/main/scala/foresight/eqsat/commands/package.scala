package foresight.eqsat

/**
 * Defines the command system for making batched, replayable edits to an [[EGraph]].
 *
 * A [[commands.Command]] is an immutable description of a single edit,
 * such as inserting one or more nodes ([[commands.AddManyCommand]]) or
 * merging e-classes ([[commands.UnionManyCommand]]).
 *
 * Commands are:
 *   - **pure values** that do not mutate an e-graph until [[commands.Command.apply]]
 *   - **self-describing**: they declare their required inputs ([[commands.Command.uses]])
 *     and any new virtual symbols they define ([[commands.Command.definitions]])
 *   - **optimizable**: they can simplify themselves for a given e-graph
 *     ([[commands.Command.simplify]])
 *
 * ## Symbols and Reification
 *
 * Commands refer to e-classes symbolically via [[commands.EClassSymbol]]:
 *   - [[commands.EClassSymbol.Real]] for existing classes
 *   - [[commands.EClassSymbol.Virtual]] for classes not yet in the graph
 *
 * When a command is applied, virtual symbols are mapped to real classes using
 * a **reification map** (`Map[Virtual, EClassCall]`). This allows command sequences
 * to be planned without knowing concrete IDs upfront.
 *
 * ## Batching and Composition
 *
 * Most real edits involve more than one command. The package provides:
 *
 *  - [[commands.CommandQueue]] — an immutable batch of commands that itself
 *    implements [[commands.Command]], so queues can be nested or chained.
 *
 *  - [[commands.CommandQueueBuilder]] — a mutable helper for incrementally building
 *    a queue without reassigning after each append.
 *
 * Queues can be:
 *   - **simplified**: removing redundant unions, skipping inserts for already-present nodes
 *   - **optimized**: merging independent commands for fewer graph rebuilds
 *
 * ## Common Command Types
 *
 *  - [[commands.AddManyCommand]] — inserts one or more [[commands.ENodeSymbol]]s,
 *    each paired with the [[commands.EClassSymbol.Virtual]] that will represent
 *    its resulting class.
 *
 *  - [[commands.UnionManyCommand]] — unifies multiple pairs of e-classes at once.
 *
 *  - [[commands.CommandQueue]] — sequences arbitrary commands and can itself
 *    be nested inside other queues.
 *
 * ## Example
 *
 * {{{
 * import foresight.eqsat.commands._
 *
 * val builder = new CommandQueueBuilder[MyNode]
 *
 * // Add a tree and get its root symbol
 * val root = builder.add(myTree)
 *
 * // Add a dependent node
 * val depNode = ENodeSymbol(op, Nil, Nil, Seq(root))
 * val depSym = builder.add(depNode)
 *
 * // Request a union of the two classes
 * builder.union(root, depSym)
 *
 * // Get the queue, optimize it, and apply it
 * val queue = builder.queue.optimized
 * val (maybeNewGraph, reif) = queue.apply(egraph, Map.empty, parallel)
 * }}}
 *
 * ## Lifecycle
 *
 *  1. **Build** — construct commands via `AddManyCommand`, `UnionManyCommand`,
 *     or convenience methods in [[commands.CommandQueue]] / [[commands.CommandQueueBuilder]].
 *  2. **Simplify** — run `simplify(egraph, partialMap)` to drop redundant work and
 *     pre-bind resolvable symbols.
 *  3. **Optimize** — call `optimized` on queues to merge unions and batch adds.
 *  4. **Apply** — run `apply(egraph, reification, parallelMap)` to produce an updated graph.
 */
package object commands
