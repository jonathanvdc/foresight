# Foresight

Foresight is a Scala library for **equality saturation**, a technique that enables compilers and program transformers to explore many semantically equivalent rewrites of a program simultaneously.
By separating the application of rewrite rules from the decision‑making process, equality saturation allows for more principled and comprehensive optimization.

Whereas existing equality saturation frameworks such as [egg](https://egraphs‑good.github.io/egg/) rely on mutable state and sequential rule execution, Foresight introduces a new architectural model built around **immutability**, **parallelism**, and **extensibility**.
Its purely functional design makes it well‑suited to modern compiler infrastructure, particularly in the context of functional languages.

Foresight’s architecture includes an immutable, slotted e‑graph representation with native support for bound variables, a parallel rewriting pipeline, and a modular strategy interface.
These innovations enable safe speculative rewriting, efficient parallel execution, and flexible rewrite control.

---

## Features

### Immutable, Thread‑Safe E‑Graphs
Foresight’s e‑graphs are built using purely functional data structures. Every operation returns a new, structurally consistent e‑graph, eliminating the need for mutable state or a rebuild phase. This makes reasoning about transformations simpler and enables speculative or parallel execution without side effects.

### Slotted E‑Graphs with Native Binding Support
E‑classes are parameterized by [slots](https://dl.acm.org/doi/10.1145/3729326), a mechanism that encodes variable binding directly in the e‑graph. This allows Foresight to correctly and efficiently unify expressions that differ only in variable names, making it ideal for functional intermediate representations with bound variables (e.g., lambdas).

### Parallel Rule Matching and Application
Rewriting in Foresight is divided into four fine‑grained stages: pattern matching, command generation, simplification, and graph mutation. The first three stages are embarrassingly parallel, and the final mutation stage benefits from deferred command batching and optimized application.

### Metadata‑Aware Analyses
Users can attach domain‑specific metadata (such as types, constants, or cost estimates) to e‑classes. Metadata is updated in parallel and remains consistent with the evolving graph. This enables type‑safe rewriting, cost‑guided extraction, and analysis‑informed rule application.

### Composable Strategy Interface
Rewrite control is decoupled from the engine using a modular strategy interface. Users can define custom saturation strategies with iteration limits, timeouts, stopping conditions, or metadata integration. Built‑in strategy combinators make it easy to experiment with complex workflows declaratively.

### Cost‑Guided Extraction
Foresight includes a built‑in analysis that tracks the lowest‑cost term in each equivalence class using a user‑defined cost model. Extraction is incremental and can occur during or after saturation, supporting efficient selection of optimized expressions.

### Configurable Parallel Execution
All parallel behavior in Foresight is implemented via a pluggable `ParallelMap` interface. Users can select or implement custom concurrency backends for fine‑grained control over resource usage, instrumentation, or cancellation behavior.
