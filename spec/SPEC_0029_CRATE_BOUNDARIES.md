# SPEC_0029: Crate Boundaries as Collaboration Guardrails

## Status
ACCEPTED

## Summary

Crate boundaries serve as collaboration guardrails — hard compiler-enforced walls that make it safe for AI agents and new developers to work on one part of the compiler without accidentally breaking another.

## Motivation

- **AI agents (LLMs) have limited context windows and no persistent memory across sessions.** They cannot hold an entire monolithic codebase in context, and they forget architectural rules between conversations.
- **New developers don't know implicit architectural rules.** Convention-based boundaries ("don't import X from Y") are invisible and unenforceable.
- **Monolithic codebases allow plausible-but-wrong cross-cutting changes.** An AI or new contributor might reasonably add a helper function to an IR crate that performs evaluation, or add a phase dependency that creates a cycle.
- **Crate boundaries make illegal dependencies a compile error, not a code review finding.** The Rust compiler enforces the dependency graph defined in each crate's `Cargo.toml`. If a dependency isn't listed, it can't be used — period.

## Specification

### 1. Bounded Context Per Task

Each crate's `Cargo.toml` defines exactly what it can see. A contributor working on `rumoca-phase-flatten` only needs to understand its dependencies (`rumoca-core`, `rumoca-eval-flat`, `rumoca-ir-ast`, `rumoca-ir-flat`), not all workspace crates. The dependency list is the reading list.

### 2. Strict DAG Dependency Graph

No circular dependencies between crates. The dependency tiers form an acyclic graph enforced by the Rust compiler. See [Dependency Tiers](#dependency-tiers) below.

### 3. IR Crates Are Pure Data

`rumoca-ir-ast`, `rumoca-ir-flat`, and `rumoca-ir-dae` contain only data types, display/debug implementations, and serde serialization. No evaluation logic, no phase logic, no side effects. This prevents leaking behavior into data definitions and keeps IRs usable by any consumer without pulling in unwanted transitive dependencies.

### 4. Phase Typing via Newtypes

`ParsedTree`, `ResolvedTree`, `TypedTree`, and `InstancedTree` wrap `ClassTree`. The type system enforces phase ordering — you can't pass unresolved data to a phase that requires resolved data. This eliminates an entire class of pipeline-ordering bugs that would otherwise require runtime checks or careful documentation.

### 5. Evaluation Decoupled from Representation

Evaluation crates are aligned to IR ownership: `rumoca-eval-ast`, `rumoca-eval-flat`, and `rumoca-eval-dae`. This keeps evaluation entry points explicit per representation and avoids cross-layer helper crates that hide where behavior lives.

### 6. Rules for Adding Dependencies

Before adding a dependency from crate A to crate B:

1. **Verify no cycle** — The dependency must not create a circular dependency. Cargo will reject it, but check first to avoid wasted effort.
2. **Verify tier ordering** — Crate B must be in a lower or equal tier than crate A (see tier diagram). Dependencies flow downward.
3. **Consider alternatives** — Would a trait or shared type in `rumoca-core` be more appropriate than a direct dependency?
4. **Cross-tier dependencies are suspect** — If the dependency crosses multiple tiers (e.g., a binding crate importing an IR crate directly instead of going through session), it may indicate a design issue.

### 7. Rules for Creating New Crates

**Split when:**
- A new IR representation is needed (new data layer)
- A new compiler phase is introduced (new transformation)
- Consumers need data types without pulling in evaluation or phase logic
- Two unrelated concerns are growing in the same crate

**Keep together when:**
- The code is small (<500 lines) and has exactly one consumer
- Splitting would create a crate with no clear single responsibility
- The functionality is tightly coupled and always changes together

### 8. Import and Re-export Discipline

To keep layer boundaries obvious in code (not only in `Cargo.toml`), use explicit crate namespaces.

In non-IR crates:
- Import IR crates as namespaces:
  - `use rumoca_ir_ast as ast;`
  - `use rumoca_ir_flat as flat;`
  - `use rumoca_ir_dae as dae;`
- Prefer qualified references (`ast::...`, `flat::...`, `dae::...`) over direct type imports.
- Avoid direct IR type imports such as `use rumoca_ir_flat::{Expression, VarName}` outside the owning IR crate.

Re-export guardrails:
- Low-level compiler crates (`rumoca-ir-*`, `rumoca-phase-*`, `rumoca-eval-*`) MUST NOT
  re-export types from other Rumoca crates to tunnel around dependency boundaries.
- Do not add wildcard forwarding such as `pub use some_lower_layer_crate::*;` in low-level crates.
- Facade crates (for example `rumoca-session`) MAY re-export selected types intentionally to
  provide ergonomic top-level APIs, but those exports must stay curated and namespaced.

CI enforcement:
- Violations of the low-level re-export guardrails MUST fail CI.
- The workspace test `crates/rumoca/tests/architecture_hardening_test.rs::test_no_new_cross_crate_public_exports`
  is the enforcement gate for `pub use rumoca_*::...` and `pub type X = rumoca_*::...`
  in low-level crates.
- Any new low-level cross-crate export not explicitly approved in that test must be treated
  as a policy violation.

### 9. Session Facade Root API

`rumoca-session` is the orchestration facade crate for top-level entry points.
Its root API MUST stay minimal:

- Allowed root exports: `Session`, `SessionConfig`.
- Compile result and helper types remain under explicit namespaces such as `rumoca_session::compile::*`.
- Non-compile helper surfaces remain under explicit namespaces (`analysis`, `parsing`, `runtime`, `source_roots`, `project`).

CI enforcement:
- Violations MUST fail CI.
- The workspace test `crates/rumoca/tests/architecture_hardening_test.rs::test_session_root_facade_exports_are_minimal`
  enforces this root export policy.

### 10. Session-Owned Source-Root And Class-Graph State

`rumoca-session` owns IDE/runtime semantic state above the phase crates.

Required boundaries:

- Source-root membership, source-root status, and source-root cache hydration MUST live in
  `rumoca-session`.
- The incremental class graph and all derived namespace/package-membership views MUST live in
  `rumoca-session`.
- Workspace roots and imported roots are semantically identical source roots.
  Differences in cache retention or warm-restore policy are implementation details only.
- Clients MUST NOT implement their own semantic invalidation policy, cache ownership, subtree
  rebuild scope, or duplicate source-root planning logic.

Allowed client responsibilities:

- `rumoca-tool-lsp` may own transport, async lane selection, request cancellation, and progress
  delivery.
- `vscode` may own editor UI and user interaction only, through `rumoca-tool-lsp`.
- `rumoca-bind-wasm` and the `rumoca` CLI may adapt input/output and transport data to
  `rumoca-session`, but not re-implement source-root/class-graph policy.

Rationale:

- This keeps one incremental class-graph story from session through all clients.
- It prevents LSP, WASM, and CLI from drifting into separate cache/invalidation implementations.
- It keeps background reindex/cache-restore state available to all clients through one session-owned
  surface.

### 11. Session Persistence Boundary

`rumoca-session` MAY persist warm-restore state, but the persisted boundary MUST stop at
source-root-scoped AST/index state plus resolved aggregate inputs.

Persisted state MAY include:

- parsed-source-root cache files
- file summaries and declaration indexes
- package-membership / namespace aggregate state
- source-root resolved aggregate inputs such as:
  - model names
  - class dependency graphs
  - dependency fingerprints derived from resolved class graphs

Persisted state MUST NOT include typed, flat, or DAE artifacts by default.

Those tiers remain in-memory-only unless a later measured design change explicitly updates this spec.

Rationale:

- Typed/flat/DAE artifacts are target-scoped, larger, and more invalidation-sensitive than the
  source-root aggregate state above them.
- The current warm-restore goal is to avoid rebuilding front-end and resolved dependency inputs on
  reopen, not to serialize the full downstream compile pipeline.
- Once AST/index and resolved aggregate inputs are restored, downstream typed/flat/DAE caches can
  rebuild lazily behind existing dependency fingerprints inside the same process.
- This keeps persistence simple, source-root-scoped, and semantically uniform across workspace and
  imported roots.

### 12. Runtime, Backend, Stepper, And Visualization Layering

Runtime and simulation surfaces MUST not collapse back into one facade crate.

Required dependency direction:

```
compiler/session -> runtime contracts -> solver backend -> stepper -> reporting -> visualization
```

Required boundaries:

- `rumoca-session` owns compilation/session orchestration only.
- Runtime contract types used across simulation consumers MUST live below `rumoca-session`.
- Concrete solver backends MUST be explicit opt-in dependencies.
- Interactive stepper APIs MUST be separate from backend-neutral runtime contracts.
- Reporting payload contracts MUST be separate from visualization assets.
- Visualization crates MUST NOT own solver/backend policy.
- Transport-neutral lockstep I/O contracts MUST be separate from protocol codecs and app-specific
  controller/viewer loops.
- Protocol codec crates MUST NOT own simulation policy, controller/gamepad handling, HTTP serving,
  or scene assets.

Transitional rule:

- During migration, CI MUST NOT require `rumoca-session` to enable a specific solver backend
  feature transitively.
- During migration, CI MUST NOT require the runtime-contract crate to default-enable a concrete
  backend.

Steady-state rule:

- Once the split crates exist, CI MUST reject reverse dependencies across this chain.
- `rumoca-session` MUST NOT directly depend on concrete solver packages or visualization asset
  crates.
- Backend selection inputs exposed by user-facing APIs MUST affect runtime behavior, not only
  metadata or diagnostics.

## Dependency Tiers

The workspace crates are organized into six tiers. Dependencies flow strictly downward.

```
Tier 6 — Binary & Bindings (top-level entry points)
  rumoca                    CLI binary
  rumoca-bind-python        Python bindings (PyO3)
  rumoca-bind-wasm          WebAssembly bindings
  rumoca-contracts          Specification contract tests

Tier 5 — Integration (combine session + simulation/tools)
  rumoca-codec                 Transport-neutral lockstep I/O contracts
  rumoca-codec-flatbuffers              FlatBuffer schema/codec support for lockstep I/O
  rumoca-sim                Runtime contracts + shared simulation helpers
  rumoca-solver-diffsol        Diffsol runtime backend
  rumoca-solver-rk45           Explicit ODE RK45 backend
  rumoca-viz-web            Web visualization assets
  rumoca-tool-lsp           Language server protocol

Tier 4 — Orchestration (pipeline coordination)
  rumoca-session            Compilation pipeline orchestrator
  rumoca-tool-fmt           Code formatter
  rumoca-tool-lint          Linter

Tier 3 — Phases & Evaluation (transformations)
  rumoca-phase-parse        Source → AST
  rumoca-phase-resolve      Name resolution, DefId assignment
  rumoca-phase-typecheck    Type inference, variability
  rumoca-phase-instantiate  Class instantiation, modifier merging
  rumoca-phase-flatten      Instance tree → flat equations
  rumoca-phase-dae          Flat equations → DAE system
  rumoca-phase-solve   Structural analysis
  rumoca-phase-codegen      DAE → solver code
  rumoca-eval-ast           AST-level evaluation helpers
  rumoca-eval-flat          Flat-IR evaluation
  rumoca-eval-dae           DAE-IR runtime/compiled evaluation

Tier 2 — IR (pure data, no logic)
  rumoca-ir-ast             Syntax tree, class definitions
  rumoca-ir-flat            Flat equations, qualified names
  rumoca-ir-dae             DAE representation

Tier 1 — Foundation (shared primitives)
  rumoca-core               Spans, errors, config, traits

                    ┌─────────────────────┐
                    │  Tier 6: Binary &    │
                    │  Bindings            │
                    └────────┬────────────┘
                             │
                    ┌────────▼────────────┐
                    │  Tier 5: Integration │
                    └────────┬────────────┘
                             │
                    ┌────────▼────────────┐
                    │  Tier 4: Orchestr.   │
                    └────────┬────────────┘
                             │
                    ┌────────▼────────────┐
                    │  Tier 3: Phases &    │
                    │  Evaluation          │
                    └────────┬────────────┘
                             │
                    ┌────────▼────────────┐
                    │  Tier 2: IR (data)   │
                    └────────┬────────────┘
                             │
                    ┌────────▼────────────┐
                    │  Tier 1: Foundation  │
                    └─────────────────────┘
```

## How This Helps AI Collaboration

### Cargo.toml Is the Reading List

When an AI agent is asked to modify `rumoca-phase-flatten`, it reads `crates/rumoca-phase-flatten/Cargo.toml` and immediately knows the complete set of crates it needs to understand. There is no hidden coupling to discover.

### Compile Errors Catch Architectural Violations

If an AI agent adds `use rumoca_session::Session` inside a phase crate, the code won't compile. The violation is caught at build time, not during code review. This is critical because AI-generated code may be plausible-looking but architecturally wrong.

### Flat Functions Stay Within Context Windows

SPEC_0021 (Code Complexity) limits nesting depth and function length. Combined with crate boundaries, this means an AI agent can read an entire function and all of its dependencies without exceeding context limits.

### Phase-Typed Wrappers Eliminate Ordering Mistakes

The newtype wrappers (`ParsedTree`, `ResolvedTree`, etc.) mean an AI doesn't need to remember the correct pipeline ordering — the type system enforces it. Passing a `ParsedTree` where a `ResolvedTree` is expected is a type error.

### Serde IRs Enable Template-Based Codegen

The IR crates use serde serialization, so code generation templates work from data structures, not code. An AI can write a codegen template by inspecting the serialized IR format without understanding Rust compiler internals.

### Session Owns Compile-Side Codegen Access

Bindings, CLI export paths, and shared regression harnesses should call template render helpers
through `rumoca-session::codegen`. Direct `rumoca-phase-codegen` dependencies are reserved for
phase-local codegen tests, which keeps adapter surfaces on the compile/session side of the DAG.

## How This Helps New Developers

### Clear Onboarding Path

Start with one crate, understand its dependencies, expand outward. A new contributor can be productive in `rumoca-phase-parse` after reading only `rumoca-core` and `rumoca-ir-ast` — not the full workspace.

### Impossible to Accidentally Break Other Phases

Changes to `rumoca-phase-flatten` cannot affect `rumoca-phase-resolve` because there is no dependency path between them (they are siblings in the DAG, not parent-child). Crate boundaries guarantee blast radius.

### Single Responsibility Per Crate

Each crate has one clear job. When a bug is in "flattening," you look in `rumoca-phase-flatten`. When a type definition is wrong, you look in the appropriate IR crate. There is no ambiguity about where code belongs.

## Related Specs

- [SPEC_0009](SPEC_0009_COMMON_CRATE.md): Single Foundation Crate — defines the `rumoca-core` foundation layer
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md): Code Complexity Guidelines — flat functions complement crate boundaries by keeping individual functions within context window limits
- [SPEC_0023](SPEC_0023_CRATE_ARCHITECTURE.md): Crate Architecture for MLS Compliance — maps MLS specification sections to crates (complementary: SPEC_0023 is *what* maps where, SPEC_0029 is *why* the boundaries exist)
