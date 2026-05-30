# SPEC_0029: Crate Boundaries as Collaboration Guardrails

## Status
ACCEPTED

## Summary

Crate boundaries are compiler-enforced collaboration guardrails. A crate's
`Cargo.toml` is its reading list, and illegal coupling should fail at build or
architecture-test time rather than during review.

## Specification

### 1. Bounded Context Per Task

Each crate's `Cargo.toml` defines exactly what it can see. Contributors should
read that dependency set before editing the crate.

### 2. Strict DAG Dependency Graph

No circular dependencies between crates. Dependency tiers form an acyclic graph
enforced by the Rust compiler. See [Dependency Tiers](#dependency-tiers).

### 3. IR Crates Are Pure Data

`rumoca-ir-ast`, `rumoca-ir-flat`, and `rumoca-ir-dae` contain only data types,
display/debug implementations, and serde serialization. No evaluation logic, no
phase logic, no side effects. This keeps IRs usable without unwanted transitive
dependencies.

Allowed exception: IR crates MAY provide read-only traversal/query helpers over
their own data when those helpers have no side effects, do not evaluate
expressions, do not depend on phase crates, and do not encode backend policy.
Helpers needing modification environments, typechecking state, solver layout,
incidence data, or runtime state are not IR helpers.

IR crates MAY also provide rewrite-shape helpers that recursively rebuild their
own IR nodes without consulting semantic state. These helpers are limited to
structural ownership-preserving rewrites such as "visit every expression and
allow a caller-supplied replacement"; they MUST NOT perform name lookup,
constant evaluation, type inference, lowering, balance analysis, backend
selection, or runtime behavior. Keep read-only traversal/query helpers and
rewrite-shape helpers in separate modules so reviewers can see when code crosses
from observation into mutation.

### 3a. Foundation Types Live in rumoca-core

`rumoca-core` is the **sole** Tier 1 foundation crate. It owns shared IDs,
source locations, diagnostics/`PhaseError`, shared semantic IR vocabulary, and
small helpers used by multiple crates.

`SourceId`, `Span`, and `SourceMap` are owned by `rumoca-core`. `SourceId` is a
stable identity derived from a source name, not a source-map slot number. IR and
phase crates must carry `Span` values through transformations and must not add
sidecar span-rebasing helpers to compensate for unstable source ids.

`VarName` is the shared flattened-variable path identity. It MUST be interned
inside `rumoca-core` and expose a compact process-local `VarNameId` for
equality/hash-heavy paths, while preserving string display and serialization as
the stable external representation. Downstream crates must not add sidecar
variable-name interners or serialized ID compatibility layers; if a phase needs
symbol identity beyond `VarNameId`, it should introduce a phase-specific ID at
that phase boundary.

The `VarName` interner lifecycle is process-local and monotonic. Interned text
is retained until process or WASM worker teardown so `VarNameId` stays stable
for all in-process IR values that may still be referenced by caches, diagnostics,
or snapshots. There is intentionally no public reset API. Long-running hosts
must treat the process/worker/session host lifetime as the memory boundary, and
must serialize/display `VarName` as text rather than persisting `VarNameId`.

Do not create `rumoca-ir-core`, `rumoca-foundation`, or another micro-crate for
spans, diagnostics, IDs, or shared IR vocabulary without updating this spec.

IR-stage-specific types belong in the matching `rumoca-ir-*` crate. A type
belongs in `rumoca-core` only if referenced by multiple IR stages or by both an
IR crate and a phase crate.

### 3b. Single-Source Helpers Across the Pipeline

Helpers referenced from multiple crates **must** have exactly one designated
implementation.

| Helper(s) | Owner | Notes |
|---|---|---|
| `balance`, `balance_detail` | `rumoca-phase-dae::balance` | DAE equation/unknown balance arithmetic |
| `runtime_defined_unknown_names`, `runtime_defined_continuous_unknown_names` | `rumoca-phase-structural::runtime_defined` | Single implementation; phase-structural is the authoritative caller. |
| `expressions_semantically_equal`, `Expression::semantically_eq_ignoring_spans` | `rumoca-core` | Shared Flat/DAE expression identity. This is structural identity only; evaluation stays in `rumoca-eval-*`. |
| `INTERNAL_SAMPLE_FUNCTION_NAME`, `source_temporal_function_name`, `source_temporal_function_short_name`, `source_temporal_builtin_name` | `rumoca-core` | Single source for source temporal operator vocabulary shared by DAE and Solve boundary validation. |
| `expr_contains_var` | `rumoca-ir-dae::expr_query` | Handles every `Expression` variant |
| `expr_refers_to_var` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `expr_contains_der_of` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| Solver runtime time-event helpers (`event_right_limit_time`, scheduled/periodic time-event filtering, dynamic time-event parameter lookup) | `rumoca-solver::timeline` | Concrete solver backends call the shared runtime policy instead of copying time-grid or event-boundary rules. |
| Solver pre-parameter snapshot helpers (`write_pre_params_from_sources`, `update_slot`) | `rumoca-solver::runtime::pre_params` | Concrete solver backends call the shared pre-state write policy instead of copying `pre(...)` snapshot mechanics. |

Required rules:

- Every helper listed above has exactly one implementation, in the listed
  module. All callers MUST import from that module path.
- Do not fork helpers "for convenience." If the owning module creates a
  forbidden dependency, move the helper by spec update instead.
- Adding a new helper to this list requires updating this spec.

### 4. Phase Typing via Newtypes

`ParsedTree`, `ResolvedTree`, `TypedTree`, and `InstancedTree` wrap `ClassTree`.
The type system enforces phase ordering — you can't pass unresolved data to a
phase that requires resolved data. This eliminates an entire class of
pipeline-ordering bugs that would otherwise require runtime checks or careful
documentation.

The production compiled-model path is:

```text
ParsedTree -> ResolvedTree/ClassTree -> InstanceOverlay -> typecheck_instanced -> flat::Model -> Dae
```

`TypedTree` remains the artifact for the standalone resolved-tree typecheck API.
Model compilation uses post-instantiation type checking because modifier and
structural-parameter values are available only after instantiation.

### 5. Evaluation Decoupled from Representation

Evaluation crates are aligned to IR ownership: `rumoca-eval-ast`, `rumoca-eval-flat`, and `rumoca-eval-dae`. `rumoca-eval-solve` builds on DAE evaluation primitives for solver-facing row evaluation. This keeps evaluation entry points explicit per representation and avoids cross-layer helper crates that hide where behavior lives.

Phase crates MAY depend on the evaluation crate for the IR they are actively processing
when the phase needs compile-time evaluation of that representation. For example,
`rumoca-phase-flatten` may use `rumoca-eval-flat` for Flat-level constant and shape
evaluation instead of duplicating that logic inside the phase.

### 6. Rules for Adding Dependencies

Before adding a dependency from crate A to crate B:

1. No cycle.
2. Dependency target is lower or equal tier.
3. A `rumoca-core` trait/shared type would not be cleaner.
4. Cross-tier shortcuts are justified by spec, not convenience.

### 7. Rules for Creating New Crates

**Split when:** adding a new IR, compiler phase, data-only consumer surface, or
separating unrelated concerns. **Keep together when:** code is small, has one
consumer, or always changes as a unit.

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
- Non-facade crates MUST NOT re-export symbols from other Rumoca crates.
- Downstream crates import the owning crate directly; no intermediate routing.
- Wildcard forwarding is forbidden outside approved facades.
- Only approved facade crates MAY expose selected cross-crate API surfaces:

  | Facade | Scope | Allowed cross-crate exports |
  |---|---|---|
  | `rumoca-compile` | compilation/session | curated compile, parsing, codegen, analysis APIs |
  | `rumoca-sim` | simulation/runtime | solver/reporting runner APIs behind features |
  | `rumoca-codec` | transport-neutral lockstep I/O | `SignalFrame`, codec traits/factories, typed codec config |

  These exports stay curated, namespaced, and documented. CLI/bindings may
  depend on facades but must not add lower-layer forwarding surfaces.
- Root/foundation crates MUST NOT act as compatibility facades for moved symbols.
  If a primitive is owned by a crate, downstream code must import it from the
  owning crate, not via re-export through an intermediate crate. For example,
  `Span` is owned by `rumoca-core` (§3a) and must be imported as
  `use rumoca_core::Span`, not via a re-export through some other crate.

CI: `architecture_hardening_test::test_no_new_cross_crate_public_exports`
rejects `pub use rumoca_*::...` and `pub type X = rumoca_*::...` in non-facade
crates.

### 9. Session Facade Root API

`rumoca-compile` is the orchestration facade crate for top-level entry points.
Its root API MUST stay minimal:

- Allowed root exports: `Session`, `SessionConfig`.
- Compile result and helper types remain under explicit namespaces such as `rumoca_compile::compile::*`.
- Non-compile helper surfaces remain under explicit namespaces (`analysis`, `parsing`, `runtime`, `source_roots`, `project`).

CI enforcement:
- Violations MUST fail CI.
- The workspace test `crates/rumoca/tests/architecture_hardening_test.rs::test_session_root_facade_exports_are_minimal`
  enforces this root export policy.

### 10. Session-Owned Source-Root And Class-Graph State

`rumoca-compile` owns IDE/runtime semantic state above the phase crates so
LSP, WASM, and CLI cannot drift into separate cache/invalidation policies.

| Rule | Where | Why |
|---|---|---|
| Source-root membership, status, cache hydration live here | `rumoca-compile` | Single source of truth for project membership |
| Incremental class graph + namespace/package views live here | `rumoca-compile` | One incremental story across all clients |
| Workspace roots and imported roots are semantically identical | `rumoca-compile` | Retention/restore differ; semantics do not |
| Clients MUST NOT implement their own invalidation policy or rebuild scope | tool-lsp / bind-wasm / CLI | Avoid divergent cache stories |
| `rumoca-tool-lsp` owns transport, async, cancellation, progress | tool-lsp | Editor delivery, not compile semantics |
| `rumoca-bind-wasm` and the CLI adapt input/output only | bind-wasm / CLI | They are clients, not owners |

Session snapshots are the read-side IDE/binding boundary. They MUST be
detached from the mutable host revision, allow concurrent reads, and reserve
exclusive locking for snapshot creation or query-cache warming.

Dependency fingerprint caches are session-owned. Rebuilt class hashes/edges
invalidate changed classes plus the reverse dependency closure, not every
cached model fingerprint.

### 11. Session Persistence Boundary

`rumoca-compile` MAY persist warm-restore state, scoped to source-root AST/index
plus resolved aggregate inputs. Typed/flat/DAE artifacts are NOT persisted by
default — they rebuild lazily behind dependency fingerprints.

| Persisted (MAY) | Not persisted (MUST NOT) |
|---|---|
| parsed-source-root cache files | typed-tree artifacts |
| file summaries, declaration indexes | flat-IR artifacts |
| package-membership / namespace state | DAE-IR artifacts |
| model names, class dependency graphs, dependency fingerprints | solve-IR artifacts |

Rationale: the warm-restore goal is to skip rebuilding front-end and resolved
dependency inputs on reopen, not to serialize the full downstream pipeline.

### 12. Runtime, Backend, Stepper, And Visualization Layering

```
compiler/session → DAE structural → solve-IR lowering → runtime contracts → solver backend → stepper → reporting → visualization
```

| Rule | Owner | Why |
|---|---|---|
| Compilation/session orchestration | `rumoca-compile` | Pipeline coordination only; no runtime |
| DAE structural analysis (Pantelides, BLT, tearing, demotion) | `rumoca-phase-structural` | SPEC_0007 §Structural Transformation Scope |
| Solver-facing prepared data + row ops | `rumoca-ir-solve` | Backend-neutral execution IR |
| DAE → solve-IR lowering | `rumoca-phase-solve` | Lowering only, not structural mutation |
| Textual generated artifacts and templates | `rumoca-phase-codegen` | Jinja/minijinja rendering owns generated C, Rust, CUDA C, MLIR, FMI, and packaging text |
| Compiled/JIT execution adapter crates | `rumoca-exec-*` | Invoke tools, load artifacts, wrap Cranelift/LLVM/CUDA/NVRTC APIs, expose ergonomic runtime calls; no compiler semantics |
| Backend-neutral solver interface types | `rumoca-solver` | Single contract shared across backends |
| Concrete solver backends | `rumoca-solver-{diffsol,rk45,...}` | MUST consume solve-IR only; no DAE/phase deps |
| Simulation facade/runner | `rumoca-sim` | Composes solvers/reporting/viz behind features |
| Interactive stepper APIs | separate from runtime contracts | Stepper is one runtime mode, not THE runtime |
| Reporting payload contracts | separate from viz assets | Payload is data; viz is presentation |
| Visualization | `rumoca-viz-web` | MUST NOT own solver/backend policy |
| Transport-neutral lockstep I/O | `rumoca-codec` | Separate from protocol codecs |
| Protocol codecs (FlatBuffers, etc.) | `rumoca-codec-*` | No simulation, no controller, no HTTP, no scene |

Execution adapter crates are not compiler phases. Textual C/Rust/CUDA
C/MLIR/FMI artifacts are rendered by `rumoca-phase-codegen`; `rumoca-exec-*`
crates wrap tool invocation, ABI adaptation, loading, GPU/accelerator
integration, packaging, runtime compilation, or stable APIs over compiled
artifacts. Text-only targets stay in codegen. Non-codegen phase crates MUST NOT
depend on target encoder/JIT libraries such as `wasm-encoder`, Cranelift,
Inkwell, LLVM ORC bindings, CUDA Driver APIs, or NVRTC; backend bytecode,
native/JIT execution, runtime compilation, and device launch policy belong in
`rumoca-exec-*` or another backend-facing layer above the IR-lowering phase.

Target-language and target-format policy belongs in target manifests and
templates, not in Rust control flow. Rust code MAY provide generic target
manifest parsing, template rendering, safe path handling, schema validation, and
language-neutral feature probes over IR data. Rust code MUST NOT hard-code
target-language-specific capability decisions, file layouts, emitted language
names, or backend feature tables for textual targets such as C, Rust, CUDA C,
MLIR, FMI, or future custom targets. A new textual/codegen target should be
addable by providing a `target.toml` plus Jinja templates; any required
capability declaration or unsupported-feature contract must be expressed in that
manifest schema and enforced by generic manifest validation. Unsupported
manifest capability failures MUST report a stable
`unsupported-feature:<feature_id>` category derived from the manifest feature ID
so CI, MSL reports, and release compatibility summaries can aggregate capability
gaps without knowing the emitted target language.

JIT targets follow the same layering rule, but they are execution adapters
rather than textual template targets. Cranelift, LLVM ORC/Inkwell, CUDA
NVRTC/Driver, and browser WebAssembly module compilation are allowed only in
backend-facing execution crates or host bindings. They consume Solve IR or
generated artifacts through a stable execution ABI and must share the same
prepared-interpreter equivalence tests as concrete solver backends.

Steady-state CI rules: reject reverse dependencies across this chain;
`rumoca-compile` MUST NOT directly depend on concrete solver packages or
visualization asset crates; backend-selection user APIs MUST affect runtime
behavior, not only metadata.

## Dependency Tiers

The workspace crates are organized into six tiers. Dependencies flow strictly
downward.

```
Tier 6 — Binary & bindings: rumoca, bind-python, bind-wasm, contracts
Tier 5 — Integration/runtime: codec/input/solver/sim/viz/tool-lsp families
Tier 4 — Orchestration: rumoca-compile, tool-fmt, tool-lint
Tier 3 — Phases & evaluation: rumoca-phase-*, rumoca-eval-*
Tier 2 — IR data: rumoca-ir-*
Tier 1 — Foundation: rumoca-core

Tier 6 (Binary/Bindings) → Tier 5 (Integration) → Tier 4 (Orchestration) →
Tier 3 (Phases/Evaluation) → Tier 2 (IR data) → Tier 1 (Foundation).
Dependencies flow strictly downward.
```

Input boundary rule:

- `rumoca-input` owns abstract input identifiers, config compilation, local state, and signal
  mapping only. It MUST NOT depend on `rumoca-input-gamepad`, `rumoca-input-keyboard`, `gilrs`, or
  `crossterm`.
- Concrete input adapters depend on `rumoca-input` and translate native device events into
  `rumoca-input` snapshots/events.
- Higher-level facades such as `rumoca-sim` MAY compose input adapters behind explicit optional
  runner features, but adapter dependencies MUST remain opt-in.

Simulation composition rule:

- Simulation applications are data/config composition, not per-vehicle framework code.
- `rumoca-sim` and CLI MAY wire axes from config; app-specific signal names,
  routes, controller conventions, and viewer keys stay in examples/config/assets.
- The durable simulation axes are separate crate families:
  - `rumoca-codec` and codec implementations own logical signal-frame encoding.
  - Transport crates own bytes-on-the-wire movement.
  - Solver crates own numerical integration backends.
  - Input crates own abstract input state and native device adapters.
  - Visualization crates own HTTP/viewer assets only.
- Coupled and standalone modes share compiler/solver contracts; loop policy is runtime behavior.
- Configured signal references MAY read compiled model values, local input state,
  runtime counters, and constants. The signal-reference language must stay in the
  simulation/config layer and MUST NOT leak into compiler IR.

## Related Specs

- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — maintainability and deterministic-collection rules.
