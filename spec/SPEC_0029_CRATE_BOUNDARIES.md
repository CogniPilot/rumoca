# SPEC_0029: Crate Boundaries as Collaboration Guardrails

## Status
ACCEPTED

## Summary

Crate boundaries are compiler-enforced guardrails. A crate's `Cargo.toml` is
its reading list; illegal coupling should fail before review.

## Specification

### 1. Bounded Context Per Task

`Cargo.toml` defines what each crate can see. Read dependencies before editing.

### 2. Strict DAG Dependency Graph

No circular dependencies. Dependency tiers form an acyclic graph enforced by the
Rust compiler. See [Dependency Tiers](#dependency-tiers).

### 3. IR Crates Are Pure Data

`rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`, and `rumoca-ir-solve`
contain only data types, display/debug implementations, and serde
serialization. No evaluation logic, phase logic, or side effects.

IR data types own the checked constructors needed to make their local
invariants unrepresentable. `rumoca-ir-dae` also owns private current-version
wire decoding, checked root assembly, and closed root-bound operations that
atomically rebuild invariant-related objects. These are data-integrity APIs,
not semantic analysis: phase crates decide which transformation is valid and
supply its typed proof/input. DAE exposes no public whole-root validator,
unchecked builder, mutable partition callback, or invariant-bearing child
`Deserialize` implementation.

DAE construction enters through `Dae::construct`. Its generatively branded,
sequential semantic-owner closures share one expression arena; expression
insertion requires `expr.at(provenance)` and cannot allocate source-free nodes.

Allowed exception: IR crates MAY provide read-only traversal/query helpers over
their own data when those helpers have no side effects, do not evaluate
expressions, depend on phase crates, or encode backend policy.
Helpers needing modification environments, typechecking state, solver layout,
incidence data, or runtime state are not IR helpers.

IR crates MAY provide rewrite-shape helpers that recursively rebuild their own
IR nodes without semantic state. These helpers are limited to structural
ownership-preserving rewrites such as "visit every expression and allow a
caller-supplied replacement"; they MUST NOT perform name lookup, constant
evaluation, type inference, lowering, balance analysis, backend selection, or
runtime behavior. A multi-object DAE rewrite must be a consuming, root-bound,
non-cloneable checked operation so catalog and expression identity change
atomically. Keep read-only traversal/query helpers and rewrite-shape helpers
separate so reviewers can see observation versus mutation.

### 3a. Foundation Types Live in rumoca-core

`rumoca-core` is the **sole** Tier 1 foundation crate. It owns shared IDs,
source locations, diagnostics/`PhaseError`, shared semantic IR vocabulary, and
small shared helpers.

`SourceId`, `Span`, and `SourceMap` are owned by `rumoca-core`. `SourceId` is a
stable identity derived from a source name, not a source-map slot number. IR and
phase crates must carry `Span` values through transformations and must not add
span-rebasing sidecars.

`VarName` is the shared flattened-variable path identity. It MUST be interned
inside `rumoca-core` and expose compact process-local `VarNameId` for
equality/hash-heavy paths, while preserving string display and serialization as
the stable external representation. Downstream crates must not add sidecar
variable-name interners or serialized ID compatibility layers; if a phase needs
symbol identity beyond `VarNameId`, introduce a phase-specific ID at that
boundary.

The `VarName` interner is process-local, monotonic, and has no public reset.
Hosts serialize/display text, never process-local `VarNameId`.

Do not create `rumoca-ir-core`, `rumoca-foundation`, or another micro-crate for
spans, diagnostics, IDs, or shared IR vocabulary without a spec update.

IR-specific types stay in their matching crate; shared multi-stage vocabulary
belongs in `rumoca-core`.

### 3b. Single-Source Helpers Across the Pipeline

Shared helpers **must** have one designated implementation.

| Helper(s) | Owner | Notes |
|---|---|---|
| `balance`, `balance_detail` | `rumoca-phase-dae::balance` | DAE equation/unknown balance arithmetic |
| `runtime_defined_unknown_names`, `runtime_defined_continuous_unknown_names` | `rumoca-phase-structural::runtime_defined` | Single implementation; phase-structural is the authoritative caller. |
| `expressions_semantically_equal`, `Expression::semantically_eq_ignoring_spans` | `rumoca-core` | Shared Flat/DAE expression identity. This is structural identity only; evaluation stays in `rumoca-eval-*`. |
| `modelica_sign`, `escape_modelica_string` | `rumoca-core` | MLS `sign` and source-string escaping. |
| UTF-8 byte offset ↔ UTF-16 text position/range helpers | `rumoca-core::text_position` | Protocol-neutral `TextPosition`/`TextRange`; LSP crates convert to/from `lsp_types` locally. No `rumoca-lsp-position` micro-crate. |
| `INTERNAL_SAMPLE_FUNCTION_NAME`, `source_temporal_function_name`, `source_temporal_function_short_name`, `source_temporal_builtin_name` | `rumoca-core` | Single source for source temporal operator vocabulary shared by DAE and Solve boundary validation. |
| `expr_contains_var` | `rumoca-ir-dae::expr_query` | Handles every `Expression` variant |
| `expr_refers_to_var` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `expr_contains_der_of` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| Solver runtime time-event helpers (`event_right_limit_time`, scheduled/periodic time-event filtering, dynamic time-event parameter lookup) | `rumoca-solver::timeline` | Shared time-grid rules. |
| Solver runtime event-boundary helpers (`process_runtime_event_boundary`, `runtime_event_horizon`, `runtime_root_event_application_time`, `RuntimeEventBoundaryHandler`) | `rumoca-solver::runtime::event` | Backends hook row application/state reset; MLS event-boundary policy stays shared. |
| Solver zero-state orchestration helpers (`run_no_state_output_schedule`, `NoStateOrchestrationBackend`, `NoStateEventStep`) | `rumoca-solver::runtime::no_state` | Backends hook row/root/event; output/event-loop policy stays shared. |
| Solver pre-parameter snapshot helpers (`write_pre_params_from_sources`, `update_slot`, `commit_pre_params_after_event`) | `rumoca-solver::runtime::pre_params` | Shared `pre(...)` snapshot mechanics. |
| Solver algebraic projection helpers (`project_algebraics`, `project_algebraics_and_detect_changes`, `project_initial_*`) | `rumoca-solver::runtime::projection` | Backends supply residual/JVP via `AlgebraicProjectionModel`; projection/change-detection policy stays shared. |
| Solve runtime state machine and backend-neutral simulation driver (`SolveRuntime`, `simulate_state_targets`, event/discrete row application, algebraic settle, Jacobian/sensitivity reports) | `rumoca-solver::runtime::{solve_runtime,driver}` | Backends adapt via `SolverAdvanceBackend`; no backend-specific tracing namespaces. |

Required rules:

- Each helper has exactly one implementation in its listed module; callers MUST
  import that path.
- Do not fork helpers. If ownership creates a forbidden dependency, move it by
  spec update.
- List additions require a spec update.

### 4. Phase Typing via Newtypes

`ParsedTree`, `ResolvedTree`, `TypedTree`, and `InstancedTree` wrap `ClassTree`.
The type system enforces phase ordering: you can't pass unresolved data to a
phase that requires resolved data. This eliminates pipeline-ordering bugs that
would otherwise require runtime checks or careful documentation.

The production compiled-model path is:

```text
ParsedTree -> ResolvedTree/ClassTree -> InstanceOverlay -> typecheck_instanced -> flat::Model -> Dae
```

`TypedTree` remains the artifact for the standalone resolved-tree typecheck API.
Model compilation uses post-instantiation type checking because modifier and
structural-parameter values are available only after instantiation.

### 5. Evaluation Decoupled from Representation

Evaluation crates are aligned to IR ownership: `rumoca-eval-ast`, `rumoca-eval-flat`, and `rumoca-eval-dae`. `rumoca-eval-solve` is the Tier 3 solver-facing row evaluator/scalarizer, including its own tensor-kernel selection, and MUST NOT depend on a Tier 4/5 crate; the runtime state machine and simulation driver live in `rumoca-solver::runtime`. This keeps evaluation entry points explicit per representation and avoids cross-layer helper crates that hide where behavior lives.

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
  | `rumoca-sim` | simulation/runtime | solver/reporting/scheduling APIs behind features |
  | `rumoca-codec` | transport-neutral lockstep I/O | `SignalFrame`, codec traits/factories, typed codec config |

  These exports stay curated, namespaced, and documented. CLI/bindings may
  depend on facades but must not add lower-layer forwarding surfaces.
- Root/foundation crates MUST NOT act as compatibility facades for moved symbols.
  If a primitive is owned by a crate, downstream code must import it from the
  owning crate, not via re-export through an intermediate crate.

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

### 12. Runtime, Backend, Simulation Session, And Visualization Layering

```
compiler/session → DAE structural → solve-IR lowering → runtime contracts → solver backend → simulation session → reporting → visualization
```

| Rule | Owner | Why |
|---|---|---|
| Compilation/session orchestration | `rumoca-compile` | Pipeline coordination only; no runtime |
| DAE structural analysis (Pantelides, BLT, tearing, demotion) | `rumoca-phase-structural` | SPEC_0007 §Structural Transformation Scope |
| Solver-facing prepared data + row ops | `rumoca-ir-solve` | Backend-neutral execution IR |
| DAE → solve-IR lowering | `rumoca-phase-solve` | Lowering only, not structural mutation |
| Optimization/training orchestration | `rumoca-opt` | Consumes Solve/eval APIs; no Modelica semantics |
| DAE/Solve → checked GALEC lowering | `rumoca-phase-galec` | Semantic export lowering and admissibility only; no text, templates, packaging, or target-language helpers |
| Checked GALEC executable semantics | `rumoca-eval-galec` | Small explicit interpreter over `rumoca-ir-galec`; no DAE/Solve, lowering, rendering, target, or runtime-host dependencies |
| Textual generated artifacts and templates | `rumoca-phase-codegen` | Jinja/minijinja rendering owns generated C, Rust, CUDA C, MLIR, FMI/eFMI and FMU/eFMU packaging text |
| GALEC `.alg` text | `rumoca-phase-codegen` | MiniJinja renders a checked GALEC semantic view; the language IR owns no text emitter (SPEC_0034 GAL-009) |
| eFMI packaging XML (`__content.xml`, manifests) | `rumoca-phase-codegen` | Rendered like FMI `modelDescription`; validators + generic checksum/container build step, not typed serializers (SPEC_0034 D3 amended) |
| Compiled/JIT execution adapter crates | `rumoca-exec-*` | Invoke tools, load artifacts, wrap Cranelift/LLVM/CUDA/NVRTC APIs, expose ergonomic runtime calls; no compiler semantics |
| Backend-neutral solver interface types | `rumoca-solver` | Single contract shared across backends |
| Concrete solver backends | `rumoca-solver-{diffsol,rk45,...}` | MUST consume solve-IR only; no DAE/phase deps |
| Simulation facade | `rumoca-sim` | Composes solvers/reporting/viz behind features |
| Simulation session APIs | separate from runtime contracts | Simulation sessions are the scheduled runtime surface |
| Reporting payload contracts | separate from viz assets | Payload is data; viz is presentation |
| Browser visualization assets | `packages/rumoca-web` | Frontend source/deps; no solver/backend policy |
| Transport-neutral lockstep I/O | `rumoca-codec` | Separate from protocol codecs |
| Protocol codecs (FlatBuffers, etc.) | `rumoca-codec-*` | No simulation, no controller, no HTTP, no scene |

Execution adapters are not phases. Non-codegen phases must not depend on target
encoders, JITs, toolchains, or device APIs. Textual target policy lives in
`target.toml` and templates; Rust provides generic rendering, validation, and
IR capability probes. Unsupported capabilities report
`unsupported-feature:<feature_id>`. JIT/device adapters consume Solve IR or
generated artifacts through stable execution ABIs and equivalence tests.

Each target manifest selects one proven-valid canonical or checked export IR.
`rumoca-phase-codegen` exposes a typed, read-only semantic view for each
supported IR and dispatches that view generically. Rendering never performs a
compiler transformation or repairs an artifact. Adding another target over an
existing view MUST require no Rust change; adding support for another IR adds
only its target-neutral semantic view and capability vocabulary. An export IR
selectable by a target remains outside the canonical compiler pipeline.

`rumoca-phase-codegen` Rust may derive target-neutral typed contexts, schedules,
shapes, dependency/bounds proofs, symbols, and provenance. It MUST NOT spell or
assemble target-language tokens, expressions, statements, declarations, or
files. Those belong entirely to each target's `target.toml` and MiniJinja
templates, so adding a textual target does not require a Rust dialect or
renderer. Generic template operations consume semantic IR vocabulary and fail
closed; they do not return pre-rendered language fragments.

Target-specific semantic lowering is a compiler phase, not code generation.
`rumoca-phase-codegen/src` MUST NOT contain target-named subsystems such as
`galec/`, C lowering, XML manifest models, target manglers, or target dispatch.
It MAY contain small IR-specific adapters under `views/` when they expose only
typed, read-only semantic data. Checked export data and constructors belong to
their `rumoca-ir-*` crate; semantic projection belongs to its
`rumoca-phase-*` crate; all target syntax and presentation belong to the target
directory.

Within `rumoca-phase-codegen`, `src/codegen/` is reserved for the public
MiniJinja extension-command surface. Rendering orchestration belongs in generic
renderer modules and IR adapters belong under `src/views/`. Every registered
command MUST be pure, deterministic, target-neutral, fail closed, and have
documented template syntax, typed inputs/outputs, failure behavior, complexity,
and focused tests. A single registry is the source of truth for registration
and user-facing command documentation. Commands may return semantic values or
checked arithmetic/query results; they MUST NOT return target-language
fragments or perform lowering, name resolution, type repair, target dispatch,
file assembly, or escaping for a particular output language.

Architecture CI MUST reject production `rumoca-phase-codegen` Rust that builds
generated or template-context text with formatting, concatenation, replacement,
writer, or incremental string-assembly APIs. Diagnostic messages and generic
template/file transport are the only string-handling exceptions; their values
must not enter semantic template contexts. Target names are rendered from typed
identity/path segments in templates, not pre-mangled Rust strings.

Steady-state CI rejects reverse dependencies across this chain. `rumoca-compile`
MUST NOT depend on concrete solvers or visualization assets; backend-selection
APIs MUST affect runtime behavior, not only metadata.

## Dependency Tiers

Workspace crates use six tiers. Dependencies flow downward.

```
Tier 6 — Binary & bindings: rumoca, bind-python, bind-wasm, contracts
Tier 5 — Integration/runtime: codec/input/solver/sim/opt/viz/tool-lsp families
Tier 4 — Orchestration: rumoca-compile, tool-fmt, tool-lint
Tier 3 — Phases & evaluation: rumoca-phase-*, rumoca-eval-*
Tier 2 — IR data: rumoca-ir-*
Tier 1 — Foundation: rumoca-core
```

Input boundary:

- `rumoca-input` owns abstract input identifiers, config compilation, local
  state, and signal mapping only. It MUST NOT depend on concrete adapters or
  native device crates such as `gilrs` or `crossterm`.
- Concrete adapters depend on `rumoca-input` and translate device events.
- Facades MAY compose input adapters behind opt-in scheduling/input features.

Simulation composition:

- Simulation apps are data/config composition, not per-vehicle framework code.
- `rumoca-sim` and CLI MAY wire axes from config; app-specific signal names,
  routes, controller conventions, and viewer keys stay in examples/config/assets.
- Durable simulation axes are separate crate families:
  - `rumoca-codec` and codec implementations own logical signal-frame encoding.
  - Transport crates own bytes-on-the-wire movement.
  - Solver crates own numerical integration backends.
  - Input crates own abstract input state and native device adapters.
  - Browser packages own HTTP/viewer assets and npm locks; Rust crates MAY
    serve prepared assets, but MUST NOT build frontend packages.
- Coupled and standalone modes share compiler/solver contracts; loop policy is runtime.
- Configured signal references MAY read compiled model values, local input state,
  runtime counters, and constants. The signal-reference language must stay in the
  simulation/config layer and MUST NOT leak into compiler IR.

## Related Specs

- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — maintainability and deterministic-collection rules.
