# SPEC_0041: Crate Ownership Catalog

## Status
REFERENCE

## Summary

Lookup catalog of single-source helper owners, session-owned state, session
persistence, and layer ownership referenced by
[SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md).

## How To Use This Catalog

This annex holds no rules of its own. Every row below is a SPEC_0029 ownership
assignment and is **normative by reference from SPEC_0029**; the owning section
in SPEC_0029 states the governing requirement and links here. Adding, moving,
or removing a row is a spec change, not an implementation detail.

## Specification

### 1. Single-Source Helper Catalog (SPEC_0029 §3b)

Each helper has exactly one implementation in its listed module; callers MUST
import that path.

| Helper(s) | Owner | Notes |
|---|---|---|
| `balance`, `balance_detail` | `rumoca-phase-dae::balance` | DAE equation/unknown balance arithmetic |
| `runtime_defined_unknown_names`, `runtime_defined_continuous_unknown_names` | `rumoca-phase-structural::runtime_defined` | Single implementation; phase-structural is the authoritative caller. |
| `expressions_semantically_equal`, `Expression::semantically_eq_ignoring_spans` | `rumoca-core` | Shared Flat/DAE expression identity. This is structural identity only; evaluation stays in `rumoca-eval-*`. |
| `modelica_sign`, `escape_modelica_string` | `rumoca-core` | MLS `sign` and source-string escaping. |
| `dependency_first_sccs`, `DependencyScc` | `rumoca-core::dependency_graph` | Deterministic, iterative dependency-first SCC decomposition shared by checked recursive-owner construction. |
| `InstanceId` | `rumoca-core` | Compact concrete-occurrence identity shared by Instanced and Flat reference vocabulary; source declarations continue to use `DefId`. |
| UTF-8 byte offset ↔ UTF-16 text position/range helpers | `rumoca-core::text_position` | Protocol-neutral `TextPosition`/`TextRange`; LSP crates convert to/from `lsp_types` locally. No `rumoca-lsp-position` micro-crate. |
| `INTERNAL_SAMPLE_FUNCTION_NAME`, `source_temporal_function_name`, `source_temporal_function_short_name`, `source_dae_forbidden_builtin_name` | `rumoca-core` | Single source for typed source temporal/synchronous operator vocabulary shared by DAE and Solve boundary validation. |
| `expr_contains_var` | `rumoca-ir-dae::expr_query` | Handles every `Expression` variant |
| `expr_refers_to_var` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `expr_contains_der_of` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `DaeView::record_field_layout`, `RecordFieldLayout` | `rumoca-ir-dae::model::view` | Read-only element-major packing query shared by explicit DAE evaluation and Solve scalar-projection boundaries; compact record arrays remain owned by DAE-IR. |
| Solver runtime time-event helpers (`event_right_limit_time`, scheduled/periodic time-event filtering, dynamic time-event parameter lookup) | `rumoca-solver::timeline` | Shared time-grid rules. |
| Solver runtime event-boundary helpers (`process_runtime_event_boundary`, `runtime_event_horizon`, `runtime_root_event_application_time`, `RuntimeEventBoundaryHandler`) | `rumoca-solver::runtime::event` | Backends hook row application/state reset; MLS event-boundary policy stays shared. |
| Solver zero-state orchestration helpers (`run_no_state_output_schedule`, `NoStateOrchestrationBackend`, `NoStateEventStep`) | `rumoca-solver::runtime::no_state` | Backends hook row/root/event; output/event-loop policy stays shared. |
| Solver pre-parameter snapshot helpers (`write_pre_params_from_sources`, `update_slot`, `commit_pre_params_after_event`) | `rumoca-solver::runtime::pre_params` | Shared `pre(...)` snapshot mechanics. |
| Solver algebraic projection helpers (`project_algebraics`, `project_algebraics_and_detect_changes`, `project_initial_*`) | `rumoca-solver::runtime::projection` | Backends supply residual/JVP via `AlgebraicProjectionModel`; projection/change-detection policy stays shared. |
| Solve runtime state machine and backend-neutral simulation driver (`SolveRuntime`, `simulate_state_targets`, event/discrete row application, algebraic settle, Jacobian/sensitivity reports) | `rumoca-solver::runtime::{solve_runtime,driver}` | Backends adapt via `SolverAdvanceBackend`; no backend-specific tracing namespaces. |
| MSL parity observation-grid policy (`msl_sim_output_dt`, `MSL_SIM_OUTPUT_INTERVALS`) | `rumoca-worker` | A valid Modelica experiment interval owns the grid; otherwise Rumoca uses the same scale-invariant uniform base grid as the OMC oracle. Solver event instants remain additional output points. |

### 2. Session-Owned Source-Root And Class-Graph Catalog (SPEC_0029 §10)

| Rule | Where | Why |
|---|---|---|
| Source-root membership, status, cache hydration live here | `rumoca-compile` | Single source of truth for project membership |
| Portable source-root cache parsing/serialization lives here | `rumoca-compile` | `xtask` may invoke the compiler-owned command but MUST remain orchestration-only with no Rumoca workspace dependency; no documentation-tool micro-crate |
| Incremental class graph + namespace/package views live here | `rumoca-compile` | One incremental story across all clients |
| Workspace roots and imported roots are semantically identical | `rumoca-compile` | Retention/restore differ; semantics do not |
| Clients MUST NOT implement their own invalidation policy or rebuild scope | tool-lsp / bind-wasm / CLI | Avoid divergent cache stories |
| `rumoca-tool-lsp` owns transport, async, cancellation, progress | tool-lsp | Editor delivery, not compile semantics |
| `rumoca-bind-wasm` and the CLI adapt input/output only | bind-wasm / CLI | They are clients, not owners |

### 3. Session Persistence Catalog (SPEC_0029 §11)

| Persisted (MAY) | Not persisted (MUST NOT) |
|---|---|
| parsed-source-root cache files | typed-tree artifacts |
| file summaries, declaration indexes | flat-IR artifacts |
| package-membership / namespace state | DAE-IR artifacts |
| model names, class dependency graphs, dependency fingerprints | solve-IR artifacts |

### 4. Layering Ownership Catalog (SPEC_0029 §12)

| Rule | Owner | Why |
|---|---|---|
| Compilation/session orchestration | `rumoca-compile` | Pipeline coordination only; no runtime |
| DAE structural analysis (Pantelides, BLT, tearing, demotion) | `rumoca-phase-structural` | SPEC_0007 §Structural Lowering Scope |
| Solver-facing prepared data + row ops | `rumoca-ir-solve` | Backend-neutral execution IR |
| DAE → solve-IR lowering | `rumoca-phase-solve` | Lowering only, not structural mutation |
| Optimization/training orchestration | `rumoca-opt` | Consumes Solve/eval APIs; no Modelica semantics |
| GALEC `.alg` → checked GALEC parsing | `rumoca-phase-parse-galec` | Recoverable syntax state stays out of checked IR |
| DAE/Solve → checked GALEC lowering | `rumoca-phase-galec` | Semantic export lowering and admissibility only; no text, templates, packaging, or target-language helpers |
| Checked GALEC executable semantics | `rumoca-eval-galec` | Small explicit interpreter over `rumoca-ir-galec`; no DAE/Solve, lowering, rendering, target, or runtime-host dependencies |
| Executable reference semantics for differential validation | `rumoca-reference` | Independent definitional interpreter of the Modelica event core; MUST carry no production dependency on any `rumoca-*` crate, because a reference that imported the compiler would agree with it by construction. Compiler deps are dev-only, for the differential harness. Optimizing it is a defect (SPEC_0037 verification track) |
| Textual generated artifacts and templates | `rumoca-phase-codegen` | Jinja/minijinja rendering owns generated C, Rust, CUDA C, MLIR, FMI/eFMI and FMU/eFMU packaging text |
| GALEC `.alg` text | `rumoca-phase-codegen` | MiniJinja renders a checked GALEC semantic view; the language IR owns no text emitter (SPEC_0034 GAL-009) |
| eFMI packaging XML (`__content.xml`, manifests) | `rumoca-phase-codegen` | Rendered like FMI `modelDescription`; validators + generic checksum/container build step, not typed serializers (SPEC_0042 D3 amended) |
| Compiled/JIT execution adapter crates | `rumoca-exec-*` | Invoke tools, load artifacts, wrap Cranelift/LLVM/CUDA/NVRTC APIs, expose ergonomic runtime calls; no compiler semantics |
| Backend-neutral solver interface types | `rumoca-solver` | Single contract shared across backends |
| Concrete solver backends | `rumoca-solver-{diffsol,rk45,...}` | MUST consume solve-IR only; no DAE/phase deps |
| Simulation facade | `rumoca-sim` | Composes solvers/reporting/viz behind features |
| Simulation session APIs | separate from runtime contracts | Simulation sessions are the scheduled runtime surface |
| Reporting payload contracts | separate from viz assets | Payload is data; viz is presentation |
| Browser visualization assets | `packages/rumoca-web` | Frontend source/deps; no solver/backend policy |
| Transport-neutral lockstep I/O | `rumoca-codec` | Separate from protocol codecs |
| Protocol codecs (FlatBuffers, etc.) | `rumoca-codec-*` | No simulation, no controller, no HTTP, no scene |

### 5. Input and Simulation Composition Catalog (SPEC_0029 §Dependency Tiers)

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

## References

- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — owning boundary rules, tier
  graph, and the requirements each catalog row serves.
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — maintainability and
  deterministic-collection rules.
