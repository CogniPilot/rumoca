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
| nearest named tool-config discovery and `ToolConfigError` | `rumoca-core::tool_config` | One parent-directory walk and one typed read/parse error shape shared by fmt/lint; each tool owns only its accepted filenames and config schema. |
| `eval_ast_integer_binary` | `rumoca-core` | Checked MLS integer arithmetic shared by AST/Flat structural evaluators; `/` folds only when its Real result is exactly integral. |
| AST scalar constant evaluation (`AstScalarContext`, `eval_integer`, `eval_real`, `eval_boolean`) | `rumoca-eval-ast::ast_scalar` | One syntax dispatch; compiler phases provide lookup, function-call, coercion, and diagnostic policy through adapters. |
| `dependency_first_sccs`, `DependencyScc` | `rumoca-core::dependency_graph` | Deterministic, iterative dependency-first SCC decomposition shared by checked recursive-owner construction. |
| `InstanceId` | `rumoca-core` | Compact concrete-occurrence identity shared by Instanced and Flat reference vocabulary; source declarations continue to use `DefId`. |
| UTF-8 byte offset ↔ UTF-16 text position/range helpers | `rumoca-core::text_position` | Protocol-neutral `TextPosition`/`TextRange`; LSP crates convert to/from `lsp_types` locally. No `rumoca-lsp-position` micro-crate. |
| `INTERNAL_SAMPLE_FUNCTION_NAME`, `source_temporal_function_name`, `source_temporal_function_short_name`, `source_dae_forbidden_builtin_name` | `rumoca-core` | Single source for typed source temporal/synchronous operator vocabulary shared by DAE and Solve boundary validation. |
| `expr_contains_var` | `rumoca-ir-dae::expr_query` | Handles every `Expression` variant |
| `expr_refers_to_var` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `expr_contains_der_of` | `rumoca-ir-dae::expr_query` | Same single-source rule. |
| `DaeView::record_field_layout`, `RecordFieldLayout` | `rumoca-ir-dae::model::view` | Read-only element-major packing query shared by explicit DAE evaluation and Solve scalar-projection boundaries; compact record arrays remain owned by DAE-IR. |
| `derive_target_assignment_shapes`, `derive_target_assignment_shape_for_output`, `ScalarProgramYDependency` | `rumoca-ir-solve` (`refresh::{assignment_shape, dependency}` internally) | One structural interpretation of checked Solve scalar programs shared by refresh-owner construction/wire replay and reference evaluation; the public helpers are re-exported from the crate root, and backend admissibility remains outside IR. |
| Solve structural presence queries (`solve_has_events`, `solve_has_runtime_events`, `solve_has_clocks`, `solve_has_initialization`) and the event-class composition (`solve_event_class`, `SolveEventClass`) | `rumoca-ir-solve` (`feature_query` internally) | One reading of which partitions a checked `SolveProblem` contains, re-exported from the crate root and shared by `rumoca-compile`'s target-capability gate and the checked FMI event-free narrowing, so a class cannot be recognised by one and missed by the other. These are SPEC_0029 §3 read-only IR queries: presence only. Admissibility (comparing a class against a declared target capability, or against what a template can render) stays with the consumer and MUST NOT move here. |
| Solver runtime time-event helpers (`event_right_limit_time`, `sample_time_match_with_tol`, scheduled/periodic filtering, dynamic time-event lookup) | `rumoca-solver::timeline` | One semantic-instant implementation. |
| `bounded_event_right_limit_time` | `rumoca-solver::timeline` | Sole horizon-bounded right-limit composition. |
| Root scanning, localization, and application construction | `rumoca-solver::fmi_me::root` | Host owns earliest FMI domain change. |
| FMI event-indicator inventory | `rumoca-ir-solve::fmi::FmiComponent` | One checked ordered linked/package source. |
| `MeSimulationSession`, `MeComponentHost`, common batch/live drivers | `rumoca-solver::fmi_me::{session,driver}` | FMI ME master-algorithm owners. |
| `MeIntegratorBackend`, `MeAdvanceRequest`, `MeAcceptedStep`, host-private `MeRootSearchPolicy`, accepted-interval roundoff and containment helpers | `rumoca-solver::fmi_me::{integrator,root}` | FMI ME numerical and host-root contract owners; every plugin imports the same host-issued accepted-interval containment rule. |
| `MeTraceRecorder`, `TraceObservationRole` | `rumoca-solver::fmi_me::trace` | ME trace-construction owners. |
| Component-side Modelica event-boundary helpers | `rumoca-solver::fmi_me::kernel` | FMI component Event Mode implementation. |
| Solver pre-parameter snapshot helpers (`write_pre_params_from_sources`, `update_slot`, `commit_pre_params_after_event`) | `rumoca-solver::runtime::pre_params` | Shared `pre(...)` snapshot mechanics. |
| Component-private algebraic settle helpers (`project_algebraics`, `project_algebraics_and_detect_changes`, `project_initial_*`) | `rumoca-solver::runtime::projection` | Used only while evaluating or initializing the FMI component; numerical plugins cannot import this policy. |
| Component-private Solve evaluation state (`SolveRuntime`, event/discrete row application, algebraic settle, Jacobian/sensitivity reports) | `rumoca-solver::runtime::solve_runtime` | Used only behind the FMI component projection; the common host reaches it solely through the FMI ME kernel. |
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
| Checked DAE causal-discrete orientation and dependency plan | `rumoca-phase-structural` | One target/value authority shared by Solve and GALEC; ambiguous residuals remain unowned |
| Typed executable programs plus distinct `SolveProblem` and `SolveAlgorithmBlock` (pending: 2026-08-08 plan, M3-4) roots | `rumoca-ir-solve` | Backend-neutral numerical and controller execution IR |
| DAE → `SolveProblem`; checked Algorithm Code → `SolveAlgorithmBlock` lowering (pending: 2026-08-08 plan, M3-4) | `rumoca-phase-solve` | Exhaustive semantic lowering only, not structural mutation or rendering |
| Checked DAE pure-function graph → numerical `SolveProblem` typed program regions and pure-call owners | `rumoca-phase-solve` | Numerical Solve lowering is distinct from the GALEC-first Algorithm Code refinement path |
| Checked FMI component aggregate (pending: SPEC_0038 both-crate absorption) | `rumoca-ir-solve::fmi` | Private invariant-bearing binding of DAE metadata/shape/provenance to one executable checked kernel; no parallel IR crate, ABI text, or runtime behavior |
| DAE + Solve → checked FMI component lowering (pending: SPEC_0038 both-crate absorption) | `rumoca-phase-solve::fmi` behind its `fmi` feature | One target-neutral semantic projection shared by FMI 2 and FMI 3; non-FMI consumers do not acquire its phase dependencies |
| Linked FMI component execution (pending: SPEC_0038 both-crate absorption) | `rumoca-solver` | Runtime reads `rumoca_ir_solve::fmi` through its existing Solve-IR dependency, never a phase crate |
| Optimization/training orchestration | `rumoca-opt` | Consumes Solve/eval APIs; no Modelica semantics |
| GALEC `.alg` → checked GALEC parsing | `rumoca-phase-parse-galec` | Recoverable syntax state stays out of checked IR |
| DAE → checked GALEC Algorithm Code lowering | `rumoca-phase-galec` | Sole expression, function, lifecycle, and admissibility lowering for the GALEC-first export path; no text, templates, or packaging |
| Checked GALEC executable semantics | `rumoca-eval-galec` | Small explicit interpreter over `rumoca-ir-galec`; no DAE/Solve, lowering, rendering, target, or runtime-host dependencies |
| Typed Solve program evaluation and `SolveAlgorithmBlock` lifecycle execution (pending: 2026-08-08 plan, M3-4) | `rumoca-eval-solve` | Production reference/scalar fallback; remains independent of GALEC evaluation |
| Executable reference semantics for differential validation | `rumoca-reference` | Independent definitional interpreter of the Modelica event core; MUST carry no production dependency on any `rumoca-*` crate, because a reference that imported the compiler would agree with it by construction. Compiler deps are dev-only, for the differential harness. Optimizing it is a defect (SPEC_0037 verification track) |
| Textual generated artifacts and templates | `rumoca-phase-codegen` | Jinja/minijinja rendering owns generated C, Rust, CUDA C, MLIR, FMI/eFMI and FMU/eFMU packaging text |
| Generic checksum-web and archive assembly | `rumoca::packaging` behind `fmu-packaging` | Transactional target-declared filesystem/zip assembly only; no FMI/eFMI semantics and no scheduled-simulation feature dependency |
| GALEC `.alg` text | `rumoca-phase-codegen` | MiniJinja renders a checked GALEC semantic view; the language IR owns no text emitter (SPEC_0034 GAL-009) |
| GALEC-derived Production C/H text | `rumoca-phase-codegen` | MiniJinja mechanically renders checked `SolveAlgorithmBlock` operations and its preselected ABI; no semantic lowering (pending: 2026-08-08 plan, M3-4; templates still lower today) |
| eFMI packaging XML (`__content.xml`, manifests) | `rumoca-phase-codegen` | Rendered like FMI `modelDescription`; validators + generic checksum/container build step, not typed serializers (SPEC_0042 D3 amended) |
| Compiled/JIT execution adapter crates | `rumoca-exec-*` | Invoke tools, load artifacts, wrap Cranelift/LLVM/CUDA/NVRTC APIs, expose ergonomic runtime calls; no compiler semantics |
| Backend-neutral solver interface types | `rumoca-solver` | Single contract shared across backends |
| Generated FMI 2/3 lifecycle and ABI adapter text | `rumoca-phase-codegen` | Thin target-version templates over one checked FMI component; no Modelica, DAE, or Solve lowering |
| Concrete solver backends | `rumoca-solver-{diffsol,rk45,...}` | MUST consume only `rumoca-solver`'s generic FMI ME importer/host contract; no DAE, Solve, FMI export-IR, or phase deps |
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
