# SPEC_0038: Unified FMI Execution

## Status
DRAFT

## Summary
Rumoca's sole internal solver boundary is FMI 3 Model Exchange. FMI 2/3 Model
Exchange and Co-Simulation are projections or hosts of that interface; eFMI
eFMI Production Code over a prepared Solve-owned block is the planned
safety-oriented code-generation path; GALEC Algorithm Code is its retained,
reviewable semantic reference and never authorizes C/H.

## Specification

```text
Modelica -> checked IR pipeline -> checked Solve/GALEC kernel
                                      |
                                      +-> FMI 2 ME / CS ----+-> native C
                                      |                     +-> in-process
                                      +-> FMI 3 ME / CS ----+-> packaged FMU
                                      |                     +-> Wasm component
                                      +-> eFMI Algorithm Code / Production Code
```

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| All FMI forms consume one checked kernel | FMI lowering | Prevent semantic drift |
| ME and CS are capability profiles, not backends | target discovery | Avoid duplicate lowering |
| Native, packaged, and Wasm are deployment forms | FMI packaging | Packaging cannot change behavior |
| The only solver-facing model interface is FMI 3 ME | simulation runtime | Remove the competing private model API |
| In-process simulation hosts FMI 3 ME with Diffsol | simulation runtime | One lifecycle |
| FMI CS embeds an FMI 3 ME host plus a selected integrator | CS runtime | Reuse integration semantics |
| `rumoca-solver` implements the FMI 3 ME importer/host contract | solver facade | Solver code never consumes `SolveModel` directly |
| Numerical methods implement an internal FMI 3 ME-host integrator contract | solver implementations | Solver choice does not change model semantics |
| Private `MeRuntimeHost` implements `MeSimulationSession`'s sole FMI 3 ME master algorithm | solver facade | The session remains the semantic owner; initialization, Event Mode, discrete-state iteration, output scheduling, and trace roles cannot fork by numerical method |
| Numerical plugins implement only `MeIntegratorBackend` | solver implementations | A new solver supplies numerical advance/reset; it cannot invoke FMI lifecycle transitions, schedule observations, or construct traces |
| `MeSimulationSession` is the incremental master algorithm | solver facade | Batch simulation, live stepping, inputs, reset, events, timeouts, and observation ordering share one state machine |
| One retained ME instance grants either one exclusive borrowed lease or one consuming lease | solver facade | Rust borrowing makes concurrent hosts and mutation behind a live session unrepresentable without a runtime ownership flag |
| Session policy carries no start-time coordinate; a leased host derives its admitted start from the retained pristine FMU state, reset replays it, and separately named retiming replaces that session coordinate atomically | solver facade | Component initialization and host scheduling cannot acquire independent start-time authorities |
| Batch admission is one affine product of the retained pristine start, checked options, and complete output grid; consuming it creates one indivisible host/grid owner whose private cursor remains paired through execution | solver facade | Invalid or unrepresentable schedules cannot partially initialize a component, execution cannot recompute a different grid, and callers cannot forge, clone, or recombine cursors |
| A failed borrowed host initialization or plugin construction restores the captured pristine snapshot before releasing the retained borrow; failed restoration outranks and retains the attempted typed failure | solver facade | Construction failure is atomic and never becomes retry or fallback policy |
| Active FMI initialization constructs one exact-width state/nominal outcome; termination is a distinct outcome carrying the component-issued exact Event-Mode state snapshot captured before `fmi3Terminate`, never an absent or fabricated numerical point | solver facade | Short or surplus state results are rejected once at construction and are never padded by host creation or restart |
| A Terminated ME component retains FMI 3.0.2 §2.3.8 final-value getter permissions while rejecting every mutator and active-algorithm operation | FMI component | Final Float64/outputs, continuous states, nominals, derivatives, directional derivatives, and indicators remain inspectable without weakening Terminated's absorbing lifecycle |
| Parameterless reset replays the component-issued pristine start; explicit start-coordinate replacement is a separately named retime operation | solver facade / Wasm adapter | Reset cannot invent a deployment-specific zero start |
| Every scheduled-session implementation explicitly supplies total batched-value access and an optional maximum advance; the trait supplies no policy defaults | simulation runtime | Batch admission rejects absent or incomplete requested-name coverage; direct and facade backends cannot acquire path-dependent fallback behavior or a magic schedule step; current RK and Diffsol policies both return no maximum advance |
| Automatic integrator selection probes and returns the same retained ME instance; the selected host restores that instance's pristine snapshot before initialization | simulation runtime / solver facade | Capability selection neither re-instantiates the component nor leaks the probe lifecycle into execution |
| `FmiComponent` is the only linked or packaged component source | checked FMI projection | Runtime and emitted metadata share one inventory |
| Component operations are exact FMI 3.0.2 semantic projections | FMI component | Private extensions cannot become solver dependencies |
| One closed total operation/state relation admits every dynamic component call and issues a non-duplicable operation-specific guard tied to that lifecycle instance | FMI component | Private kernels cannot be called with a broad, foreign, stale, or recombined lifecycle proof |
| Batched Float64 writes consume construction-issued causality, variability, initial, and write-policy evidence; batched reads validate the complete instance-branded reference inventory before evaluation or output mutation | FMI component | Names cannot reconstruct write authority, and a foreign trailing reference cannot partially mutate component or caller storage |
| Continuous-state derivative, directional-derivative, nominal, state, and event-indicator getters require exact checked buffer widths and leave every caller slot unchanged on refusal or evaluation failure | FMI component | The linked component never pads, truncates, resizes, or partially repairs an FMI array operation |
| Root-step acceptance, interval scanning, and retained-indicator refresh join the root policy, fixed workspace, derivative controller, and kernel only inside the private component owner that issued them together. The session supplies only its numerical backend, time budget, and checked proposal; no policy/workspace/scan-target projection exists, and a syntax-aware exact call graph rejects any second join site | ME host component | Two legitimate equal-width sessions cannot cross-pair component capabilities and silently seed one event search from another component's relation history |
| Integrator boundary values use checked constructors and private fields | solver facade | Invalid outcomes never enter the master algorithm |
| Native in-process calls may be zero-copy | FMI host | Preserve current performance |
| Repeated directional seeds may reuse a bitwise-identical settled coordinate | FMI component | Avoid redundant algebraic projection |
| Root evaluation may warm-start its complete checked refresh plan from a bitwise-identical derivative-settled coordinate | FMI component | Keep roots on the same algebraic branch without omitting root dependencies |
| At a bitwise-identical derivative-settled coordinate, root evaluation may omit covered value stages and execute only a construction-issued checked-BLT remainder; it may omit the complete refresh only when that remainder is empty | FMI component | Remove duplicate work without turning a warm start into an unchecked semantic shortcut |
| Settled-coordinate caches invalidate on lifecycle or parameter mutation | FMI component | Never reuse stale algebraics |
| Wasm uses the FMI layered-standard WIT profile | Wasm adapter | Avoid a private ABI |
| Native and Wasm hosts expose batched state/variable access | FMI host | Avoid per-scalar boundary overhead |
| `rumoca-input` writes model inputs only through typed FMI setters | input/runtime boundary | One input lifecycle |
| Input mappings resolve to FMI value references before execution | scenario preparation | Reject unknown or mistyped inputs early |
| Input clocks use `fmi3SetClock`; structural parameters use Configuration Mode | input/runtime boundary | Preserve FMI lifecycle |
| Name-based private solver input setters are prohibited | bindings/runtime | Keep one model API |
| Native slot writes only implement validated FMI operations | linked runtime | Optimization is non-semantic |
| `rumoca-codec` remains bytes-to-signal-frame only | codec facade | Preserve transport neutrality |
| Codec/model mappings use typed batched FMI get/set operations | runtime adapter | One model boundary |
| Invalid codec/model mappings fail during preparation | scenario preparation | No runtime data loss |
| Transport crates move payloads and controls only | UDP/Zenoh/WebSocket | No model semantics |
| Transport timeout, disconnect, and failure remain distinct | scheduler boundary | Prevent false successful steps |
| Transport errors never become no-input or successful-step outcomes | scheduler boundary | Failures remain visible |
| FMI 2 and FMI 3 adapters share semantic state | FMI runtime | Reduce certification surface |
| eFMI Algorithm Code plus Solve-owned Production Code remains a main product | Correlated `SolveAlgorithmProduct` views | Safety-oriented deployment without an Algorithm-Code-to-C route |
| Symbolic targets project from computable checked Solve | analysis codegen | Avoid duplicate structural analysis |
| Every deployment form retains checked provenance | all adapters | Traceable evidence |
| Unsupported lifecycle capability fails before execution | capability analysis | No plausible bad results |
| Cross-form traces must be equivalent | conformance tests | Packaging is non-semantic |
| OMC comparison consumes a backend-neutral trace-runner contract | parity harness | Preserve evidence across hosts |

### Internal Solver Boundary

`SolveProblem` remains compiler IR. It is projected once into an FMI 3 ME
component kernel. Diffsol, RK methods, BDF implementations, and future
integrators interact only through the FMI 3 ME lifecycle, state, derivative,
event-indicator, time, continuous-state, and discrete-state operations. They
MUST NOT inspect Solve rows, layouts, opcodes, events, or private runtime
objects.

The sole-host state machine, checked integrator aggregates, event-domain rule,
cutover deletion inventory, and required differential evidence are cataloged in
[SPEC_0044 §§6-8](SPEC_0044_FMI_EXECUTION_CATALOG.md#6-common-me-host-and-integrator-contract).
Those rows are normative by reference. A concrete numerical solver implements
only that one-step contract; it never owns an FMI lifecycle transition, output
schedule, trace policy, or component-private Modelica state.

The component-facing surface is an exact semantic projection of FMI 3.0.2 ME.
Host conveniences derive only from standard calls and the checked
`modelDescription`; no convenience reveals Solve rows, relation memory,
projection artifacts, internal delay samples or storage layout, or event
ownership. A namespaced annotation may identify a normal FMI variable carrying
an importer numerical bound derived from current Modelica expressions; that
value is not a private component operation or a view of delay storage. The
strict surface and removal disposition are cataloged in
[SPEC_0044 §8](SPEC_0044_FMI_EXECUTION_CATALOG.md#8-strict-fmi-component-surface).

Native static dispatch, borrowed slices, and batching MAY optimize this
interface but MUST preserve its state machine and observable results. In-process
execution is a deployment form, not another model or solver interface.

Automatic integrator selection is importer numerical policy. Its exact
capability decision and failure-preservation obligations are cataloged in
[SPEC_0044 §5](SPEC_0044_FMI_EXECUTION_CATALOG.md#5-automatic-integrator-selection).

### Bounded ME Verification Profile

The linked FMI 3 ME component exposes a checked lifecycle aggregate and pure
property functions shared by production code and verification drivers. Small
finite domains are exhausted by ordinary tests; bounded Kani harnesses are
reserved for symbolic floating-point and typed-state domains that cannot be
practically enumerated. The normative transition table, obligations, evidence
kind, exact bounded domains, and claim limits are cataloged in
[SPEC_0044 §1](SPEC_0044_FMI_EXECUTION_CATALOG.md#1-bounded-me-verification-profile).
That bounded evidence does not claim arbitrary-model trajectory correctness,
floating-point accuracy, solver convergence, or end-to-end Modelica refinement.

### Phasing

The cutover has four phases. Code movement MUST prove bit-identical traces
against the pre-phase binary unless its row states otherwise.

| Phase | Scope | Exit evidence |
|---|---|---|
| 1 | Internal ME kernel trait in `rumoca-solver` and one `SolveModel` projection; migrate the rk-like session onto it | Bit-identical traces vs. the pre-migration binary; the rk-like crate links Solve IR from no dependency table except `dev-dependencies`, so no production path there can name it |
| 2 | Replace the transitional extended kernel surface with the strict FMI 3.0.2 ME component and importer contracts; construct one checked FMI component aggregate | Official-schema-valid model description, exhaustive finite lifecycle tests plus bounded symbolic checks where justified, and bit-identical traces except for separately proved semantic fixes |
| 3 | FMI CS profile as an ME host plus a selected integrator | ME/CS trace parity on one kernel artifact |
| 4 | Packaged FMU and Wasm deployment forms | Linked-versus-packaged and native-versus-Wasm lifecycle parity |

#### Acceptance Contract: the reduced state-only system

Phase 2 deletes the Diffsol backend's general/implicit DAE construction, so the
reduced state-only ODE becomes the only system a state-carrying model is
integrated as. Per SPEC_0008 *Acceptance Contract Before Rejection*, the new
rejection path ships with the shapes that stay legal:

- [ ] `EX002` (`SimError::StateOnlyPathUnavailable`) rejects a state-carrying
      model whose state-derivative rows read a solver coordinate that the
      algebraic projection plan cannot produce, or that does not present one
      derivative row per continuous state;
      accepts (a) models with zero continuous states, which keep the no-state
      runtime path, (b) models whose derivative rows read only continuous
      states, and (c) models whose non-state reads are transitively produced by
      the algebraic projection plan — including chains through several producer
      rows, and including algebraic counts far above any MSL model's;
      owned by `rumoca-solver-diffsol::bdf::require_state_only_bdf`, with the
      rejection minted in `rumoca-solver-diffsol::error::StateOnlyRejection`
      and bucketed by `rumoca-worker::failure_classification` at
      `SimFailureStage::BackendBuild`;
      evidence `rumoca-solver-diffsol/src/tests/state_path_integration.rs::state_only_bdf_accepts_projection_backed_derivative_dependencies`
      (acceptance plus its ablation),
      `::state_only_bdf_accepts_transitive_projection_dependencies` (chained
      producers), and
      `::simulate_rejects_an_unprojectable_derivative_dependency_by_name`
      (the rejection reaches the caller by name).

### External I/O Profile Semantics

`rumoca-input` and `rumoca-codec` do not select ME or CS and do not own an FMI
instance. The host that owns the instance compiles their signal mappings into
a profile-specific I/O schedule. Signal names, types, shapes, causality, and
value references are shared metadata; legal update times and lifecycle calls
are profile capabilities.

| Active profile | Input application | Time advance | Output collection |
|---|---|---|---|
| FMI 3 ME | Host issues typed setters in a legal ME state and processes required events | Selected ME integrator sets time/states and evaluates derivatives | Host issues typed getters at requested observation points |
| FMI 3 CS | Importer issues typed setters at communication points | `fmi3DoStep` | Importer issues typed getters after accepted steps |
| FMI 3 CS with Intermediate Update | Importer may update declared inputs only in supported callbacks | `fmi3DoStep` with intermediate callbacks | Declared intermediate variables may be read in callbacks |

Linked native simulation uses the FMI 3 ME row; packaged or embedded CS uses
the CS row. An unavailable update cadence or intermediate value fails schedule
preparation. Input updates MUST NOT be delayed, dropped, interpolated, or
replaced to make a profile execute.

FMI 2 ME adapts the same component state. FMI 2/3 Co-Simulation is an FMI 3 ME
host with an integration method and communication-step policy. FMU or Wasm
packaging adds only transport and resources. These projections MUST NOT repeat
Modelica, DAE, or Solve lowering.

### Target Surface

| User-visible profile | Solver ownership | Permitted deployment |
|---|---|---|
| FMI 2 ME | Host | linked native, FMU |
| FMI 2 CS | Component | linked native, FMU |
| FMI 3 ME | Host, including Diffsol | linked native, FMU, Wasm |
| FMI 3 CS | Component, optionally Diffsol | linked native, FMU, Wasm |
| FMI 3 ME+CS | Selected at instantiation | FMU, Wasm |
| eFMI Algorithm Code | Integrator/toolchain | eFMU |
| eFMI Production Code | Generated production runtime | eFMU, generated C |

CLI profile names MUST select capabilities of one generator and MUST NOT own
independent equation lowering, initialization, event, or state-machine code.
A raw derivative-only C kernel may remain an internal fixture, but MUST NOT be
a user-visible target once FMI 2/3 are exposed.

Symbolic exports project computable checked Solve IR and MUST NOT repeat
structural analysis. They are not FMI profiles; a symbolic engine's FMI ME
host role is separate.

### FMI-LS-DAE Layered Profile

FMI-LS-DAE is a layered profile of one FMI 3 Model Exchange component, not a
second DAE lowering. The component remains a valid ODE-form ME FMU when DAE
mode is disabled and exposes its original algebraic variables and residuals
when the structural `enableDAEModeVariable` is enabled in Configuration Mode.
The checked-construction, tensor, lifecycle, packaging, versioning, and target
registration obligations in
[SPEC_0044 §3](SPEC_0044_FMI_EXECUTION_CATALOG.md#3-fmi-ls-dae-layered-profile)
are normative by reference. The target stays unregistered until both profiles
execute and their negative controls pass.

The computable primal `SolveProblem` is the required projection input.
Compiler-produced AD, Jacobian, and mass-matrix `SolveArtifacts` are a separate,
optional product: a symbolic backend may consume compatible artifacts or derive
them itself. Optionality is represented by the presence of the artifact product,
not by silent empty or default derivatives inside a claimed artifact product.

The OMC trace comparator owns model selection, time grids, output selection,
tolerances, diagnostics, and result classification. Candidate runners only
compile a model to a runnable artifact, enumerate outputs, execute the requested
grid, and return a trace. Native FMI 3 and Wasm FMI-LS are separate runners of
that same contract; neither the comparator nor its model inventory depends on
Diffsol or a private in-memory Rumoca backend.

### Evidence

CI MUST test the exact standard version, interface profile, platform form, and
advertised capabilities. A profile is user-visible only after its mandatory
positive, negative-control, metadata, ABI, lifecycle, and execution evidence in
[SPEC_0044 §2](SPEC_0044_FMI_EXECUTION_CATALOG.md#2-standards-conformance-ci)
and its cross-form evidence in
[SPEC_0044 §4](SPEC_0044_FMI_EXECUTION_CATALOG.md#4-cross-form-evidence)
passes. Those catalog rows are normative by reference.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md)
- [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md)
- [Modelica 3.7 §3.7.2.1 `delay`](https://specification.modelica.org/maint/3.7/MLS.html)
- [FMI validation tools](https://fmi-standard.org/validation/)
- [FMI 3.0.2 specification](https://fmi-standard.org/docs/3.0.2/)
- [eFMI resources and compliance tools](https://www.efmi-standard.org/resources/)
- [FMI layered standard for WebAssembly](https://github.com/modelica/fmi-ls-wasm)
- [FMI layered standard for DAE](https://github.com/modelica/fmi-ls-dae)
- [Rumoca issue #34](https://github.com/CogniPilot/rumoca/issues/34)
