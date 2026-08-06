# SPEC_0038: Unified FMI Execution

## Status
DRAFT

## Summary
Rumoca's sole internal numerical-solver boundary is the FMI 3 Model Exchange
interface. Rumoca will expose FMI 2/3 Model Exchange and Co-Simulation in
native C and Wasm forms as projections or hosts of that one interface, with
eFMI GALEC/Production Code as the primary safety-oriented code-generation
path.

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
| Native in-process calls may be zero-copy | FMI host | Preserve current performance |
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
| eFMI GALEC/Production Code remains a main target | eFMI projection | Safety-oriented deployment |
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

The in-process implementation may statically link and devirtualize this
interface, use borrowed slices, and batch variable access. Those optimizations
MUST preserve the FMI 3 ME state machine and observable results; "in process"
does not define a second interface.

#### Strict FMI 3 ME surface

The component-facing interface MUST be a semantic projection of the normative
FMI 3.0.2 Model Exchange functions and `modelDescription.xml`. A Rust method
MUST NOT be named or documented as an FMI operation while omitting a standard
argument, return value, capability flag, lifecycle effect, or XML-declared
ordering rule. Native batching and static dispatch may change representation,
but not the information available to either side of the boundary.

Importer orchestration and numerical policy live in a separate ME-host layer.
That layer may provide conveniences such as "next integration stop", root
localization, trace recording, timeout enforcement, and solver reset, but each
convenience MUST be derived only from standard FMI calls, their returned
values, and `modelDescription.xml`. It MUST NOT call a private component
operation to reveal Modelica relations, `pre()` state, event ownership, delay
storage, Solve rows, or a constraint projection.

The current linked-kernel extensions are disposed as follows during phase 2:

| Current surface | Required disposition |
|---|---|
| `MeTime::event_boundary` | Remove; `fmi3SetTime` carries only `time`. Continuous-Time Mode keeps relations frozen until the importer enters Event Mode. |
| `MeEventEntry` arguments to `enter_event_mode` | Remove; `fmi3EnterEventMode` has no arguments. Event cause is importer state, not component input. |
| `MeStepCompletion` | Replace with the complete `fmi3CompletedIntegratorStep` contract: `noSetFMUStatePriorToCurrentPoint`, `enterEventMode`, and `terminateSimulation`, gated by `needsCompletedIntegratorStep`. |
| `project_continuous_states` | Remove from Continuous-Time Mode. A DAE projection may change states only through Event Mode / `fmi3UpdateDiscreteStates`, reported by `valuesOfContinuousStatesChanged`. |
| `next_event_stop` | Make host-only derivation of `nextEventTimeDefined` / `nextEventTime` returned by `fmi3UpdateDiscreteStates`; the component does not expose a second scheduling query. |
| `event_indicator_crossings` | Make importer-owned classification over `fmi3GetEventIndicators` results. |
| `capture_pre_event_state` / `arm_state_event` | Remove from the host boundary. The component updates its own discrete/relation state when standard Event Mode is entered. |
| `max_step_size` | Remove as a component capability. Delay history is updated through `fmi3CompletedIntegratorStep`; any accuracy step cap is importer numerical policy. |
| `observe`, output recorders, and initial-observation queues | Keep only as host conveniences composed from legal `fmi3Get{VariableType}` calls at the current FMI state; they are not component operations. |
| `restart_from_fmu_state` | Keep only as explicit host composition of standard reset/state operations, subject to advertised FMU-state capabilities. |
| `extend_stop_time` | Remove. An extendable live session instantiates without a defined stop time or resets and re-enters Initialization Mode with a new experiment. |
| fixed-list directional derivative helper | Make host-only preparation of the standard value-reference lists passed to `fmi3GetDirectionalDerivative`. |

The linked model description is the same checked artifact used to emit a
packaged FMU's `modelDescription.xml`. It MUST include the FMI version,
instantiation token, `<ModelExchange>` capabilities (including
`needsCompletedIntegratorStep`), typed model variables and value references,
continuous-state derivative ordering, event-indicator ordering, clocks,
dependencies, units, dimensions, causality, variability, initial/start data,
and `ModelStructure` required by the implemented model. Packaged XML MUST
validate against the official FMI 3 schema, and linked-versus-packaged tests
MUST prove that the importer sees the same metadata and call results.

SPEC_0029 §12 and SPEC_0041 §4 assign the checked FMI aggregate, its lowering,
the shared FMI runtime, and the concrete-solver boundary. A concrete numerical
solver consumes only `rumoca-solver`'s generic FMI ME importer/host contract.

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

The cutover has four phases. Code-movement phases MUST prove bit-identical
traces against the pre-phase binary unless their row states otherwise.

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

Rumoca's linked native simulation uses the FMI 3 ME row above. A packaged or
embedded CS component uses the CS row. A scenario requesting an update cadence
or intermediate value unavailable in the selected profile fails during
schedule preparation. It is prohibited to delay, drop, interpolate, or replace
an input update merely to make the selected profile execute.

FMI 2 ME is a version adapter over the same component state. FMI 2/3
Co-Simulation is an FMI 3 ME host plus an owned integration method and
communication-step policy. Packaging an FMU or Wasm component adds transport
and resources only. None of these projections may repeat Modelica, DAE, or
Solve lowering.

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

The CLI may provide distinct profile names for discoverability, but those
profiles MUST select capabilities of the same generator. They MUST NOT own
independent equation lowering, initialization, event, or state-machine code.
A raw derivative-only C kernel may remain an internal lowering or diagnostic
fixture, but it MUST NOT be a user-visible deployment target once FMI 2/3 are
exposed.

Symbolic exports remain analysis projections from computable checked Solve IR,
which retains their residuals, Jacobian structure, initialization, roots, and
events. They are not FMI profiles and MUST NOT repeat structural analysis. A
symbolic engine's separate FMI ME host role is not symbolic export.

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
- [FMI validation tools](https://fmi-standard.org/validation/)
- [FMI 3.0.2 specification](https://fmi-standard.org/docs/3.0.2/)
- [eFMI resources and compliance tools](https://www.efmi-standard.org/resources/)
- [FMI layered standard for WebAssembly](https://github.com/modelica/fmi-ls-wasm)
- [FMI layered standard for DAE](https://github.com/modelica/fmi-ls-dae)
- [Rumoca issue #34](https://github.com/CogniPilot/rumoca/issues/34)
