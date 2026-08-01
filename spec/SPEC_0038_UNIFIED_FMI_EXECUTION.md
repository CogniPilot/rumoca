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
| `rumoca-codec` remains bytes-to-signal-frame only | codec facade | Preserve transport neutrality |
| Codec/model mappings use typed batched FMI get/set operations | runtime adapter | One model boundary |
| Transport crates move payloads and controls only | UDP/Zenoh/WebSocket | No model semantics |
| Transport timeout, disconnect, and failure remain distinct | scheduler boundary | Prevent false successful steps |
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

`rumoca-input` retains physical-device adapters, local input state, debounce,
preconditions, derived controls, and signal mapping. Its model-facing product
is a typed batch of FMI value references and values, not `(name, f64)` calls
against a Rumoca simulation session. Scenario preparation resolves configured
names through FMI model-variable metadata, checks causality, primitive type,
shape, variability, and writable lifecycle states, and fails before execution
when a mapping is invalid. Runtime application uses the corresponding
`fmi3Set{VariableType}` operation in a legal FMI state. Input clocks use
`fmi3SetClock`; structural parameters use Configuration Mode rather than the
ordinary input path. Native linked execution may lower a validated batch to
direct slot writes only as an implementation of those same FMI operations and
state transitions.

The existing public `SimulationSession::set_input(name, f64)` family and
backend-specific equivalents are removed during this cutover. Bindings and
interactive tools expose FMI value-reference or metadata-resolved input
operations; they do not recreate name-based private solver setters.

`rumoca-codec` does not depend on an FMI runtime and does not interpret model
semantics. It continues to encode and decode transport-neutral signal frames.
Scenario preparation compiles the separate frame-to-model mapping against FMI
model-variable metadata. The runtime adapter applies decoded fields with
typed, batched `fmi3Set{VariableType}` operations and obtains outgoing model
fields with typed, batched `fmi3Get{VariableType}` operations. Unknown fields,
type or shape mismatches, illegal writes, and unavailable outputs are rejected
when the mapping is prepared rather than ignored during a simulation step.

The `rumoca-transport-udp`, `rumoca-transport-zenoh`, and
`rumoca-transport-websocket` crates carry bytes or control messages. They do
not depend on FMI, codecs, Solve IR, or a simulation session. The scenario
runtime composes transport → codec → prepared FMI binding for inbound data and
prepared FMI binding → codec → transport for outbound data. Viewer controls
enter `rumoca-input` local/control state before the resulting model inputs use
the same FMI binding.

Transport APIs return typed outcomes that distinguish data, a configured
timeout, orderly disconnect, backpressure/data loss, and I/O failure. The
scenario policy may explicitly tolerate a timeout or lossy link, but it records
that decision in the trace. A transport implementation may not swallow a send
error, convert a receive error to "no input", or allow a failed lockstep
exchange to advance as a successful model step.

### Phasing

The cutover lands in four phases. Each phase is behaviour-freezing unless its
row says otherwise: a phase that only moves code MUST prove bit-identical sim
traces against the pre-phase binary, because a divergence introduced while
relocating semantics cannot be told apart from the divergence being fixed.

| Phase | Scope | Exit evidence |
|---|---|---|
| 1 | Internal ME kernel trait in `rumoca-solver` and one `SolveModel` projection; migrate the rk-like session onto it | Bit-identical traces vs. the pre-migration binary; the rk-like crate links Solve IR from no dependency table except `dev-dependencies`, so no production path there can name it |
| 2 | Migrate the Diffsol/BDF session; delete the private model paths both backends used | One shared event loop; recorded rk-like/diffsol divergences resolve on the shared semantics, with the band table as the record |
| 3 | FMI CS profile as an ME host plus a selected integrator | ME/CS trace parity on one kernel artifact |
| 4 | Packaged FMU and Wasm deployment forms | Linked-versus-packaged and native-versus-Wasm lifecycle parity |

Phase 1 does not fix event-semantics divergences; it removes the reason they
have to be fixed twice. Fixing them is phase 2 work, on the one event loop.

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

The rejection is deliberately *not* reachable from `SimulationSession`, which
constructs its own problem and so bypasses the check; consolidating the session
onto the same reduction is follow-on work, recorded on `FS-SIM-015`.

**Fixture note.** Ten Diffsol unit tests previously exercised the
general/implicit construction only because their `SolveModel` fixtures were
under-specified: they set `implicit_rhs` but left `derivative_rhs` empty, so the
eligibility walk saw zero derivative rows and routed them to the general path.
Their edits add the `derivative_rhs` and `full_jacobian_v` rows Solve actually
emits for those systems — the mass matrix is the identity in every one, so the
derivative program is the same rows as the residual, and the added Jacobians are
the exact JVPs (`der(x)=1` -> `0`; `der(x)=v, der(v)=-9.81` -> `[seed[1], 0]`;
`der(x)=0` -> `0`). No assertion was weakened: the edits complete the fixtures
towards the shape Solve emits, so each test now pins its original semantics on
the path the model would really take.

Composing work: the target/backend registry (#121) supplies the capability
discovery the profile table above assumes, and declarative buffer starts (#119)
remove the last initialization state a host would otherwise have to reach for
directly.

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

CasADi, JAX, SymPy, SymForce, Julia ModelingToolkit, and ONNX exports remain
first-class analysis projections from computable checked Solve IR. Solve must
retain sparse implicit residuals, Jacobian structure, initialization systems,
roots, and events needed by those projections. They are not FMI deployment
profiles and MUST NOT repeat DAE structural analysis or become alternate
semantic pipelines. A symbolic engine may separately act as an FMI ME host,
but that host role is distinct from symbolic export.

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

| Evidence | Proves |
|---|---|
| One checked-kernel artifact hash across profiles | Identical model semantics |
| ME host parity across Diffsol and another solver | Solver separation |
| ME/CS native-versus-Wasm trace parity | Deployment equivalence |
| Linked-versus-packaged FMU lifecycle parity | Packaging equivalence |
| `rumoca-input` device/scenario tests through linked and Wasm FMI setters | One input contract |
| Codec frame mappings through linked and Wasm FMI get/set operations | One model boundary |
| Injected transport timeout/disconnect/failure tests | No false successful steps |
| Invalid name/type/shape/state input tests fail before model evaluation | No silent input loss |
| Batched native/Wasm throughput and boundary-latency budgets | Fast interactive and production simulation |
| FMI conformance tests for each advertised profile | Interface compliance |
| Identical OMC inventory through native and Wasm runners | Backend-neutral parity evidence |
| eFMI schema and checksum validation | Production package integrity |
| Injected unsupported-capability failures | Early-error behavior |

## Motivation

- Separate FMI implementations multiply semantic and certification evidence.
- A solver facade parallel to FMI duplicates lifecycle, event, state, and
  capability abstractions; the solver layer should implement the ME host.
- An FMU interface does not require an operating-system process.
- Wasm offers an in-process portable deployment form without a private runtime.
- The current Diffsol path is naturally an FMI 3 ME host, not another model API.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md)
- [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md)
- [FMI layered standard for WebAssembly](https://github.com/modelica/fmi-ls-wasm)
- [Rumoca issue #34](https://github.com/CogniPilot/rumoca/issues/34)
