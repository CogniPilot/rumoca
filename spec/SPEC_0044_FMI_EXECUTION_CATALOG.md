# SPEC_0044: FMI Execution Contract Catalog

## Status
REFERENCE

## Summary

Normative lookup catalog for the FMI execution and bounded-verification rules
owned by SPEC_0038.

## Specification

Rows and domains below are normative by reference from SPEC_0038.

### 1. Bounded ME Verification Profile

The dynamic FMI facade delegates every call through one closed total
`MeLifecycleOperation × MeState` relation. Each admitted call mints a
non-`Clone`, operation-specific guard exclusively tied to that lifecycle
instance; private kernels consume it without another state check. Transitions
follow this table; snapshot restore is the scoped total-state operation. Per
FMI 3.0.2 §2.3.4, `terminate` is admitted only from the Model Exchange
constituents of Initialized (Event, Reconfiguration, and Continuous-Time
Modes):

| Current state | Command | Next state |
|---|---|---|
| Instantiated | enter configuration when a structural parameter is declared | Configuration Mode |
| Configuration Mode | exit configuration | Instantiated |
| Instantiated | enter initialization | Initialization Mode |
| Initialization Mode | exit initialization | Event Mode |
| Event Mode | update discrete states | Event Mode |
| Event Mode | enter configuration when a tunable structural parameter is declared | Reconfiguration Mode |
| Reconfiguration Mode | exit configuration | Event Mode |
| Event Mode | enter continuous-time mode | Continuous-Time Mode |
| Continuous-Time Mode | enter event mode | Event Mode |
| Event Mode | terminate | Terminated |
| Reconfiguration Mode | terminate | Terminated |
| Continuous-Time Mode | terminate | Terminated |
| Any state | restore a component snapshot | state recorded by the snapshot |

Every other transition returns a typed contract failure without mutation.
Terminated is absorbing except for snapshot restore, while FMI 3.0.2 §2.3.8
final-value getters remain legal there: typed Float64/output observation,
continuous states, nominals, derivatives, directional derivatives, and event
indicators as applicable.
Every mutator and active-algorithm operation checks lifecycle before mutation.
Configuration and Reconfiguration Mode
rows are enabled only for components declaring the corresponding structural
parameter capability; components without it reject entry without mutation.

| Obligation ID | Obligation | Required evidence |
|---|---|---|
| ME-LIFE-001 | Construction starts Instantiated; the complete operation relation equals the guarded table and FMI §§2.3.2–2.3.8/3.2.1 permissions | Exhaustive ordinary test of 7 states × 20 operations × enabled/absent structural-parameter capabilities |
| ME-LIFE-002 | Rejected operations preserve lifecycle, component state, and caller buffers; successful guards are operation-specific, non-`Clone`, and exclusively tied to the lifecycle instance that issued them | Exhaustive pure relation and production-facade matrix; type/privacy evidence rejects broad, foreign, stale, or recombined admissions |
| ME-LIFE-003 | Terminated is absorbing except for reusable opaque-snapshot restore; §2.3.8 final-value getters remain permitted while every mutator and active-algorithm operation is refused | Exhaustive 20-operation permission matrix, terminal getter/mutator facade witnesses, and all 7 reusable snapshot restore targets |
| ME-LIFE-004 | Bounded convergent events settle; divergent fixed points return staged non-convergence | Ordinary examples and property tests over finite divergence increments |
| ME-LIFE-005 | `fmi3UpdateDiscreteStates` and `fmi3CompletedIntegratorStep` evaluate against one construction-reserved detached mutable continuation and publish it once only after every fallible evaluation succeeds; construction reserves the live and staged event-entry, initialization-settle, indicator-domain/crossing, latch, scratch, callback-cache, delay/history, and runtime vectors, and both paths use infallible fixed-size copy/swap rather than first-use allocation; the immutable construction-issued model, linked facts, and indicator plan/table are shared pointer-identically by live and staged continuation; rejection retains the exact continuation for an identical retry | Injected post-evaluation failure/retry ablations for initial and runtime discrete updates, indicator crossing completion, and delay-bearing completion, each compared with the bit-exact component-state oracle; executable model/facts/table and live/staged buffer pointer-identity witness across ordinary completion plus Event Mode and `UpdateDiscreteStates`; source ablation excluding detached construction, plan cloning, and deep step-path allocation |
| ME-LIFE-006 | `fmi3SetTime` accepts only finite time no earlier than `startTime`, the second-last successful `fmi3CompletedIntegratorStep` time, and the last successful `fmi3EnterEventMode` time; these exact lower-bound facts are component continuation included in FMU state, rejection is atomic, and snapshot restore reinstates the recorded earlier bound | Start-time rejection; successful 0.25/0.5 completed-step sequence followed by atomic 0.1 rejection; Event-Mode-at-0.5 followed by atomic 0.1 rejection; pre-bound snapshot restores that make 0.1 legal again |
| ME-ERR-001 | Stage annotation is idempotent; innermost stage wins | Exhaustive ordinary test of 6 recorded-stage choices × 5 incoming stages |
| ME-BUF-001 | Invalid bounded inputs do not partially mutate state or host buffers | Exhaustive ordinary tests of the named invalid-input classes below |
| ME-BRAND-001 | Foreign value-reference batches and snapshots reject before component or caller-buffer mutation | Exhaustive ordinary tests with a valid leading item and foreign trailing item for reads and writes, plus foreign snapshot refusal |
| ME-STATE-001 | Snapshot restore re-establishes lifecycle and observable state | Exhaustive ordinary test of all 7 lifecycle states |

The Kani profile contains exactly this symbolic bounded property:

| Claim | Symbolic domain | Counterexample meaning |
|---|---|---|
| SIM-010 | Three arbitrary typed event-history lanes and ownership masks | Pre advancement can lose type fidelity or partially mutate on rejection |

Kani proves only this property over its declared assumptions and unwind bound.
This is bounded runtime-kernel evidence, not a proof of compiler semantics.

ME-BUF-001 covers NaN and infinities as time or one state; exact under- and
over-width state, nominal, derivative, serialized directional-seed, serialized
directional-result, and event-indicator buffers; non-finite, foreign, and out-of-range value
references; Float64 result/value buffers whose length differs from the total
serialized width of the referenced scalar-or-array variables, including
array-plus-scalar batches in both reference orders; and evaluation
failure.
Every batch is admitted completely before the first component or caller-buffer
write. No component getter resizes, pads, truncates, or partially repairs a
slice.

Harnesses call production property functions. Finite exhaustive tests and
property-test samples are validation, not proof. This profile excludes
arbitrary-model trajectory correctness, floating-point accuracy, solver
convergence, and end-to-end Modelica refinement.

### 2. Standards-Conformance CI

Passing a C compiler or parsing one XML document is necessary evidence, but is
not conformance. Each artifact profile requires the following evidence:

| Artifact/profile | Mandatory CI evidence |
|---|---|
| FMI 2 Model Exchange FMU | Official FMI 2 XSD validation; archive-layout and declared-source validation; importer build from packaged `sources/`; exported-symbol and lifecycle checks; instantiate, initialize, integrate, terminate, and free through the FMI 2 C API; trace parity with the linked checked kernel |
| FMI 2 Co-Simulation FMU | Applicable FMI 2 ME package checks; advertised CS capability and lifecycle checks; repeated `fmi2DoStep`, including rejected/failed step behavior; trace parity with the same ME kernel and owned integrator |
| FMI 3 Model Exchange FMU | Official FMI 3 XSD and `buildDescription.xml` validation; archive-layout and declared-source validation; importer build from packaged `sources/`; exported-symbol and lifecycle checks; instantiate, initialize, enter required modes, integrate, terminate, and free through the FMI 3 C API; native-array get/set checks when dimensions are advertised; checked-kernel trace parity |
| FMI 3 Co-Simulation FMU | Applicable FMI 3 ME package checks; advertised CS capability and lifecycle checks; repeated `fmi3DoStep`, including early-return, event, and failed/discarded behavior when advertised; trace parity with the same ME kernel and owned integrator |
| eFMI Algorithm Code eFMU | Exact declared official-schema revision; packaged-byte reference/checksum recomputation; GALEC parse/round-trip; generated-algorithm trace parity with the checked GALEC kernel |
| eFMI Production Code eFMU | Applicable Algorithm Code integrity checks; exact Production Code schema revision; generated-code compilation and execution; trace parity with Algorithm Code and the checked kernel |
| FMI 3 ME with FMI-LS-DAE | Applicable FMI 3 ME checks; exact pinned layered schemas and package paths; one checked DAE/FMI provenance aggregate; default ODE-mode execution; Configuration-Mode enablement followed by importer-driven algebraic values and residual evaluation; trace/residual parity against the aggregate; rejected mismatched provenance, missing residual ownership, invalid dependencies, and ablated manifests |

FMI Model Exchange event inventory has this additional checked-lowering
obligation:

| Obligation ID | Obligation | Owner | Required evidence |
|---|---|---|---|
| ME-EVENT-001 | `rumoca-phase-solve::fmi` constructs the ordered `FmiComponent` indicator inventory from continuously monitored non-scheduled Solve roots and continuously state-dependent dynamic-time rows; periodic-clock roots and parameter/discrete-only time events are excluded and announced through `nextEventTime` | checked FMI lowering | Linked state-dependent indicators change FMI domain; parameter-only and periodic events appear only as time events; constructor ablations reject missing, extra, reordered, duplicate, or scheduled indicator sources |
| ME-EVENT-002 | Linked runtime and codegen consume the same `FmiComponent` inventory; until generated C implements indicator evaluation and relation freezing, packaging an event-bearing FMI 2/3 component fails closed | checked FMI aggregate and codegen | Event-free linked/XML counts agree and validate now; event-bearing packaging rejects; generated indicator ABI, lifecycle, XSD, and linked/package trace parity gate removal of that rejection |

FMI 2 and FMI 3 jobs use at least two independent compatible validators. At
least one executes the FMU. Validator and schema versions are pinned, and the
fixtures include valid controls plus deliberately broken archives proving that
each gate rejects malformed input.

An eFMI checker is evidence only for the exact emitted schema revision. Until
an official checker supports that revision, CI uses the official schemas,
strict reference/checksum validation, GALEC round-trip, and executable
differential tests; a checker for another revision cannot support a compliance
claim.

Capabilities absent from an artifact are tested as absent. An ME-only component
cannot declare Co-Simulation, and an FMU cannot claim clocks, native arrays,
intermediate update, FMU-state serialization, or directional derivatives that
it does not implement. Negative capability tests are mandatory.

### 3. FMI-LS-DAE Layered Profile

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Construct one DAE/FMI aggregate from the same checked DAE provenance | `rumoca-ir-solve::fmi` | Prevent mismatched ODE and residual models without a parallel IR crate |
| Preserve DAE residual owners, dependencies, and tensor domains | FMI-LS-DAE lowering | Never reconstruct from scalar Solve rows |
| Derive default ODE mode from the checked FMI kernel | FMI runtime | One executable model in both modes |
| Enable DAE mode only through the declared structural parameter | FMI lifecycle | Match the layered state machine |
| Expose importer-set algebraics and residual outputs in DAE mode | FMI adapter | Preserve the residual contract |
| Package revision-defined manifest and schemas under `extra/` | FMI packaging | Make the profile discoverable |
| Reject unsupported DAE forms before rendering | checked constructor | No plausible incomplete interface |
| Keep the target unregistered until both modes execute | target discovery | Schema-only output is not a product |

The target README states the first supported slice, and the aggregate
constructor enforces it. Readiness zero may identify a narrow pinned draft;
rendering still cannot infer residual ownership, dependencies, or shapes.
Updating the pin updates schemas, lifecycle tests, and negative controls.

### 4. Cross-Form Evidence

| Evidence | Proves |
|---|---|
| One checked-kernel artifact hash across profiles | Identical model semantics |
| ME host parity across two solvers | Solver separation |
| ME/CS native-versus-Wasm trace parity | Deployment equivalence |
| Linked-versus-packaged lifecycle parity | Packaging equivalence |
| Device/scenario input through linked and Wasm setters | One input contract |
| Codec mappings through linked and Wasm get/set | One model boundary |
| Injected transport failure tests | No false successful steps |
| Invalid input mappings fail before evaluation | No silent input loss |
| Batched boundary performance budgets | Viable interactive and production use |
| Multi-validator metadata, ABI, lifecycle, and execution tests | Substantive interface compliance |
| Identical OMC inventory through native and Wasm runners | Backend-neutral parity evidence |
| Revision-matched eFMI integrity, round-trip, and execution | Production package integrity |
| Injected unsupported-capability failures | Early-error behavior |
| FMI-LS-DAE ODE and DAE execution from one aggregate | Mode does not change model identity |

### 5. Automatic Integrator Selection

| Obligation ID | Obligation | Required evidence |
|---|---|---|
| ME-AUTO-001 | `auto` selects an integrator before time integration; it MUST NOT retry or replace a failed trajectory | An integration failure after selection remains the original typed failure |
| ME-AUTO-002 | BDF is eligible only when every initial-state basis direction has a finite FMI ME directional derivative after initialization | A locally undefined algebraic sensitivity rejects BDF eligibility while the finite value path remains executable by the explicit host |
| ME-AUTO-003 | Only the typed `DirectionalDerivativeUnavailable` capability result selects the explicit host; lifecycle, contract, assertion, allocation, and ordinary evaluation failures remain failures | Negative tests for at least one non-capability failure plus typed dispatch tests |
| ME-AUTO-004 | Explicit `bdf` and `rk-like` requests are never redirected | Backend-specific tests retain their requested success or failure |
| ME-AUTO-005 | The capability probe and selected host use one retained FMI instance. Every selected lease restores that instance's pristine snapshot before initialization, including after the probe has entered FMI initialization and evaluated a directional derivative | A compiler-lowered model with a finite value path and singular initial algebraic sensitivity produces a real `DirectionalDerivativeUnavailable` probe result, selects the explicit host, and completes a finite trace from the returned retained instance without re-instantiation |

The initial linearization probe is a capability query over the same initialized
FMI 3 ME component and the same `fmi3GetDirectionalDerivative` semantics used
by BDF. It is not a finite-difference heuristic. A model may have a finite ODE
value at a point where its directional derivative is undefined (for example,
`atan2(0, 0)`). That is not a compiler-semantic failure and does not make an
explicit integration result less valid; it makes the derivative-dependent BDF
host ineligible at that point. A directional derivative that becomes
unavailable after integration starts remains a visible numerical failure under
ME-AUTO-001.

### 6. Common ME Host And Integrator Contract

The common host owns the FMI master algorithm; a numerical plugin owns one
numerical advance at a time. These obligations refine SPEC_0038's sole-host and
thin-plugin rules:

| Obligation ID | Contract | Required evidence |
|---|---|---|
| ME-HOST-001 | `MeSimulationSession` alone owns initialization, FMI modes, discrete iteration, time-event caching, timeout, inputs, reset, termination, output roles, and trace construction | One conformance suite over Diffsol, RK45, and time-only plugins; architecture rejection of lifecycle or trace calls in concrete solver crates |
| ME-HOST-002 | Batch execution supplies an output cursor to the same incremental session and contains no second event/integration loop | Batch/live event and input parity; call-graph architecture test |
| ME-HOST-003 | A retained component grants one exclusive borrowed lease through `&mut self`, or is consumed into one owned lease; the lease lifetime reaches every derived session and no runtime lease flag exists | Compile-fail evidence keeps both a first borrowed host and a session derived from it live across rejected second leases; consuming-after-probe reset evidence |
| ME-HOST-004 | `MeSessionOptions` contains no start coordinate. Initial host time and the first output coordinate derive from the retained pristine FMU state. Parameterless reset replays that same component-issued start; a caller-requested replacement coordinate is an explicitly named retime operation that atomically replaces the session start. Scheduled-session traits use only the latter name for a coordinate-taking operation | Compile-fail evidence rejects a start field on the public request; nonzero pristine-start host/grid, consuming-lease, selected-backend, and Wasm reset witnesses; stop-before-component-start rejection; delay-history retiming evidence; zero coordinate-taking `reset` trait methods |
| ME-HOST-005 | Before any reset or initialization, batch execution constructs one non-cloneable admission product from the immutable retained pristine start, checked options, and complete output grid. The product keeps the exclusive retained borrow and is consumed once into one indivisible host/grid owner; its cursor remains private through execution. There is no fail-open grid builder, post-initialization grid construction, second validation, recomputation, cursor clone, or cursor recombination | Oversized and nonadvancing grids reject with bit-exact lifecycle and observable state unchanged; compile-fail evidence rejects cursor construction, cloning, and admission dissolution; the admitted nonzero-start grid is consumed by one successful batch run; zero references to a fail-open output-grid builder |
| ME-HOST-006 | Every borrowed host attempt restores pristine before start/options admission. After that restoration, every fallible host initialization or plugin construction restores the captured pristine snapshot before returning failure. If restoration also fails, its typed failure outranks and retains the attempted typed failure. This is failure atomicity only: it never retries construction, selects another plugin, or falls back | An invalid second lease after prior mutation leaves the component pristine; injected FMI-initialization and plugin-initialization failures each audit pristine lifecycle/observable state immediately and then successfully reuse the same retained component; typed dual-failure precedence test |
| ME-HOST-007 | Active initialization produces one checked outcome whose state and nominal vectors exactly match the component state count. Initialization termination is a distinct outcome carrying the component-issued exact Event-Mode state snapshot captured before `fmi3Terminate`, never an absent or fabricated numerical point. Host construction and restart consume the checked outcome without resizing or padding it | Constructor rejection of short and surplus state vectors; zero initialization-state resize repairs; real nonzero-state initialization termination retains exact-width final getters through host construction, reset, and retime |
| ME-HOST-008 | Every scheduled-session backend explicitly implements nonoptional batched-value access and its optional maximum schedule advance. Batch admission requires exact requested-name coverage and has no scalar fallback. The trait provides no default body or inferred numeric cap; a facade forwards the exact selected backend policy | Incomplete batch results reject at admission; direct/facade RK and Diffsol policy-equivalence tests; RK and Diffsol each explicitly return no cap; source/type checks reject omitted obligations and coordinate-taking `reset` |
| ME-HOST-009 | The linked runtime issues a typed continuous-state domain and the checked indicator plan issues its typed published width. The linked kernel alone consumes those facts into one opaque root-scan shape. One private host-component aggregate consumes exactly one kernel and atomically derives its root-search state, fixed scan storage, retained derivative controller, and kernel-branded maximum-step reference; neither the aggregate nor its host exposes parallel fields, a cloneable kernel owner handle, or a mutable component-derived capability that can pair capabilities from different kernels. Initialization and nominal refresh are narrow operations on the indivisible component owner and derive policy state from its own retained domain. Proposal acceptance, interval scanning, and retained-indicator refresh construct their scan target and join policy, workspace, derivative controller, and kernel only inside that same component owner; no sibling-visible policy/workspace/scan-target projection or second call site exists. The plugin's numerical setup is assembled only from that host's already-checked options and nominal vector and has no public or bare-count constructor. Width projection occurs only at allocation, FMI/public-report, and raw plugin-result admission boundaries; exchanging roles at an issuer is a compile error | Syntax-aware architecture and mutation evidence covers typed source capabilities, the kernel issuer, the sole consuming host-component constructor, private correlated fields, absence of cloneable owner-handle and mutable-capability projections, the exact accept/scan/retained-refresh call graph including foreign-call mutations, role-typed buffer issuers, absence of bare-count constructors/parallel fields, and nonconstructible numerical setup; behavioral scan/refinement evidence preserves every buffer's pointer and width, and wrong-width plugin output fails at the single raw-result admission boundary |
| ME-INT-001 | `MeIntegratorBackend` exposes only initialize, exactly one accepted numerical-step advance over a host-provided derivative callback, that step's native continuous extension plus declared positive local order, and generic truncate/reset | Diffsol, RK45, and time-only implementations plus a compile-only unrelated solver containing no concrete-backend type and manufactured-solution order evidence |
| ME-INT-002 | No common type contains dense coefficients, RK stages, BDF history, nonlinear-solver state, Solve rows, relation memory, projection artifacts, or backend errors | Public-API and dependency scans |
| ME-INT-003 | Component, integrator, timeout, allocation, and discard failures remain typed; standard termination is successful `SimTermination` | Exhaustive conversion tests; `sim_timeout` remains a distinct 566-model bucket |
| ME-ZERO-001 | Zero continuous states select the time-only plugin after constructing the same component/session | Zero-state state-event, time-event, input, reset, timeout, and trace parity |

The public boundary aggregates have private fields and checked constructors:

| Aggregate | Construction contract |
|---|---|
| `MeContinuousPoint` | Finite time and values; state width matches the component domain carried from its consumed request or host; callers cannot supply a bare component width |
| `MeAdvanceRequest` | Exactly one accepted internal step from the current point; optional non-crossable FMI hard stop; public yield boundary; optional soft nominal observation; optional positive finite `max_step_duration`; all coordinates are host-issued and ordered, while the duration is not a coordinate |
| `MeAcceptedStep` | Finite previous/accepted points of exact component width, progress beyond roundoff, positive declared local continuous-extension order, sampling covers the complete interval, and stop/yield/maximum-duration bounds hold under the host roundoff policy |
| `MeTraceRecorder` | Host-only constructor of evidence satisfying SPEC_0033's trace-producer contract; it emits `SimResult` only after successful construction |

No root result crosses the numerical-plugin boundary. The host retains the
full standard event-indicator vector from the previous completed step. For each
`MeAcceptedStep`, it samples monotonically from that host-owned point toward the
accepted endpoint, sets each sampled time/state with standard FMI calls, and
classifies the full `fmi3GetEventIndicators` vector with FMI's exact `z > 0`
versus `z <= 0` domains. Sampling resolution and location tolerance come from a
private checked `MeRootSearchPolicy` constructed by the session, never from the
plugin. The policy also owns finite positive absolute and relative
state-consistency tolerances. The session constructs it from host options and
the positive finite state nominals returned by
`fmi3GetNominalsOfContinuousStates`; a non-finite or non-positive nominal is a
typed construction failure. For endpoint states `x0` and `x1`, component `i`
uses `max(abs_tol, rel_tol * max(nominal_i, abs(x0_i), abs(x1_i)))`. Before any
interior use, the host samples both interval endpoints and requires componentwise
agreement with the checked points under that bound. The zero-state check is
vacuous.

On every Event Mode discrete update, the host consumes
`terminateSimulation`; true ends the iteration immediately, transitions to
Terminated, and returns successful `SimTermination` without another numerical
request, even if `discreteStatesNeedUpdate` is also true. For each
non-terminating update it accumulates the two continuous-state change flags.
After the iteration converges and before another plugin request, if accumulated
`valuesOfContinuousStatesChanged` is true it
re-reads the complete continuous-state vector and reinitializes the plugin at
that checked point. If accumulated `nominalsOfContinuousStatesChanged` is true
it re-reads the complete positive finite nominal vector and reconstructs the
private policy from the same host options. Both refreshes commit atomically; a
failed read or invalid nominal leaves no partially refreshed host or plugin
state.

The declared order MUST equal the native continuous extension's positive local
accuracy order for that accepted step; a higher-order step MUST NOT report order
1. A backend is admitted only after its sampler passes the solver-neutral
manufactured-solution convergence suite at multiple refinements, including
curved polynomials, inflection points, and equal endpoint derivatives. The suite
consumes the declared order and MUST reject a deliberately degraded linear
sampler. The host does not infer or reject interpolation order from finitely many
runtime samples: endpoint checks reject stale, misindexed, or non-finite
samplers, while convergence evidence validates the declaration. The time-only
case is vacuous over empty state. No solver representation enters the common
type.

The common root locator stops at the first adjacent sampled pair containing any
domain change, refines every changed indicator against that same host-issued
tolerance, chooses the least application coordinate, and derives the complete
simultaneous set there. It then constructs a private `MeRootApplication` from
the checked left/application points and full vectors. Before destroying the
retained interval, the host samples and materializes every pending nominal and
event-left observation and the application state. Only then is the uncompleted
trial endpoint discarded, the backend truncated/reset to the application
point, and the component restored there before the completed-step callback and
Event Mode. No sample is requested after truncation. A failure is typed
`RootApplicationUnavailable`; it does not enter Event Mode, clamp an indicator,
or repair evidence. Root-search types and capabilities are host-private and
have no unchecked constructor.

The scan begins at the previous completed point and never queries before it or
the last Event Mode boundary. Its checked policy limits every adjacent sampled
interval and takes its tolerance from session options, so a backend supplies
neither side of an accuracy comparison. One policy applies to every plugin;
scan resolution never caps the integrator step, and the host may sample a large
accepted interval at many checked coordinates. When several indicators change
in one accepted interval, the host refines all first observed brackets and
applies the earliest domain change observable at the checked policy resolution,
implementing FMI 3.0.2's closest-event obligation. An interior component error
aborts with its typed status; the host does not skip, subdivide, retry, or repair
that observation.

`MeAdvanceRequest` distinguishes three times:

Before constructing a request, the host resolves any hard stop or yield within
roundoff of the current coordinate without numerical advance: experiment end
terminates or reports, cached `nextEventTime` enters Event Mode, and a yield
returns the current coordinate. A current soft observation is materialized
directly. Every constructed non-soft bound is therefore strictly later than the
current coordinate beyond roundoff.

| Coordinate | Meaning |
|---|---|
| `hard_stop_time: Option<f64>` | Defined experiment end or cached standard `nextEventTime`; MUST NOT be crossed beyond host roundoff |
| `yield-time` | Public incremental-session boundary; MUST NOT be crossed beyond host roundoff and, on the reaching request absent an earlier event/termination, becomes the exact reported coordinate |
| `observation_time: Option<f64>` | Soft intermediate output; exact plugins land, retained plugins may sample from a private retained interval |

An open live session has no fabricated FMI hard stop, but `advance_to(t)` still
reports exactly `t`, so an input applied after return has the requested
timestamp. A plugin's accepted time may differ from a hard stop, yield, or
current-time-plus-maximum-duration only within the session's scale-aware
roundoff tolerance; the checked step normalizes a matching public endpoint to
the host-issued coordinate. This does not truncate/reset the backend. The
tolerance is solver-neutral and equals
`max(100 * f64::EPSILON * (abs(current_time) + abs(accepted_interval_duration)),
f64::MIN_POSITIVE)`. The common accepted-interval containment predicate admits
only finite sample coordinates inside the closed interval under that tolerance;
all plugins MUST import that predicate rather than restating it. A private
numerical trial may overshoot, but the accepted step may not. A public batch
call asks the common session to reach its defined
end once; inside that call the session issues one backend request per accepted
internal step and consumes intermediate soft observations. Thus the numerical
request, duration-bound re-read, and completed-step callback have one identical
granularity.

After the materialization and root-application sequence above, the host
completes exactly one point for every accepted interval: the accepted endpoint
when no earlier event exists, or the truncated application point when root
search finds one. When `needsCompletedIntegratorStep` is true it calls
`fmi3CompletedIntegratorStep` exactly once at that point; otherwise it performs
no such call. If the callback requests Event Mode, all preceding observations
are already durable.

Backends do not receive event-indicator callbacks and do not contain crossing,
root-finder, arming, application-side, or simultaneous-event policy. RK dense
coefficients and Diffsol interpolation are private implementations of the same
declared-order interval sampler. The time-only plugin samples empty-state
points. A third solver owes one numerical step, its native continuous extension,
and truncate/reset; event meaning and lifecycle authority are identical host
behavior. No common type contains any solver's dense representation.

`fmi3Discard` is a typed component evaluation result, not a promised universal
step-rejection hook. A plugin may retry only when its library can prove that the
trial was rejected without committing state. Otherwise it returns the common
typed `MeIntegrationError::ComponentDiscard`; it MUST NOT turn the status or
the library's exhaustion into a string.

Rumoca follows MLS 3.7 §3.7.2.1's variable-step guidance by respecting the
current minimum positive delay time to avoid delay-buffer extrapolation. A
delay-bearing `FmiComponent` therefore declares the optional FMI metadata and
normal Float64 variable specified in §8. The host discovers that annotation
from `modelDescription`, reads the value only with standard `fmi3GetFloat64`,
maps the declared maximum-finite sentinel to no bound, and places any remaining
value in `MeAdvanceRequest.max_step_duration`. It re-reads and validates the
value immediately before every one-step backend request; it never caches it
across an accepted step, completed Event Mode, input mutation, or reset. Every
plugin ensures its accepted interval duration does not exceed this bound beyond
host roundoff by a private mechanism that MUST NOT mark solver state modified
merely to enforce the bound. Scan subdivision remains solely host policy. A
non-finite or non-positive non-sentinel read and an overlong accepted interval
are typed failures. Component metadata and preparation rules are owned solely
by §8; `DefaultExperiment.stepSize` remains an unrelated ignorable initial
hint.

### 7. Common-Host Cutover Inventory And Differential Evidence

The cutover removes every alternate master-algorithm or zero-state routing
producer in one change:

| Removed surface | Disposition |
|---|---|
| `SimulationBackend`, `run_with_runtime_schedule`, `runtime::orchestration`, `LoopStats`, `runtime::driver`, `SolverAdvanceBackend`, `SampleRecorder` | Delete; numerical bodies implement `MeIntegratorBackend` |
| `runtime::no_state`, `fmi_me::no_state`, `rumoca-solver-rk45::no_state` and crate-root re-exports | Delete; the common host scans, while `fmi_me::integrator::time_only` only advances/samples empty state |
| Diffsol `RuntimeOnlyDriver`, `SimulationSessionInner::RuntimeOnly`, `PreparedSimulationState::NoState`, and both prepared/batch no-state call sites | Delete; common session selects time-only plugin |
| RK45 `SimulationSessionInner::NoState` and all dispatch arms | Delete; common session selects time-only plugin |
| Diffsol/RK45 root callbacks, crossing classifiers, arming state, application probes, and root-result variants | Delete or disable; common host scans every accepted interval through the required sampler |
| `runtime::event` | Delete; `process_runtime_event_boundary`, `RuntimeEventBoundary`, `RuntimeEventBoundaryOutcome`, `RuntimeEventBoundaryHandler`, and `runtime_event_horizon` move to the component kernel; both moved processing and retained initialization consumers replace `runtime_event_right_limit` with the sole `timeline::bounded_event_right_limit_time(event_time, horizon, tolerance)`, defined as `event_right_limit_time(event_time, tolerance).min(horizon)`; `runtime_root_event_application_time` is deleted because only the host constructs that coordinate; delete or update all crate-root re-exports |
| `timeline::event_right_probe_time` | Delete with the superseded backend and old `fmi_me` host root probing; the common host uses the least sampled coordinate in the entered FMI domain |
| Backend-owned sessions, recorders, `SimResult` builders, event loops, schedule rebuilders, and horizon extenders | Delete; facade clients wrap `MeSimulationSession` |
| Frozen compatibility methods, duplicate initialization, component `max_step_size`, private crossing/arming, and component schedule queries | Remove under §8 dispositions |
| `MeRootProfile::DiffsolFrozen`, `MeNumericsProfile::DiffsolFrozen`, and every kernel or component branch on them | Delete; FMI and Modelica fix component semantics, while numerical choices remain private backend configuration and no common type names a solver |
| `rumoca-ir-fmi`, `rumoca-phase-fmi`, `FmiComponent::construct(SolveProblem, ...)`, public FMI metadata/wire inputs, consuming `into_solve`, and the separate `new_owned_with_fmi` artifacts argument | Delete without shims; `rumoca_ir_solve::fmi` owns the checked projection, the sole DAE-to-`SolveModel` operation retains its sealed variable catalog, and the always-available `rumoca_phase_solve::fmi` module consumes only that completed root under SPEC_0043 §8 |

Every production consumer of the deleted delay operation has an explicit
cutover:

| Former production consumer | Common-contract mapping |
|---|---|
| `rumoca-solver-rk45/src/me_integrator.rs::proposed_step` | Read only the request's checked latest accepted coordinate and cap the private RK trial duration |
| `rumoca-solver-diffsol/src/me_integrator.rs::advance` | Enforce §6's non-state-modifying obligation while privately converting the request's checked latest accepted coordinate to a Diffsol stop |
| `rumoca-solver/src/runtime/driver.rs::bound_stop_by_delay_history` call in the runtime driver | Delete the artificial master-loop stop; the selected plugin consumes the same `max_step_duration` |
| `rumoca-solver/src/fmi_me/no_state.rs::NoStateOrchestration::max_accepted_step_size` and `runtime/no_state.rs::no_state_root_scan_step_ceiling` | Delete; the common host owns one scan policy and the time-only plugin has no root policy |

Backend-version-specific mappings remain adapter-local:

| Reviewed backend surface | Adapter mapping |
|---|---|
| Diffsol 0.13 derivative callback and rejection surface | Because the reviewed library surface has no fallible callback or uncommitted reject hook, component `Discard` and library exhaustion map to typed `MeIntegrationError::ComponentDiscard`; dependency updates MUST re-audit this mapping |

The evaluator oracle becomes test-only component inspection: two components
receive one standard call sequence and compare the complete solver vector,
parameters, relation memory, and delay state after each call. It detects
evaluator drift only; the sole-host conformance suite replaces the old
independent sequencing failure class.

This migration deliberately changes these coordinates and classifications:

| Change | Required comparison |
|---|---|
| Diffsol evaluates retained nominal outputs before the later accepted-point callback/delay commit | Delay-free and variable-delay traces; every changed delayed value explained |
| Diffsol suppresses only a nominal point immediately behind a typed settled-event semantic instant | `SwitchWithArc`, grid-aligned root, and malformed-regression rejection |
| Diffsol and RK45 stop detecting/reporting roots; the common host scans their accepted intervals, chooses the earliest crossing, and constructs the application | Delay-bearing, multiple-root, direction-sensitive, equality-departure, and simultaneous-root traces across both backends |
| A backend's native continuous extension supplies its declared local order, agrees at both endpoints, and passes the manufactured-solution convergence suite | Full-order versus deliberately degraded first-order convergence, inflection and equal-endpoint-derivative witnesses, and event-coordinate deltas for RK45, Diffsol, time-only, and an unrelated solver |
| Continuous-Time Mode orients exact zero for static domains and the frozen previous-completed indicator domain; Event Mode biases all zero values into the newly settled domain | Both crossing directions and tangencies for every `RootZeroDomain`, exact-zero Diffsol boundaries, roots without relation-memory targets, no no-op tangency events, and post-Event-Mode nonzero vectors |
| RK45 receives shared event-left/settled roles and removes tolerance-only sample dropping | Cross-backend event traces |
| Zero-state stops routing through `NoContinuousStates` / `EmptySystem` | Zero-state batch/live/error census including worker `ModelFailureBucket::EmptySystem`, `ES011_EMPTY_SYSTEM`, and MSL `sim.empty_system` consumers |
| Live sessions use undefined stop metadata plus exact yield boundaries | Native/Wasm step/input/reset parity and `terminal()` fixtures |
| Event Mode may change continuous states and state nominals | Dynamic-state-selection fixtures refresh both before the next request; invalid nominal and failed-read injections prove atomic rollback |
| Scheduled roots leave the FMI indicator inventory; dynamic state-dependent time rows enter it | Count/order, root, coincidence, and `nextEventTime` evidence |
| Component discard, timeout, allocation, and termination mappings become common typed outcomes | Exhaustive conversions and worker diagnostic buckets |
| Host scanning adds component evaluations while Diffsol truncation after an applied root restarts BDF order | Scan-evaluation budgets; grazing/double-crossing resolution convergence; evidence that non-event bounds/yields do not reset order/difference history, binding bounds may defer order growth, and actual roots alone restart |

Before deleting an old producer, compare event-free, scheduled-event,
state-event, simultaneous-root, zero-state, input-updated live, reset,
allocation-failure, timeout, and variable-delay behavior. Cross-backend OMC
parity includes `SwitchWithArc` and a grid-aligned root. A complete 566-model
run must retain `sim_timeout` separately and explain every trace/error delta.
Because this is a runtime-contract cutover, any incomparable certified floor
reset MUST use SPEC_0033's one-shot baseline-migration process; an explained
delta alone cannot silently change a certified band.

### 8. Strict FMI Component Surface

The linked component operation set is the implemented FMI 3.0.2 ME surface.
Host conveniences are compositions outside it. Current extensions have these
mandatory dispositions:

| Current surface | Required disposition |
|---|---|
| `MeTime::event_boundary` | Remove; `fmi3SetTime` carries only time and relations remain frozen in Continuous-Time Mode |
| Argument-bearing `enter_event_mode` / `MeEventEntry` | Remove; `fmi3EnterEventMode` has no arguments |
| `MeStepCompletion` | Replace with full `fmi3CompletedIntegratorStep` inputs/outputs gated by `needsCompletedIntegratorStep` |
| `project_continuous_states` | Remove from Continuous-Time Mode; Event Mode reports changed continuous states |
| `next_event_stop` | Host derives only from cached `nextEventTimeDefined` / `nextEventTime` |
| `event_indicator_crossings` | Host classifies checked raw FMI vectors; no relation-memory value crosses the boundary |
| `capture_pre_event_state` / `arm_state_event` | Remove; component updates its state in argument-free Event Mode |
| `max_step_size` / `delay_step_limit` | Remove; optional namespaced annotation plus a standard getter supplies a maximum step duration, while component `Discard` stays typed and root scanning remains host-owned |
| `observe`, recorder, initial-observation queue | Host compositions of legal typed FMI getters; not component calls |
| `restart_from_fmu_state` | Host composition of advertised standard reset/state operations |
| `extend_stop_time`, public `ensure_end_time` / `set_end_time` | Remove; defined-stop sessions reset to change stop metadata |
| State-only directional-derivative helper | Remove; construction issues branded state and state-derivative value references, the host prepares and validates the standard `knowns[]` and `unknowns[]` lists once, and `fmi3GetDirectionalDerivative` checks the serialized `nSeed` and `nSensitivity` widths before evaluation |

The checked `FmiComponent` supplies model identity, variables/value references,
state/derivative order, event-indicator inventory, clocks, dependencies, units,
dimensions, causality, variability, starts, write policy, and ModelStructure.
Each FMI 3 Float64 variable retains its one exact value reference and checked
aggregate storage run. `fmi3GetFloat64` and `fmi3SetFloat64` concatenate the
row-major serialized values of those referenced variables, so
`nValueReferences` equals `nValues` only when every reference is scalar. The
linked component admits the complete branded reference batch and its total
checked serialized width before evaluating, writing component storage, or
copying a caller result; scalar names are not another FMI 3 value-reference
ABI. Acceptance includes one width-two array and one scalar in both orders
(`nValueReferences=2`, `nValues=3`), exact under/over-width rejection, caller
buffer preservation, and setter rollback.
For continuous states, construction derives the FMI `reinit` fact from the
typed source `reinit` owners once: Event-Mode Float64 state writes require the
issued `reinit=false` write policy, while reinitializable states remain
writable through the same setter only in Continuous-Time Mode.
It obeys the sole aggregate and consuming-view construction contract in
SPEC_0043 §8. The linked runtime receives
`rumoca_ir_solve::fmi::FmiComponent` rather than a `SolveModel`; concrete solver
crates receive only opaque component/evaluation handles. The always-available
`rumoca-phase-solve::fmi` module is the sole checked projection from the sealed
catalog retained by one completed `SolveModel`. DAE inspection and numeric
attribute evaluation occur only while that root is constructed; no FMI path
receives the DAE or repeats evaluation. The facade's `fmi` feature gates export
APIs, not a second construction path.
`rumoca-phase-codegen` consumes only the correlated view specified by
SPEC_0043 §8. The CLI requests this lowering only through the `rumoca-sim` facade
and has no production dependency on `rumoca-phase-solve`; the existing
`test_cli_uses_facades_not_phase_crates` architecture gate enforces that edge.

`fmi3GetEventIndicators` in Continuous-Time Mode returns each checked indicator
with exact zero oriented to `+EPSILON` for `RootZeroDomain::Positive`, to
`-EPSILON` for `NonPositive`, and for `Previous` into that indicator's frozen
previous-completed domain. The component owns one private domain cache aligned
with the complete checked indicator inventory, including indicators without a
relation-memory target. It seeds the cache after Initialization Mode and after
Event Mode, updates it only at a completed integrator step, and includes it in
FMU state and reset. `fmi3SetTime`, continuous-state setters, and getters MUST
NOT mutate the cache. An event-bearing Rumoca component declares
`needsCompletedIntegratorStep="true"` so the standard callback advances it.
Missing constructed frozen-domain storage is a typed refusal checked before
the caller's indicator buffer is mutated; the implementation does not
synthesize a side. A non-finite timestamp returned by an evaluator's `assert`
or `terminate` event action is likewise a typed refusal and is never replaced
with the enclosing event time.
The component also retains FMI 3.0.2 §3.2.1's exact `fmi3SetTime` lower-bound
continuation: `startTime`, the second-last completed-integrator-step time, and
the last enter-Event-Mode time. These facts participate in FMU state/reset;
rejection neither changes time nor any other component continuation.
The host alone classifies FMI's asymmetric domains (`z > 0` versus `z <= 0`);
exact-zero tangencies therefore cannot manufacture a no-op event.
`RootZeroDomain` is not a host crossing override. After Event Mode the component
biases every otherwise-zero indicator into the newly settled domain before
seeding the cache. Periodic-clock roots are absent from this vector and occur
only through the standard announced time event. A state/time coincidence enters
Event Mode once.

The maximum-step-duration annotation is optional FMI metadata and never a
private component operation. In Rumoca's namespace it names a normal Float64
local in the checked `FmiComponent` value-reference inventory with
`variability="continuous"` and `initial="calculated"`. The value evaluates the
current accepted coordinate's checked Modelica delay-time expressions rather
than delay history. When no positive delay expression currently constrains a
step it returns the documented IEEE 754 maximum finite Float64 sentinel. For a
checked Rumoca delay-bearing component, preparation rejects a missing
annotation, a non-Float64 or undeclared value reference, or an unreadable
referenced variable. The private `max_step_size` / `delay_step_limit` operation
is deleted. A Rumoca delay-bearing component declares
`needsCompletedIntegratorStep="true"`, because its accepted history is committed
only by that standard callback. The component's atomic typed `fmi3Discard`
remains authoritative when an importer ignores or outruns the hint. §6 solely
owns host discovery, fresh reads, sentinel mapping, validation, request
construction, and plugin enforcement.

Event-bearing packaged C does not yet implement this behavior. Dated
disposition 2026-08-18: the common-host cutover adds a fail-closed packaging
capability check; ME-EVENT-002 gates removing it on generated indicator
evaluation, relation freezing, official schema/ABI validation, and linked/package
trace parity. Event-free packages continue to emit and execute an empty checked
inventory. No XML may advertise an indicator the generated component cannot
evaluate.

Aggregate construction and consuming-view ownership is cataloged only in
SPEC_0043 §8, shared-helper ownership only in SPEC_0041 §1, crate and module
ownership only in SPEC_0041 §4, trace validity only in SPEC_0033, and Rust
module layout only in SPEC_0021. The FMI aggregates above add no parallel owner
or maintenance rule.
