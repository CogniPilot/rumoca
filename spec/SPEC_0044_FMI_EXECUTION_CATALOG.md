# SPEC_0044: FMI Execution Contract Catalog

## Status
REFERENCE

## Summary

Normative lookup catalog for the FMI execution and bounded-verification rules
owned by SPEC_0038.

## Specification

Rows and domains below are normative by reference from SPEC_0038.

### 1. Bounded ME Verification Profile

The dynamic FMI facade delegates to a private transition aggregate. Ordinary
transitions follow this table; snapshot restore is the scoped exception:

| Current state | Command | Next state |
|---|---|---|
| Instantiated | enter initialization | Initialization Mode |
| Initialization Mode | exit initialization | Event Mode |
| Event Mode | update discrete states | Event Mode |
| Event Mode | enter continuous-time mode | Continuous-Time Mode |
| Continuous-Time Mode | enter event mode | Event Mode |
| Any non-terminated state | terminate | Terminated |
| Any state | restore a component snapshot | state recorded by the snapshot |

Every other transition returns a typed contract failure without mutation.
Terminated is absorbing except for snapshot restore. Evaluation checks its
lifecycle capability before mutation.

| Obligation ID | Obligation | Required evidence |
|---|---|---|
| ME-LIFE-001 | Construction starts Instantiated; transition relation equals the table | Exhaustive ordinary test of 5 states × 6 commands |
| ME-LIFE-002 | Rejected transitions preserve lifecycle state | Exhaustive ordinary test of the same finite relation and production façade |
| ME-LIFE-003 | Terminated is absorbing except for snapshot restore | Exhaustive ordinary test of 6 commands, 17 active operations, and 5 restore targets |
| ME-LIFE-004 | Bounded convergent events settle; divergent fixed points return staged non-convergence | Ordinary examples and property tests over finite divergence increments |
| ME-ERR-001 | Stage annotation is idempotent; innermost stage wins | Exhaustive ordinary test of 6 recorded-stage choices × 5 incoming stages |
| ME-BUF-001 | Invalid bounded inputs do not partially mutate state or host buffers | Exhaustive ordinary tests of the named invalid-input classes below |
| ME-BRAND-001 | Foreign value references, observations, and snapshots reject before mutation | Exhaustive ordinary test of the 3 capability classes |
| ME-STATE-001 | Snapshot restore re-establishes lifecycle and observable state | Exhaustive ordinary test of all 5 lifecycle states |

The Kani profile contains exactly this symbolic bounded property:

| Claim | Symbolic domain | Counterexample meaning |
|---|---|---|
| SIM-010 | Three arbitrary typed event-history lanes and ownership masks | Pre advancement can lose type fidelity or partially mutate on rejection |

Kani proves only this property over its declared assumptions and unwind bound.
This is bounded runtime-kernel evidence, not a proof of compiler semantics.

ME-BUF-001 covers NaN and infinities as time, event boundary, or one state;
length mismatch; non-finite, foreign, and out-of-range value references; and
nine host-buffer classes: oversized state output, undersized directional seed,
invalid crossing shape, undeclared state-event index, oversized nominal output,
oversized state input, oversized sensitivity output, non-finite indicator input,
and output-series column mismatch.

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
| Construct one DAE/FMI aggregate from the same checked DAE provenance | `rumoca-ir-fmi` | Prevent mismatched ODE and residual models |
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
