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

| Proof ID | Obligation |
|---|---|
| ME-LIFE-001 | Construction starts Instantiated; transition relation equals the table |
| ME-LIFE-002 | Rejected transitions preserve lifecycle state |
| ME-LIFE-003 | Terminated is absorbing except for snapshot restore |
| ME-LIFE-004 | Bounded convergent events settle; divergent fixed points return staged non-convergence |
| ME-ERR-001 | Stage annotation is idempotent; innermost stage wins |
| ME-BUF-001 | Invalid bounded inputs do not partially mutate state or host buffers |
| ME-BRAND-001 | Foreign value references, observations, and snapshots reject before mutation |
| ME-STATE-001 | Snapshot restore re-establishes lifecycle and observable state |

Kani proves these properties only over each harness's declared bounds. Fixtures
contain one continuous state, at most one writable input, and enumerated finite
lifecycle, command, and operation classes. Non-convergence fixtures vary their
finite increment while retaining one Solve topology.

ME-BUF-001 covers NaN and infinities as time, event boundary, or one state;
length mismatch; non-finite, foreign, and out-of-range value references; and
nine host-buffer classes: oversized state output, undersized directional seed,
invalid crossing shape, undeclared state-event index, oversized nominal output,
oversized state input, oversized sensitivity output, non-finite indicator input,
and output-series column mismatch.

Harnesses call the production pure transition/property functions. Every finite
class has reachability coverage; separate reimplementations and property-test
fallbacks are validation, not proof. This profile excludes arbitrary-model
trajectory correctness, floating-point accuracy, solver convergence, and
end-to-end Modelica refinement.
