# SPEC_0007: Lean DAE (No Derived Data)

## Status
ACCEPTED

## Summary
The DAE representation contains only essential mathematical content. Derived data (incidence matrix, sparsity patterns, BLT ordering) is computed by downstream tools, not stored in the DAE.

## Specification

### What DAE Contains

Located in `rumoca-ir-dae/src/lib.rs`:

```rust
pub struct Dae {
    // ── Variables (all IndexMap<VarName, Variable>) ──
    pub states: ...,              // x — continuous with derivatives
    pub algebraics: ...,          // y — without derivatives
    pub inputs: ...,              // u — externally provided
    pub outputs: ...,             // w — computed from states/algebraics
    pub parameters: ...,          // p — fixed during simulation
    pub constants: ...,           // fixed at compile time
    pub discrete_reals: ...,      // z — change only at events (MLS B.1b)
    pub discrete_valued: ...,     // m — Boolean, Integer, enum (MLS B.1c)
    pub derivative_aliases: ...,  // defined by ODE equations but not states

    // ── Equations (MLS Appendix B) ──
    pub f_x: Vec<Equation>,   // 0 = f_x(v, c) — continuous (B.1a)
    pub f_z: Vec<Equation>,   // z = f_z(v, c) — discrete Real (B.1b)
    pub f_m: Vec<Equation>,   // m := f_m(v, c) — discrete-valued (B.1c)
    pub f_c: Vec<Equation>,   // c := f_c(relation(v)) — conditions (B.1d)
    pub relation: Vec<Expression>,               // relation(v) used by f_c (B.1d)
    pub synthetic_root_conditions: Vec<Expression>, // extra root guards
    pub scheduled_time_events: Vec<f64>,         // compile-time event instants
    pub clock_constructor_exprs: Vec<Expression>, // clock() constructor forms
    pub clock_schedules: Vec<ClockSchedule>,     // lowered periodic schedules
    pub initial_equations: Vec<Equation>,

    // ── Metadata ──
    pub is_partial: bool,
    pub class_type: ClassType,
    pub functions: IndexMap<VarName, Function>,
    pub interface_flow_count: usize,
}
```

### What DAE Does NOT Contain

| Derived Data | Why Excluded | Computed By |
|--------------|--------------|-------------|
| Incidence matrix | O(n*m) storage, stale if equations change | Downstream structural analysis |
| Sparsity patterns | Solver-specific | CasADi `jacobian_sparsity()`, diffsol |
| BLT ordering | Depends on solver mode (DAE vs ODE) | `rumoca-phase-structural` |
| Index reduction | May add dummy derivatives | Pantelides algorithm |
| Alias elimination | Heuristic — "which variable is primary?" | Backend optimization |

### Design Rules

**REQUIRED:**
- Variables stored as flat `IndexMap<VarName, Variable>` fields (not wrapped in a sub-struct)
- Equations use MLS Appendix B naming: `f_x`, `f_z`, `f_m`, `f_c`
- All variable categories in separate fields (not a single `vars` map with a kind discriminant)

**PROHIBITED:**
- Storing incidence matrices, BLT orderings, or sparsity patterns in the Dae struct
- Adding mutable "cached" fields that must be recomputed on change
- Merging variable categories into a single collection
- Carrying model-level `when_clauses`, `algorithms`, or `initial_algorithms` in solver-facing DAE
- Emitting unlowered synchronous operators in solver equation partitions (`f_x`, `initial_equations`), including `sample/hold/Clock/subSample/superSample/shiftSample/backSample/noClock/firstTick/previous`

## Rationale
- DAE is a data exchange format, not a solver
- Downstream tools have domain-specific optimization
- Smaller serialized size, no staleness bugs
- MLS Appendix B naming makes the mapping to the specification self-documenting

## References
- MLS Appendix B: Modelica DAE Representation
- SPEC_0019: Array Preservation (dims on Variable)
- SPEC_0020: Model Algorithm Lowering and Function Algorithm Preservation
