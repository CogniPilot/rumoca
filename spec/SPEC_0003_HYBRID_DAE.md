# SPEC_0003: Complete Hybrid DAE Formulation

## Status
ACCEPTED

## Summary
The DAE representation follows the complete Hybrid DAE formulation from Modelica Book Chapter 17, including continuous variables, discrete variables, and events.

## Motivation
Modelica models are hybrid systems combining:
- Continuous-time dynamics (ODEs, DAEs)
- Discrete-time events (when-clauses)
- Parameters and constants

A complete DAE representation must capture all these aspects for downstream solvers.

## Specification

### Variable Categories

| Symbol | Name | Description |
|--------|------|-------------|
| x(t) | States | Variables that appear differentiated |
| ẋ(t) | Derivatives | Time derivatives of states |
| z(t) | Algebraic | Continuous variables, not differentiated |
| u(t) | Inputs | External inputs to the model |
| y(t) | Outputs | Designated output variables |
| p | Parameters | Known before simulation, constant during |
| c | Constants | Known at compile time |
| q(te) | Discrete | Change only at event times te |
| pre(q) | Pre-values | Value of q just before event |

### Equation Categories

**Continuous-time (between events):**
```
0 = f(x, ẋ, z, u, t, q, pre(q), p)    — differential-algebraic
0 = g(x, z, u, t, q, pre(q), p)        — algebraic constraints
```

**Discrete-time (at events):**
```
q := h(x, z, u, pre(q), p, c)          — discrete updates
c := conditions that triggered event
```

### Data Structures

```rust
pub struct Vars {
    pub x: IndexMap<VarId, Var>,       // States
    pub z: IndexMap<VarId, Var>,       // Algebraic
    pub u: IndexMap<VarId, Var>,       // Inputs
    pub y: Vec<VarId>,                 // Output refs
    pub p: IndexMap<VarId, Var>,       // Parameters
    pub c: IndexMap<VarId, Const>,     // Constants
    pub q: IndexMap<VarId, DiscreteVar>, // Discrete
}

pub struct Eqs {
    pub ode: Vec<(EqId, Eq)>,   // Differential
    pub alg: Vec<(EqId, Eq)>,   // Algebraic
    pub init: Vec<(EqId, Eq)>,  // Initial
}

pub struct Event {
    pub id: EventId,
    pub condition: Expr,
    pub actions: Vec<EventAction>,
}

pub enum EventAction {
    Assignment { target: VarId, value: Expr },
    Reinit { var: VarId, value: Expr },
    Assert { condition: Expr, message: String },
    Terminate { message: String },
}
```

### Variable Classification Algorithm

```
1. Collect all variables from Flat model
2. For each variable:
   a. If appears in der(x), classify as state (x)
   b. If has input causality, classify as input (u)
   c. If has parameter variability, classify as parameter (p)
   d. If has constant variability, classify as constant (c)
   e. If has discrete variability, classify as discrete (q)
   f. Otherwise, classify as algebraic (z)
3. Mark output causality variables in y (may overlap with x, z, q)
```

### Residual Form
All equations are stored in residual form: `0 = residual`

```rust
pub struct Eq {
    pub residual: Expr,
}

// Original: x = y + z
// Residual: 0 = y + z - x
```

## Rationale
- Follows established mathematical formulation from Modelica literature
- Explicit partitioning aids downstream tools (solvers, code generators)
- Event representation supports hybrid simulation

## References
- Modelica Book, Chapter 17: Hybrid DAE
- Modelica Specification §8: Equations
- Cellier & Kofman: Continuous System Simulation
