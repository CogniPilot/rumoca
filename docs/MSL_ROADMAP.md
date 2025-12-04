# Rumoca MSL 100% Roadmap

This document outlines the path to achieving 100% compile and balance rates for the Modelica Standard Library (MSL) 4.1.0.

## Current Status (December 2024)

| Metric | Current | Target |
|--------|---------|--------|
| Parse Rate | 100% (2551/2551) | 100% |
| Compile Rate | 38% (860/2283) | 100% |
| Balance Rate | 62% (532/860) | 100% |
| Total Failures | 1751 models | 0 |

## Error Analysis Summary

### Compile Errors: 1423 models

| Category | Models | Priority | Complexity |
|----------|--------|----------|------------|
| Extends/Modifier resolution | 256 | **P0** | Medium |
| Modelica.Constants | 115 | **P0** | Low |
| Complex number type | 111 | P2 | High |
| Protected variable resolution | 97 | **P0** | Medium |
| StateSelect enumeration | 63 | **P1** | Low |
| GravityTypes enumeration | 53 | P1 | Low |
| Dynamics enumeration | 49 | P1 | Low |
| Clocked Modelica | 48 | P3 | Very High |
| Qualified name resolution | 46 | **P0** | Medium |
| Quasi-static references | 43 | P1 | Medium |
| inner/outer (World) | 41 | P2 | High |
| SI Unit conversions | 33 | P2 | Medium |
| Table interpolation | 31 | P2 | High |
| cardinality() function | 28 | **P1** | Low |
| Clock type | 26 | P3 | Very High |
| Connector resolution | 23 | **P0** | Medium |
| semiLinear() function | 16 | **P1** | Low |
| SPICE constants | 15 | P2 | Low |
| Init enumeration | 12 | **P1** | Low |
| Stream connectors | 9 | P2 | High |
| String() function | 7 | **P1** | Low |
| delay() function | 6 | P2 | Medium |
| Other | 268 | Various | Various |

### Unbalanced Models: 328 models

| Category | Models | Root Cause |
|----------|--------|------------|
| Under-determined | 264 | Missing equations from extends/connectors |
| Zero equations | 55 | Digital/visualization/abstract models |
| Over-determined | 9 | Connect equation expansion issues |

---

## Phase 1: Quick Wins (Target: 55% → 70% compile rate)

### 1.1 Modelica.Constants Support (~115 models)

**Impact: 115 models**

Add built-in constants from `Modelica.Constants`:

```rust
// In flatten/builtin.rs or similar
const MODELICA_CONSTANTS: &[(&str, f64)] = &[
    ("pi", std::f64::consts::PI),
    ("e", std::f64::consts::E),
    ("mu_0", 1.25663706212e-6),  // Magnetic constant
    ("epsilon_0", 8.8541878128e-12),  // Electric constant
    ("c", 299792458.0),  // Speed of light
    ("h", 6.62607015e-34),  // Planck constant
    ("k", 1.380649e-23),  // Boltzmann constant
    ("N_A", 6.02214076e23),  // Avogadro constant
    // ... etc
];
```

**Files to modify:**
- `src/flatten/resolve.rs` - Add constant lookup
- `src/ir/builtin.rs` - Define constants

### 1.2 Enumeration Types (~178 models)

**Impact: StateSelect (63) + GravityTypes (53) + Dynamics (49) + Init (12) = 177 models**

Implement proper enumeration resolution for MSL types:

```modelica
type StateSelect = enumeration(never, avoid, default, prefer, always);
type Init = enumeration(NoInit, SteadyState, InitialState, InitialOutput);
type GravityTypes = enumeration(NoGravity, UniformGravity, PointGravity);
type Dynamics = enumeration(DynamicFreeInitial, FixedInitial, SteadyStateInitial, SteadyState);
```

**Implementation:**
1. Parse enumeration type definitions
2. Resolve enumeration literal references (e.g., `StateSelect.prefer`)
3. Store enumeration values as integers in DAE

**Files to modify:**
- `src/flatten/resolve.rs` - Enumeration lookup
- `src/ir/ast.rs` - Enumeration representation

### 1.3 Built-in Functions (~51 models)

**Impact: cardinality (28) + semiLinear (16) + String (7) = 51 models**

```rust
// Built-in functions to implement
fn cardinality(connector) -> Integer  // Deprecated but needed
fn semiLinear(x, positiveSlope, negativeSlope) -> Real
fn String(value, ...) -> String
```

**Files to modify:**
- `src/flatten/builtin_functions.rs` - Function implementations

---

## Phase 2: Core Flattening Fixes (Target: 70% → 85% compile rate)

### 2.1 Extends/Modifier Resolution (~256 models)

**Impact: 256 models (largest category)**

Single-letter variables (L, C, R, S, T, f, i, j, k, v, mh, Ri, nu) are not being resolved from extended classes. This indicates issues with:

1. **Modifier propagation** - Values passed to extended classes
2. **Protected section handling** - Variables in protected sections of base classes
3. **Redeclaration** - `redeclare` of components

**Root causes to investigate:**
```modelica
model Child
  extends Parent(L=1e-3);  // L should be resolved from Parent
end Child;
```

**Files to modify:**
- `src/flatten/flatten.rs` - Extends handling
- `src/flatten/modifier.rs` - Modifier propagation

### 2.2 Protected Variable Resolution (~97 models)

**Impact: 97 models**

Variables like `LossPower`, `PRef`, `IaNominal` in protected sections are not being found. This is related to 2.1 but specifically for protected members.

**Files to modify:**
- `src/flatten/resolve.rs` - Protected section lookup

### 2.3 Qualified Name Resolution (~46 models)

**Impact: 46 models**

Nested component references like `sine.Modelica`, `frame_a.R`, `diode_p.p.v` are not being resolved correctly.

**Files to modify:**
- `src/flatten/resolve.rs` - Multi-level lookup

### 2.4 Connector Resolution (~23 + 43 = 66 models)

**Impact: 66 models**

- `in_p`, `in_n` - Input connectors not resolved
- `port_p.reference` - Quasi-static reference angles
- `apparentPower` - Calculated connector quantities

**Files to modify:**
- `src/flatten/connector.rs` - Connector handling

---

## Phase 3: Advanced Features (Target: 85% → 95% compile rate)

### 3.1 Complex Number Type (~111 models)

**Impact: 111 models**

Implement `Complex` record type and operators:

```modelica
record Complex
  Real re "Real part";
  Real im "Imaginary part";
end Complex;

// Operators: +, -, *, /, conj, abs, arg, etc.
```

**Files to modify:**
- `src/ir/builtin_types.rs` - Complex type
- `src/flatten/operators.rs` - Complex operators

### 3.2 inner/outer (~41 models)

**Impact: 41 models (specularCoefficient, World)**

Implement `inner`/`outer` component lookup:

```modelica
model World
  inner Modelica.Mechanics.MultiBody.World world;
end World;

model Body
  outer Modelica.Mechanics.MultiBody.World world;  // Lookup to inner
end Body;
```

**Files to modify:**
- `src/flatten/resolve.rs` - inner/outer lookup
- `src/flatten/flatten.rs` - Scope management

### 3.3 SI Unit Conversions (~33 models)

**Impact: 33 models**

Handle `displayUnit` and `conversionTable` annotations:

```modelica
type Temperature = Real(unit="K", displayUnit="degC");
```

**Files to modify:**
- `src/flatten/units.rs` - Unit conversion

### 3.4 Table Interpolation (~31 models)

**Impact: 31 models**

Implement `CombiTable1D`, `CombiTable2D`:
- External file reading (CSV, MAT)
- Interpolation algorithms

**Files to modify:**
- `src/ir/builtin_classes.rs` - Table classes
- `src/flatten/external.rs` - External data handling

### 3.5 Stream Connectors (~9 models)

**Impact: 9 models**

Implement `inStream()` and `actualStream()`:

```modelica
connector FluidPort
  flow Real m_flow;
  stream Real h_outflow;
end FluidPort;
```

**Files to modify:**
- `src/flatten/stream.rs` - Stream connector semantics

### 3.6 delay() Function (~6 models)

**Impact: 6 models**

Implement time delay:

```modelica
y = delay(u, delayTime);
```

**Files to modify:**
- `src/flatten/builtin_functions.rs` - delay implementation

---

## Phase 4: Clocked Modelica (Target: 95% → 100% compile rate)

### 4.1 Clock Types (~26 models)

```modelica
Clock c = Clock(0.1);  // Periodic clock
Clock c = Clock(condition);  // Event clock
```

### 4.2 Clocked Operators (~48 models)

```modelica
y = previous(x);       // Previous value
y = hold(u);          // Hold value
y = sample(u, clock); // Sample continuous signal
dt = interval(u);     // Time since last tick
// Plus: shiftSample, backSample, subSample, superSample, noClock, firstTick
```

**Note:** Clocked Modelica (Modelica 3.3 Synchronous Elements) is a significant undertaking requiring:
- Clock inference algorithm
- Partitioning of clocked/continuous equations
- Base clock and sub/super sampling

**Files to modify:**
- `src/ir/clocked.rs` - Clock representation
- `src/flatten/clocked.rs` - Clock inference
- `src/dae/clocked.rs` - Clocked DAE generation

---

## Phase 5: Balance Fixes (Target: 62% → 100% balance rate)

### 5.1 Connect Equation Expansion

**Impact: ~100 models**

Ensure connect equations generate correct number of equations:
- Flow variables: sum = 0
- Potential variables: equality
- Stream variables: proper mixing equations

### 5.2 Digital Logic Models

**Impact: ~55 models (zero equations)**

Digital models have algorithm sections, not equations. Balance checking should:
- Count algorithm assignments as equations
- Handle discrete-valued variables correctly

### 5.3 Visualization/Abstract Models

**Impact: ~50 models**

Some models are intentionally unbalanced (partial, visualization):
- Skip balance check for `partial` models
- Handle `annotation(__Dymola_LockedEditing="...")`

### 5.4 MultiBody Joints

**Impact: ~15 models**

Large under-determination in joint models due to:
- Missing equations from `frame_a`, `frame_b` resolution
- `Orientation` record handling

---

## Implementation Priority

### Sprint 1 (Quick Wins)
1. [ ] Modelica.Constants (115 models) - 1-2 days
2. [ ] Enumeration types (177 models) - 2-3 days
3. [ ] cardinality/semiLinear/String (51 models) - 1-2 days

**Expected result: ~55% → ~70% compile rate**

### Sprint 2 (Core Fixes)
1. [ ] Extends/modifier resolution (256 models) - 1-2 weeks
2. [ ] Protected variable resolution (97 models) - 3-5 days
3. [ ] Qualified name resolution (46 models) - 3-5 days
4. [ ] Connector resolution (66 models) - 3-5 days

**Expected result: ~70% → ~85% compile rate**

### Sprint 3 (Advanced Features)
1. [ ] Complex number type (111 models) - 1 week
2. [ ] inner/outer (41 models) - 1 week
3. [ ] SI unit conversions (33 models) - 3-5 days
4. [ ] Table interpolation (31 models) - 1 week
5. [ ] Stream connectors (9 models) - 1 week
6. [ ] delay() function (6 models) - 1-2 days

**Expected result: ~85% → ~95% compile rate**

### Sprint 4 (Clocked Modelica)
1. [ ] Clock types and operators (74 models) - 2-3 weeks

**Expected result: ~95% → ~100% compile rate**

### Sprint 5 (Balance Fixes)
1. [ ] Connect equation expansion - 1 week
2. [ ] Digital/algorithm handling - 3-5 days
3. [ ] Abstract model detection - 1-2 days
4. [ ] MultiBody joint fixes - 1 week

**Expected result: ~62% → ~100% balance rate**

---

## Testing Strategy

### Per-Feature Tests
Each feature should have:
1. Unit test with minimal Modelica code
2. Integration test with relevant MSL models
3. Regression test to prevent breakage

### MSL Regression Suite
Run full MSL test after each major change:
```bash
MSL_PATH=/path/to/MSL cargo test test_msl_balance_with_json_export -- --nocapture
```

### Target Models for Validation
Priority validation models:
- `Modelica.Blocks.Continuous.TransferFunction` - Core control block
- `Modelica.Electrical.Analog.Basic.Resistor` - Basic electrical
- `Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum` - MultiBody
- `Modelica.Fluid.Examples.HeatingSystem` - Fluid system

---

## Success Metrics

| Milestone | Compile Rate | Balance Rate | Timeline |
|-----------|--------------|--------------|----------|
| Current | 38% | 62% | - |
| Sprint 1 | 70% | 65% | +1 week |
| Sprint 2 | 85% | 75% | +3 weeks |
| Sprint 3 | 95% | 85% | +6 weeks |
| Sprint 4 | 100% | 90% | +9 weeks |
| Sprint 5 | 100% | 100% | +11 weeks |

---

## References

- [Modelica Specification 3.6](https://specification.modelica.org/)
- [MSL 4.1.0 Source](https://github.com/modelica/ModelicaStandardLibrary)
- [Modelica Synchronous Elements (Ch. 16)](https://specification.modelica.org/master/synchronous-language-elements.html)
- [Stream Connectors (Ch. 15.2)](https://specification.modelica.org/master/stream-connectors.html)
