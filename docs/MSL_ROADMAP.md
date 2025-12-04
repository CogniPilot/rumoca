# Rumoca MSL 100% Balance Rate Roadmap

This document outlines the path to achieving 100% balance rate for the Modelica Standard Library (MSL) 4.1.0.

## Current Status (December 2024)

| Metric | Current | Target |
|--------|---------|--------|
| Parse Rate | 100% (2551/2551) | 100% |
| Compile Rate | **100% (2283/2283)** | 100% |
| Balance Rate | 32% (728/2283) | 100% |
| Unbalanced Models | 1555 | 0 |

**Milestone achieved:** 100% compile rate!

---

## Unbalanced Model Analysis

### Summary: 1555 unbalanced models

| Category | Est. Models | Priority | Complexity |
|----------|-------------|----------|------------|
| Connect equation expansion | ~600 | **P0** | Medium |
| Algorithm sections | ~350 | **P0** | Low |
| Partial/abstract models | ~250 | P1 | Low |
| Array equation handling | ~200 | **P0** | Medium |
| External functions | ~100 | P2 | High |
| When equation counting | ~55 | P1 | Low |

### Detailed Breakdown

#### Under-determined Models (~1200)
Models with fewer equations than unknowns:
- Missing flow equations from connect expansion
- Algorithm assignments not counted
- Array equations not properly counted
- Inherited equations missing

#### Over-determined Models (~100)
Models with more equations than unknowns:
- Duplicate connect equations
- Array equation over-counting
- Conditional equation issues

#### Zero-equation Models (~255)
Models with no equations:
- Digital logic (algorithm-only)
- Visualization models
- Abstract/partial base classes
- Icon-only models

---

## Phase 1: Connect Equation Fixes (Target: 32% → 55% balance rate)

### 1.1 Flow Variable Summation (~300 models)

**Impact: ~300 models**

Connect equations with flow variables should generate sum=0 equations:

```modelica
connect(a.i, b.i, c.i);
// Should generate: a.i + b.i + c.i = 0
```

**Current issue:** Flow equations may be missing or duplicated.

**Files to modify:**
- `src/ir/transform/flatten.rs` - `expand_connect_equations()`

### 1.2 Potential Variable Equality (~200 models)

**Impact: ~200 models**

Connect equations with potential variables should generate equality:

```modelica
connect(a.v, b.v);
// Should generate: a.v = b.v
```

**Current issue:** Some potential equations missing.

**Files to modify:**
- `src/ir/transform/flatten.rs` - Connect expansion logic

### 1.3 Hierarchical Connect Resolution (~100 models)

**Impact: ~100 models**

Connectors inside components need proper flattening:

```modelica
model M
  Resistor R;
equation
  connect(R.p, ...);  // R.p.v, R.p.i need resolution
end M;
```

**Files to modify:**
- `src/ir/transform/flatten.rs` - Nested connector handling

---

## Phase 2: Algorithm Section Handling (Target: 55% → 70% balance rate)

### 2.1 Algorithm Assignment Counting (~250 models)

**Impact: ~250 models**

Algorithm sections contain assignments that define variables:

```modelica
algorithm
  x := 1;      // Defines x
  y := x + 1;  // Defines y
```

Each assigned variable in an algorithm section counts as one equation.

**Implementation:**
1. Count unique assigned variables in algorithm sections
2. Add to equation count for balance checking

**Files to modify:**
- `src/ir/transform/balance.rs` - Algorithm counting
- `src/dae/mod.rs` - DAE equation counting

### 2.2 When Statement Handling (~55 models)

**Impact: ~55 models**

When statements in algorithms define discrete variables:

```modelica
algorithm
  when event then
    x := pre(x) + 1;
  end when;
```

**Files to modify:**
- `src/ir/transform/balance.rs` - When statement counting

### 2.3 Discrete Variable Detection (~45 models)

**Impact: ~45 models**

Discrete variables from `pre()`, `edge()`, `change()` need special handling.

**Files to modify:**
- `src/dae/mod.rs` - Discrete variable classification

---

## Phase 3: Array Equation Handling (Target: 70% → 85% balance rate)

### 3.1 Array Equation Expansion (~150 models)

**Impact: ~150 models**

Array equations count as multiple scalar equations:

```modelica
Real x[3], y[3];
equation
  x = y;  // This is 3 equations, not 1
```

**Implementation:**
1. Evaluate array dimensions at compile time
2. Multiply equation count by array size

**Files to modify:**
- `src/ir/transform/balance.rs` - Array size multiplication
- `src/dae/mod.rs` - Array equation counting

### 3.2 For-Loop Equation Counting (~50 models)

**Impact: ~50 models**

For-loop equations generate multiple equations:

```modelica
equation
  for i in 1:n loop
    x[i] = y[i];  // n equations
  end for;
```

**Files to modify:**
- `src/ir/transform/balance.rs` - For-loop expansion counting

---

## Phase 4: Partial/Abstract Model Handling (Target: 85% → 95% balance rate)

### 4.1 Partial Model Detection (~200 models)

**Impact: ~200 models**

Partial models are intentionally incomplete:

```modelica
partial model Base
  Real x;
  // No equation for x - intentional
end Base;
```

**Implementation:**
1. Detect `partial` keyword
2. Skip balance check for partial models
3. Or mark as "intentionally unbalanced"

**Files to modify:**
- `src/ir/ast.rs` - Add partial flag to ClassType
- `src/dae/mod.rs` - Skip balance for partial

### 4.2 Abstract/Icon-Only Models (~50 models)

**Impact: ~50 models**

Some models are for documentation/icons only:
- `Modelica.Icons.*`
- `Modelica.UsersGuide.*`
- Models with `__Dymola_LockedEditing`

**Implementation:**
1. Detect icon-only patterns (no components, no equations)
2. Mark as documentation models

**Files to modify:**
- `src/dae/mod.rs` - Icon model detection

---

## Phase 5: External Functions & Edge Cases (Target: 95% → 100% balance rate)

### 5.1 External Function Handling (~75 models)

**Impact: ~75 models**

External functions don't have Modelica equations but define outputs:

```modelica
function myFunc
  input Real x;
  output Real y;
  external "C" y = c_func(x);
end myFunc;
```

**Files to modify:**
- `src/ir/transform/balance.rs` - External function handling

### 5.2 Record Constructor Equations (~25 models)

**Impact: ~25 models**

Record constructors implicitly define all fields:

```modelica
Complex c = Complex(re=1, im=2);
// Implicitly: c.re = 1; c.im = 2;
```

**Files to modify:**
- `src/ir/transform/flatten.rs` - Record constructor expansion

### 5.3 Conditional Component Handling (~30 models)

**Impact: ~30 models**

Conditional components affect equation count:

```modelica
parameter Boolean use_T = true;
HeatPort port if use_T;  // Only exists if use_T=true
```

**Files to modify:**
- `src/ir/transform/flatten.rs` - Conditional evaluation

---

## Implementation Priority

### Sprint 1: Connect Equations
1. [x] ~~Basic connect expansion~~ (done)
2. [ ] Flow variable summation fix
3. [ ] Potential variable equality fix
4. [ ] Hierarchical connector handling

**Expected result: 32% → 55% balance rate**

### Sprint 2: Algorithm Sections
1. [ ] Count algorithm assignments as equations
2. [ ] When statement handling
3. [ ] Discrete variable detection

**Expected result: 55% → 70% balance rate**

### Sprint 3: Array Equations
1. [ ] Array equation size multiplication
2. [ ] For-loop equation counting
3. [ ] Parameter-dependent array sizes

**Expected result: 70% → 85% balance rate**

### Sprint 4: Partial Models
1. [ ] Partial model detection
2. [ ] Icon-only model handling
3. [ ] Documentation model exclusion

**Expected result: 85% → 95% balance rate**

### Sprint 5: Edge Cases
1. [ ] External function handling
2. [ ] Record constructor equations
3. [ ] Conditional component evaluation

**Expected result: 95% → 100% balance rate**

---

## Testing Strategy

### Per-Feature Tests
Each fix should have:
1. Unit test with minimal Modelica code
2. Integration test with affected MSL models
3. Regression test to prevent breakage

### MSL Regression Suite
Run full MSL test after each sprint:
```bash
MSL_PATH=/path/to/MSL cargo test test_msl_balance_all -- --ignored --nocapture
```

### Sample Validation Models

**Connect equations:**
- `Modelica.Electrical.Analog.Basic.Resistor`
- `Modelica.Electrical.Analog.Examples.ChuaCircuit`

**Algorithm sections:**
- `Modelica.Blocks.Discrete.*`
- `Modelica.Electrical.Digital.*`

**Array equations:**
- `Modelica.Blocks.Continuous.StateSpace`
- `Modelica.Mechanics.MultiBody.*`

**Partial models:**
- `Modelica.Electrical.Analog.Interfaces.*`
- `Modelica.Blocks.Interfaces.*`

---

## Success Metrics

| Sprint | Balance Rate | Balanced Models | Unbalanced |
|--------|--------------|-----------------|------------|
| Current | 32% | 728 | 1555 |
| Sprint 1 | 55% | ~1256 | ~1027 |
| Sprint 2 | 70% | ~1598 | ~685 |
| Sprint 3 | 85% | ~1940 | ~343 |
| Sprint 4 | 95% | ~2169 | ~114 |
| Sprint 5 | 100% | 2283 | 0 |

---

## References

- [Modelica Specification 3.6 - Equation Balance](https://specification.modelica.org/)
- [MSL 4.1.0 Source](https://github.com/modelica/ModelicaStandardLibrary)
- [Connect Equations (Ch. 9.1)](https://specification.modelica.org/master/connectors-and-connections.html)
- [Algorithm Sections (Ch. 11)](https://specification.modelica.org/master/statements-and-algorithm-sections.html)
