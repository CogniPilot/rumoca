# SPEC_0035: Complex Numeric Types In Solve IR

## Status
DRAFT

## Summary

Solve IR operates on real and complex tensors. Complex is a tensor element
type, not a preserved record; ordinary records stay scalarized.

## Motivation

- Hardware and backends with native complex support (GPU complex types, the MLIR
  complex dialect, complex BLAS kernels) cannot use structure an earlier phase
  dissolved into components.
- A complex product carries algebraic structure a pair of reals does not: the
  Jacobian block is `[[a, -b], [b, a]]`, and a complex linear solve beats the
  equivalent real system of twice the dimension.
- `TensorElementType` is currently a single-variant enum (`Real64`). The
  extension point exists; nothing uses it.
- MLS §14 is general, so recognition must come from the operator declaration,
  not from matching the name `Complex`.

## Specification

### 1. The Value Domain

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Solve IR values are real or complex tensors | `rumoca-ir-solve` | Numeric types hardware understands |
| Complex is a precision-neutral `TensorElementType` | `ir-solve/src/tensor.rs` | Not a record, not a pair |
| Ordinary records stay scalarized | Flat/DAE | Organizational structure, no numeric semantics |
| Element type and layout are separate | tensor metadata | Interleaved vs planar is a backend concern |
| Canonical element types contain no deployment width | DAE/Solve | One target-neutral problem |

**Width is a target policy, not a Modelica type.** MLS §4.8.1 defines exactly
one floating-point type, `Real`, with implementation-defined precision; there is
no `Real32`/`Real64` in the language, and GALEC likewise declares only `Real`
(`rumoca-ir-galec` `ScalarType`). The width is chosen at the codegen boundary
from the target's capability — eFMI's production-code manifest already carries
`efmiFloat32`/`efmiFloat64` for exactly this reason, and embedded control
targets commonly want the 32-bit form.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Source and GALEC declare `Real`, never a width | frontend / algorithm code | MLS has one float type |
| Canonical DAE/Solve use `Real` and `Complex` | compiler IR | Precision-neutral semantics |
| The target selects the width | codegen | Precision is a deployment property |
| A narrowing selection is explicit and recorded | export manifest | Silent precision loss is a defect |

Target projection uses total-width ecosystem names:

| Canonical + target format | NumPy / PyTorch / TF | MLIR | CUDA | WGSL |
|---|---|---|---|---|
| `Real + F64` | `float64` | `f64` | `double` | *unavailable* |
| `Real + F32` | `float32` | `f32` | `float` | `f32` |
| `Complex + F64` | `complex128` | `complex<f64>` | `cuDoubleComplex` | *unavailable* |
| `Complex + F32` | `complex64` | `complex<f32>` | `cuComplex` | `vec2<f32>` |

**The test for preservation is numeric semantics, not aggregation.** A `Pin`
with `v` and `i` is organizational: no solver, kernel or hardware unit benefits
from knowing the two are grouped, so it scalarizes. Complex has an algebra, a
hardware representation and derivative rules, so it survives as a type.

**PROHIBITED:**
- Preserving ordinary records into Solve IR.
- Representing a complex value as two independent real unknowns in Solve IR.
- Branching on the record name `Complex`, or on member names `re`/`im`.

### 2. Frontend Recognition

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Operator applications resolve to the declaring function | resolve/instantiate | Semantics are not name matching |
| A record whose operator bodies are proved equivalent to complex formulas maps to the complex type | DAE lowering | Recognition is semantic |
| An unrecognized operator record scalarizes | DAE lowering | Only numeric types earn a type |
| A refusal to map is reason-coded | lowering | Fallbacks stay legible |

MLS §14 operator records are the mechanism by which a frontend recognizes
complex arithmetic. Signatures alone cannot prove the algebra. Recognition
uses resolved declaration identities and an exact restricted normalizer over a
two-Real-field record. It proves constructor/pack, zero, negation, addition,
subtraction, multiplication, division, equality, conjugate, and magnitude
formulas. Core type recognition requires constructor/pack, zero, negation,
addition, and multiplication. Other operations receive individual proofs;
unproved operations retain their exact real-field lowering.

Recognition never branches on record or field names. Its proof receipt retains
the effective `TypeId`, operator `DefId`s, source spans, and normalized formula
fingerprints.

### 3. Solver State And Layout

The solver state vector stays REAL. Modelica has no complex primitive; a complex
state is two real states, and every integrator is real-valued. Making an
integrator complex-typed would invent a type the language does not have and fork
every solver.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| State slots are real | solver runtime | Integrators are real-valued |
| The layout records the component pairing | variable layout | One id, N component offsets |
| Complex layout defaults to interleaved | `TensorLayout` | `cuComplex`, MLIR, NumPy, C99 all interleave |
| Planar layout is opt-in | `TensorLayout` | Only some FFT/BLAS-like kernels want it |
| View and typed form agree by construction | all consumers | Choice is performance, not semantics |

The pairing rides on `(DefId, offset)` layout identity: one id, two offsets.

### 4. Kernels And Derivatives

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Existing tensor nodes accept complex elements | `Map`, `AffineStencil`, `MatMul`, `LinSolve` | One node set, two element types |
| The evaluator runs a native complex kernel | `rumoca-eval-solve` | One operation over two lanes |
| Differentiation is exact over real lanes | AD | Modelica state remains real |
| Native complex AD requires a proved holomorphic operation | AD | Non-holomorphic rules differ |
| A backend without complex support requests the real view | codegen | Correct default |

**REQUIRED:** a complex node and its derived real view MUST produce equal
results.

Conjugate, magnitude, real/imag projections, and other non-holomorphic
operations use explicit real-lane derivative rules. A complex dependency is
expanded conservatively to a full 2-by-2 real block unless a checked derivative
rule proves internal zeros. Structural pattern ownership and expansion follow
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).

### 5. Relationship To SPEC_0032

SPEC_0032 keeps *range* structure compact: an equation over an index domain,
with scalar rows as views. This spec adds an *element type* to the same tensors.
The two are orthogonal — a model may carry an array of complex values — and
neither may dissolve the other. Range ownership is outer: a structured family
over complex elements keeps both the domain and the element type.

## Current State

DRAFT because the codebase does none of this yet:

| Observation | Evidence |
|---|---|
| `TensorElementType` has one variant | `ir-solve/src/tensor.rs` — `Real64` only |
| `TensorLayout` has one variant | same file — `RowMajorDense` only |
| Operator applications are not resolved | Magnetic converter row reaches DAE as a record-level product |
| Production still contains textual Complex special cases | frontend/lowering search gate |
| 77 Magnetic models, 1 simulates | MSL corpus run |

The name-matching sites measure the debt: each guesses at semantics the operator
declaration already states.

## Implementation Plan

| Phase | Delivers | Proves |
|---|---|---|
| 1 | Operator applications resolve to the declaring `operator function` | A minimal model shows the resolved `DefId` in Flat |
| 2 | Restricted exact normalizer and branded complex-algebra proof | Correct unnamed record is recognized; incorrect named record is refused |
| 3 | Precision-neutral DAE/Solve complex element + real-lane view | Solve IR round-trips a complex node |
| 4 | Native complex kernel in the evaluator | Evaluation equals the derived real view |
| 5 | One native backend consumes complex; exact real views elsewhere | Backend emits a complex operation |
| 6 | Retire the `"Complex"` name matches; add the enforcement gate | Gate fails on a reintroduced name match |

**Promotion:** ACCEPTED when phases 1-3 are implemented and the name-match debt
is ratcheted — SPEC_0000 §4 prohibits ACCEPTED specs describing unimplemented
behaviour.

**Ordering:** phases 3-5 deliver the hardware benefit; phase 6 last, but not
indefinitely — the name matches are what make the fix look optional.

**This spec is a capability, not a bug fix.** An earlier draft claimed phase 2
would unblock the Magnetic cohort. That claim was measured and is FALSE.
Complex arithmetic already resolves today (`phase-structural/src/scalarize/
projection.rs` `project_complex_mul_or_div`), and a hand-scalarized rewrite of
`Magnetic.FundamentalWave.Examples.Components.SinglePhaseInductance` with no
Complex rows at all produces the byte-identical structural failure. That cohort
is blocked by index reduction and by constant folding of parameter bindings,
not by operator records. Record-level rows do surface later, as an `EL002`
missing-binding error in Solve lowering, which is what phases 1-2 address —
but only after the earlier blockers are cleared.

## References

- MLS 3.7 §14 — Overloaded Operators
- MLS 3.7 §12.4 — Built-in and Overloaded Operator Functions
- `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md` — range ownership, the orthogonal axis
- `spec/SPEC_0039_PROOF_CARRYING_SPARSITY.md` — element-block dependency patterns
- `spec/SPEC_0007_IR_PIPELINE.md` — phase ownership and stage contracts
- `spec/SPEC_0008_PHASE_ERRORS.md` — spanned errors, no silent recovery
- `Complex.mo` in the Modelica Standard Library — the canonical operator record
