# SPEC_0049: Solve Grammar and Effect Catalog

## Status
REFERENCE

## Summary

The bound variant catalog for the closed factored Solve grammar of
[SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md) SEV-001, and
the capability keys that
[SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) TRP-042 makes
targets declare.

## How To Use This Catalog

This annex holds no governing rules; SEV-001, SEV-002, SEV-007, and TRP-042 do.
Each row is one grammar variant with its capability key, typed signature,
region and termination contract, effect and status class, and definitional
evaluator rule. Rows are normative by reference from those rules, and both
parents are DRAFT, so rows bind when their parent is accepted.

**Capability keys are SEMANTIC LEAVES, not discriminants.** A top-level
variant does not discharge SEV-007 or TRP-042: `Unary` alone would let a
backend admitting `Negate` claim `Sin`, `Binary` would hide integer-versus-real
and divide-by-zero status, and `Convert` would hide distinct source,
destination, and failure relations. Every embedded operator enum —
`SolveUnaryOperator`, `SolveBinaryOperator`, `SolveCompareOperator`,
`SolveConversionOperator`, `SolveReductionOperator` — therefore contributes its
own keys (§2), and a leaf may be further split by element kind where the
contract class differs (§3).

**Contract class.** Each leaf carries exactly one class, and ONLY the applicable
classes resolve a §4.3 arithmetic contract:

| Class | Meaning | Resolves §4.3? |
|---|---|---|
| `NotApplicable` | Loads, stores, control, calls, effects, terminators, pure index and shape operations | No — and a backend may not invent arithmetic for them |
| `ExactIntegral` | Integer arithmetic and integer-facing conversion; exact-in-domain or typed failure (SEV-025) | Domain and status fields only |
| `FloatingPrimitive` | Correctly-rounded real primitives | Yes |
| `FloatingTranscendental` | Real functions with a declared accuracy contract, not correct rounding | Yes, plus its accuracy contract |
| `Reduction` | Contraction and reduction over a domain | Yes, plus accumulator and order |

**Binding mechanism (NOT YET IMPLEMENTED).** The intended check is a test
comparing this catalog's key set against the Rust discriminants of
`SolveOperation` and every embedded operator enum in
`crates/rumoca-ir-solve/src/typed_program/program.rs`, failing on any difference
in either direction, so a new variant fails that test, every capability matcher,
and every total dispatcher until classified. **That test does not exist today**
— it is a promotion gate for this annex, recorded in SPEC_0047 §1.

**The grammar is not closed yet.** SEV-001's declared end-state factors
`EffectOp` and `Terminator` have no discriminants in the current tree, and the
volatile/atomic effect owners of SEV-016 are unspecified. §1 classifies the
CURRENT `SolveOperation` union; those factors are uncataloged, and this DRAFT
does not claim closure until they are.

**Snapshot.** §1 reflects the working tree; that crate is under concurrent
edit, so re-read before relying on it.

## Specification

### 1. Value, Invoke, and Effect Variants

Factor legend: **V** = `ValueOp`, **I** = `InvokeOp`, **E** = `EffectOp`,
**T** = `Terminator`. Effect class: **pure** = no effect, replayable and
CSE-eligible under SEV-046/049; **store** = writes a named slot; **status** =
may raise an observable status effect (SEV-026); **call** = effect footprint is
the callee's declared footprint.

| Capability key | Factor | Typed signature | Region / termination | Effect class | Evaluator rule |
|---|---|---|---|---|---|
| `Constant` | V | `() -> T` | none | pure | Yields the stored typed literal |
| `Load` | V | `() -> T` | none | pure | Reads the named slot's current generation |
| `Store` | E | `T -> ()` | none | store | Writes the slot; ordered against other effects on it |
| `Unary` | V | `T -> T` | none | pure or status | Applies the resolved SEV-024 contract |
| `Binary` | V | `(T, T) -> T` | none | pure or status | Applies the resolved SEV-024 contract; operands share `T` |
| `Compare` | V | `(T, T) -> Boolean` | none | pure | Ordering or equality per the profile's NaN rule; not a conversion (SEV-149) |
| `Convert` | V | `T -> U` | none | pure or status | The ONLY representation-changing operation (SEV-010) |
| `Select` | V | `(Boolean, T, T) -> T` | none | pure | Total: both arms are values, neither is skipped |
| `Conditional` | V | `(Boolean, region, region) -> T` | two regions, both terminate | pure or status | Exactly one region evaluates; arms share result type |
| `Map` | V | `(Tensor<T>…) -> Tensor<U>` | one body region over the domain | pure or status | Elementwise over the compact domain; empty domain yields empty |
| `Fold` | V | `(Tensor<T>, U) -> U` | one body region, bounded trip count | pure or status | Accumulates in the profile's declared order (SEV-024) |
| `Reduce` | V | `Tensor<T> -> T` | none | pure or status | Declared reduction with the profile's accumulator and order |
| `Scale` | V | `(T, Tensor<T>) -> Tensor<T>` | none | pure or status | Scalar-by-tensor product |
| `BroadcastBinary` | V | `(Tensor<T>, Tensor<T>) -> Tensor<T>` | none | pure or status | Shape-checked broadcast; no implicit rank change |
| `Transpose` | V | `Tensor<T> -> Tensor<T>` | none | pure | Index permutation only |
| `MatrixMultiply` | V | `(Tensor<T>, Tensor<T>) -> Tensor<T>` | none | pure or status | Contraction under the profile's accumulator and order |
| `Cross` | V | `(Tensor<T>, Tensor<T>) -> Tensor<T>` | none | pure or status | Three-element cross product |
| `Identity` | V | `() -> Tensor<T>` | none | pure | Square identity of the declared extent |
| `Diagonal` | V | `Tensor<T> -> Tensor<T>` | none | pure | Diagonal extraction or construction per declared direction |
| `Concatenate` | V | `(Tensor<T>…) -> Tensor<T>` | none | pure | Shape-checked join along the declared axis |
| `Fill` | V | `T -> Tensor<T>` | none | pure | Broadcasts one value over the declared shape |
| `ConstructAggregate` | V | `(T…) -> Tensor<T>` | none | pure | N authored operands; N is source-authored, not extent-derived (TRP-021) |
| `ProjectElement` | V | `Tensor<T> -> T` | none | pure | Static index; not a conversion (SEV-149) |
| `ProjectElementDynamic` | V | `(Tensor<T>, Integer) -> T` | none | pure or status | Dynamic index; out-of-range is a typed status, never UB |
| `ProjectSlice` | V | `Tensor<T> -> Tensor<T>` | none | pure | Checked static slice |
| `ProjectView` | V | `Tensor<T> -> Tensor<T>` | none | pure | Borrowed reshape or stride view; no copy implied |
| `SelectElement` | V | `(Tensor<T>, …) -> T` | none | pure or status | Checked element selection over the compact domain |
| `UpdateElement` | V | `(Tensor<T>, T) -> Tensor<T>` | none | pure or status | Functional update; the input tensor is unchanged |
| `UpdateSlice` | V | `(Tensor<T>, Tensor<T>) -> Tensor<T>` | none | pure or status | Shape-checked functional slice update |
| `UpdateView` | V | `(Tensor<T>, Tensor<T>) -> Tensor<T>` | none | pure or status | Functional update through a checked view |
| `Call` | I | `(T…) -> (U…)` | callee body is a separate relation | call | Invokes one `FunctionRelationId`; never merges with another invocation (SEV-048) |

Rows above carry class `NotApplicable` unless §2 or §3 assigns otherwise;
`Unary`, `Binary`, `Compare`, `Convert`, and `Reduce` are containers whose real
keys are the leaves in §2.

### 2. Operator Leaves

| Enum | Leaf keys | Class |
|---|---|---|
| `SolveUnaryOperator` | `Not` | `NotApplicable` (Boolean) |
| `SolveUnaryOperator` | `Negate`, `Abs`, `Sign` | `ExactIntegral` on integers, `FloatingPrimitive` on reals (§3) |
| `SolveUnaryOperator` | `Sqrt`, `Floor`, `Ceiling`, `Truncate` | `FloatingPrimitive` |
| `SolveUnaryOperator` | `Sin`, `Cos`, `Tan`, `Asin`, `Acos`, `Atan`, `Sinh`, `Cosh`, `Tanh`, `Exp`, `Log`, `Log10` | `FloatingTranscendental` |
| `SolveBinaryOperator` | `And`, `Or` | `NotApplicable` (Boolean) |
| `SolveBinaryOperator` | `Add`, `Subtract`, `Multiply`, `Min`, `Max` | `ExactIntegral` on integers, `FloatingPrimitive` on reals (§3) |
| `SolveBinaryOperator` | `Divide` | `ExactIntegral` on integers with divide-by-zero and `MIN/-1` status (SEV-025); `FloatingPrimitive` on reals |
| `SolveBinaryOperator` | `Power` | `FloatingTranscendental`; an integer exponent form is a distinct leaf with its own domain and status |
| `SolveBinaryOperator` | `Atan2` | `FloatingTranscendental` |
| `SolveCompareOperator` | `Equal`, `NotEqual`, `Less`, `LessEqual`, `Greater`, `GreaterEqual` | `NotApplicable`; result is `Boolean`, NaN ordering per the profile |
| `SolveConversionOperator` | `IntegerToReal` | `ExactIntegral` source, exact or inexact per destination format |
| `SolveConversionOperator` | `RealToIntegerTowardZero`, `RealToIntegerTowardNegativeInfinity` | `ExactIntegral` destination; out-of-domain is a typed failure, never UB. Each rounding direction is a SEPARATE key |
| `SolveReductionOperator` | `Sum`, `Product` | `Reduction` |
| `SolveReductionOperator` | `Minimum`, `Maximum` | `Reduction`, NaN handling per the profile |
| `SolveReductionOperator` | `All` | `NotApplicable` (Boolean) |

### 3. Element-Kind Splitting

A leaf whose contract class differs by element kind is TWO keys, not one: an
integer `Add` and a real `Add` are separate admissions with separate contracts.
A target admitting the real leaf has admitted nothing about the integer leaf,
and neither implies the Complex leaf. Where §2 names two classes, the split is
mandatory.

### 4. Capability Key Rules

| Rule | Why |
|---|---|
| One capability key per row; a target's §4.26 profile admits or rejects keys, never wildcards | Deny-unknown is only meaningful over a closed key set |
| A key admitted for one element type is NOT admitted for another; the value profile (§4.24) and the operation profile (§4.26) are checked together | `f32` `MatrixMultiply` proves nothing about `Binary64` |
| Region-bearing variants are admitted only with their body's transitive closure (SEV-148) | An admitted `Fold` whose body calls a rejected op is not admissible |
| A status-class variant requires the product's declared status transport (§4.25) | An operation that can raise needs somewhere to raise to |

## References

- [SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md) SEV-001,
  SEV-002, SEV-007 — the grammar rules these rows enumerate.
- [SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) TRP-042 —
  the capability profile these keys close.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) — evidence,
  field catalogs, and the gate registry.
