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
| `ExactIntegral` | Integer arithmetic; exact-in-domain or typed failure (SEV-025) | Domain and status fields only |
| `FloatingPrimitive` | Correctly-rounded real primitives | Yes |
| `FloatingTranscendental` | Real functions with a declared accuracy contract, not correct rounding | Yes, plus its accuracy contract |
| `Reduction` | Contraction and reduction over a domain | Yes, plus accumulator and order |
| `RelationalEquality` | Equality and inequality; same-shape Boolean result | No arithmetic, but binds NaN and signed-zero policy |
| `RelationalOrdering` | Ordering comparisons; same-shape Boolean result | No arithmetic, but binds NaN ordering and signed-zero policy |
| `Conversion` | Representation change, keyed by source repr, destination repr, direction, rounding, and status | Its own conversion contract, not the operand's |
| `Indexing` | Dynamic coordinate access; total or failing per the leaf | No arithmetic; binds the out-of-range relation |

**Binding mechanism (NOT YET IMPLEMENTED).** The promotion gate for this annex
has TWO parts, because a name-set comparison alone would have passed every
factual error this catalog previously contained — wrong arities, a missing
`out_of_range` operand, tuple destinations described as single values, and
integer leaves the constructor rejects:

1. **Key-set exhaustiveness** — compare this catalog's key set against the Rust
   discriminants of `SolveOperation` and every embedded operator enum in
   `crates/rumoca-ir-solve/src/typed_program/program.rs`, failing on any
   difference in either direction.
2. **Constructor PARITY cases** — for each row, a mutation case proving the
   synopsis matches the checked constructor: an admitted element kind the row
   claims and a rejected one it excludes, the exact operand arity, and each
   distinguishing operand such as `SelectElement`'s `out_of_range`.

Until both exist, a new variant fails nothing. **Neither exists today**;
SPEC_0047 §1 records this.

**The grammar is not closed yet.** SEV-001's declared end-state factors
`EffectOp` and `Terminator` have no discriminants in the current tree, and the
volatile/atomic effect owners of SEV-016 are unspecified. §1 classifies the
CURRENT `SolveOperation` union; those factors are uncataloged, and this DRAFT
does not claim closure until they are.

**Snapshot.** §1 reflects the working tree; that crate is under concurrent
edit, so re-read before relying on it.

## Specification

### 1. Value, Invoke, and Effect Variants

**The checked constructors are the semantic authority.** `TypedProgram`'s
`push_*` constructors in `crates/rumoca-ir-solve/src/typed_program/program.rs`
and `program/tensor.rs` define these operations; this section is a complete
faithful SYNOPSIS of them, verified field by field, never a competing
definition. Where synopsis and constructor disagree, the constructor governs
and this row is a defect.

Status: **Current** = constructible in the working tree today; **Proposed** =
declared end state that construction does not admit yet.

| Capability key | Status | Synopsis of the checked constructor | Region / termination | Effect class | Evaluator rule |
|---|---|---|---|---|---|
| `Constant` | Current | `() -> T` typed literal | none | `NotApplicable` | Yields the stored typed literal |
| `Load` | Current | `() -> T` from a named slot | none | `NotApplicable` | Reads the slot's current generation |
| `Store` | Current | `T -> ()` to a named slot | none | store | Writes the slot; ordered against other effects on it |
| `Unary` | Current | `T -> T`, element kind per §2 | none | per §2 leaf | Applies the resolved SEV-024 contract |
| `Binary` | Current | `(T, T) -> T`, operands identical, element kind per §2 | none | per §2 leaf | Applies the resolved SEV-024 contract |
| `Compare` | Current | `(T, T) -> Boolean` of the SAME SHAPE. Equality accepts any two identical types including Boolean; ordering additionally requires a numeric element | none | `Relational` (§2) | Elementwise relation under the profile's NaN and signed-zero policy; not a conversion (SEV-149) |
| `Convert` | Current | `T -> U` per the §2 conversion leaf | none | `Conversion` (§2) | The ONLY representation-changing operation (SEV-010) |
| `Select` | Current | `(Boolean, T, T) -> T` | none | `NotApplicable` | Total: both arms are values, neither is skipped |
| `Conditional` | Current | `condition: Boolean`, `captures: [reg]`, `destinations: [reg]` — a TUPLE of results — and two region bodies | two regions, both terminate | `NotApplicable` | Exactly one region evaluates; each destination takes that region's corresponding result |
| `Map` | Current | `domain`, `captures: [reg]`, one `destination`, body region | one body region over the compact domain | `NotApplicable` at this level | Elementwise over the domain; empty domain yields empty |
| `Fold` | Current | `domain`, `initial: [reg]` and `destinations: [reg]` — CARRIED TUPLES — `captures: [reg]`, transition region | one transition region, trip count bounded by the domain | `NotApplicable` at this level | Threads the carried tuple through the domain in the profile's declared order |
| `Reduce` | Current | `Tensor<T> -> T` scalar. `Sum`/`Product`/`Minimum`/`Maximum` require a numeric element; `All` requires Boolean. Operand must be non-scalar | none | `Reduction` for numeric leaves, `NotApplicable` for `All` | Declared reduction with the profile's accumulator and order |
| `Scale` | Current | `(scalar, aggregate) -> aggregate`; aggregate non-scalar, scalar rank-0, numeric element | none | per §2 `Multiply` leaf | Elementwise multiplication of the aggregate by the scalar |
| `BroadcastBinary` | Current | `(aggregate, scalar) -> aggregate` with `scalar_on_lhs` recording operand order; operator drawn from §2 | none | per §2 leaf | Elementwise with the scalar broadcast, preserving operand order |
| `Transpose` | Current | `Tensor<T> -> Tensor<T>` index permutation | none | `NotApplicable` | Permutes indices; no element conversion |
| `MatrixMultiply` | Current | Checked product dimensions; result is a tensor, or a SCALAR when the product dimensions are empty | none | `Reduction` | Contraction under the profile's accumulator and order |
| `Cross` | Current | Both operands exactly `[3]`, numeric element | none | `Reduction` | Three-element cross product |
| `Identity` | Current | `identity(element_type, extent) -> [extent, extent]`. Real always admitted; Integer admitted only when its domain contains 0 AND 1; Boolean REJECTED; element type must belong to the root arithmetic | none | `NotApplicable` | Square identity of the declared extent |
| `Diagonal` | Current | Rank-1 `[extent]` numeric operand to a square matrix — CONSTRUCTION only, never extraction | none | `NotApplicable` | Places the vector on the diagonal, zero elsewhere |
| `Concatenate` | Current | Same-rank operands joined on `axis` | none | `NotApplicable` | Shape-checked join |
| `Fill` | Current | `T -> Tensor<T>` over the declared shape | none | `NotApplicable` | Broadcasts one value |
| `ConstructAggregate` | Current | N authored `elements` | none | `NotApplicable` | N is source-authored, never extent-derived (TRP-021) |
| `ProjectElement` | Current | Static checked index | none | `NotApplicable` | In-bounds by construction |
| `ProjectElementDynamic` | Current | `indices: [reg]` — ONE scalar Integer register per rank, one-based Modelica coordinates. No fallback operand | none | `Indexing` — status | Out-of-range is a typed failure; there is no in-band result |
| `SelectElement` | Current | `indices: [reg]` — ONE per rank — PLUS an explicit `out_of_range` value register | none | `Indexing` — total | Out-of-range yields the supplied `out_of_range` value; distinct from `ProjectElementDynamic`'s failure relation |
| `ProjectSlice` | Current | Zero-based checked `origin` | none | `NotApplicable` | Checked static slice |
| `ProjectView` | Current | Affine `axes` view, rank-preserving or rank-reducing | none | `NotApplicable` | Borrowed view; no copy implied |
| `UpdateElement` | Current | `(aggregate, value, indices: [reg])` — one index per rank | none | `Indexing` — status | Functional update; input aggregate unchanged |
| `UpdateSlice` | Current | `(aggregate, value, origin)` zero-based | none | `NotApplicable` | Functional slice update |
| `UpdateView` | Current | `(aggregate, value, axes)` | none | `NotApplicable` | Functional update through a checked view |
| `Call` | Current | `owner: SolvePureCallOwnerId`, `arguments: [reg]`, `destinations: [reg]`. Aggregate arguments and results each retain ONE typed register; destinations are ordered VALUE results followed by ASSERTION PREDICATES exactly as declared by the owner interface | callee body is a separate `TypedProgram` | call | Invokes one compiler-issued pure-call owner atomically; never merges with another invocation (SEV-048) |

**Aspirational.** SEV-001's `InvokeOp`/`EffectOp`/`Terminator` factoring is the
proposed end state. Today `Call` is the single pure-call owner above — there is
no general effectful invoke, no volatile/atomic effect owner (SEV-016), and no
terminator discriminant. Those factors are uncataloged and the grammar is NOT
closed; SPEC_0047 §1 carries the gap rows.

### 2. Operator Leaves

Element-kind columns record what the checked constructor ADMITS today.

| Enum | Leaf keys | Admitted element kinds (constructor) | Class | Status |
|---|---|---|---|---|
| `SolveUnaryOperator` | `Not` | Boolean only | `NotApplicable` | Current |
| `SolveUnaryOperator` | `Negate`, `Abs`, `Sign` | numeric: Real or Integer | `ExactIntegral` on Integer, `FloatingPrimitive` on Real (§3) | Current |
| `SolveUnaryOperator` | `Sqrt`, `Floor`, `Ceiling`, `Truncate` | Real only | `FloatingPrimitive` | Current |
| `SolveUnaryOperator` | `Sin`, `Cos`, `Tan`, `Asin`, `Acos`, `Atan`, `Sinh`, `Cosh`, `Tanh`, `Exp`, `Log`, `Log10` | Real only | `FloatingTranscendental` | Current |
| `SolveBinaryOperator` | `And`, `Or` | Boolean only | `NotApplicable` | Current |
| `SolveBinaryOperator` | `Add`, `Subtract`, `Multiply`, `Min`, `Max` | numeric: Real or Integer | `ExactIntegral` on Integer, `FloatingPrimitive` on Real (§3) | Current |
| `SolveBinaryOperator` | `Divide` | **Real only** | `FloatingPrimitive` | Current |
| `SolveBinaryOperator` | `Power` | **Real only** | `FloatingTranscendental` | Current |
| `SolveBinaryOperator` | `Atan2` | **Real only** | `FloatingTranscendental` | Current |
| `SolveBinaryOperator` | Integer `Divide` with divide-by-zero and `MIN/-1` status (SEV-025) | — | `ExactIntegral` | **Proposed** |
| `SolveBinaryOperator` | Integer-exponent `Power` with its own domain and status | — | `ExactIntegral` | **Proposed** |
| `SolveCompareOperator` | `Equal`, `NotEqual` | any two IDENTICAL types, Boolean included | `RelationalEquality` | Current |
| `SolveCompareOperator` | `Less`, `LessEqual`, `Greater`, `GreaterEqual` | numeric: Real or Integer | `RelationalOrdering` | Current |
| `SolveConversionOperator` | `IntegerToReal` | Integer to Real | `Conversion` | Current |
| `SolveConversionOperator` | `RealToIntegerTowardZero` | Real to Integer | `Conversion` | Current |
| `SolveConversionOperator` | `RealToIntegerTowardNegativeInfinity` | Real to Integer | `Conversion` | Current |
| `SolveReductionOperator` | `Sum`, `Product` | numeric | `Reduction` | Current |
| `SolveReductionOperator` | `Minimum`, `Maximum` | numeric | `Reduction`, NaN handling per the profile | Current |
| `SolveReductionOperator` | `All` | Boolean only | `NotApplicable` | Current |

**`Relational*` classes.** A comparison resolves no arithmetic contract, but it
is not policy-free: the Real relation depends on the profile's NaN ordering and
signed-zero policy, so `RelationalEquality` and `RelationalOrdering` each bind
that policy and are split by element kind (§3). Both produce a Boolean of the
SAME SHAPE as their operands.

**`Conversion` class.** A conversion is keyed by source representation,
destination representation, direction, rounding, and status — not by the
destination alone. `IntegerToReal` has no source exception but MAY round at the
destination format; `RealToInteger*` carries source exceptional and rounding
behavior PLUS a destination-domain failure. Each rounding direction is a
separate key.

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
