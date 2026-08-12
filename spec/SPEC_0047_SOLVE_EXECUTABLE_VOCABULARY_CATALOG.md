# SPEC_0047: Solve Vocabulary and Target Refinement Catalog

## Status
REFERENCE

## Summary

Shared evidence and field-catalog annex for **two** parents:
[SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)
(`SEV-NNN` rules) and
[SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) (`TRP-NNN`
rules).

## How To Use This Catalog

This annex holds no governing rules. Every rule lives in SPEC_0045 or SPEC_0048;
the rows here are the inventory, gate registry, recorded alternatives, and bound
field catalogs those rules link. Rows are normative by reference from the parent
section that links them, and both parents are DRAFT, so the rows bind when their
parent is accepted. §1 is present-tense fact maintained with the implementation:
a change that closes a gap updates its row in the same commit. Every §2 row
names its parent rule in `Covers`, so "every gate cited" is checkable.

## Specification

### 1. Current Implementation State

Checked typed programs, arithmetic profiles, exact values, wire replay,
`Map`/`Fold` tensor operations, and several consumers exist today. Each row is
`Partial` (present, not to contract) or `Absent`.

| State | Present-tense fact | Where | Closing edge |
|---|---|---|---|
| Partial | `arithmetic_profile()` is hard-coded Binary64 and full-width i64; target selection does not bind it | `rumoca-phase-solve/src/lower/typed_functions.rs` | SEV-023 |
| Partial | `TensorCapabilities.dtypes` is `Option<Vec<String>>` validated only against empty strings; WGSL advertises `f32` while consuming the f64 path | `rumoca-compile/src/codegen_target.rs`; WGSL | TRP-012; red until SEV-108 |
| Absent | `SolveTemplateRenderer::{new, new_with_dae, …}` receives Solve/DAE/artifacts, never a checked target profile or sealed plan | `rumoca-phase-codegen` | TRP-018 |
| Partial | `solve_template_compute_block_json` builds the native partition AND `to_scalar_program_block` unconditionally, exposing both views before any strategy is selected | `rumoca-phase-codegen/src/codegen/mod.rs` | TRP-018 |
| Partial | `scalar_fallback` is `Option<bool>` treated as true, so an omitted decision silently licenses scalarization | `rumoca-compile/src/codegen_target.rs` | TRP-012, TRP-016 |
| Partial | CUDA and rust-fixed templates choose `TensorLoad`/`MatrixMultiply` shapes in MiniJinja and spell every register `f64`. A Jinja extent loop is legal when it passively renders a sealed plan; the defects are template-side CHOICE and the unbound `f64` | target templates | TRP-014, TRP-018 |
| Partial | `lower_solve_package` exists and `rumoca-sim` retains `pure_calls`, but the problem-only `lower_solve_problem` entry point remains and no session seals one package for all consumers | `rumoca-phase-solve/src/lib.rs` | TRP-010 |
| Partial | Capability validation lowers a Solve root and discards it; renderer resolution lowers again (FMI explicitly, the generic renderer via `OnceLock`); AlgorithmCode lowers DAE independently; WGSL selection is a hard-coded manifest-name case | `rumoca-compile` | TRP-010, TRP-011 |
| Partial | `embedded-c-galec` and `galec-production` consume `ir="algorithm-code"`, spell C types independently, and hardcode `efmiFloat32`/I32/Bool | GALEC targets | TRP-030 |
| Partial | Typed Integer construction admits same-typed `Add`/`Sub`/`Mul` without range closure: under int32, MAX+1 fails in the interpreter, is out-of-domain `2147483648` in Cranelift I64, and is UB in generated C; `Real64(2147483648) -> int32` diverges likewise | construction; backends | SEV-025 |
| Partial | Binary32 interpreter intrinsics run f32 while Cranelift promotes to f64 and demotes; the public Cranelift typed adapter transports Boolean/Integer through a universal f64 slice | interpreter; Cranelift | SEV-024; SEV-102 |
| Partial | Records and empty values are a phase-private leaf layout: record identity disappears, zero-extent tensors reject, all-zero outputs cannot own a call | Solve lowering | SEV-013, SEV-015 |
| Absent | No structural term merging exists: DAE issues fresh `ExprId`s, lowering memoizes those exact IDs, and `TypedProgramBuilder` emits a new spanned op per use | `rumoca-ir-dae`, `rumoca-phase-solve` | SEV-044…SEV-049 |
| Absent | `RootDigest`, `PreparedDigest`, and `ArtifactDigest` do not exist as distinct recomputable digests | `rumoca-ir-solve`, `rumoca-compile` | SEV-041, TRP-011 |
| Absent | No volatile or atomic effect owner exists; `volatile` is an emitter spelling | backends, templates | SEV-016 |
| Absent | No WASM, MLIR, or C consumer of `SolveOperation` exists | backend crates | SEV-007 |
| Absent | `ValueCapabilityProfile`, `OperationEffectCapabilityProfile`, and `ExecutionEnvironmentProfile` do not exist; a target's numeric declaration is the only admission check, so nonnumeric families, grammar operations, and effects are never closed over | `rumoca-compile` | TRP-039, TRP-042 |
| Partial | `rust-fixed` is HOSTED and explicitly not `no_std` per its own README; it is allocator-free only inside the derivative call, and its template, runtime-math, panic, and library behavior is not transitively checked | rust-fixed target | TRP-039 |
| Absent | No Kani rows prove the type, conversion, identity, or refinement claims | proof manifest | §2 rows |

### 2. Red Gates And Witnesses (Preregistered)

| ID | Gate | Passes when | Covers |
|----|------|-------------|--------|
| SEV-100 | int32 MAX+1 and conversion boundaries | Interpreter, Cranelift, and generated C agree, including the typed failure class | SEV-025 |
| SEV-101 | Binary32 parity | Bit-exact results and identical status across interpreter, native, generated code | SEV-024 |
| SEV-102 | Boolean and large-integer typed backend ABI | Values cross as tagged bit-exact cells; no f64 transport path exists | SEV-012 |
| SEV-103 | Profile mutation | Changing the arithmetic profile changes the constructed `RootDigest` | SEV-020, SEV-041 |
| SEV-104 | Forged wire mutation | Representation, value, record, enum-brand, and empty-layout forgeries reject on decode | SEV-091 |
| SEV-105 | Nested capability closure | Nested typed calls and regions cannot evade target capability closure | SEV-002, TRP-010 |
| SEV-106 | Million-element preparation | Metadata and preparation stay O(owner + rank) for loads AND outputs, with zero per-coordinate hashes | TRP-020, TRP-021 |
| SEV-107 | View exclusivity | No complete target exposes both native and scalar alternatives to a template | TRP-018 |
| SEV-108 | WGSL f32 | Standing RED profile-mismatch witness until its typed root is truly Binary32 | SEV-023 |
| SEV-120 | Volatile double read | Two source reads of one volatile cell stay two reads; no interning, hoisting, or reuse merges them | SEV-016, SEV-026 |
| SEV-121 | Logical empty shapes | `[0,3]` and `[0,4]` round-trip distinctly; swapping or dropping an empty argument rejects | SEV-015 |
| SEV-122 | Mixed signed/unsigned widths | A mixed-signedness operation without an explicit conversion rejects in every backend | SEV-010, SEV-022 |
| SEV-123 | Enum-brand swap | Substituting ordinal 1 of one enum for another rejects at construction and on decode | SEV-014 |
| SEV-124 | Sensitivity profile IDs | Ideal, quantized, and straight-through programs carry distinct IDs and cannot be substituted | SEV-030…SEV-032 |
| SEV-125 | Dot-product discriminator | `[1e20, -1e20, 1] · [1,1,1]` plus NaN, signed-zero, subnormal, and status cases give one declared result and status across loop, unroll, fused, and catalog-kernel candidates; numerically different candidates require distinct roots | SEV-024, TRP-015 |
| SEV-126 | Kernel-receipt mutation | Altering any receipt field invalidates the prepared plan | TRP-013 |
| SEV-127 | Composite-plan coverage | A tiled kernel plus remainder proves total coverage with no gap or overlap, and equivalence on predicate overlaps only | TRP-014 |
| SEV-128 | Two-line `x+y` | Both spans survive through wire; a forced hash collision still compares exact keys | SEV-045…SEV-047 |
| SEV-129 | `a:=x+y; x:=x+1; b:=x+y` | The two terms do not merge; read versions distinguish them | SEV-045 |
| SEV-130 | Identical calls and asserts | They remain two ordered owners; an inactive faulting branch never merges with an active one | SEV-048 |
| SEV-131 | Interning invariance | Off, local, and eager interning produce byte-identical canonical wire | SEV-046, SEV-091 |
| SEV-132 | Root distinctness | One DAE built under Binary32 and Binary64 yields two distinct roots (`16_777_217`) | SEV-003 |
| SEV-133 | Layer movement | A template-only change preserves `RootDigest` and `PreparedDigest` while moving `ArtifactDigest` | SEV-041, TRP-011 |
| SEV-134 | eFMI correlation mutation | Mutating any AC↔Solve source, body, lifecycle, or effect correlation makes the package reject | TRP-030 |
| SEV-135 | Four-way comparison | `eval-galec`, the definitional Solve evaluator, compiled C, and OMC agree; PC→AC traceability holds within preregistered code-size, metadata, and construction-time budgets | TRP-030, TRP-031 |
| SEV-136 | Sensitivity digest separation | One source under one arithmetic profile yields DISTINCT `RootDigest`s for ideal versus quantized sensitivity; claimed profile metadata that disagrees with the recomputed digest rejects; no artifact ever presents one digest with two claimed profiles, and a foreign directional root substituted under a primal's derivation edge rejects | SEV-034, SEV-041 |
| SEV-137 | `NumericProfile` parse and normalization | `f32`+`i32`, `f64`+`i64`, and a mixed allowed set each parse, deny unknown keys, normalize to §4.21 fields, and produce the expected distinct `RootDigest`s; changing EITHER default — Real or Integer — independently changes the normalized profile bytes and `RootDigest`; unsigned widths parse only as declared representations; an unavailable format rejects with its reserved-shape reason | SEV-021, TRP-012, TRP-033 |
| SEV-138 | Profile mismatch against a built root | A target request disagreeing with an already-profiled root rejects; no path converts a built root to another width | TRP-034 |
| SEV-139 | Range refinement without conversion | Two `i32` inputs with ranges `[0,10]` and `[0,100]` add with NO type conversion while the result interval is derived; the same value narrowed to `i8` requires an explicit checked conversion; `i8` `MAX+1` is statically rejected or returns the one typed overflow status, never host promotion or UB | SEV-018, SEV-025 |
| SEV-140a | Admitted-unroll compactness | One million-element `Map` whose `BoundedUnroll` budget ADMITS it stays O(source ops + rank) through root and through prepared-plan construction, measured BEFORE rendering; no extent-sized preparation metadata exists at any point | TRP-035, TRP-036 |
| SEV-140b | Over-budget rejection | A separately over-budget million-element case rejects, or selects its explicitly admitted loop fallback and emits exactly one loop | TRP-035, TRP-036 |
| SEV-140c | Bounded small shape | A small admitted fixed shape renders exactly the bounded instruction count; changing the threshold moves `PreparedDigest` and never `RootDigest`, provided both outputs pass the TRP-015 exact root-relation receipt | TRP-036, TRP-015 |
| SEV-141 | Signed representation stability | An `i32` value with derived range `[0,10]` retains semantic `I32` and an unchanged `RootDigest`; only an explicit checked conversion yields semantic `U32`; unsigned storage selected under TRP-037 leaves the semantic type and digest untouched and carries its §4.23 receipt | SEV-018, TRP-033, TRP-037 |
| SEV-142 | Profile set normalization | Permutations and duplicates of an allowed set normalize to identical canonical profile bytes and `RootDigest`; an empty kind set rejects; EITHER default absent from its kind's set — Real or Integer, independently — rejects rather than being unioned in | SEV-021, TRP-033 |
| SEV-143 | Lifecycle root separation | One f32/i32 profile and one expression body under Simulation versus AlgorithmBlock lifecycle contracts yield DISTINCT `RootHandle`s and `RootDigest`s; substituting one for the other rejects, while their issued expression and function correlations still match | TRP-010, TRP-038 |
| SEV-144 | Value-capability closure | Rank-0 and rank-N Boolean, sized integers, reals, nested record arrays, empty fields and values, enum brands, complex, and opaque VALUE handles each accept or reject per the declared profile, checked transitively over every root owner before plan selection; a capability change moves `PreparedDigest` while a numeric-width semantic change still moves `RootDigest`. Operations and effects are OUT of scope here — SEV-148 gates those. SEV-102 covers backend value TRANSPORT and SEV-104 covers WIRE forgery; neither covers this admission closure | TRP-039 |
| SEV-145 | Execution-environment separation | One Solve root prepared under `{environment = no_std, allocation = forbidden, failure = returned_status}` and under a hosted default yields distinct `PreparedDigest`s; EACH artifact obeys ITS OWN requested policy, so the constrained product contains no allocation or unwinding path while the hosted one may; both declare identical value and status behavior; an absent required math, status, or scratch capability rejects typed before rendering. Environment, allocation, and failure transport are INDEPENDENT dimensions — `no_std` alone neither forbids `alloc` nor a panic handler | TRP-039 |
| SEV-146 | Nominal type equality | Two equal-cardinality enums remain UNEQUAL types; two field-isomorphic nominal records remain UNEQUAL despite identical physical domain and encoding; one nominal record prepared AoS versus SoA retains ONE semantic type | SEV-010, SEV-013, SEV-014 |
| SEV-147 | Prepared layout round-trip | One nominal nested record array prepared as AoS and as SoA or padded variants retains the same semantic type and values through FMI/eFMI; changing field offset, order, alignment, address space, enum mapping, or empty-field mapping moves `PreparedDigest` and either reissues a valid receipt or rejects | SEV-013, TRP-044 |
| SEV-148 | Operation and effect capability | A target admitting `f32` tensors but not `MatrixMultiply` REJECTS; one admitting pure calls but no assert/status effect REJECTS; a nested `Fold` whose body reaches a call or effect is checked through the full transitive closure before candidate selection | SEV-007, TRP-042 |
| SEV-149 | Typed result, not coercion | `Index`/view (`Tensor<T>` to `T`), record `Field` projection, `Compare` (`T`,`T` to `Boolean`), and `Reduce` each declare an exact typed result and are NOT conversions; an `f64` to `f32` edge MUST be an explicit `Convert` and rejects otherwise | SEV-010 |

**First vertical witness (SEV-109, covers TRP-014/TRP-018).** One real target
(prefer `rust-fixed` or a minimal embedded C target); one declared format; one
`MatrixMultiply` candidate list; a matching typed Solve root; exactly one issued
plan; rendered, compiled, executed, and compared against the typed evaluator;
with proof of no scalar-program construction on the compact path, and that
target's template-side choice deleted in the same change.

**Preregistration examples.** RDD2 `WaypointMission` registers ≥18% median
hot-path improvement over 20 runs against a control at identical `RootDigest`,
with ≤10% code-size and ≤15% peak-RSS growth (TRP-040). Default eager interning
registers ≤5% compiler wall and ≤10% peak-RSS regression on a no-duplicate
corpus plus ≥25% term and operand-byte reduction on a named duplicate-heavy
corpus; execution CSE registers its own hot, code, and RSS budget plus a
translation-validation witness mapping each eliminated execution to a
dominating exact-context owner (SEV-046, SEV-049).

### 3. Rejected Alternatives

| Alternative | Cost that defeated it | Reversal gate |
|---|---|---|
| SX/MX-style dual graphs | Duplicates evaluator, AD, dependency, codegen, and provenance proofs; conversion breaks identity | SEV-110 |
| A canonical tensor-to-scalar stage | Same contract on both sides; fails the SEV-007 stage test | SEV-110 |
| One god operation enum instead of the factored union | Every consumer pays for every factor; pure-term and AD proofs stop being local | SEV-114 |
| Graph-level format genericity (`graph<F>`) and pseudotype `SolvePackage<P>` | Mixing is the point; genericity pushes it to the least checkable boundaries | SEV-111 |
| Rounding policy on value types | Doubles the lattice and forces fake conversions between equal widths | SEV-111 |
| A dependent subrange integer type (range carried in the value type) | Range refinement becomes a type change, so two equal-representation `i32` values with different proven ranges are different types and need conversions between identical machine encodings; type equality, term keys, and ABI mappings all fragment along proof strength | SEV-111; an advocate must exhibit a witness showing it creates NO conversion between equal-representation values |
| Precision-neutral `Real` with codegen-time width (SPEC_0035) | Intermediate rounding, comparisons, guards, and assertions differ from the built program | SEV-111 |
| Importing the C type system wholesale | A god IR: pointers, unions, bitfields, and vendor structs have no portable value relation | SEV-111 |
| Dtype strings and template-side semantic choice | Fail-open and unverifiable; recreates an untyped backend | TRP-012 |
| A universal f64 backend ABI slice | Boolean and large signed integers are unrepresentable; NaN or nonzero becomes true | SEV-111 |
| Per-operation serialized interval certificates | Wire and checksum bloat; construction derives and replay rederives | SEV-092 |
| One universal concrete `TargetProgram` | Premature universal over unlike products; hides per-product invariants | TRP-017 |
| One universal lifecycle-root container holding Simulation and AlgorithmBlock products together | Lifecycle contract is part of root identity: one container makes two admissibility regimes share a digest, so a substitution defect becomes unrepresentable in evidence rather than impossible in fact. Distinct from the `TargetProgram` row, which is about product PLANS, not roots | TRP-038; reopening needs two lifecycle contracts proven observationally identical over the whole admissibility surface, not merely over expression bodies |
| Forced deletion of every prepared artifact | Over-broad; the rule is noncanonical-and-never-fed-back | TRP-002 |
| `volatile` as an ABI-only spelling | An MMIO read changes value and its access count is observable | SEV-016 |
| Saturation or Q-rescaling as a target strategy | Changes results for ordinary Modelica arithmetic | SEV-027 |
| Universal 18% RDD2 threshold for all prepared artifacts | One mission benchmark cannot govern mandatory plans, caches, and backend IR alike | TRP-040 |

**eFMI alternatives (all three recorded).**

| Option | Cost | Status |
|---|---|---|
| **Regions-first with a complete projected body** — checked Solve regions mechanically project a COMPLETE GALEC-language AlgorithmCode body, yielding a syntactically valid `.alg`. Centralizes executable semantics in one place and may reduce construction cost | Makes the auditor-facing, profile-neutral AC projection DOWNSTREAM of a profile-bound root; risks losing direct source, lifecycle, and effect identity; and weakens the independent `eval-galec` leg unless exact reverse correlations and projection validation are supplied | Rejected only if TRP-030's gates beat it; TRP-041 reopens |
| Regions-first with a body-free AlgorithmCode reference (separate, weaker variant) | A body-free `.alg` is not valid Algorithm Code | Rejected outright |
| AC-first (current): independently lowered AlgorithmCode bodies | A second semantic compiler for the same expressions | Rejected; TRP-041 reopens |
| Shared-construction correlated siblings | Requires exact bidirectional correlation and a preregistered projection-overhead budget | **Chosen** (TRP-030) |

The correlated-sibling hybrid wins over the complete-projection alternative
ONLY if the specified correlation obligations (§4.19), the four-leg
discriminator (SEV-135), and the preregistered code-size and construction-time
budgets are met. If they are not, the complete-projection design is the better
answer and TRP-041 is the route back.

**Boundary counterposition (retained).** "Solve can represent anything a C
target might use" MUST NOT mean an unbounded union of every platform ABI detail,
and `target.toml` MUST NOT retroactively reinterpret an already-built graph. The
durable boundary is an extensible semantic type algebra plus profile-indexed,
checked ABI and refinement mappings.

### 4. Bound Field Catalogs

Each row is bound by the parent rule naming it.

| Ref | Bound by | Fields |
|---|---|---|
| §4.1 | SEV-012 | Boolean; sized signed and unsigned integers; Binary32; Binary64; branded enums; compact tensors of checked shape; finite acyclic by-value records. Binary16, BFloat16, and fixed point are reserved descriptor shapes needing evaluator, conversion, and status contracts before admission |
| §4.2 | SEV-013 | AoS/SoA choice, padding, field offsets, alignment, address space, `repr(C)`, CMSIS descriptors, interleaved or planar complex storage |
| §4.3 | SEV-024 (contract fields), TRP-015 (exact target-relation preservation) | Accumulator format, evaluation order, per-step and result rounding, contraction/FMA, signed zero, NaN payload and quieting, infinity, subnormal/FTZ, status, transcendental contract |
| §4.4 | SEV-044 | Callee, ordered arguments, activation, clock, domain, captures, effects, read versions, profile |
| §4.5 | SEV-045 | Root handle and profile, result type, opcode plus arithmetic and status policy, ordered operand terms, compact shape/domain/view metadata, issued SSA and read-version atoms |
| §4.6 | SEV-047 | Exact span and origin, instance and scope path, statement and operand role, ordered child occurrences, execution-owner correlation |
| §4.7 | SEV-049 | Dominance, identical lazy activation, coordinate or loop invariance, read/history/external generations, arithmetic and AD seed or mode, total/fault/status/effect behavior, profitability |
| §4.8 | TRP-012 | `ExecutionMode::{NativeRequired, HybridMigration}`, ordered candidates with compiler-decidable predicates, budgets, receipt selectors; deny-unknown. `NumericProfile` is the closed request schema of §4.21; the final-emission policy is §4.22 |
| §4.21 | TRP-012, TRP-033 | `RealRepr::{Binary32, Binary64}`; `IntRepr::{I8, I16, I32, I64, U8, U16, U32, U64}`; ONE default mapping for source Modelica `Real` and one for `Integer`; the allowed representation set for mixed-width Solve values; and the arithmetic-contract or profile ID closing rounding, overflow and status, subnormal, and reduction behavior. The allowed set is KIND-TAGGED — a `RealRepr` set and an `IntRepr` set, or one union whose members carry a kind tag; a raw heterogeneous list is inadmissible. Each kind's set is NONEMPTY and normalizes by canonical sort and dedup, and BOTH defaults MUST be members of their kind's set: a profile whose default is absent REJECTS, and normalization never unions into or mutates the request. A compiler-known named profile is admissible only when it expands to exactly these normalized fields. Reserved Binary16, BFloat16, and fixed forms REJECT until their §4.1 contracts exist; no extension string adds semantics |
| §4.9 | TRP-013 | Library version or binary hash, build flags, accumulator and order, alias and overlap, alignment, workspace, preconditions, status |
| §4.10 | TRP-017 | ABI, coverage, loop/kernel, arithmetic relation, provenance, and the typed resource request of §4.25 — the bare word "resources" is not a request |
| §4.24 | TRP-039 | `ValueCapabilityProfile`, deny-unknown and closed: per-family admission for rank-0 and rank-N Boolean, sized signed and unsigned integers, each admitted real format, nested record arrays, empty fields and values, enum brands, complex, and opaque VALUE handles. Effect owners are NOT here — they are §4.26. Checked TRANSITIVELY against every root owner before plan selection |
| §4.26 | TRP-042 | `OperationEffectCapabilityProfile`, deny-unknown and closed, keyed EXHAUSTIVELY to the `ValueOp`, `InvokeOp`, `EffectOp`, and `Terminator` families, including declared structured, control, and lifecycle subsets, and the volatile/atomic EFFECT owners. Adding a grammar variant MUST make every capability matcher fail to compile or explicitly reject: no wildcard or default-support arm exists |
| §4.25 | TRP-039 | `ExecutionEnvironmentProfile`, deny-unknown and closed: hosted versus freestanding/`no_std`; allocation and scratch limits; recursion and stack limits; panic versus returned status; available runtime math; concurrency and atomic model; admitted library contracts and ISA features. Declaring an ISA or library available NEVER authorizes a kernel — only its §4.9 receipt does |
| §4.11 | SEV-014 | Opaque handles admit no literals, ordering, generic wire, arithmetic, address inspection, AD, or tensorization |
| §4.13 | SEV-041 | Hash-domain separation tag; typed SEMANTIC ROOT KIND identity; typed LIFECYCLE CONTRACT identity; canonical semantic payload excluding the claimed digest (semantic event schedules included); normalized arithmetic and sensitivity profiles; semantic schema and lowering version. Root kind and lifecycle contract are named typed fields, not implied by the domain tag |
| §4.14 | TRP-001 | Mandatory legality/refinement (no speed gate); optional optimizations (hot, code, RSS); persistent caches (compile, start, storage); backend-local SSA/CFG (local identity, zero authority) |
| §4.15 | SEV-015 | Type, field path, occurrence, ABI ordinal, wire identity; `[0,3]`, `[0,4]`, two empty arguments, and no argument are four distinct values; empty reductions and `[m,0]×[0,n]` carry exact semantics |
| §4.16 | SEV-016 | An issued method-entry snapshot, or an explicit effect-token volatile/atomic load-store owner; `restrict` additionally requires a no-alias proof |
| §4.17 | TRP-002 | Machine code, physical layouts, loop schedules, dispatch tables, references into the canonical owner |
| §4.18 | TRP-014 | `DirectCompact`, `Loop`, `BoundedUnroll`, `Kernel`, `Composite`, `CheckedDispatch`. The recorded plan carries its budget and coverage receipt |
| §4.22 | TRP-012, TRP-035 | Final-emission policy: `Loop`, `BoundedUnroll { max_elements, max_instructions }`, or `Kernel { contract, fallback }`, each optionally scoped by compiler-decidable predicates over op, rank, static shape, alignment, and alias. `AlwaysUnroll` is INADMISSIBLE; finite unrolling is represented only by `BoundedUnroll` |
| §4.23 | TRP-037 | Unsigned-storage receipt: proven nonnegative domain of the semantic signed value, the exact storage representation, a total round-trip proof that load-after-store returns the same semantic value, and a no-reinterpretation obligation — no arithmetic, comparison, or ABI edge observes the unsigned encoding |
| §4.19 | TRP-030 | One shared checked expression and function construction over untouched DAE plus GALEC admissibility and lifecycle facts; co-issued auditor-visible `AlgorithmCodePackage` carrying the final GALEC-language body projection; co-issued profile-bound `SolveAlgorithmBlock` consumed by Production and embedded C; exact source, statement, effect, and value correlations both ways; package checksum graph binding AC identity, `RootDigest`, and PC artifacts |

#### §4.20 Current Type Signatures And Proposed Deltas

Snapshot as of the working tree (`crates/rumoca-ir-solve/src/typed_program/types.rs`);
that crate is under concurrent edit, so re-read before relying on it. Private
fields are shown as declared.

```rust
pub enum SolveRealFormat { Binary32, Binary64 }

pub struct SolveArithmeticProfile {
    real_format: SolveRealFormat,
    rounding: SolveRoundingMode,     // enum SolveRoundingMode { NearestTiesToEven }
    integer_domain: SolveIntegerDomain, // struct { minimum: i64, maximum: i64 }
}

pub enum SolveScalarType {
    Real { format: SolveRealFormat, rounding: SolveRoundingMode },
    Integer(SolveIntegerDomain),
    Boolean,
}

pub struct SolveValueType {
    scalar: SolveScalarType,
    dimensions: Box<[u32]>,
    scalar_count: u32,
}
```

| Type | Proposed delta | Bound by |
|---|---|---|
| `SolveRealFormat` | Gains the reserved descriptor shapes (Binary16, BFloat16, fixed point) only once evaluator, conversion, and status contracts exist; Complex becomes an element type over an admitted format, not a record | SEV-012, SEV-017 |
| `SolveArithmeticProfile` | Gains the full §4.3 contract set — accumulator, order, per-step and result rounding, contraction, signed zero, NaN payload and quieting, infinity, subnormal/FTZ, status, transcendentals — and becomes root-identity-bearing | SEV-020, SEV-024 |
| `SolveScalarType` | `Real` drops `rounding`, since rounding is operation and profile policy, not a value-type discriminator. `Integer(SolveIntegerDomain)` becomes `Integer { repr: IntRepr }` over the §4.21 set: today's `SolveIntegerDomain { minimum, maximum }` conflates representation with range and is the transitional state, since range belongs to separate root-bound facts. Branded enums join the union | SEV-010, SEV-011, SEV-014, SEV-018 |
| `SolveValueType` | Gains finite acyclic by-value records with nominal field identity, and empty extents that keep type, field path, occurrence, ABI ordinal, and wire identity | SEV-013, SEV-015 |
