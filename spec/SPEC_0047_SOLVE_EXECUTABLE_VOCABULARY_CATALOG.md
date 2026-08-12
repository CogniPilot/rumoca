# SPEC_0047: Solve Vocabulary and Target Refinement Catalog

## Status
REFERENCE

## Summary

Shared evidence and field-catalog annex for **three** parents:
[SPEC_0045](SPEC_0045_SOLVE_EXECUTABLE_VOCABULARY_AND_PROFILES.md)
(`SEV-NNN` rules),
[SPEC_0046](SPEC_0046_SCHEDULED_DISCRETE_OWNERSHIP.md) (`SDO-NNN` rules), and
[SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) (`TRP-NNN`
rules).

## How To Use This Catalog

This annex holds no governing rules — §§5, 6, and 8 read as obligations only
because TRP-049, TRP-050, and SPEC_0048 §1 impose them, and §7's rows are bound
by SPEC_0046 §9. Every rule lives in SPEC_0045, SPEC_0046, or SPEC_0048;
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
| Partial | `dae_has_external_functions` returns a hard-coded `false`, so the `external_functions == Some(false)` capability check at `codegen_target.rs:722` never fires: a target declaring no external-function support is admitted against a DAE that has them. The capability fails OPEN | `rumoca-compile/src/codegen_target/feature_analysis.rs` | SEV-155 |
| Absent | No WASM, MLIR, or C consumer of `SolveOperation` exists | backend crates | SEV-007 |
| Absent | `ValueCapabilityProfile`, `OperationEffectCapabilityProfile`, and `ExecutionEnvironmentProfile` do not exist; a target's numeric declaration is the only admission check, so nonnumeric families, grammar operations, and effects are never closed over | `rumoca-compile` | TRP-039, TRP-042 |
| Partial | `rust-fixed` is HOSTED and explicitly not `no_std` per its own README; it is allocator-free only inside the derivative call, and its template, runtime-math, panic, and library behavior is not transitively checked | rust-fixed target | TRP-039 |
| Absent | Neither SPEC_0049 promotion gate exists: no key-set exhaustiveness test compares the catalog to `SolveOperation` and its embedded operator enums, and no constructor PARITY cases check each row's admitted and rejected element kinds, operand arity, and distinguishing operands. A name-set check alone would pass a wrong arity or a missing operand | `rumoca-ir-solve` tests | SPEC_0049 promotion gates |
| Absent | SEV-001's `EffectOp` and `Terminator` factors and SEV-016's volatile/atomic owners have no discriminants and are uncataloged; the grammar is NOT closed today | `rumoca-ir-solve` | SEV-001, SEV-016 |
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
| SEV-124 | Sensitivity profile IDs | Ideal, quantized, and straight-through programs carry distinct IDs and cannot be substituted | SEV-030, SEV-031, SEV-032 |
| SEV-125 | Dot-product discriminator | `[1e20, -1e20, 1] · [1,1,1]` plus NaN, signed-zero, subnormal, and status cases give one declared result and status across loop, unroll, fused, and catalog-kernel candidates; numerically different candidates require distinct roots | SEV-024, TRP-015 |
| SEV-126 | Kernel-receipt mutation | Altering any receipt field invalidates the prepared plan | TRP-013 |
| SEV-127 | Composite-plan coverage | Coverage is checked over `(owner, logical domain point)`: a tiled kernel plus compact remainder covering ONE owner in disjoint subdomains passes, a gap or a duplicated `(owner, point)` rejects, and a `CheckedDispatch` predicate overlap passes only with proved equivalence and a deterministic selected branch | TRP-014 |
| SEV-128 | Two-line `x+y` | Both spans survive through wire; a forced hash collision still compares exact keys | SEV-045…SEV-047 |
| SEV-129 | `a:=x+y; x:=x+1; b:=x+y` | The two terms do not merge; read versions distinguish them | SEV-045 |
| SEV-130 | Identical calls and asserts | They remain two ordered owners; an inactive faulting branch never merges with an active one | SEV-048 |
| SEV-131 | Interning invariance | Off, local, and eager interning produce byte-identical canonical wire | SEV-046, SEV-091 |
| SEV-132 | Root distinctness | One DAE built under Binary32 and Binary64 yields two distinct roots (`16_777_217`) | SEV-003 |
| SEV-133 | Layer movement | A template-only change preserves `RootDigest` and `PreparedDigest` while moving `ArtifactDigest`; each record binds only the digest of the layer it annotates | SEV-041, SEV-090, TRP-011 |
| SEV-134 | eFMI correlation mutation | Mutating any AC↔Solve source, body, lifecycle, or effect correlation makes the package reject | TRP-030 |
| SEV-135a | Four-way comparison under a declared relation | `eval-galec` PARAMETERIZED by the §4.32 mapping, the definitional Solve evaluator, compiled C, and OMC agree UP TO that declared relation and tolerance — never by assumed equality; PC→AC traceability holds within preregistered code-size, metadata, and construction-time budgets | TRP-030, TRP-031 |
| SEV-135b | Numeric boundary cases | The relation is exercised at the `16_777_217` f32 boundary, on reduction order and contraction, on NaN payload and quieting, on subnormal and FTZ handling, and on every status case; a case outside the declared tolerance REJECTS rather than being reported as agreement | TRP-031 |
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
| SEV-144 | Value-capability closure | Rank-0 and rank-N Boolean, sized integers, reals, nested record arrays, empty fields and values, enum brands, and opaque VALUE handles each accept or reject per the declared profile, checked transitively over every root owner before plan selection; a capability change moves `PreparedDigest` while a numeric-width semantic change still moves `RootDigest`. Operations and effects are OUT of scope here — SEV-148 gates those. SEV-102 covers backend value TRANSPORT and SEV-104 covers WIRE forgery; neither covers this admission closure | TRP-039 |
| SEV-145a | Crossed environment requests | One Solve root prepared under `{freestanding, forbidden, returned_status}`, `{freestanding, admitted, panic{halt, handler::named}}`, `{hosted, forbidden, returned_status}`, and `{hosted, admitted, panic{unwind, handler::none}}` yields four distinct `PreparedDigest`s, each artifact obeying ITS OWN requested policy. The crossed pairs defeat a backend that branches only on `freestanding` | TRP-039, TRP-045 |
| SEV-145b | Positive capability exercise | Each admitted capability is exercised by a plan that genuinely NEEDS it: the `{freestanding, admitted, …}` case prepares a plan that really allocates scratch, and each `returned_status` case drives a failure path that really transports status. A tuple whose plan never requires the capability does not discharge this row | TRP-039, TRP-043 |
| SEV-145c | Crossed negatives | The same needing plans REJECT when their capability is forbidden: allocation-needing under `allocation::forbidden`, status-transport-needing under a profile lacking it, and a scratch-needing plan over its limit | TRP-039, TRP-043 |
| SEV-145d | Disposition fidelity | Each product implements exactly its REQUESTED disposition across `returned_status`, `abort`, `unwind`, `halt`, and `reset`: an `abort` request never unwinds, an `unwind` request never aborts, and `halt` and `reset` are distinguished from each other and from `abort` | TRP-039 |
| SEV-145e | Handler contract | `handler::named` carries a full §4.27 contract whose NORMALIZED CONTENT the receipt records; `handler::none` emits none, and `returned_status` has no handler field to set. `freestanding` with `halt` or `reset` and `handler::none` REJECTS; `abort` passes whether reached THROUGH a named handler or directly | TRP-045 |
| SEV-145h | Handler contract mutation | Mutating any §4.27 field — entry ABI, disposition, observable effects, stack or allocation need, or binary hash — reissues a valid receipt or REJECTS; two handlers sharing a label but differing in normalized content yield different `PreparedDigest`s, and a label alone never selects | TRP-045 |
| SEV-145f | One-axis digest mutation | Changing exactly ONE dimension — environment, allocation, disposition, or `handler::{none, named}` — while holding the others fixed moves `PreparedDigest`, wherever the §4.25 compatibility rules admit that single change; the four axes are independent, so `freestanding` alone neither forbids `alloc` nor fixes a disposition, and a named handler alone fixes no disposition | TRP-045 |
| SEV-145g | Environment-invariant semantics | Across all requests the declared value and status RELATION is identical and only its transport differs as requested; an absent required math, status, or scratch capability rejects typed before rendering | TRP-039, TRP-043 |
| SEV-146 | Nominal type equality | Two equal-cardinality enums remain UNEQUAL types; two field-isomorphic nominal records remain UNEQUAL despite identical physical domain and encoding; one nominal record prepared AoS versus SoA retains ONE semantic type | SEV-010, SEV-013, SEV-014 |
| SEV-147 | Prepared layout round-trip, per family | For each §4.2 layout family a product ADMITS, its SELECTED mapping gets a receipt, digest-mutation, and round-trip witness: record AoS versus SoA versus padding; `repr(C)` field offset and order; CMSIS descriptor; enum mapping; empty-field mapping; and unsigned storage (SEV-141). The selected mapping retains the same semantic type and values through FMI/eFMI, each mutation moves `PreparedDigest`, and each selection carries a valid receipt or rejects. A product that REJECTS a family — GALEC/eFMI may reject empty extents, enums, Complex, or opaque handles — owes a typed rejection, not a round-trip | SEV-013, TRP-044 |
| TRP-100 | Prepared category registry (governance) | A SOURCE SCAN proves every prepared artifact is registered in exactly one §4.14 category, and that NO speed gate guards a mandatory legality or refinement plan. A mandatory plan behind a performance threshold FAILS | TRP-001 |
| TRP-101 | Prepared-wire allowlist (governance) | A WIRE SCHEMA SCAN proves a prepared record admits only §4.17 contents; an identity-bearing op-DAG clone, a second wire form, and a stored scalar analysis graph each FAIL to construct | TRP-002 |
| TRP-102 | Backend-local IDs are never authority (governance) | A SOURCE SCAN proves backend-local DAG, SSA, and CFG identities are unserializable, are absent from every semantic-analysis and wire path, and correlate to canonical owners only. A backend ID reaching semantic analysis FAILS | TRP-003 |
| TRP-103 | No premature universal (governance) | A SOURCE SCAN proves no universal `TargetProgram` type and no promoted common base exists while fewer than three genuinely different products share an IDENTICAL mandatory invariant and checker flow; a base with a target-name branch or catch-all FAILS | TRP-017 |
| TRP-104 | Coverage mode behavior | `NativeRequired` REJECTS an incomplete coverage plan with no fallback taken; `HybridMigration` admits one only with the fallback EXPLICIT and RECORDED in the receipt. A silent fallback under either mode FAILS | TRP-016 |
| TRP-105 | Product schema exhaustive matrix | ONE matrix over every §10 `ProductKind` × `RootKind` legal pair asserts: each R field present and each F field absent (a missing R or present F rejects); the kernel semantics end at the profile-bound root while lifecycle metadata enters only prepared/artifact/package identity; each product carries its budget and ONE §4.18 execution path where `RootKind` is not `None`, and neither where it is; and every §4.1 family, §4.24 key, SPEC_0049 key, §4.25 environment, status class, and §4.2 layout is an explicit admit or an explicit typed rejection through the one checker flow | TRP-019, TRP-022, TRP-049, TRP-050 |
| TRP-106 | One construction, projection only | A fixture proves ONE `rumoca-phase-solve` expression and function construction feeds both siblings, and a SOURCE SCAN proves `rumoca-phase-galec` contains no expression or function lowering — only projection and container authority. A second lowering path there FAILS | TRP-032 |
| SEV-157 | Complex is not admitted | A Complex rank-0 value, a Complex tensor, a Complex type declaration, a Complex literal, a Complex wire record, a Complex operation leaf, and a Complex capability declaration each REJECT at construction or decode — BEFORE any backend is consulted. A rejection arriving from a backend instead FAILS this row | SEV-017 |
| SEV-158 | Term-storage reuse is not selected | While no reuse implementation exists the gate asserts `NotImplemented`/`NotSelected`: no interning table is built and no term is shared. Once reuse exists, EVERY §4.7 mutation disables it, and each eliminated execution carries translation evidence mapping it to a dominating exact-context owner plus its preregistered budgets | SEV-049 |
| SEV-159 | Superseded scalar vocabulary deleted (governance) | A SOURCE and WIRE SCAN proves zero `ScalarOp`/`LinearOp` definitions, discriminants, wire tags, constructors, readers, or writers survive, and that no borrowed scalar view is `Serialize`. A residual definition FAILS even with no behavioral difference | SEV-006 |
| SEV-160 | Handles are unserializable (governance) | A SOURCE SCAN proves `RootHandle` and every root-local typed ID are NOT `Serialize`/`Deserialize` and that no wire struct carries a raw-ID field. A derive or a manual impl each FAIL | SEV-040 |
| SEV-161 | No derived ranges on the wire (governance) | A WIRE SCHEMA SCAN proves no serialized record carries a derived range or interval fact, and a replay fixture proves those facts are REDERIVED from semantic inputs. A stored range FAILS even when it recomputes to the same value | SEV-092 |
| SEV-162 | No graph-kind vocabulary (governance) | A SOURCE and WIRE SCAN proves there is no graph-kind flag, discriminant, or cross-flavor conversion, and no per-flavor cache, AD path, or call ABI; a positive check confirms one rank-0 and one rank-N use share ONE operation leaf | SEV-005 |
| SEV-163 | Exhaustive grammar dispatch | The SPEC_0049 §1/§2 catalog key set equals the Rust discriminant set in both directions; every total dispatcher and capability matcher covers all four factors with no wildcard arm; adding a variant fails the catalog test, every matcher, and every dispatcher | SEV-001 |
| SEV-164 | One lowering, many profile roots | ONE lowering implementation builds Binary32 and Binary64 roots from the same immutable inputs, and the two roots are DISTINCT with distinct digests; a shared executable body, or a second profile-specific lowering path, each FAIL | SEV-004 |
| SEV-165 | One contract per applicable leaf | Every occurrence of an `ExactIntegral`, `FloatingPrimitive`, `FloatingTranscendental`, `Reduction`, `RelationalEquality`, `RelationalOrdering`, or `Conversion` leaf resolves EXACTLY ONE class-correct contract; every `NotApplicable` leaf resolves NONE, and a backend inventing one FAILS | SEV-011 |
| SEV-166 | AD conversion policy is named | A narrowing, fixed-point, or saturating conversion inside an AD-required region REJECTS unless the SELECTED sensitivity profile names it; a widening conversion is admitted as a refinement. An unnamed policy admitting the narrowing FAILS | SEV-033 |
| SEV-167 | Definition and dominance | `ValueDefinitionId` identifies one SSA definition; dominance is computed from region and block position and is never stored in an identity. Two definitions with equal operands stay distinct, and a prepared schedule is a relation over handles rather than definition data | SEV-043 |
| SEV-168 | Every identity family, swapped and duplicated and omitted | For EACH of `ValueDefinitionId`, `PureTermId`, `OccurrenceId`, `OccurrenceFamilyId`, `FunctionRelationId`, `InvocationOwnerId`, and the typed value/effect projections: swapping, duplicating, and omitting it each REJECT for the IDENTITY reason. Covering only two call sites does NOT discharge this row | SEV-044 |
| SEV-152 | Persisted plan replay | A plan persisted as `RootDigest` plus canonical `OwnerPath` replays through checked root construction to fresh handles with owner kind and type rechecked; a forged path, a wrong-root digest, and a path whose owner kind changed each REJECT; interning order and construction order do not change the stored path | TRP-046 |
| SEV-153 | Candidate-specific fitting | Two candidates for ONE root with different scratch or stack needs: only the fitting one is selectable, the other rejects during selection rather than at admission, and the sealed receipt records which was chosen | TRP-048 |
| SEV-154 | Manifest product tagging | A Solve-executable product missing any required profile REJECTS; a Flat/DAE/AC-only export carrying root or preparation fields REJECTS; an omitted capability key behaves as DENY, never as permissive-unknown | TRP-047 |
| SEV-148 | Operation and effect capability | A target admitting `f32` tensors but not `MatrixMultiply` REJECTS; one admitting pure calls but no assert/status effect REJECTS; a nested `Fold` whose body reaches a call or effect is checked through the full transitive closure before candidate selection | SEV-007, TRP-042 |
| SEV-150a | Cross-layer substitution | Five negatives reject: a prepared plan under the WRONG root, the WRONG target profile, a WRONG or missing receipt, an artifact under the WRONG template or toolchain, and any digest presented at the wrong layer of the ladder | SEV-042, SEV-090 |
| SEV-150b | Decode recompute | Decoding recomputes `RootDigest`, `PreparedDigest`, and `ArtifactDigest` from their §4.13/§4.28/§4.29 fields; a claimed digest disagreeing with the recomputation rejects, and an artifact whose ancestry link is absent rejects | SEV-042, SEV-090 |
| SEV-150c | Provenance is sidecar | Editing only a source span or occurrence annotation moves NO digest in the ladder; conversely no digest change is caused by provenance alone | SEV-042 |
| SEV-150d | Provenance sidecar integrity, two-sided | Canonical record PERMUTATION must NOT move `ProvenanceDigest` — reordering the sidecar's records is not a semantic change — while a semantic CHILD-ORDER mutation MUST move it, since ordered child occurrences are payload. Mutating any other §4.6 field, including execution-owner correlation, also moves it; a claimed digest disagreeing with the recomputation rejects, as does one bound to a different `RootDigest` or a second sidecar bound to the same root | SEV-042, SEV-090 |
| SEV-156 | External functions REJECT (Current) | Until the Effect/Invoke slice lands there is no invoke, effect, status, or ABI grammar for an external function, so EVERY external function REJECTS at the capability boundary — there is no admitting path. The transitive cases are preregistered: an external call nested in a `Fold` body, behind a `Conditional` arm, and reached through a `Call` callee each reject through the same closure | TRP-042 |
| SEV-155 | External-function capability fails CLOSED | A DAE carrying an admitted external interface makes a target declaring `external_functions = false` REJECT. The explicit NEGATIVE against the constant-false probe: a DAE with an external interface must make the check FIRE — a test that passes only because `dae_has_external_functions` returns a hard-coded `false` (`rumoca-compile/src/codegen_target/feature_analysis.rs`) does not discharge this row, since the check at `codegen_target.rs:722` cannot fire today and the capability fails OPEN | TRP-042, TRP-048 |
| SEV-151 | Receipt issuance and replay | A receipt replayed against the wrong owner, wrong root, wrong descriptor, wrong effect footprint, or a different binary version REJECTS; a receipt lacking issuing authority or evidence is not selectable. A candidate that SATURATES, WRAPS, or Q-RESCALES ordinary Modelica arithmetic is never admitted as a strategy: it builds a distinct arithmetic root declaring those semantics, or rejects | SEV-027, TRP-013, TRP-045 |
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

**Benchmark topology binding.** A performance discriminator over an EXTERNAL
model is vacuous unless its topology is pinned. The RDD2 estimator discriminator
therefore selects ONE BINDING CERTIFICATE: either the external model's revision
or content digest, or the compiled structural precondition it depends on —
exactly one correlated `step` occurrence; predict evaluated before correction;
the exact mocap, joint-GPS, GPS-position, GPS-velocity, optical, and hold
priority chain; and the later `navigationEstimateArrays` current read. A
topology change INVALIDATES the gate rather than silently passing a simpler
model. Counters bind the issued `OccurrenceId` and `InvocationOwnerId` plus THE
SELECTED BINDING CERTIFICATE — never owner ordinals, display names, or function
names, which are unstable across construction (SEV-044, SEV-047).

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
| Raw dtype strings in the manifest | Fail-open and unverifiable; an unknown string names no checked semantics, so a target can appear to support a representation it never implements | TRP-012; IRREVERSIBLE for admitted representations — the only route is a NEW typed schema with parser, evaluator, and refinement proofs, which is not a reopening of strings |
| Template-side semantic choice | The template is outside every checked boundary: a decision made there has no receipt, no coverage proof, and no digest, so it cannot be validated or replayed. No equivalence argument can rescue it — an unissued decision has no authority against which equivalence could be established | TRP-018; the route back is EXTENDING OR REPLACING THE CHECKED PLAN VOCABULARY — a new plan variant carrying receipt, coverage, and digest, plus proof that passive rendering still suffices. Semantic choice in a template is never authorized |
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
discriminator (SEV-135a, SEV-135b), and the preregistered code-size and construction-time
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
| §4.1 | SEV-012 | Boolean; sized signed and unsigned integers; Binary32; Binary64; branded enums (SEV-014); compact tensors of checked shape; finite acyclic by-value records, whose recursion passes through an explicit reference or opaque capability; and opaque VALUE handles (SEV-014, §4.24). Complex is NOT admitted (SEV-017); Binary16, BFloat16, and fixed point are reserved descriptor shapes needing evaluator, conversion, and status contracts before admission |
| §4.2 | SEV-013 | AoS/SoA choice, padding, field offsets, alignment, address space, `repr(C)`, CMSIS descriptors |
| §4.3 | SEV-024 (contract fields), TRP-015 (exact target-relation preservation) | Accumulator format, evaluation order, per-step and result rounding, contraction/FMA, signed zero, NaN payload and quieting, infinity, subnormal/FTZ, status, transcendental contract |
| §4.4 | SEV-044 | Callee, ordered arguments, activation, clock, domain, captures, effects, read versions, profile |
| §4.5 | SEV-045 | Root handle and profile, result type, opcode plus arithmetic and status policy, ordered operand terms, compact shape/domain/view metadata, issued SSA and read-version atoms |
| §4.6 | SEV-047 | Exact span and origin, instance and scope path, statement and operand role, ordered child occurrences, execution-owner correlation |
| §4.7 | SEV-049 | Dominance, identical lazy activation, coordinate or loop invariance, read/history/external generations, arithmetic and AD seed or mode, total/fault/status/effect behavior, profitability |
| §4.8 | TRP-012, TRP-047 | Product tag; `CoverageMode::{NativeRequired, HybridMigration}` (distinct from the existing `execution_mode = compiled \| jit \| source-transform \| symbolic \| packaged`); ordered candidates with compiler-decidable predicates; budgets; receipt selectors; deny-unknown, with an omitted capability key meaning DENY. `NumericProfile` is §4.21, value/op/effect profiles are §4.24/§4.26, environment is §4.25, emission policy is §4.22 |
| §4.21 | TRP-012, TRP-033 | `RealRepr::{Binary32, Binary64}`; `IntRepr::{I8, I16, I32, I64, U8, U16, U32, U64}`; ONE default mapping for source Modelica `Real` and one for `Integer`; the allowed representation set for mixed-width Solve values; and the arithmetic-contract or profile ID closing rounding, overflow and status, subnormal, and reduction behavior. The allowed set is KIND-TAGGED — a `RealRepr` set and an `IntRepr` set, or one union whose members carry a kind tag; a raw heterogeneous list is inadmissible. Each kind's set is NONEMPTY and normalizes by canonical sort and dedup, and BOTH defaults MUST be members of their kind's set: a profile whose default is absent REJECTS, and normalization never unions into or mutates the request. A compiler-known named profile is admissible only when it expands to exactly these normalized fields. Reserved Binary16, BFloat16, and fixed forms REJECT until their §4.1 contracts exist; no extension string adds semantics |
| §4.9 | TRP-013 | Kernel receipt, content-addressed and compiler-known: owning `RootDigest` and canonical OwnerPath; owner and operation identity; entry symbol, signature, and calling convention; SEMANTIC operand types plus PHYSICAL layouts and strides; the predicate domain it is selected for; library version or binary hash and build flags; accumulator and order; alias and overlap; alignment and address space; workspace; preconditions; status behavior; effect footprint (`errno`, floating-point environment, globals, threading); issuing authority and its evidence; and replay protection. CMSIS descriptors additionally bind buffer lifetime, shape/stride/quantized format, and address-space alias rules |
| §4.28 | SEV-042 | `PreparedDigest = H(domain, RootDigest, normalized target/capability/environment profiles, ALL receipts, the selected coverage plan, preparer and schema and toolchain contract)`. Ancestry on `RootDigest` is mandatory |
| §4.29 | SEV-042 | `ArtifactDigest = H(preimage)` where the preimage is a DOMAIN-SEPARATED, LENGTH-DELIMITED ordered sequence: the domain tag; the parent `PreparedDigest`; framed emitter, template, asset, and toolchain identities; then, in canonical path order, each output file as exactly three framed members: its canonical RELATIVE PATH, its BYTE LENGTH, and its EXACT RAW BYTES. There is no content-digest alternative, so one artifact has one preimage and one digest. Ancestry on `PreparedDigest` is mandatory. The `ArtifactDigest` claim record lies OUTSIDE every preimage member, the manifest schema included (TRP-011), so no placeholder or exclusion machinery exists. Bytes IDENTIFY an output; they never prove refinement |
| §4.30 | SEV-042 | Occurrence and source provenance is a CORRELATED SIDECAR, not semantic Root payload, keyed by canonical wire-local occurrence and owner PATHS reissued under `RootDigest` — never a serialized root-local ID or ordinal (SEV-040). `ProvenanceDigest = H(provenance domain tag, owning RootDigest, provenance schema version, canonical sorted sequence of records, each carrying the COMPLETE §4.6 payload: owner path, occurrence path, span, origin, instance and scope path, statement and operand role, ORDERED CHILD occurrences, and execution-owner correlation)`, excluding the claimed digest. `RootDigest` ancestry is mandatory, decode RECOMPUTES the claim, and one record binds exactly one sidecar to one root. It verifies independently of the §4.28/§4.29 ladder, so provenance edits move no ladder digest yet stay tamper-evident |
| §4.10 | TRP-017 | ABI, coverage, loop/kernel, arithmetic relation, provenance, and the typed resource request of §4.25 — the bare word "resources" is not a request |
| §4.24 | TRP-039 | `ValueCapabilityProfile`, deny-unknown and closed: per-family admission for rank-0 and rank-N Boolean, sized signed and unsigned integers, each admitted real format, nested record arrays, empty fields and values, enum brands, and opaque VALUE handles. Effect owners are NOT here — they are §4.26. Checked TRANSITIVELY against every root owner before plan selection |
| §4.26 | TRP-042 | `OperationEffectCapabilityProfile`, deny-unknown and closed, keyed EXHAUSTIVELY to the `ValueOp`, `InvokeOp`, `EffectOp`, and `Terminator` families, including declared structured, control, and lifecycle subsets, and the volatile/atomic EFFECT owners. Adding a grammar variant MUST make every capability matcher fail to compile or explicitly reject: no wildcard or default-support arm exists |
| §4.25 | TRP-039, TRP-045 | `ExecutionEnvironmentProfile`, deny-unknown and closed: `environment::{hosted, freestanding}`; `allocation::{forbidden, admitted}` with scratch limits; recursion and stack limits; `failure::{returned_status, panic { disposition::{abort, unwind, halt, reset}, handler::{none, named(§4.27)} }}`; available runtime math; concurrency and atomic model; admitted library contracts and ISA features. CANONICAL SHAPE: `handler` exists ONLY nested under `panic`; `returned_status` carries no handler field at all, so absence is its canonical form and two serializations of one meaning never exist. DISPOSITION is the observable outcome and `handler` the owning MECHANISM — independent axes, since one disposition may be reached through a handler or directly. Compatibility is declared, not inferred: `freestanding` with `halt` or `reset` REQUIRES `named`; `abort` and `unwind` admit either, subject to the environment's own rules. Declaring an ISA or library available NEVER authorizes a kernel — only its §4.9 receipt does |
| §4.32 | TRP-031 | AC-to-PC operational refinement, declared per product: storage rounding and INTERMEDIATE rounding; contraction and evaluation order; the exceptional and status mapping (§6); and any allowed approximation with its exact relation and tolerance. AlgorithmCode is auditor-facing and profile-neutral, so the relation is a REFINEMENT, never equality; if eFMI admits approximation the tolerance is stated numerically, and if it does not the relation is stated as bit-exact for the declared mapping |
| §4.31 | TRP-039, TRP-045 | Emitted-language and toolchain semantic contract: C, Rust, or WASM standard and runtime identity; toolchain identity; and the flags that change legality or results — fast-math, contraction and floating-point environment, overflow checks, panic behavior, and atomics. LEGALITY-CHANGING facts enter preparation and `PreparedDigest`; purely spelling and packaging facts enter `ArtifactDigest`. A source or binary hash IDENTIFIES an output and is never a refinement proof |
| §4.33 | TRP-050 | The `ProductKind` and `RootKind` enums, their legal pairs, and the per-product field obligations are §10 |
| §4.34 | TRP-013, TRP-015 | Receipt root-equivalence: a CMSIS descriptor or quantized-format mapping is root-equivalent ONLY when its receipt proves a LOSSLESS relation on the proven domain — same values, same exceptional and status behavior, same order — AND an identical observable effect footprint and multiplicity over the §4.9 list (`errno`, floating-point environment, globals, threading), or proves that footprint unobservable. Absent either proof it never rides the original root as an optimization. ORDERING: a non-lossless fixed-point or Q mapping REJECTS today; the distinct-root branch becomes available only once the reserved fixed-point semantic type, operation leaves, and profile contracts of §4.1 and SPEC_0049 §2 land |
| §4.27 | TRP-045 | Handler contract, closed and checked — never a bare label: entry symbol and ABI; language, runtime, and toolchain identity; the disposition it implements; termination and non-return behavior; stack and allocation needs; reentrancy, concurrency, and interrupt assumptions; observable effects; and a binary or source hash, or a compiler-known contract version. Selection CHECKS the contract against the environment and the requested disposition; its NORMALIZED CONTENT — never the label — enters `PreparedDigest` |
| §4.11 | SEV-014 | The nominal capability family — opaque external handles AND the explicit reference that makes a record acyclic — admits no literals, ordering, generic wire, arithmetic, address inspection, AD, or tensorization. A reference is reachable only through its declared owner |
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

### 5. Product Closure Matrix

Every advertised consumer accepts or rejects every admitted value family,
operation and effect key, environment, exceptional status, and layout through
the SAME §4.24/§4.25/§4.26 profiles and the one checker flow of TRP-039/042/043.
A blank is not permitted: each cell is an explicit admit or an explicit typed
rejection recorded in the product's own profiles.

| Product | `RootKind` (§10) | Closure obligations |
|---|---|---|
| Hosted Rust | Simulation `SolveProblem` | Every §4.1 family, §4.24 key, SPEC_0049 §1/§2 key, §4.25 environment, status class, and §4.2 layout, at `environment::hosted` |
| `no_std` Rust | Simulation `SolveProblem` | As above at `environment::freestanding`, with its allocation, disposition, and handler decisions |
| Native / Cranelift | Simulation `SolveProblem` | As above, plus the typed backend ABI of SEV-102 |
| WASM | Simulation `SolveProblem` | As above, for the WebAssembly product |
| WGSL | Simulation `SolveProblem` | A DISTINCT product from WASM: a GPU shading-language product whose f32 declaration remains the standing red (SEV-108) |
| Simulation C-ODE | `SolveProblem` | A distinct PRODUCT (a C source ODE), sharing the root kind above |
| FMI component | `SolveProblem` | Kernel semantics end at that root; checked FMI metadata binds into prepared, artifact, and package identity only (TRP-019) |
| Embedded C | `SolveAlgorithmBlock` | A distinct PRODUCT; it does NOT collapse into eFMI Production Code and carries no eFMI container obligations |
| eFMI Algorithm Code | `None` | No `RootDigest` (§10). Admissibility receipt only; MAY reject families it does not model, per SEV-147's selected-mapping scope |
| eFMI Production Code | `SolveAlgorithmBlock` | As Embedded C, plus the §4.32 AC-to-PC refinement and the co-issuance obligations of §4.19 |

Seven products share `RootKind = SolveProblem` per §10: the five language and
backend products above — Hosted Rust, `no_std` Rust, Native/Cranelift, WASM,
WGSL — plus Simulation C-ODE and FMI component. They differ by environment, ABI,
emitter, and packaging, not by root, and are listed separately because their
§4.24 and §4.25 obligations differ. The `ProductKind` names above are packaging
identities, never root kinds; the two enums are independent (§10).

### 6. Failure and Status Mapping

Solve typed failures and status effects (SEV-025/026) reach each product's
declared transport by ONE declared relation, never by convention:

| Solve concept | GALEC / eFMI | C / Rust |
|---|---|---|
| Typed failure (exact-in-domain violated) | A GALEC Signal raised on the owning method | The product's `failure::` decision (§4.25): returned status, or the declared panic disposition |
| Observable status effect (SEV-026) | `ErrorSignalStatus` in the method's declared signal set | Returned status value, or the declared disposition |
| Effect multiplicity | One Signal per raising occurrence; suppression is declared, never implicit | One transport event per raising occurrence |
| No transport for a raisable operation | Rejects at admission (§4.26 status class) | Rejects at admission |

### 7. Scheduled Discrete Ownership (SPEC_0046)

Bound by [SPEC_0046](SPEC_0046_SCHEDULED_DISCRETE_OWNERSHIP.md) §12. Every row
names the `SDO` rule it covers.

**Present state.** None of SPEC_0046 §2–§11 is implemented.

| State | Present-tense fact | Where | Covers |
|---|---|---|---|
| Absent | No total lazy `next` relation: SOLVE-C57's admitted remainder reads held storage, so an inactive target's same-instant read is a storage read | construction | SDO-001, SDO-004 |
| Absent | Laziness is not semantic: work under an inactive target is skipped by evaluation order, not proven to execute zero times | runtime | SDO-003 |
| Partial | Migration-period uniformity and row-filter runtime proofs exist in the working tree, each with a named deletion edge; they are transition debt, not the strata | `rumoca-solver` runtime | SDO-031, SDO-032 |
| Partial | The one-tick and settle restorations are LANDED behavior and are the only strata semantics presently correct. Whole-event ACTION STAGING is NOT landed — see the next row | `rumoca-solver` runtime | SDO-033 |
| Absent | No whole-event candidate/commit relation: outcomes publish as they settle, no owner defines whole-event publication, and effects are not staged | `rumoca-solver` runtime | SDO-040, SDO-044 |
| Partial | `SolveRuntimeSnapshot` already captures the static refresh cache, evaluator random and impure state, and delay state. ABSENT are: one enclosing `EventAttempt` invoking it on EVERY failure path; relation and condition memory; integrator invalidation/restart and FMI lifecycle, termination, and next-event state; schedule-consumption and history restoration; and the observable action ledgers | `rumoca-solver` runtime | SDO-041 |
| Absent | The `Publish`/`Abort` outcome form does not exist; warning and terminate paths are ad hoc and staged-effect order is not retained | `rumoca-solver` runtime | SDO-042 |
| Absent | No `EventInstantExecutionPlan` static owner with nonoverlapping child coverage and one outer commit | `rumoca-ir-solve` | SDO-010, SDO-011 |
| Absent | `InvocationOwnerId` is not keyed by source call occurrence plus activation, domain, and profile | construction | SDO-050, SDO-051 |
| Partial | SOLVE-C55 already requires one `EventTransactionProgram` with a complete result tuple, so the tuple itself is not the gap. What is Absent is COMPOSITION and PUBLICATION: cross-owner cycles have no opaque-block rule, and the transaction's publication is not part of a whole-event commit | construction | SDO-060, SDO-061 |
| Absent | No symbolic activation proof: no lattice or phase analysis accepts a false cycle or rejects a jointly active one, and no certificate reissues on tunable change | construction | SDO-070, SDO-071, SDO-073 |
| Absent | Counters are not split by phase, so an initialization firing and a runtime tick are indistinguishable | evidence | SDO-080, SDO-081 |
| Absent | `ScheduledActivationId` and `ClockId` are conflated: Boolean `sample(start, interval)` is owned as a periodic clock | `rumoca-phase-dae` | SDO-035 |

**Preregistered gates.**

| ID | Gate | Passes when | Covers |
|----|------|-------------|--------|
| SDO-200 | Total next, both polarities | An ACTIVE target's same-instant read observes the lazily selected RHS; an INACTIVE target's read observes the held entry through the SAME relation, with no storage-fallback path taken | SDO-001, SDO-002 |
| SDO-201 | Inactive effects execute zero times | Calls, assertions, folds, and tensor kernels under an inactive target or untaken arm execute EXACTLY zero times, counted by issued identity | SDO-003 |
| SDO-202 | Residual SCC rejection | A legal coupled or nonlinear B.1b SCC rejects at its owning source spans; no order is invented and no coverage hole appears | SDO-021 |
| SDO-203 | Independent base-clock permutation | Permuting independent base clocks leaves every observable identical — the between-partition invariance half of SDO-046 | SDO-031, SDO-046 |
| SDO-204 | Scheduled results stay current | Scheduled `m = pre(m) + 1` followed by unclocked `n = pre(m)` cascades correctly: the scheduled owner does not rerun in later passes, and only iterative `pre` advances | SDO-033 |
| SDO-205 | Coincident directions | In one fixture at one instant: Boolean code reading `hold(clockVar)` observes THIS tick's newly solved value, while a Clock partition sampling a Boolean-updated variable observes its captured left limit | SDO-034 |
| SDO-206b | Round-2 activation | A condition-triggered unclocked algorithm activating at Appendix-B round `k >= 2` consumes current scheduled and iterative definitions and exposes its final tuple to later iterative members; feeding a once-only owner rejects absent a joint owner; the post-settle suffix then consumes the settled tuple before the single commit | SDO-036, SDO-037, SDO-038 |
| SDO-206 | Cross-period cascade | An equation → algorithm → equation chain across different periods observes each stage's total-next result exactly once per activation | SDO-033, SDO-060 |
| SDO-207 | Retry exactness | A retried attempt publishes bit-identical state to a first-try success from the same entry state, restoring completely between tries and duplicating no effect | SDO-040, SDO-041, SDO-044 |
| SDO-208 | Sibling-commit rollback | One sibling's failure restores every other sibling's targets, histories, schedule consumption, and evaluator/delay/cache state | SDO-041 |
| SDO-209 | Full rollback scope | Abort restores relation and condition memory, random and impure state, integrator invalidation and restart, FMI lifecycle and next-event state, and observable ledgers — each checked separately | SDO-041 |
| SDO-210 | Nonconvergence rollback | A non-converging iteration restores entry state completely rather than publishing a partial settle | SDO-041 |
| SDO-211 | Ordered outcomes | `Publish { ordered_staged_effects }` emits each staged effect once in issued and source order, multiplicity retained — the within-owner ordering half of SDO-046; `Publish` with a terminate emits both and publishes the terminal state; `Abort { fatal_failure }` restores and emits only that failure | SDO-042, SDO-046 |
| SDO-211b | Fatal after warnings | An attempt that stages warnings and then fails FATALLY emits its specified failure ONCE and emits NONE of the earlier staged warning, terminate, or status effects; the preregistered failing-late-action fixture publishes nothing | SDO-043, SDO-044 |
| SDO-212 | Non-rollbackable effect | A transactional external effect that cannot be snapshot-and-replayed rejects BEFORE the attempt starts | SDO-045 |
| SDO-213 | Two same-body occurrences | Two calls to ONE function body at one instant issue TWO invocation and effect owners sharing ONE immutable relation body; counts and effects are per occurrence | SDO-050 |
| SDO-214 | Static step identity | Identity issuance is static: IR size is invariant to simulated duration across a long run | SDO-051 |
| SDO-214b | Static plan count, no attempt issuance | PLAN COUNT and IR size are invariant to simulated duration: a long run reuses the same `EventInstantExecutionPlan` for every attempt. The other half: an `EventAttempt` issues NO static structure — no plan, owner, region, or identity is created by running one — so a run of N attempts allocates the same static set as a run of one | SDO-010, SDO-012 |
| SDO-215 | Statement atomicity | `x := a; x := b(x)` publishes `b(a)` as the single final target value, and no consumer observes the intermediate | SDO-060 |
| SDO-216 | Opaque cross-owner cycle | A cross-owner cycle through a transaction rejects as one block; no split or interleaved schedule is produced | SDO-061 |
| SDO-217 | False-cycle intersection | A cyclic edge set whose activation INTERSECTION is empty ACCEPTS via lattice proof; a jointly satisfiable cycle REJECTS at its owning spans | SDO-071 |
| SDO-218 | Schedule exactness | Two equal-lattice occurrences stay distinct; no epsilon comparison, repeated-`f64` drift, or near-instant merge occurs over a long run; a tunable change reissues the certificate and the stale one is never reused | SDO-072, SDO-073 |
| SDO-219 | Coprime cost | Coprime periods stay O(owners + compact edges + rank); no hyperperiod table or activation bitset is allocated | SDO-070, SDO-074 |
| SDO-220 | Initialization phase | `sample(0, T)` does not fire during Modelica initialization, and the three-way counter split distinguishes initialization, the estimator init arm, and runtime ticks | SDO-080, SDO-081 |
| SDO-221 | Million-element lazy aggregate and repeated consumers | TWO requirements. (a) EXTENT: a million-element aggregate target under a lazy arm keeps semantic-graph, wire, preparation, and stored-body METADATA at O(compact bodies + owners + edges + rank); inherent payload storage may scale with source extent, and only the SELECTED branch's work and transient payload scale at execution. (b) REPEATED CONSUMERS: with N consumers of ONE compact body, the producer BODY storage count is EXACTLY ONE and metadata grows only by the N compact consumer references and edges — O(N) references, NEVER O(N × producer-body size). Re-lowering, inlining, a duplicate body, or runtime memoization of the producer graph FAILS | SDO-011, SDO-090, SDO-091 |
| SDO-222 | Four-backend parity | Interpreter, Cranelift, generated C, and the definitional evaluator agree on final state AND status for one discrete fixture spanning active, inactive, warning, and terminate outcomes | SDO-001, SDO-002, SDO-003, SDO-042, SDO-043, SDO-044 |
| SDO-224 | EXCHANGE/HOLD static cutover (amendment governance, not a runtime discriminator) | A SOURCE and WIRE SCAN proves the deleted vocabulary is absent after the atomic amendment: no `EXCHANGE` or `HOLD-FALLBACK` member kind, discriminant, wire tag, or construction path survives in the tree, and the SPEC_0043 §5 HOLD evidence cases are gone rather than skipped. A residual member kind FAILS even if no runtime behavior differs | SDO-004 |
| SDO-225 | Acyclic admission passes | A proved-acyclic scheduled equation owner set ADMITS and executes under ordinary Appendix-B iteration, with no rejection and no coverage hole; the admission proof is the compact acyclicity certificate, not a successful run | SDO-020 |
| SDO-226 | B.1c cycle rejects | A B.1c assignment cycle REJECTS at its owning source spans; it is never admitted, never ordered, and never converted into an iterative fixed point | SDO-022 |
| SDO-227 | SIM-010 stays Partial | A legal coupled or nonlinear B.1b residual SCC yields a typed rejection AND the SIM-010 evidence records `Partial`, naming the absent `ResidualSccOwner`. A green SIM-010 claim without that owner FAILS the gate | SDO-023 |
| SDO-228 | Four history lanes are distinct | One fixture where `LeftLimit`, `SampledLeftLimit`, iterative-`pre`, and clock `Previous` hold FOUR DIFFERENT values at the same instant, and each consumer receives its OWN lane. Any two lanes collapsing to one value, or a consumer reading another's lane, FAILS | SDO-030 |
| SDO-229 | Stratum order, both boundaries in one fixture | ONE fixture presents BOTH boundaries at one coincident instant: at least one ACTIVE synchronous base-clock partition, AND an unclocked relation forcing Appendix-B round `k >= 2`. The Boolean `sample(start, interval)` owner then runs EXACTLY ONCE, strictly AFTER the active clock stratum and strictly BEFORE the later round. A fixture exhibiting only one boundary does not discharge this row | SDO-032 |
| SDO-230 | Schedule and clock IDs noninterchangeable | Passing a `ScheduledActivationId` where a `ClockId` is required FAILS TO COMPILE, and a wire record swapping one for the other REJECTS on decode for the TYPE reason. A conversion helper between them is itself a failure | SDO-035 |
| SDO-231 | Action identity replay negatives | Swapping, duplicating, or omitting an `EventActionId` or its output projection identity each REJECT, and each rejection cites the IDENTITY reason — a downstream type, arity, or value mismatch accidentally catching it does NOT discharge this row | SDO-052 |
| SDO-232 | Coordinate and lane identity replay negatives | Swapping, duplicating, or omitting an event coordinate, an entry or history lane, a relation-side probe, a refresh closure, or a consumption token each REJECT for the IDENTITY reason; a downstream accident does not discharge this row | SDO-053 |

**Coverage exclusions (governance and reversal, by design).** Three rules carry
no discriminator because none is meaningful: TRP-004 is a GOVERNANCE
cross-reference whose evidence is SEV-110's reversal record; TRP-040 and TRP-041
are REVERSAL gates, evaluated only when someone proposes the reversal. Inventing
a runtime test for any of the three would be theatre. Every other SEV, TRP, and
SDO governing rule is covered above.

**RDD2 estimator discriminator (SDO-223).** Bound by ONE SELECTED BINDING
CERTIFICATE — either the external model's source digest, or a structural
precondition that is itself non-vacuous:
exactly ONE `step` occurrence; `predict` evaluated BEFORE correction; the exact
priority chain `mocap → joint GPS → GPS position → GPS velocity → optical →
hold`; and the later `navigationEstimateArrays` read observing the CURRENT
total-next value. A topology change invalidates the gate rather than passing a
simpler model.

Phase counts are pinned exactly: **0** Modelica-initialization activations,
**1** estimator-initialization arm, **100** normal ticks at 0.5 s over a 5 ms
step. Per-arm ZERO-EXECUTION proofs per `navigationSource`: `= 1` exercises the
joint-GPS arm only; `= 2` the optical arm on post-initialization fresh ticks;
`= 0` leaves `step`, `predict`, and `navigationEstimateArrays` with corrections
zero. The four legs are named: the interpreter, `NativeRequired` Cranelift,
generated C, and the INDEPENDENT AlgorithmCode evaluator — all four report
identical per-owner counts split into initialization and runtime. Branch, call,
assertion, fold, tensor, and commit identities are each correlated to their
issued `OccurrenceId` and `InvocationOwnerId` plus THE SELECTED BINDING
CERTIFICATE — whichever of the two was chosen — never to ordinals or names, and
cost stays O(source owners + rank) independent of covariance extents. Covers
SDO-001, SDO-002, SDO-003, SDO-050, SDO-051, SDO-060, SDO-080, SDO-081,
SDO-090. It does NOT cover SDO-036: the RDD2 Boolean `sample` transaction
activates in round 1, so this fixture proves no `k >= 2` behavior.

**Rejected alternatives.**

| Alternative | Cost that defeated it | Reversal gate |
|---|---|---|
| EXCHANGE / HOLD-FALLBACK split (SOLVE-C57) | The hold-fallback member makes a same-instant read a STORAGE read, so an inactive target's value depends on evaluation history rather than a relation — unsound, not merely complex | SDO-110 |
| A universal execution order | Invents an order among INDEPENDENT base clocks that MLS §16.5.1.1 leaves unordered, making a permutation-invariant observable order-dependent | SDO-111 |
| One giant re-lowered body | Program size grows with CONSUMER count rather than producer count, and every consumer re-lowers the producer graph | SDO-112 |
| A rollback journal instead of a private arena | ID ALIAS OR REUSE makes journal rollback UNSOUND; monotonic IDs with tombstones CAN be sound, so the journal is not wrong in principle. OBSERVATIONAL ROLLBACK is the correctness gate; the ≤2% prepare delta is a performance discriminator only, never a correctness argument | SDO-113 |
| One unified history buffer | The four lanes advance differently at coincident instants, so one buffer must pick a wrong advancement for at least one lane | SDO-114 |
| A runtime row cache | A dichotomy with no escape: a cache keyed loosely enough to hit serves a STALE REPLAY, and one keyed tightly enough to be correct re-executes the row's EFFECTS | SDO-116 |
| Value convergence as execution cardinality | Equal values across passes prove nothing about how many times a row executed | SDO-051 |
| Combined first-pass ordering | Merging the scheduled and clocked first pass with the unclocked round invents the order SDO-031 forbids | SDO-111 |

### 8. Acceptance-Time Amendment Map

Bound by SPEC_0048 §1. Each clause was verified against its source before
listing; on acceptance the voted series amends all of them atomically.

SPEC_0007
line 270 states `SolveAlgorithmBlock` is constructed ONLY FROM checked Algorithm
Code; TRP-030 states neither sibling lowers from the other, so that clause and
its bound SPEC_0040 SOLVE-C34 (method ownership) and SOLVE-C38 (injective
mapping to Algorithm Code identity) are amended to co-issuance from one shared
construction with bidirectional correlation. SPEC_0034 GAL-004 ("checked
construction closes the package after lowering") and GAL-005 ("accepted
constructs lower to semantic operations", owner `rumoca-phase-galec`) assign
expression lowering to phase-galec and are amended to projection-and-
admissibility only, per TRP-032.

The scalar-program conflicts are enumerated, not promised: SPEC_0040 SOLVE-C03
(flow-action calls in "Solve-IR scalar programs"), SOLVE-C25 (scalar root
projection for call-scoped assertions), SOLVE-C39 (`FunctionFoldProgram`),
SOLVE-C43 and SOLVE-C50 (`ScalarProgramBlock` owner table and its immutable
execution certificate), and SOLVE-C45 (scalar-view projections over one pure
aggregate-call occurrence) all presume a stored scalar program that TRP-020/021
and TRP-035 confine to post-seal emission; each is amended to the final-emitter
projection. Their SPEC_0043 §9 counterparts — the `ScalarProgramBlock`
function-conditional table row and the scalar-program execution-certificate row
— are amended in lockstep. SPEC_0036 "Solve Algorithm Block Construction" states
`SolveAlgorithmBlock::construct` is the sole authority for the GALEC-DERIVED
executable root and "consumes one checked `AlgorithmCodePackage`"; TRP-030
co-issues the siblings from one shared construction, so that clause and its
SPEC_0043 §9 link are amended together.

### 9. Complex Contract (SPEC_0035 Narrowing)

Bound by SEV-017, under which Complex is **not admitted**; this is the future
admission checklist. SPEC_0035
is NOT retired: it stays the DRAFT owner of these rules. The table below is the
checklist a future Complex slice must discharge before Complex becomes
computable — value form, wire form, per-operation leaves, and classes included.
Until then §4.1 does not admit Complex at all, and every Complex use rejects.

| Carried rule | From | Placement |
|---|---|---|
| Operator applications resolve to the DECLARING function, never by name matching | SPEC_0035 §2 | Frontend resolve/instantiate; unchanged by this series |
| A record whose operator bodies are proved equivalent to complex formulas maps to the complex element type; an unrecognized operator record does NOT, and the refusal is reason-coded | SPEC_0035 §2 | DAE lowering. Under TRP-020/021 the refusal is a typed rejection or a real-lane record, never a scalarized program |
| Solver STATE slots are real; the layout records component pairing as one id with N component offsets | SPEC_0035 §3 | Real-state boundary; the integrator contract is unchanged |
| Interleaved is the default layout, planar is opt-in, and the view and typed form agree by construction | SPEC_0035 §3 | A §4.2 prepared layout under TRP-044, receipted like every other |
| One node set over two element types: existing tensor operations accept complex elements and the evaluator runs a native complex kernel | SPEC_0035 §4 | SEV-005 shape-polymorphism; SPEC_0049 §3 splits the keys by element kind |
| Differentiation is exact over REAL lanes; native complex AD requires a PROVED HOLOMORPHIC operation, since non-holomorphic rules differ | SPEC_0035 §4 | SEV-030's ideal-real claim; a non-holomorphic operation in an AD-required region REJECTS under SEV-033 |
| A backend without complex support requests the real view | SPEC_0035 §4 | Now a typed rejection or an admitted real-lane family under §4.24, never an implicit fallback (TRP-021) |

**SPEC_0029 §5/§12 ownership rows (SPEC_0041 §4), split by TRP-032.**

| SPEC_0041 §4 row | Amended to |
|---|---|
| *"Compilation/session orchestration"* (`rumoca-compile`) | Gains atomic sibling-package orchestration: one construction transaction, the correlation web, and checksum binding |
| *"DAE → `SolveProblem`; checked Algorithm Code → `SolveAlgorithmBlock` lowering"* (`rumoca-phase-solve`) | Gains the shared expression and function relation construction plus profile-bound root closure |
| *"Checked DAE pure-function graph → shared typed Solve program regions and pure-call owners"* (`rumoca-phase-solve`) | Same shared construction; the AC sibling references it rather than re-lowering |
| *"DAE/Solve → checked GALEC lowering"* (`rumoca-phase-galec`) | Narrows to projection and container authority, with NO expression or function lowering |

### 10. Product and Root Schema (§4.33)

Bound by TRP-050.

**`ProductKind`** (closed, ten variants): `HostedRust`, `NoStdRust`,
`NativeCranelift`, `Wasm`, `Wgsl`, `SimulationCOde`, `FmiComponent`,
`EmbeddedC`, `EfmiAlgorithmCode`, `EfmiProductionCode`.

**`RootKind`** (closed, independent): `SolveProblem`, `SolveAlgorithmBlock`,
`None`. Product PACKAGING is never a root kind.

**Legal pairs.**

| `ProductKind` | `RootKind` |
|---|---|
| `HostedRust`, `NoStdRust`, `NativeCranelift`, `Wasm`, `Wgsl`, `SimulationCOde`, `FmiComponent` | `SolveProblem` |
| `EmbeddedC`, `EfmiProductionCode` | `SolveAlgorithmBlock` |
| `EfmiAlgorithmCode` | `None` — no `RootDigest` |
| Flat and DAE exports (not products of this schema) | `None` — no `RootDigest` |

**Per-product fields.** R = REQUIRED, F = FORBIDDEN. No field has a default;
an absent R rejects and a present F rejects.

| Field | `RootKind = SolveProblem` | `RootKind = SolveAlgorithmBlock` | `RootKind = None` |
|---|---|---|---|
| Root input and `RootDigest` reference | R | R | F |
| Numeric profile (§4.21) | R | R | F |
| Value capability profile (§4.24) | R | R | F |
| Operation/effect profile (§4.26) | R | R | F |
| Environment profile (§4.25) | R | R | F |
| Emission policy (§4.22) | R | R | F |
| `CoverageMode` | R | R | F |
| Ordered candidates | R | R | F |
| Budgets (TRP-022) | R | R | F |
| Receipt SELECTORS (manifest inputs) | R | R | F |
| Issued RECEIPTS (preparation outputs) | R | R | F |
| Sibling inputs (eFMI pairs) | F | R for `EfmiProductionCode` (it carries the AC sibling and correlation input), F for `EmbeddedC` | F |

Candidates are REQUIRED exactly where `CoverageMode` is, which removes the
earlier circular condition: both key off `RootKind`, never off each other.
Receipt SELECTORS are manifest INPUTS naming what may be chosen; issued RECEIPTS
are preparation OUTPUTS recording what was chosen and proven — the two are
distinct fields and neither substitutes for the other.
