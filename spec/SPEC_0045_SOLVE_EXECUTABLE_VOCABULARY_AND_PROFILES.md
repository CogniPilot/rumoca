# SPEC_0045: Solve Executable Vocabulary and Profiles

## Status
DRAFT

## Summary

One closed factored Solve operation grammar, one identity system, value types
owning semantic kind, nominal identity, shape, representable domain, and
encoding, and all arithmetic and sensitivity policy on root-bound profiles.

## Specification

### 1. Governance, Scope, And Acceptance-Time Amendment Map

This DRAFT proposes the amendments below and claims none today. A SPEC_0000
amendment adding the missing lifecycle edges (PROPOSED in the required-status
and README tables; PROPOSED → DRAFT → vote → ACCEPTED) is planned for the same
voted series; until it passes, this file stays DRAFT.

On acceptance this amends SPEC_0035 Summary and §§1/3/4 — precision-neutral
`Real`, codegen-time width selection, and record scalarization are superseded by
§3–§4, SPEC_0035's Complex rules are narrowed into SEV-017, and SPEC_0035 is
retired in the same series so two numeric authorities never coexist. It also
amends DRAFT SPEC_0036 and SPEC_0043 §9 with its rounding rows (profile-bound
identity and the §6 split). Target-facing parents are amended by
[SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) §1.

**Series arithmetic.** 17 before this proposal series; +0045 and +0048 = 19;
planned 0046 = 20; retiring 0035 = 19.

Governed: the Solve grammar, type algebra, root-bound profiles, executable
identity, term sharing, and wire replay. Target refinement, prepared products,
and the final expansion boundary are SPEC_0048. Not governed: DAE equation
ownership, FMI, solver algorithms, GALEC syntax, and discrete events (proposed
sibling SPEC_0046, not in tree).

### 2. One Typed Operation Grammar

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-001 | The grammar factors into `ValueOp`, `InvokeOp`, `EffectOp`, `Terminator`; a `RegionId` names regions of blocks and ops. | `rumoca-ir-solve` | No god enum |
| SEV-002 | All factors share ONE wire form, ONE definitional-semantics and total-dispatch contract, ONE provenance model, and ONE capability union, enumerated variant by variant in [SPEC_0049 §1](SPEC_0049_SOLVE_GRAMMAR_CATALOG.md#1-value-invoke-and-effect-variants); a root admits a checked SUBSET, never a dialect. Interpreter, Cranelift, C, and other executors are SEPARATE implementations checked against that contract. The prohibition is a scalar-versus-tensor semantics fork, never multiple independent executable oracles. | `rumoca-ir-solve` | One contract, many executors |
| SEV-003 | The canonical executable product is an opaque package binding ONE concrete profile value; Binary32 and Binary64 are different roots before folding, CSE, AD, and range proofs (`16_777_217`). | `rumoca-phase-solve` | Profiles change results |
| SEV-004 | Lowering code and immutable inputs are shared; an executable profile-neutral body is not. | `rumoca-phase-solve` | Inputs, not bodies |
| SEV-005 | Rank-0 and rank-N are values of one grammar: no scalar-versus-tensor GRAPH-KIND bit, graph-kind conversion, cache, AD path, or call ABI. Explicit numeric conversions inside the one grammar remain REQUIRED by SEV-010/022. Opcodes are shape-polymorphic only where the algebra is identical. | `rumoca-ir-solve` | Flavors duplicate proofs |
| SEV-006 | `ScalarOp`/`LinearOp` is a frozen superseded adapter awaiting deletion; a scalar projection is a borrowed final view, never stored. | `rumoca-ir-solve` | Views are not owners |
| SEV-007 | Consumers cover the vocabulary exhaustively or reject a declared capability. An UNCLASSIFIED variant — one absent from SPEC_0049 §1 — fails every matcher and total dispatcher until classified. **Stage test:** a new stage needs a different CONTRACT, not a granularity. | Solve consumers | Granularity duplicates proofs |

**Why:** CasADi is the cited warning, precisely. `SX` builds one scalar node per
element while `MX` admits matrix-valued primitives, so `3*x+y` on a 2-vector is
eight `SX` operations and two `MX` operations; the two cannot mix in one
expression, and the only sanctioned boundary is an `MX` call to an `SX` function,
with `expand()` trading speed for memory. The split had a REAL profitability
basis — low-overhead scalar relations and compact aggregate owners genuinely
differ — and Rumoca keeps that benefit through one grammar's compact
shape-polymorphic ops, `FunctionRelationId`/`InvokeOp` boundaries, borrowed final
views (SEV-006), and backend-private SSA (TRP-003). What is rejected is only the
rest: two incompatible semantic graph universes, identity-changing graph-kind
conversion, and duplicated evaluator, AD, and wire proofs. Function boundaries
are the useful lesson; graph-kind identity is the mistake.

### 3. Semantic Type Algebra

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-010 | A value type owns semantic KIND, NOMINAL IDENTITY, SHAPE, REPRESENTABLE DOMAIN, and ENCODING — and nothing else. Arithmetic, sensitivity, execution, and target-layout policy are EXCLUDED. Any representation-changing COERCION of a value is FORBIDDEN: a representation change requires an explicit typed `Convert`. Every other operation declares its exact typed result. | Solve types | Policy doubles lattices |
| SEV-011 | The root profile declares defaults and admissible contracts; construction resolves EXACTLY ONE arithmetic contract per occurrence of an operation whose [SPEC_0049](SPEC_0049_SOLVE_GRAMMAR_CATALOG.md) contract class is applicable — a `NotApplicable` leaf such as a load, store, Boolean control, call, effect, or terminator resolves NONE, and no backend may invent one. Neither the value type nor a backend chooses. The resolved contract is part of the term and op key; profile admissibility is part of `RootDigest`. | construction | One authority per occurrence |
| SEV-012 | The admitted families are exactly [§4.1](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); records are FINITE ACYCLIC by value, recursing only via an explicit reference or opaque capability. | Solve types | Not all float |
| SEV-018 | `SolveScalarType::Integer { repr: IntRepr }` owns signedness, width, and encoding ONLY. Interval and range facts are SEPARATE root-bound facts on SSA definitions and slots, derived from source declarations and construction. An explicit narrowing conversion changes `repr`; ordinary range refinement does NOT change value-type equality. | Solve types | Representation is not a range |
| SEV-013 | Solve keeps nominal record, field, and shape identity; every layout fact in [§4.2](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) is a prepared mapping, injective on the proven domain and round-tripping across FMI/eFMI. | Solve types | Layout is not identity |
| SEV-014 | Enum TYPE identity is `{EnumTypeId, cardinality}`; an ordinal identifies a VALUE of that type, so ordinal 1 of two enums is two values of two unequal types. Opaque handles and the explicit REFERENCE that makes a record acyclic (SEV-012) are one nominal capability family restricted by [§4.11](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). | Solve types | Else a pointer |
| SEV-015 | Zero storage is not zero identity: an empty value keeps every identity in [§4.15](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) and zero domains never run bodies. | construction | Empty is a value |
| SEV-016 | `volatile` is NOT ABI-only: an MMIO read may change value and its access count is observable, so a cacheable `Load` cannot map to volatile. Admit only the owners in [§4.16](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), or reject. | Solve effects | Access count observable |
| SEV-017 | Complex is one element type over an admitted binary format, never a record or pair; interleaved versus planar storage is a prepared layout. This narrows and replaces SPEC_0035 §1, and every other rule SPEC_0035 owned — operator-record recognition, the real-state boundary, evaluator equivalence, and holomorphic AD — is carried in [SPEC_0047 §9](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#9-complex-contract-spec_0035-retirement); Complex is admitted only while those rows hold. | `rumoca-ir-solve` | One numeric authority |

### 4. Root-Bound Profiles And Arithmetic Closure

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-020 | The bound profile is ROOT-IDENTITY-BEARING; folding is profile-bound, and interning ACROSS profiles is forbidden. | construction | Protects constant identity |
| SEV-021 | A profile admits a SET of types plus ONE declared default specialization for source `Real` and ONE for source `Integer`, neither context-dependent; one format per program is a special case. | checked profile | Stable reasoning |
| SEV-022 | Registers are exactly typed; mixed-format programs are normal, each cross-format edge a licensed conversion. | construction | Mixing is normal |
| SEV-023 | The profile binds BEFORE construction of the executable root; changing widths later is FORBIDDEN. | pipeline order | Rounding differs |
| SEV-024 | For each APPLICABLE contract class, construction resolves exactly one contract over [§4.3](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs): `ExactIntegral` resolves domain and status only, `FloatingPrimitive` the rounding fields, `FloatingTranscendental` those plus its accuracy contract, and `Reduction` those plus accumulator and order. | construction | Width alone does not close arithmetic |
| SEV-025 | Integer arithmetic and conversion are exact-in-domain or a typed failure, with divide-by-zero and `MIN/-1` explicit; host UB, wrapping, and saturation are prohibited. Operations use the SEV-018 interval facts to prove overflow unreachable, or emit the profile's checked typed-failure path. Replay rederives those facts under SEV-092; they are never serialized. | construction | UB disagrees silently |
| SEV-026 | Observable status is an EFFECT: it blocks execution CSE unless multiplicity is proven unobservable. | construction | Two raises, not one |
| SEV-027 | Saturation, wrapping, and Q-format rescaling are NEVER a target strategy for ordinary arithmetic: admit a distinct operation or prove it unreachable. | construction | Strategy preserves results |

### 5. Sensitivity (AD) Profiles

Three semantics exist, MUST NOT be conflated, and carry distinct profile IDs:

| ID | Semantics | Status |
|----|-----------|--------|
| SEV-030 | Ideal-real differentiation specialized to a declared profile, with primal and tangent rounding stated. | Only formal claim |
| SEV-031 | Derivative of the actually quantized function; usually zero or undefined at conversion boundaries. | Never the default |
| SEV-032 | Straight-through / engineering sensitivity. | NAMED capability only |

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-033 | Narrowing, fixed-point, or saturating conversion inside an AD-required region REJECTS unless the sensitivity profile defines it; widening MAY refine the ideal derivative. | construction | Rounding is discontinuous |
| SEV-034 | AD-capable profiles admit Binary64 first, Binary32 once parity is defined. A primal and its directional program are DISTINCT executable roots with distinct `RootHandle`s and `RootDigest`s, because the sensitivity profile differs and SEV-041 hashes normalized profiles. They share the SEV-047 source and occurrence correlation, the SEV-044 `FunctionRelationId` and owner correlation, and the SEV-024 resolved PRIMAL arithmetic contracts, and are bound by a mechanically checked DERIVATION EDGE: the directional root records which primal root and which sensitivity profile it derives from. | profiles | Distinct roots, checked edge |

### 6. Identity, Occurrence, And Term Sharing

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-040 | `RootHandle<'r>` and root-local typed IDs are in-process authority and never serialize. | `rumoca-ir-solve` | Handles are not digests |
| SEV-041 | `RootDigest` hashes [§4.13](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) excluding the claimed digest; semantic schedules are payload, implementation schedules enter `PreparedDigest`, and decode recomputes the claim. | wire | Noncircular, recomputable |
| SEV-042 | The digest ladder is closed and ancestral: `PreparedDigest` hashes [§4.28](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) and `ArtifactDigest` hashes [§4.29](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), each naming its parent digest. Provenance placement is [§4.30](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs). Decode RECOMPUTES every claimed digest; no layer may be substituted across roots, targets, receipts, or templates. | wire, preparation | Ancestry or substitution |
| SEV-043 | `ValueDefinitionId` identifies ONE SSA definition; region and block position give canonical dominance; prepared schedules are checked relations over handles. | construction | Dominance is structural |
| SEV-044 | The split adds an OPTIONAL span-free `PureTermId` quotient, `OccurrenceId` with compact `OccurrenceFamilyId`, a monomorphic `FunctionRelationId`, an `InvocationOwnerId` keyed by [§4.4](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs), and typed value/effect projections. | construction | Three questions differ |
| SEV-045 | `TermKey` binds the generative root handle and [§4.5](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs) — never a span, never a target strategy. | construction | Keys precede digests |
| SEV-046 | `TermKey` construction, OPTIONAL hash-consing, and execution CSE are three separate decisions; a hash is never identity, wire, or order, and exact collision comparison is mandatory. | construction | Storage is not reuse |
| SEV-047 | The occurrence sidecar keeps [§4.6](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); structured generated uses own ONE provenance, role, and domain family, never one per coordinate. | construction | Two lines, two spans |
| SEV-048 | Pure predicate and argument STORAGE may be shared by any operation, calls and assertions included; execution owners stay distinct and never merge. | construction | Sharing is not merging |
| SEV-049 | Execution reuse discharges every obligation in [§4.7](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#4-bound-field-catalogs); large tensor results key one compact owner, never coordinates. | preparation | Reuse is scheduling |

### 7. Wire, Replay, And Schema

| ID | Rule | Owner/Where | Brief Justification |
|----|------|-------------|---------------------|
| SEV-090 | Canonical Solve serialization binds `RootDigest` only; prepared and evidence records bind `PreparedDigest`, and checksum graphs bind output bytes. | Solve wire | One layer per record |
| SEV-091 | Forged representation, value, record, enum-brand, or empty-layout mutations reject on decode, which replays checked constructors; canonical byte order is interning-invariant. | wire constructors | Decode is construction |
| SEV-092 | Derived range and interval facts are rederived on replay from serialized semantic inputs, never stored per operation, unless irreducible. | wire schema | Certificates bloat wire |

### 8. Current State, Gates, And Rejected Alternatives

§6 is unimplemented: the compiler issues fresh `ExprId`s and memoizes exact IDs
instead of merging equal terms. Every row of
[SPEC_0047 §1](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#1-current-implementation-state)
is `Partial` or `Absent`. A rule is implemented only when every gate its
`Covers` row names passes:
[SPEC_0047 §2](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#2-red-gates-and-witnesses-preregistered).
Defeated alternatives are
[SPEC_0047 §3](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md#3-rejected-alternatives).

### 9. Reversal Gates

| ID | Reopening | Requires |
|----|-----------|----------|
| SEV-110 | A second canonical graph | ALL of: a genuinely different stage contract; an operation not representable in the grammar plus a checked target view; an INDEPENDENT semantic oracle for the second graph; a checked relation preserving type, profile, effect, and provenance identity across the boundary; and NAMED wire, AD, and capability proof-cost budgets it must stay within. Unrepresentability plus speed is not sufficient |
| SEV-111 | A new core type family | A product inexpressible as a semantics-preserving ABI mapping, an independent oracle, AND two consumers or one safety-critical consumer with parity evidence |
| SEV-113 | A profile-neutral construction recipe | Three-profile parity plus preregistered compile and RSS benefit; it stays non-executable |
| SEV-114 | Abandoning the §2 factoring | Cross-factor escapes exceed 20%, or boilerplate exceeds 15% without reducing proof surface; keeping it needs ≥30% fewer unrelated touch sites with ≤3% evaluator and ≤5% wire regressions |

## References

- `dev/2026-08-11-core-structure-decisions.md` §13.1–§13.6 — the user-ratified
  decision record consolidated here.
- CasADi user guide, `web.casadi.org/docs` §3.1 (`SX` scalar expression graphs),
  §3.3 (`MX` matrix primitives), §3.4 (mixing `SX` and `MX`), §4.2 (`expand()`)
  — the §2 rationale.
- [SPEC_0047](SPEC_0047_SOLVE_EXECUTABLE_VOCABULARY_CATALOG.md) — shared
  evidence and field-catalog annex; binding force lives in the rules above,
  which enumerate their field lists and gates there.
- [SPEC_0048](SPEC_0048_TARGET_REFINEMENT_AND_PREPARED_PRODUCTS.md) — target
  refinement and prepared products;
  [SPEC_0049](SPEC_0049_SOLVE_GRAMMAR_CATALOG.md) — the bound grammar catalog.
