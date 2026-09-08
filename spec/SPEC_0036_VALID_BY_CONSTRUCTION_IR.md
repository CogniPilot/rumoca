# SPEC_0036: Valid-by-Construction Compiler IR

## Status
DRAFT

## Summary

Compiler IR makes invalid stage values unrepresentable. Construction uses
sequential semantic-owner closures over private aggregates without weaker
storage.

## Specification

### Scope

This stays `DRAFT` until AST proofs and compiler roots hide invariant fields and
root validators. Solve sparsity follows
[SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md).

Normative milestone, owner, equation, and evidence catalogs are in
[SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) and linked by their owning
sections.

### DAE Milestone Acceptance

Acceptance rows and the `rumoca-ir-dae` LOC review triggers are
[SPEC_0043 §1](SPEC_0043_CONSTRUCTION_CATALOG.md#1-dae-milestone-acceptance-and-review-triggers).

### One Aggregate Owns Construction

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Aggregate owns all arenas/systems, including `ExpressionArena` | `Dae` | One authority |
| Sequential owner closures | `Dae::construct` | Shared boundary |
| Handles are borrowed, branded, nonserializable capabilities | DAE API | No owned IR |
| Success returns immutable `Dae`; failure exposes none | `Dae::construct` | No partial root |
| Finalization is O(1), excluding freezing | `Dae::construct` | No rescan |

Data-owning builders, partial roots, unchecked insertion, and
finalized mutation are prohibited. Producers own analysis; insertion checks
supplied proofs and local integrity.

### Solve Aggregate and Discrete Definitions

`SolveProblem::construct` is the one Solve construction authority; its per-owner
rules are
[SPEC_0043 §6](SPEC_0043_CONSTRUCTION_CATALOG.md#6-solve-aggregate-and-discrete-definition-catalog-spec_0036-solve-aggregate).

For target `x`, the first true `(a_k, v_k)` gives `x' = v_k`; otherwise
`x' = x`. Activations are shared per iteration; inactive values are skipped.
Each definition derives its integrator-history effect from the finalized Solve
dependency graph. `Preserve` is constructible only with a proof that the target
cannot reach continuous dynamics; missing, cyclic, ambiguous, or unsupported
evidence constructs `Restart` instead.

### Solve Algorithm Block Construction

`SolveAlgorithmBlock::construct` is implemented as the sole authority allowed
to authenticate package-branded Algorithm Code subjects through the narrow
`rumoca-ir-solve` → `rumoca-ir-galec` refinement dependency. It consumes one
non-cloneable `AlgorithmCodePackage`, derives executable arithmetic only from
the complete normalized numeric profile retained by that package, and returns
one non-cloneable co-emission product retaining the package and sealed
executable root. No second arithmetic input or mismatch state exists. Failure
exposes neither a partial root nor a second construction route. Algorithm Code
rendering borrows only the retained package; Production C/H borrows only the
block. The registered readiness-zero route currently admits only a narrow
scalar subset; unsupported calls, loops, tensors, branches, effects, and ABI
cases reject, and Production-C conformance remains pending. Detailed
obligations are
[SPEC_0043 §9](SPEC_0043_CONSTRUCTION_CATALOG.md#9-solve-algorithm-block-construction-catalog).
Package-rooted refinement consumes and cites each package-issued Real
`MatrixMultiply` occurrence contract. It cannot select, default, restate, or
override accumulator format, seed, primitive rounding, order, contraction,
special-value, subnormal, or floating-status semantics.

The shared causal-discrete structural result derives target identity,
current-value dependencies, and deterministic orientation from one branded DAE
view. Solve and GALEC may restrict it but cannot reinterpret an unresolved row.
The same causal-definition owner derives whether a complete algebraic/output
declaration is event-held. Solve stores that fact as one typed declaration
domain; scalar trace metadata is only a final presentation projection of the
declaration proof.

### Flat Aggregate Construction

`flat::Model::construct` is the one Flat construction authority; its per-owner
rules are
[SPEC_0043 §7](SPEC_0043_CONSTRUCTION_CATALOG.md#7-flat-aggregate-construction-catalog-spec_0036-flat-aggregate).

Declarations retain exact spans. Instance IR retains per-element semantics, not
array-compaction history; Flat owns structured families and derived scalar
views. Record function values stay nominal aggregates through Flat/DAE.
Connections construct from typed Instance source groups through a build-local
topology plan and one atomic final projection. Drafts, public invariant fields,
repair, compatibility, unchecked insertion, finalized mutation, and alternate
constructors are prohibited.

Occurrence ordinals are storage/diagnostic data, never authority. Instance and
Flat construction MUST satisfy the opaque origin-capability and replay rules in
[SPEC_0043 §7](SPEC_0043_CONSTRUCTION_CATALOG.md#7-flat-aggregate-construction-catalog-spec_0036-flat-aggregate),
normative by reference; numeric IDs cannot establish cross-root identity.

### Storage and Forward References

Solve catalog storage is a logical Y/P column and scalar index/count, never
`Time`, a constant operand, or an independently supplied byte displacement.
Logical construction checks index arithmetic and owning-column bounds, not a
fixed element byte width; the obsolete index-times-eight bound is retired.
Concrete displacement widths and overflow belong to target/execution layout.

Storage shape and the entries permitted to reserve are
[SPEC_0043 §2](SPEC_0043_CONSTRUCTION_CATALOG.md#2-reservation-owner-catalog-spec_0036-storage-and-forward-references).

Only catalogued entries reserve. Private linear authority and an O(1) unfilled
counter reach zero before success; all else inserts complete values in proven
order. Local checks/counters are required. Global trackers, parallel identity
maps, persistent seals, root validation/repair, and unchecked paths are
prohibited. Brands affect no finalized equality/order/display/wire data.
Acyclic functions construct in dependency order.

Construction is O(nodes + operands + total rank); insertion is amortized O(1)
plus operand/rank work. Views borrow, derived indexes build once, proof
transitions do not deep-clone IR.

Prepared guards are affine, `#[must_use]`, commit only by consuming `self`, and
leave the transaction unchanged when abandoned. The exact compiler/lint/attribute
backstops and their non-proof boundary are
[SPEC_0043 §5](SPEC_0043_CONSTRUCTION_CATALOG.md#5-enforcement-evidence-catalog),
normative by reference; successful construction still requires owner close.

### Canonical Arenas, Systems, and Environments

The aggregate owns exactly the arenas, systems, and environments listed in
[SPEC_0043 §3](SPEC_0043_CONSTRUCTION_CATALOG.md#3-canonical-arenas-systems-and-environments),
each with its required storage.

### Type and Variable Identity

Type and variable identity MUST satisfy every row in
[SPEC_0043 §11](SPEC_0043_CONSTRUCTION_CATALOG.md#11-type-and-variable-identity-catalog),
normative by reference.

Coordinates are primitive/enumeration rectangular values; function values may
include checked aggregates/external objects. Finite, inspectable proofs
monomorphize function extents before DAE construction. Loops preserve source
order as compact finite-domain transitions over typed carried values.
Unresolved, cyclic, overflowing, or zero-step domains fail at their owner; no
guessed extents/literal unrolling.
Partition ordinals are layout, never semantic identity.

### Expressions and Equations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| `expr.at(provenance).operation(...)` creates every node | `ExpressionArena` API | Provenance mandatory |
| Parallel provenance stores exact identity/range/origin; text stays in `SourceMap` | DAE root | No duplicated text |
| Variadic children use packed buffers | Expression arena | Compact dense storage |
| Coordinates carry use spans; declarations carry declaration spans | Owning arenas | Distinct occurrences |
| Operands use active-build typed IDs | Expression API | No cross-build use |
| Nodes derive type, shape, variability, domain | Expression API | O(1) checks |
| Composite variability is operand maximum; coordinate owners supply it | DAE aggregate | No tree walk |
| Nested domains name their checked lexical parent | Domain arena | Explicit scope tree |
| Domain merging requires ancestry; comprehensions consume locals and retain parents | Expression API | Typed capture |
| Source temporal/flow calls are absent | Expression grammar | Closed boundary |
| Role conversion returns typed expression IDs | Expression API | Compile-time roles |
| Equations accept role-specific IDs | Equation systems | No generic forgery |
| Optional-lhs equations are prohibited | Final DAE | Role-defined form |

Per-system equation contracts are
[SPEC_0043 §4](SPEC_0043_CONSTRUCTION_CATALOG.md#4-equation-contract-catalog-spec_0036-expressions-and-equations).

A scalar discrete coordinate determined by an initial algorithm assignment or
by an explicit initial equation `m = value` / `pre(m) = value` has one typed
initial-value owner. Recognition only selects this owner; construction derives
the target role and scalar type, proves that the value reads only
initialization-settled coordinates, and rejects a second owner for the same
target. The claimed Flat row therefore cannot also enter the numeric
initialization residual system.

Executable Solve construction issues an initialization projection only after a
complete structural matching assigns one distinct usable initialization row to
every unpinned state scalar and every unbound `fixed = false` parameter scalar.
The matching is over compiler-issued storage identities and retains declaration
provenance for every unknown. A coordinate absent from the supported incidence
graph or left unmatched rejects construction at its declaration with the exact
source-row, usable-row, component-row, and unknown counts plus the reasons rows
were unusable. A `start` value with effective `fixed = false` becomes a
numerical root-selection guess only after this ownership proof; it never fills
a missing equation. No partial projection or guess-retention fallback is
representable in executable Solve IR.

Each non-input `m` has exactly one B.1c definition owner. A source
`when`/`elsewhen` chain becomes one atomic, source-priority-ordered conditional
definition of the branch target set; independent `when` owners cannot define
the same target, as required by SPEC_0022 EQN-020. Event-only updates of `m`
are therefore B.1c definitions, not a second generic event-action assignment
path.

The B.1c topology includes every current-`m` dependency reachable through the
value, branch guard, trigger condition, condition DAG, and relation
expression. `pre(m)` is a dependency leaf. The producer supplies stable
topological owner order, and the linear construction capability independently
checks that every reachable current-`m` dependency has already been issued in
that owner group before consuming the target. No exclusivity claim, event
iteration, or final graph scan repairs an invalid order.

Structured families own compact domains, checked bodies, typed scalar views,
and constructor-derived row counts; `rumoca-eval-dae` owns evaluation/lazy projection.
Structured B.1c families additionally own checked target projections and typed
value bodies. Their constructor proves that every target is a non-input `m`,
that target/value scalar types and domain/view shapes agree, and that the
compact projection covers each owned target exactly once. Their topology and
clock obligations use the existing B.1c and clock capabilities; no parallel
domain, clock, scalar-row, or policy table exists.

### Conditions, Events, Clocks, and Temporal State

Condition, event, clock, and temporal-state ownership MUST satisfy every row in
[SPEC_0043 §12](SPEC_0043_CONSTRUCTION_CATALOG.md#12-condition-event-clock-and-temporal-catalog),
normative by reference.

Only continuously monitored closed activations receive roots. Clock-domain
environments filter typed capabilities without owning expressions. Delay
construction proves primitive shape, scalar-Real timing, and
`0 < delayTime <= delayMax` where applicable; Boolean claims/text-derived
runtime identity are prohibited.

### Transformations

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Passes consume/return immutable `Dae`; multi-object changes use closed operations | Structural/DAE API | Atomic changes |
| Operations are aggregate-bound, consuming, non-cloneable | DAE API | No replay |
| Inputs are closure-scoped to one DAE | DAE API | Lifetime ownership |
| Changed expressions use add operations | DAE API | Preserve checks |
| Repartitioning preserves provenance | Variable transform | Stable source |
| Structured owners cannot disappear silently | Structural transforms | Explicit loss |
| Mutable partition callbacks are prohibited | Public DAE API | No bypass |
| Changed contracts use named stage types | Phase boundaries | Visible semantics |
| Backend projections are immutable views | Compile/codegen | No mutation |

A transformation context is a lightweight replacement-aggregate capability,
not another IR. It may ownership-transfer unchanged immutable arenas; changed
objects use initial construction's checked adds. Persistent root seals,
change/receipt registries, generation tokens, and post-rewrite validation are prohibited.

### Serialization

Only the current wire identified by `rumoca_ir_dae::DAE_SCHEMA_VERSION` exists;
older/pre-versioned payloads, adapters, migration readers, and dual writes are
prohibited. The code constant is the single version authority.

Every current-wire storage/replay rule in
[SPEC_0043 §10](SPEC_0043_CONSTRUCTION_CATALOG.md#10-current-wire-replay-catalog)
is normative by reference from this section.

Root `Deserialize` decodes private current-version records through checked
construction; invariant children have no fieldwise `Deserialize`.

### Other IR Boundaries

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Parse preserves recovered syntax | Parse AST | Diagnostics |
| Recovery-shaped enums in `rumoca-core` and `rumoca-ir-*` do not derive or implement `Default`; there are no exceptions | Core and IR crates | Construction cannot invent semantic or source syntax |
| Semantic construction inputs, executable-semantics profiles, semantic stage aggregates, successful checked roots, and closed proof/plan carriers do not derive or implement `Default`; every policy selection enters its semantic phase as an explicit constructor argument, while intentionally empty structure uses an explicitly named constructor. SPEC_0043 names the exact current compiler-enforced frontier and does not claim that its handwritten list proves the category exhaustive. Every newly introduced public type is classified in the same change and, when it belongs to this category, added to that exact compiler assertion. Source candidate discovery is a drift backstop only. Value-preserving presentation/configuration records are not covered by this row | Semantic phase boundaries and checked IR owners | Generic construction cannot silently choose executable meaning or mint success, while an exact enforcement list cannot masquerade as whole-category proof |
| `GalecOptions::new` requires the value-affecting `AlgorithmCodeArithmeticProfile`; Algorithm Code construction has no representation-policy input and always retains the structurally reviewable form | GALEC calling boundary | DAE-to-GALEC lowering cannot reshape the auditor-facing reference; value-preserving representation optimization belongs only to checked Solve refinement |
| `StrictCompilation` issues the sole SPEC_0008 artifact stem and artifact-identity model frames from its resolved qualified model identity; consumers cannot supply or derive either | Target construction; SPEC_0043 §5 | Model identity cannot drift |
| Recovery-only nodes persist for diagnostics; no success proof is minted over them | AST phase boundary | Partial trees cannot forge proofs |
| Consuming rewrites never install a sentinel placeholder | AST rewrite | No transient invalid tree |
| Success returns opaque proofs | AST phases | Completed proof |
| Partial work gets no proof | AST phases | No forgery |
| `ParsedTree → ResolvedTree` has one mint | Resolve phase | One authority |
| Raw `&ClassTree → InstanceOverlay` remains the explicit Instantiate migration boundary | Instantiate phase | No unlanded proof claim |
| `ResolvedTree + raw InstanceOverlay → TypedInstancedTree` has one mint | Typecheck phase | Current closed Typecheck authority |
| `TypedInstancedTree → flat::Model` consumes by value and has one mint | Flatten phase | Closed proof chain |
| Proof fields/constructors are private; no `DerefMut`/mutable overlay | Owning phase | No forgery |
| Invariant fields private | `flat::Model`/`Dae`/`SolveProblem` | No bypass |
| Public root `validate()` prohibited | `flat::Model`/`Dae`/`SolveProblem` | Construction proves |
| Unchecked builders prohibited | `flat::Model`/`Dae`/`SolveProblem` | No weaker value |
| Sparsity patterns derived, not claimed | Solve construction | No unsafe under-approximation |

Only `TypedInstancedTree` enters flattening; consuming its proof transfers the
unique phase capability. Immutable payload sharing cannot forge or mutate it:
a `Clone`-able read-only projection may share the proof's immutable payload
for caching and diagnostics, but no projection is accepted in a proof input
position and no unchecked/direct conversion adopts one as a proof. A caller
can clone the raw overlay view and rerun the sole Typecheck mint, producing a
fresh checked proof rather than recovering the original. The Typecheck mint also
retains an opaque read-only projection of the exact `ResolvedTree` root it
checked. Flatten accepts no caller-selected tree and obtains its sole tree view
from the consumed proof, so a proof minted over root A cannot flatten root B.
This guarantee does not yet bind the raw `InstanceOverlay` to its originating
Resolve root: an overlay derived from tree A can still be presented with
`ResolvedTree` B at the Typecheck mint. That CE-3 pairing remains explicit
migration debt until Instantiate publishes a root-stamped proof.

The standalone resolved-tree typecheck entry is a diagnostics query, not a
mint: it returns checked data plus diagnostics, and no downstream phase
accepts its output as phase evidence.

Compile-time evaluation selects called functions only by Resolve-issued
identity. The sole delimited exception is the pre-identity structural
category: modifier and binding expressions that Resolve does not annotate,
evaluated under an evaluation environment whose category is an explicit
constructor selection, never a default. In an identity-requiring environment
an identity-free call reference selects nothing and folds nothing.

### Refinement Obligations

Each phase defines a deterministic relation `R_phase(input, output)`. Opaque
proofs enforce order; the following obligations establish semantic correctness:

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Isolate deterministic transitions from I/O/diagnostics | Phase algorithms | Proof-ready relation |
| Preserve semantic identity, ordering, provenance, and supported behavior | `R_phase` | Refinement |
| Fail unsupported input at its first owner with typed provenance | `R_phase` | No false success |
| Use explicit producer-owned cross-stage ID maps | Phase transition | No ordinal assumptions |
| Every finalized DAE variable, including a standalone synthetic fixture, carries one mandatory nonzero root-relative source occurrence; unset and duplicate occurrences fail DAE construction, so no `Outstanding`, optional, default, or fallback identity is nameable in the finalized root. The carrier survives structural replay and enters the Solve catalog separately from presentation data. The private correlated lowering product retains the exact prepared DAE beside the Solve root, and one independent exhaustive checker consumes the producer's explicit occurrence-to-catalog mapping before proof admission. The checker accepts only eagerly materialized, closed name-free fact arrays from those two roots and consumes construction-issued occurrence uniqueness (SOLVE-C60), independently proving ordered total coverage, role, causality, effective Modelica `fixed`, the Solve state-initialization class independently derived from DAE role and fixity, Solve variability independently derived from DAE variability/tunability, exact tunability, value kind, exact compact dimensions/scalar count, and storage association. The Solve projection exposes only the exact role and value-kind fields consumed by this relation, not the whole declaration or its equation-derived time domain; neither projection contains display names, scalar labels, provenance, spans, start/runtime values, or root access. The private non-Clone/non-Default/non-wire receipt is a mandatory field recording successful checking of this live construction; the architecture gate admits only the exact checker as a crate-visible mint, globally permits exactly one receipt struct literal inside that checker, forbids receipt aliases, and proves the propagated receipt binding is unique, immutable, unshadowed, unrebound, and installed directly by the sole `LoweredSolveModel` literal. The receipt does not cover start/runtime values or equation/program semantics and does not close AS-043's cross-root or persisted-identity debt | Flat-to-DAE construction, structural replay, DAE-to-Solve lowering, and proof admission | Missing identity is unrepresentable after DAE construction and the bounded live-product proof cannot certify a same-named, reordered, causality-changed, fixity-changed, state-initialization-changed, variability-changed, tunability-changed, or ordinal-paired foreign coordinate while avoiding a false claim that root-global identity is already solved |

Runtime proof capabilities are erased and add no serialized receipts or
duplicate IR. Compile-fail tests cover proof forgery/cross-stage use; property,
differential, and refinement tests exercise each relation.

Architecture gates prove either a Rust-level impossibility or coverage of a
review catalog; no gate is evidence that a constructed value is semantically
valid. For protected roots, the compiler and compile-fail API tests carry
privacy, trait, brand, sealing, and affine-use claims. Syntax-aware scans only
enumerate candidate types and public construction/mutation/check routes for
classification; they do not infer whether a route is checked.

### Enforcement

The evidence each guarantee requires is
[SPEC_0043 §5](SPEC_0043_CONSTRUCTION_CATALOG.md#5-enforcement-evidence-catalog).
Tests may privately audit the complete aggregate. Production audits, public
validation, superseded fallbacks, and compatibility are prohibited.

### Backend Capability Restoration

Capability deletion is not completion.

Structural, event, clock, temporal, algorithm, aggregate,
tensor, external-call, symbolic-export, FMI 2/3 ME/CS, eFMI, native, and Wasm
capabilities consume checked `Dae`/`SolveProblem` facts and retain equivalent end-to-end
evidence. Missing lowering fails at its first owner. Pending targets stay
undiscoverable; no stubs, alternate semantic paths, target aliases, silent
defaults, old-shape adapters, or compatibility readers.
The checked FMI projection follows
[SPEC_0043 §8](SPEC_0043_CONSTRUCTION_CATALOG.md#8-fmi-component-construction-catalog).

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — stage contracts
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — complexity and deterministic storage
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS contract index
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate and rewrite ownership
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md) — structured families
- [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md) — sound dependency patterns
- [SPEC_0043](SPEC_0043_CONSTRUCTION_CATALOG.md) — construction catalogs
- [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) —
  formal-verification architecture
- [MLS Appendix B](https://specification.modelica.org/maint/3.6/modelica-dae-representation.html)
