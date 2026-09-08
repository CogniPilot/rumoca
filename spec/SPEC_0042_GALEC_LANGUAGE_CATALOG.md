# SPEC_0042: GALEC Language and Decision Catalog

## Status
REFERENCE

## Summary

Lookup catalog of the GALEC language traps and the resolved eFMI export
decisions referenced by [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md).

## How To Use This Catalog

This annex defines no governing requirement. Each row supplies exact language,
decision, construction, or evidence details for the affirmative SPEC_0034 rule
identified by its GAL ID. Rows are normative only through that parent link;
they introduce no additional requirement. Changing a requirement amends
SPEC_0034. Cite rows by their stable catalog ID.

## Specification

### 1. Resolved Decisions (SPEC_0034 Phase 1 gates)

| # | Decision | Resolution |
|---|----------|------------|
| D1 | `.alg` text ownership | MiniJinja over the checked GALEC semantic view (GAL-009). |
| D2 | C text ownership | MiniJinja over the checked `SolveAlgorithmBlock` executable view and Algorithm Code correlations (GAL-008/GAL-038). Decided, not yet implemented (pending: 2026-08-08 plan, M3-4). |
| D3 | eFMI package ownership | MiniJinja and `target.toml` over closed target-neutral semantic views. Manifest contexts name only owning IR crates (`galec` and `solve` for eFMI); each file's closed `view` names the actual `AlgorithmCodePackage` or `SolveAlgorithmBlock` type, and construction admits that pair only as read-only views of one `SolveAlgorithmProduct`, never separately supplied roots. There is no open `serde_json::Value` or Rust eFMI schema/context type. Templates own XML hierarchy, constants, references, filenames, and C mapping. Generic declared commands compute checksums, validate schemas, and assemble artifacts from rendered bytes. `rumoca-efmi` dissolves. |
| D4 | Provenance shape | Auxiliary artifact beside DAE with an equation-correspondence map (GAL-003); never "algorithms present ⇒ ignore f_z/f_m". |
| D5 | Manifest `renderer` extension | Rejected: covered by D1. |
| D6 | Clock strictness | XSD-strict (GAL-016): `constant`, seconds; Beta-1's `tunableParameter` examples are nonconforming. |
| D7 | Beta-1 grammar gaps | AST adopts `(min=,max=)`, the error-signal statement, input/output prefixes; emitter rejects `//` comments and unsigned exponents. |
| D8 | Runtime signal scope | Full signal machinery in AST + validator; typed escape-set construction accounts for NAN from Real relational and equality operators (T9). |
| D9 | Production-Code sequencing | GAL-024/GAL-043: no standalone GALEC-derived C product exists. One `SolveAlgorithmProduct` retains its Algorithm Code package and correlated Solve executable view before AC and PC artifacts render into the same eFMI product. |
| D10 | XSD vendoring | Asset trees owned and named by the eFMI target directories; builtin discovery embeds arbitrary declared target assets recursively, while external targets resolve them relative to their own directory (GAL-008/GAL-023). |
| D11 | GALEC AST source spans | GALEC AST nodes carry `rumoca_core::Span` (the *foundation* crate, not an IR stage — GAL-001/GAL-010 intent holds). Parsed nodes span `.alg` bytes; generated nodes require typed source/generated provenance and the nearest responsible Modelica span. Production `Span::DUMMY` is prohibited. Spans are provenance, not identity (round-trip equality is span-insensitive). |
| D12 | C working-memory ownership | Current headers expose caller-owned persistent scratch. **Pending Solve cutover:** construction classifies working storage as instance-persistent or invocation-automatic. Persistent bytes belong to the caller-allocated block instance. Each public-entry invocation owns a bounded, aligned, non-escaping automatic arena disjoint from concurrent invocations. Headers report persistent bytes separately from the maximum automatic bytes/alignment per public entry. Neither class is GALEC LogicalData. |

### 2. Language Traps (T1–T14)

Each trap records an eFMI Standard 1.0.0 Beta 1 language property that differs
from Modelica, plus the consequence it imposes on the emitter. The consequence
column is the obligation; SPEC_0034 GAL-005/GAL-015/GAL-019/GAL-028 own the
governing rules.

| # | Trap | Emitter consequence |
|---|------|---------------------|
| T1 | Methods are parameter-free; I/O via `self.*` | Never emit parameters on block interface methods |
| T2 | `previous()`/`derivative()` are NOT operators — only quoted-name conventions for materialized state | `pre(x)` lowers to `'previous(x)'` state assigned at END of DoStep; `firstTick` for first-sample behavior |
| T3 | min/max ranges saturate (implicit `limit self` at method entry/return; Startup: return only); no assertions exist | Opposite of Modelica semantics; `limit` leaves qNaN as qNaN |
| T4 | Unary minus binds only to references: `a := -b^2` means `(-b)^2`; `-(b^2)`, `-f(x)` don't parse | Template renders the checked semantic rewrite as `0.0 - (expr)` or `(-1.0)*(expr)` |
| T5 | No implicit Integer↔Real promotion; equal-typed operands; `/` Real-only; no `%`; `^` returns Real | Insert explicit `real()`/`integer()` casts during lowering |
| T6 | Cross-precedence-class mixes need explicit parentheses (`a^2*b` invalid); evaluation order normative (no re-association, even `a+b+c`) | Parenthesize every cross-class mix; preserve order end-to-end |
| T7 | Real literals: decimal places and exponent sign mandatory (`1.0e+5` ✓; `1e5`, `1.`, `.5`, `1.0e5` ✗); `//` comments invalid | Strict literal formatter |
| T8 | `absolute`/`ln`/`lg`/`roundDown`/`roundUp`/`imin`/`imax`, not `abs`/`log`/`log10`/`floor`/`ceil`/Integer min-max; `min`/`max` 2-arg scalar Real only; no array reductions; `atan2(y,x)`; `sign` returns Real; GALEC `integer()` truncates toward zero and can signal, so MLS floor conversion lowers as `integer(roundDown(x))`; `mod` → reserved `remainderDown`, not callable in Beta-1 | §3.2.6 mapping table is normative; unlowerable ops get stable `unsupported-feature` diagnostics |
| T9 | Relational operators on qNaN signal NAN and return false — comparisons are side-effecting | Escape sets account NAN for any Real comparison (slice 2, D8) |
| T10 | Signal checks catch (unset before the branch); `signals` clause must EXACTLY equal the computed escape set; testing an unsettable/caught signal is a compile error | Signal dataflow mandatory in the validator |
| T11 | No `while`/`break`/`return`/`String`/enumerations (reserved words only); loop bounds, dims, subscripts statically evaluable | No runtime-computed subscripts |
| T12 | `not` requires parenthesized argument; if-expressions self-parenthesized, mandatory `else`, no stateful calls inside; a stateful call has no sibling calls/state-refs in its expression | Checked construction + template rules |
| T13 | ASCII-letter-first identifiers, not keywords/reserved, no `__` prefix, no builtin/Appendix C collisions; quoted identifiers `'a.b[2].c'` (literal positive indices, no whitespace) are the traceability device | Prefer quoted identifiers (GAL-015) |
| T14 | Division by zero / Real overflow silently IEEE-754 (±inf); only `integer()` and the three linear-solver builtins signal | Don't invent error checks; don't omit the four that signal |

### 3. Rule Rationale (SPEC_0034 GAL-016, GAL-024)

**GAL-016.** GALEC has no `previous()`/`sample()` (T2); `pre(x)` becomes
protected state `'previous(x)'` committed at end of DoStep; the sample period is a
`constant` (seconds) named by the XSD-strict manifest `<Clock>` (§3.1.2).

**GAL-024.** The manifest LogicalData mapping (every interface variable and
BlockMethod), not C naming, is the conformance surface (ch. 5); PC-only is
non-conformant (§2.2).

### 4. Rule Rationale (SPEC_0034 GAL-040, the error-signal accumulation contract)

Real comparisons signal (T9), so a guard is not a pure test: it writes
`ErrorSignalStatus`. Any optimization that changes how often a guard runs is
therefore changing an eFMI-visible output unless something says otherwise.
GAL-040 is that something. It is a contract on the emitted artifact, not an
observation about today's templates, and each clause is mechanically enforced.

1. **Reset.** `ErrorSignalStatus` is assigned `0` exactly once, at the top of
   `Startup`, `Recalibrate` and `DoStep`, before any statement of that method.
   The word a consumer reads after a method returns records that invocation
   alone.
2. **Accumulation.** Every other write is `status |= <constant mask>`: the six
   predefined bits (§3.2.5 §1.6) OR-ed in by the emitted `signal` statement, by
   the Real comparison kernels, and by `integer`. `|=` with a constant mask is
   idempotent (`x | m | m == x | m`) and commutative (`x | a | b == x | b | a`),
   so within one method the final word depends on the SET of raising evaluations
   that ran, never on their count or their order.
3. **No read.** No emitted construct reads the word. `limit` saturation raises
   nothing and reads nothing.
4. **The one construct that breaks it.** A signal check (`if signal …`)
   *catches*: it clears the bits it tests (T10) and branches on them. That is
   both a read and a non-monotone write, so clauses 2 and 3 hold only between a
   method's reset and its first signal check. The Modelica → GALEC projection
   emits no signal check, so every rumoca-generated artifact is one region; a
   hand-written `.alg` need not be, which is why the permission is a proof
   obligation rather than a global assumption.

**Permitted rewrites.** Inside one region, and only there: evaluate a construct
fewer times than the source form does, or at a different point in the region,
provided it is still evaluated on exactly the executions where the source form
evaluated it at least once, and provided the bits it raises are the same on each
of those evaluations. Concretely this licenses hoisting a loop-invariant guard
out of a loop whose trip count is *proved* to be at least one. It does not
license hoisting out of a possibly-empty loop, out of a conditional, or across a
signal check: each can make a raise reachable that the source form never
reaches, which changes the word.

**Enforcement.** `rumoca_ir_galec::signal_effect` classifies every expression,
condition and statement as `Inert`, `AccumulateOr`, `Consume` or `Opaque` over
exhaustive matches, so a construct added to the AST cannot be silently treated as
safe; `RepeatableSignalEffect` is a branded token with a private constructor,
mintable only from a repeatable classification. An optimizer holds that token
plus its own invariance and non-empty-range proofs before it may rewrite; there
is no boolean predicate to bypass. Clauses 1 to 3 are claims about emitted C,
which no Rust type constrains, so a test scans both the C template source and
rendered C and accepts only a reset, an OR with a decimal-literal mask, a
declaration, or an address handed to a helper the same scan covers.

### 5. Variable Classification and Checked Construction Scope

**Variable classification (SPEC_0034 GAL-020, normative).**

| Modelica (DAE) | GALEC declaration position | Manifest `blockCausality` |
|----------------|---------------------------|---------------------------|
| input | `input` before `protected` | `input` |
| output | `output` before `protected` | `output` |
| independent parameter | `parameter` before `protected` | `tunableParameter` |
| parameter-derived value | `parameter` after `protected` | `dependentParameter` |
| true constant | `constant` | `constant` |
| discrete state / pre-value | plain declaration (protected) | `state`, `start` mirroring Startup |

XSD enum `dependentParameter` (not `calculatedParameter`); manifest `start` is
row-major with scalar broadcast; method-local variables are unlisted;
structurally-parametric array sizes are rejected. GALEC `.alg` declarations do
not encode the manifest-bound start relation. A parsed checked block therefore
retains the distinct `NotRepresentedInSyntax` source-format fact; it is not the
construction-level `NotApplicable` disposition, cannot satisfy an exact-start
obligation, and can never authorize a package. Generated-package construction
must prove the exact start relation for every GAL-020-mandated declaration and
must never default or fabricate it.

**Checked construction scope (`rumoca-ir-galec`, per §3.2.2).**

| Analysis | Checks |
|----------|--------|
| Name | constructors reject keyword/reserved/`__`/builtin/Appendix C collisions and malformed quoted names |
| Type/shape | expressions carry exact type/extents; `/` is Real-only; `^`→Real; no implicit promotion; `else` mandatory |
| Static domain | dimensions, subscripts, and loop bounds carry checked constant-Integer proofs |
| Calls/effects | branded function IDs make unresolved/recursive calls impossible; body capabilities restrict writes and stateful calls |
| Signals | construction derives §3.2.5 escape sets, including NAN from Real comparisons; only settable signals testable; ≤16 user signals; method escape ⊆ predefined 6 |

### 6. Algorithm Code Package Identity (GAL-041)

This is a **pending, unimplemented design catalog** bound by DRAFT SPEC_0034
GAL-041. It changes no earned conformance claim.

Every subject owns one package-branded identity, exact parent/child role,
semantic edges, and typed D11 provenance. Provenance is `Exact`,
`NearestStatement`, or `Generated`, identifies the subject/origin, and carries
the responsible non-dummy span. A diagnostic must render
`NearestStatement(span)` as “in the statement at …”, never expression-exact.

| Subject | Required semantic edges |
|---|---|
| Block root | Ordered state compartments, block declarations, lifecycle methods, and user functions |
| State compartment | Exact lexical block owner, declared compartment identity, and ordered member declarations |
| Declaration | Lexical owner, classification, scalar type, dimensions, access, and initialization owner. Input, Output, TunableParameter, DependentParameter, Constant, PersistentState, and the corresponding compartment-member declaration classes additionally retain the exact `DeclarationStart` expression subject whose scalar type and fixed shape equal the declaration. Method locals, function inputs/outputs/locals, call-result placeholders, and other dummy values have no GALEC-mandated start and retain an explicit not-applicable disposition instead of a fabricated expression |
| Lifecycle method | Exact Startup/Recalibrate/DoStep kind, ordered locals/actions/signals, escape set |
| User function | Exact function kind, lexical/package owner, ordered parameter/input/output/local declarations, ordered actions, exact result declarations and result ABI, status/effect/signal facts, and its identity in the retained GAL-017 call-graph receipt |
| Ordered action | Exact statement variant and ordered expression/call/effect/branch/loop/action children |
| Branch | Owning conditional, condition, arm role/index, ordered body; `else` is explicit |
| Loop binder | Owning loop, Integer type, exact finite lower/upper/step domain, ordered body |
| Expression | Exact value type/shape, variant/operator, ordered operands/references/calls/projections |
| Reference part | Resolved declaration or binder, preceding part, ordered subscripts/projection |
| Call | Exact executable owner/use; closed branded target as user function or typed builtin; ordered argument subjects; exact result projections, order, cardinality, type, and shape; status/effect facts; and exact occurrence path |
| Call-result projection | Owning call occurrence, exact issued callee result or builtin result edge, exact receiving projection/destination subject, and result type/shape. Its package-branded identity is the invocation/result-projection identity; an ordinal is retained only as checked order and can never reconstruct that identity |
| Limit target | `SelfState` expands the checked ranged-state set; `Reference` retains its exact resolved target, type, and range |
| Signal/effect | Exact signal kind/mask, owning region, escape/consume relationship |
| Aggregate projection | Source identity, projection kind, compact domain, result shape, coordinate convention |

Successful GAL-017 closure retains exactly one nonconstructible
`AlgorithmCodeAcyclicCallGraphReceipt` in the `AlgorithmCodePackage`. Its
occurrence edges are exactly all user-call `Call` subjects; each cites the
exact call, caller lifecycle method or user function, callee user function,
and occurrence path. The same cycle/reachability analysis issues the complete
callee-before-caller user-function order together with the
Startup-builtins-only and DoStep-reachability facts. A consumer may iterate
only the receipt-issued order and paths and may retain only a
package-correlated nonconstructible receipt projection; it MUST NOT reconstruct
a graph or traverse raw subjects to re-derive order, reachability, or the call
census, infer any of those facts from names, AST/source order, spans, or
ordinals, or issue a second proof.

An identity is lent only with its typed borrowed subject view and retained
facts. A detached metadata iterator that cannot project an identity to that
subject does not satisfy GAL-041.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Whole-root construction installs each subject together with identity, parent/role, facts, and provenance | Package construction | No partial subject exists |
| Closed subject/child enums and AST structs are matched exhaustively without catch-all field patterns | Package construction | New syntax fails compilation until covered |
| Successful resolution, type, dimension, call, effect, and signal facts are retained from their sole check | `rumoca-ir-galec` construction | Consumers do not repeat semantics |
| Callable subject installation consumes one exact DAE-origin auxiliary callable plan. Every plan owner, interface, structured region, compact operation, call occurrence, result projection, effect, provenance, and receipt edge maps exactly once to its issued Algorithm Code subject/correlation or rejects before package exposure. Successful closure moves the semantics into one package-branded non-wire plan owner and consumes the DAE-plan receipt into the sole package GAL-017 receipt, which the retained plan cites but cannot duplicate or reissue; it never parses or re-lowers emitted AST syntax and no plan fact is optional | `rumoca-phase-galec` + package construction | The package retains callable meaning instead of reconstructing it in Solve |
| Every declaration class with a GALEC-mandated start issues one exact declaration/start relation during construction; the relation cites the exact start-expression subject and retains the equal scalar type and fixed shape. A missing start or unequal type/rank/extent rejects before package exposure. Classes without a mandated start issue only the closed not-applicable disposition and cannot invent, default, or fabricate a start | `rumoca-ir-galec` construction | A manifest or stronger refinement can consume the checked start once without truncating a tensor, broadcasting a scalar, or assigning initialization semantics to method-local/dummy values |
| Root close proves every AST subject installed exactly once and every issued parent/child capability consumed | Package construction | Missing and duplicate coverage cannot escape |
| Each relation is checked when created; close checks structural capability exhaustion, not semantics again | Package construction | Construction replaces revalidation |
| Capability exhaustion is proved by per-family outstanding counters maintained at issuance and consumption; close performs no whole-arena scan and no semantic recheck | Private package builder | Closure is a constant-size proof over construction state, not another validator |
| `Pending`, `Proven`, and `Rejected(DiagnosticReceipt)` relation slots exist only in the private builder; successful package types carry no pending, unknown, or optional-shape state | Builder/final package boundary | An incomplete or diagnostically rejected relation cannot inhabit a successful package |
| The checked AST is totally immutable after construction; no mutable reference or interior-mutation surface exists | Checked carrier + architecture test | Private locators stay valid |
| Stored typed subject locators are private, construction-issued, non-serialized, and never exposed; lent IDs/subject views are invariantly branded | Checked carrier | Foreign locators and IDs are unconstructible |
| Package identities are generative and unrecoverable from names, spans, hashes, or ordinals | `rumoca-ir-galec::package` | Presentation is not identity |
| Clone/wire replay invokes the same whole-root constructor and reissues the private index | Checked carrier | IDs never cross roots or bytes |
| Every target-strong operation correlation is a construction-issued typed relation from one exact Algorithm Code operation occurrence and result subject to one independently constructed stronger candidate. It retains the exact issued operand/result subjects and checked type, shape, compact-domain, layout, arithmetic, and effect facts required by that candidate; correlation is injective where the candidate claims one source occurrence and cannot be reconstructed from syntax, names, spans, hashes, or ordinals. Stronger `Identity`, `ProjectSlice`, `Concatenate`, `MatrixMultiply`, and future candidates construct separately and cite this relation and its issued subjects | Correlation construction | Inventory alone proves no optimization, and a target candidate cannot attach to a same-shaped foreign occurrence |
| A value-AFFECTING operation choice is fixed by an explicit, Default-free package-construction profile and enters package identity. Every manifest whose required product carries Algorithm Code supplies that profile through required `[arithmetic]`; omission rejects, the table rejects for every other product, and neither target names nor orchestration code select it. A later refinement may choose only value-PRESERVING representation; it consumes the package-issued relation and cannot override, default, or re-derive it. A relation/plan mismatch rejects refinement | Target/package construction + phase-solve | Signed-zero makes matrix-product seed choice observable, so codegen cannot own or reselect it |
| Every materialized Real contraction, including scalar, vector, and matrix layouts, belongs to the one `MatrixMultiply` occurrence family and carries one untrusted candidate into construction. Construction either issues its exact branded relation or rejects the package. Every expression or materialized lowering uses the same package-selected seed/order relation. `FirstProduct` on an empty inner domain rejects at that occurrence with its D11 provenance until empty GALEC storage is representable; `PositiveZero` remains the only empty-inner relation | Matrix-product projection and correlation construction | Row-only marking or a parallel expression fold would leave scalar seeding as a second authority |
| Phase-solve maps each lent subject once or fails before exposing its root | `rumoca-phase-solve` | Unsupported behavior fails at refinement |

| Evidence | Owner/Where | Brief Justification |
|---|---|---|
| Compile-fail tests reject identity construction, escape, and cross-package use | `rumoca-ir-galec` tests | Brands cannot be forged |
| Private fault-injection mutations omit, duplicate, reparent, or mis-provenance every subject family, including each call-result projection | Package tests | Coverage failures are exercised without a public invalid constructor |
| Insertion mutations attempt duplicate, reparented, and foreign capabilities; the named builtin-resolution orphan mutation rejects after result preparation and proves arena lengths, facts, and counters unchanged | Private package-builder tests | A failed insertion leaves no residue or canceled work |
| Resolution mutations vary same-spelled declarations, callees, types, shapes, and effects | Package tests | Edges are semantic, not textual |
| Declaration/start mutations remove a required start, exchange scalar and array starts, change rank or one extent, or attach/fabricate a start for a no-start declaration class; renderer regressions preserve every row-major tensor element and forbid indexing/iterator fallback to a first value | Package and Algorithm Code presentation tests | Start identity, type, fixed shape, and applicability are construction facts rather than renderer guesses |
| Correlation mutations omit, duplicate, reattach, or change one operand, result, type, shape, compact domain, layout, arithmetic contract, or effect fact of every target-strong operation correlation | Package tests | A plausible same-shaped or same-spelled candidate cannot acquire foreign authority |
| Named matrix-product edge oracles distinguish `[-0.0] * [+1.0]` under both admitted seed relations, exercise finite and non-finite values and floating-point status where observable, reject an empty `FirstProduct` occurrence with its exact span, and reject package metadata that claims a relation different from the carried legalization | Projection, evaluator, and Production C tests | Equal ordinary finite outputs cannot hide a seed mismatch |
| Architecture mutations add mutable AST access or interior mutation and must fail | Repository architecture tests | Locator validity depends on immutability |
| Phase-solve mutations omit or duplicate every mapped family | `rumoca-phase-solve` tests | Refinement close is exhaustive |


### 7. Conformance Ladder

| Claim | Machine-checked requirement | Status |
|---|---|---|
| GALEC-derived text export | `.alg` + `manifest.xml`; honest self-description | Earned (`galec`) |
| eFMI Algorithm Code export | Schema-valid eFMU with exact checksums, identifiers, and UTC timestamps | Earned (`galec`) |
| GALEC language conformance | Prior rung plus idempotent render/parse/render | Earned (`galec`) |
| eFMI Production Code export | Schema-valid correlated AC/PC, complete mapping/checksum web, Solve-only C/H, toolchain and refinement receipts | Suspended; readiness-zero scalar `efmu` is integration evidence only |

The package evaluator and Binary32/Binary64 lexical mappings have executed
integration witnesses. Those witnesses do not supply the missing Production
Code toolchain or refinement receipts and therefore do not change the ladder.

### 8. Testing Evidence Catalog

Every row is normative by reference from the cited SPEC_0034 rule.

| Test | Enforces |
|---|---|
| Independent discrete golden `.alg`; negative continuous/event/external/dynamic-clock fixtures; public gate reachability | GAL-004/007/016/023/025 |
| Accept/lower/render builtin parity; reserved-name and quoted-id round trip; typed failure without default | GAL-005/007/015 |
| Lifecycle shape, complete Startup, manifest-bound starts, and empty Recalibrate | GAL-017 |
| Full container schema/checksum/identity validation and malformed negatives | GAL-021 |
| CLI smoke and real template-CI render | GAL-011/012 |
| Generated C compiles only from checked correlated Solve roots | GAL-008/012/024/043 |
| Exact IR/context/view enumeration; invalid formats/products/root mixtures/registries/borrows reject | GAL-043/045 |
| Complete Algorithm-Code role layout; malformed families reject; source-only generic rendering cannot access package paths | GAL-043/046 |
| Source, GALEC evaluator, Solve evaluator, and generated C compare every lifecycle-visible value/effect/failure | GAL-027/038 |
| Correlated aggregate operations reject malformed facts and compare every in/out-of-range case | GAL-026/033 |
| Lexical scopes, sanitizers, tensor/RDD2 stack budgets, reaching definitions, alias/call-transfer mutations | GAL-026/030/034/035; SOLVE-C59 |
| Record calls pin one-call identity/cardinality/temporary/source size; rank-1/2 layouts remain stable | GAL-026/030/035/036/037 |
| Integer boundaries, conversion, arithmetic, and unproved-overflow refusal | GAL-028 |
| C/H profile preflight, deterministic rerender, pinned MISRA analysis/deviations, reproducible evidence links | GAL-029/030/031/032 |
| Exactly three error-status resets, OR-only writes/no reads, effect-token refusal, and per-tick differential status | GAL-040 |

### 9. Artifact Construction Catalog

Every clause is normative by reference from GAL-043/GAL-046.

| Area | Required closed facts |
|---|---|
| Semantic contexts | Exactly `ast`, `flat`, `dae`, `galec`, `solve`, bijective with `rumoca-ir-*`; roots/products/formats/targets/proof plans are excluded |
| File view | One closed context plus optional real checked view; absence resolves explicitly to canonical root, never `Default`; admitted Solve views are `FmiComponent` and `SolveAlgorithmBlock` |
| Product pairing | One product derives from file plans; standalone block, mixed model/component, and uncorrelated roots reject; only package files plus their correlated block create `SolveAlgorithmProduct` |
| Formats | C/CUDA require Solve; Algorithm Code requires GALEC; GALEC admits Algorithm Code/XML only |
| Dispatch | No target-wide root/IR field, suffix inference, target-name dispatch, alias, fallback, independent root supply, or retired spelling |
| Role family | Exactly package manifest, Algorithm Code manifest, and source resolve into one sealed `AlgorithmCodeArtifactLayout` with singleton portable paths, case-collision checks, and target order |
| Rendering | Role-bound carriers have no caller path; output and manifest references derive from retained layout; generic rendering is source-only; templates do no path comparison/reconstruction/fallback/model interpolation |
| Origin | The DAE session snapshots its exact source map before one higher-ranked lowering callback and consumes one affine branded candidate; existing packages, replacement maps, and replacement model identity cannot enter |
| Closure | The sole production phase-galec call is load-bearing for map/block correspondence; missing, out-of-range, or non-UTF-8-boundary provenance rejects; closure retains exact ordered-map digest |

### 10. GAL Rule Catalog

Every row is normative by reference from SPEC_0034 §Rules and adds no
independent requirement.

| ID | Rule | Owner/Where | Why |
|----|------|-------------|-----|
| GAL-001 | GALEC is a selectable checked export IR, but is not a canonical AST/Flat/DAE/Solve peer in phase ordering, caches, or wire schemas. | SPEC_0007 pipeline | A template input is not a compiler stage. |
| GAL-002 | Projection returns a separate `AlgorithmCodePackage`; MUST NOT mutate canonical DAE, clear symbol tables, delete condition/event/clock metadata, or store snapshots. | `rumoca-phase-galec` | One DAE contract for all consumers. |
| GAL-003 | No GALEC-only data in `rumoca_ir_dae::Dae`; algorithm structure rides as auxiliary provenance beside DAE (D4) unless SPEC_0007 is amended first. | `rumoca-ir-dae` | Backend fields rot the canonical schema. |
| GAL-004 | Generic capability checks and pre-projection admissibility run on untouched canonical artifacts; checked construction closes the package after lowering; destructive preparation MUST NOT erase unsupported constructs before checks run. | `rumoca-phase-galec` | Prevents vacuous gates. |
| GAL-005 | Parity source of truth is the §3.2.6 builtin catalog: accepted constructs lower to semantic operations that templates render exactly; Appendix C names are rejected. | `rumoca-phase-galec` + `rumoca-ir-galec` | Gate/template drift emits nonexistent functions (T8). |
| GAL-006 | Generic capability validation always runs; GALEC admissibility is additive. Manifests select checked GALEC; construction completes before rendering. | `rumoca-compile` | No validator bypass or render-time lowering (SPEC_0029 §12). |
| GAL-007 | Unsupported features fail with stable `unsupported-feature:<feature_id>` diagnostics; errors are structured phase-local enums with stable codes and spans (SPEC_0008); no silent defaults. | `rumoca-phase-galec` | Fail early; CI-aggregatable. |
| GAL-008 | Templates own syntax and artifact policy. GALEC/Algorithm Code never authorizes or generates C/H. Production C/H receives only a closed checked `SolveAlgorithmBlock` view and makes no semantic choice. The current renderer is a readiness-zero scalar-only experiment. | target directories | SPEC_0029 §12. |
| GAL-009 | MiniJinja renders `.alg` from the checked semantic view. Rust exposes typed semantics and provenance; it MUST NOT print fragments. | `rumoca-phase-codegen` templates | Same boundary as every IR. |
| GAL-010 | IR owns checked Algorithm Code/correlations; parse owns `.alg` and private recovery; `rumoca-phase-callable` owns the sole DAE pure-function semantic walk into the auxiliary checked callable plan; phase-galec owns GALEC syntax/lifecycle projection plus total admissibility and correlation from that plan; codegen owns generic rendering and typed views; target directories own syntax/XML/package policy. Algorithm Code target policy owns `.alg` and AC metadata only; C/H policy is owned by Solve-derived targets. No compatibility facade or codegen eFMI subsystem exists. | workspace layout | Enforce ownership without a second callable semantic walker. |
| GAL-011 | GALEC output is selected via `--target galec`; `--emit` stays reserved for canonical IR inspection. `embedded-c-galec` and `galec-production` are permanently retired names with no aliases. The registered readiness-zero mixed-view deployment product is named `efmu`; it is never a standalone C target. | `rumoca` CLI | Keep Algorithm Code and executable C as distinct artifacts. |
| GAL-012 | Template CI renders GALEC `.alg`/AC metadata against a dedicated smoke fixture; skipped targets MUST NOT be marked covered. Every generated C/H compile check starts from a checked Solve-owned root and is tested independently of GALEC rendering (Testing Requirements). | template CI (xtask) | False coverage or an AC-to-C shortcut hides a broken product boundary. |
| GAL-013 | Generated C/H/object outputs MUST NOT be committed except as intentional, small, documented fixtures. | CI | Repository hygiene. |
| GAL-014 | The parser constructs a checked GALEC block only — never DAE/Solve, never Modelica input. Invalid documents use a private recoverable CST for diagnostics/navigation. | `rumoca-phase-parse-galec` | Keep syntax recovery out of checked IR. |
| GAL-015 | Checked names MUST be injective AND disjoint from keywords/reserved words/builtins/Appendix C names/`__` prefix space; quoted identifiers retain source identity. | `rumoca-ir-galec` + `rumoca-phase-galec` | Injectivity alone still emits illegal names (T13). |
| GAL-016 | Clocks derive from structured metadata, never heuristics. One static base period drives each block. Exact clock-lattice proofs may admit zero-phase integer multiples and emit bounded dividers preserving tick-zero/order; dynamic, shifted, incommensurate, or cyclic schedules fail. | `rumoca-phase-galec` | **Why** below. |
| GAL-017 | Block interface: exactly `Startup`/`Recalibrate`/`DoStep` (§3.1.3), stateful, parameter-free; I/O via `self.*`; Startup initializes ALL writable block variables, builtins only (control inputs read-only); Recalibrate emitted even when empty; all other functions reachable from DoStep; acyclic call graph. | `rumoca-ir-galec` construction | §3.1.3–3.1.4. |
| GAL-018 | Runtime error signaling is language machinery, not SPEC_0008 diagnostics: checked GALEC data models signals/checks/closures/`limit`; construction enforces §3.2.5 escape-set dataflow; package data carries per-method Signals + ErrorSignalStatus. | `rumoca-ir-galec` | Not SPEC_0008 diagnostics. |
| GAL-019 | Template conformance: parenthesize every cross-precedence-class mix using the typed `PrecedenceClass`/`Associativity` facts serialized on every operator (exhaustive at `BinaryOp`; templates compare the given values and only spell the parentheses, never re-classifying operators); no unary minus over non-references (T4); strict Real literal format; `/* */` comments only; mandatory `else`; parenthesized `not`; no re-association. | `rumoca-ir-galec` operator facts + GALEC target templates | T4–T7, T12; evaluation order is normative. |
| GAL-020 | Variables classify per the Variable Classification table; independent parameters never constant-folded; dependents recomputed in Recalibrate (inline in Startup). Every generated-package declaration whose class mandates initialization proves one exact manifest-bound `start`; no default or fabrication is permitted. Parsed `.alg` instead records the distinct `NotRepresentedInSyntax` source-format fact and can never authorize a package. Dimensions are literal integers ≥ 1. | `rumoca-phase-galec` + checked construction | §3.1.6 + repo policy. |
| GAL-021 | Claims follow machine-checked Conformance Ladder rungs. `target.toml` declares artifact/checksum graphs and schema gates; generic commands use exact bytes and CI recomputes from disk. No placeholder checksum; lower-rung targets self-describe honestly. | target directories + generic artifact commands | Wrong checksums invalidate eFMUs. |
| GAL-022 | Version pinning: profile string `efmi-1.0.0-beta-1`; container XSD `0.11.0` / AlgorithmCode `0.14.0` / ProductionCode `0.17.0`; `efmiVersion` fixed `"1.0.0"`. These are literals declared by the owning target's `target.toml` and templates, never Rust constants or context fields. | target directories | Beta-fixed constants change at 1.0.0 final. |
| GAL-023 | Vendored BSD-3-Clause Beta-1 XSDs live in target assets, retain LICENSE, and are copied by declared operations. CC-BY-SA standard text/grammar/examples are not copied beyond short attributed quotes; no endorsement is implied. | target directories | License terms. |
| GAL-024 | Each C track consumes the profile-bound `SolveAlgorithmBlock` and no second width. A checked prepared ABI maps Binary32 to `efmiFloat32`/32-bit and Binary64 to `efmiFloat64`/64-bit before rendering; templates only spell that retained mapping. `rumoca-eval-galec` consumes the originating package and rounds every Real operation according to its profile. Binary64 admissibility is grounded exactly in eFMI 1.0.0 Beta 1 `ProductionCode/efmiTargetTypes.xsd`: simple type `efmiTargetDataTypeKind` declares enumeration value `efmiFloat64` as a 64-bit floating-point data type. Conformance still requires the SPEC_0047 §4.31 toolchain and §4.32 refinement receipts; spelling, compilation, and execution alone do not supply them. Binary32 transcendental cross-arm relations are governed by SPEC_0047 §4.36: the package evaluator arm is pinned to the platform float routines and claims no general-input parity with native execution or C. | Solve lowering + checked preparation + templates | **Why** below. |
| GAL-025 | v1 scope rejections say "not yet supported by the Rumoca GALEC projection" — never "unsupported by eFMI". | `rumoca-phase-galec` | eFMI expects discretized models. |
| GAL-026 | Checked GALEC and Solve data are array-native; aggregate operations are first-class and templates only render them. | IR + lowering + templates | Preserve optimization. |
| GAL-027 | `rumoca-eval-galec` defines explicit semantics for checked blocks: statement order, method transitions, signals, escape sets, `limit`, NaN comparisons, and conversions. It returns typed failures and has no lowering/codegen dependency. | `rumoca-eval-galec` | Independent proof/differential oracle. |
| GAL-028 | Every target carrying Algorithm Code makes one required signed `[arithmetic].source_integer` selection. Package construction derives and retains its exact Integer domain; evaluator, Solve, preparation, and rendering consume that fact and accept no second domain. The obsolete per-target domain declaration is an unknown key and receives no migration read. Range proofs cover every emitted Integer literal and operation; unproved operations fail with provenance. Wrapping, saturation, guesses, host promotion, and signed-C overflow are prohibited. Dynamic-operation range evidence remains unimplemented: absent such a proof, Integer `Negate`, `Abs`, `Add`, `Subtract`, and `Multiply`, plus Real-to-Integer conversion, fail before typed-program commit. The current slice therefore does not claim general executable Integer arithmetic. | target config + package construction + proof view | Beta-1 leaves overflow undefined. |
| GAL-029 | C targets pin C99 and a named MISRA C:2023 assurance profile. Generated artifacts disclaim compliance until project planning, guideline classification/enforcement, deviations, and review records satisfy MISRA Compliance:2020. Tool passes alone are not compliance. | C templates + assurance profile | Prevent false claims. |
| GAL-030 | Generated C prohibits dynamic allocation, recursion, reserved identifiers, implicit narrowing, and function-like macros; loops/storage are bounded and helper arguments evaluate once. Checked construction excludes every possible C undefined behavior or export fails. | checked view + C templates | Reviewable subset. |
| GAL-031 | Pinned assurance gates cover every emitted C/H file and produce reproducible artifacts. Accepted diagnostics require narrow machine-readable deviations naming guideline, construct, scope, rationale, and verification; global suppressions are forbidden. | target CI + records | Repeatable evidence. |
| GAL-032 | Rumoca may claim only DO-178C project support. Evidence requires deterministic C, end-to-end traceability, requirements tests, target/tool/runtime assumptions and identities, structural-coverage inputs, and a DO-330 qualification versus independent-output-verification choice. Artifacts never claim compliance or a software level. | evidence bundle | Certification is project-level. |
| GAL-033 | Target-stronger operations and bounded selection are fully checked before rendering. | Solve construction | Preserve tensors through legalization. |
| GAL-034 | C storage follows checked lexical scopes and storage classes, never names or text. | Solve construction | Bound stack lifetimes. |
| GAL-035 | Whole-tensor storage, reaching definitions, and alias rules are checked call facts, never template inference. | GALEC → Solve refinement | Remove redundant copies. |
| GAL-036 | One aggregate source call becomes one action with exact result cardinality and semantic identities. | GALEC → Solve refinement | Preserve readable dataflow. |
| GAL-037 | C renders checked aggregate operations without constructing, scalarizing, fusing, or rescheduling them. | Solve view + templates | Bound source growth. |
| GAL-038 | Independent GALEC, Solve, and C execution are compared over all lifecycle-visible effects. | evaluator/codegen tests | Avoid self-confirming defects. |
| GAL-039 | Generated C working memory is caller-owned and never mutable file-scope. The readiness-zero scalar slice uses bounded invocation-automatic storage; broader instance-persistent, call, and tensor arena classification remains pending. Construction, never a template, owns the classification, escape, and concurrency facts. D12 owns reporting. | Solve construction + C templates | Make reentrancy and RAM admission construction facts. |
| GAL-040 | `ErrorSignalStatus` is reset once per method and otherwise changes only by checked monotone accumulation inside a signal-check-free region. Rewrite and enforcement clauses are [SPEC_0042 §4](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#4-rule-rationale-spec_0034-gal-040-the-error-signal-accumulation-contract). | checked signal effects + C templates | Preserve lifecycle-visible signal behavior. |
| GAL-041 | Package construction consumes the exact DAE-branded auxiliary callable plan while issuing the complete branded semantic-subject graph and typed provenance catalogued in [SPEC_0042 §6](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#6-algorithm-code-package-identity-gal-041). It proves one total exact plan/subject correlation and retains the same semantics as one package-branded non-wire callable-plan owner. Consuming the plan's acyclic receipt issues the sole package-retained GAL-017 receipt; the retained plan cites that receipt and cannot duplicate or reissue it. Missing, duplicate, foreign, unconsumed, or malformed plan/subject coverage exposes no package; consumers never reconstruct an edge or callable body from syntax. The retained plan is not target-selectable and never authorizes C/H. | `rumoca-ir-galec::package` | Make AC-to-Solve traceability and callable meaning construction-owned. |
| GAL-042 | The sole `rumoca-ir-solve` → `rumoca-ir-galec` edge authenticates `AlgorithmCodePackage` refinement. Phase-solve consumes one package into a non-cloneable product retaining it and its sealed block. Unsupported user functions, loops, calls, tensors, branches, effects, and unprepared ABI cases reject before rendering. | IR crates + `rumoca-phase-solve` | One package owns both representations. |
| GAL-043 | The closed semantic-context enum spells exactly `ast`, `flat`, `dae`, `galec`, and `solve`, in one-to-one correspondence, in both directions, with every `crates/rumoca-ir-*` crate; architecture CI enumerates both sets and proves exact equality. Each rendered file declares one member of a closed artifact-kind enum and one admissible semantic context. Roots, products, formats, target names, and proof-plan crates cannot enter the context vocabulary. An optional closed `view` names one actual checked type inside its declared IR crate; absence resolves by an explicit construction rule to its canonical root, never `Default`. The admitted noncanonical Solve views are `FmiComponent` and `SolveAlgorithmBlock`. One required product derives from the checked file plans. A sole `SolveAlgorithmBlock`, mixed `SolveModel`/`FmiComponent`, and every uncorrelated root mixture reject; only `AlgorithmCodePackage` files paired with `SolveAlgorithmBlock` files select one non-cloneable `SolveAlgorithmProduct`. No target-wide IR/root field, extension inference, target-name dispatch, compatibility alias, or fallback exists. `c-source`, `c-header`, and `cuda-source` are legal only with `solve`; `galec` admits only `algorithm-code` and `xml`, and `algorithm-code` is legal only with `galec`. The registered readiness-zero `efmu` target's AC and PC files borrow the product's retained package and correlated block; those roots are never independently supplied or paired. A retired target spelling never returns. | target manifests + generic rendering + architecture CI | Make AC-to-C and manifest-selected root drift unrepresentable instead of relying on filename scans or parallel selectors. |
| GAL-044 | Every manifest whose required product carries `AlgorithmCodePackage` declares the complete Default-free `[arithmetic]` triplet: `source_real`, signed `source_integer`, and the matrix-product relation; every other product rejects that table. The semantic-default catalog covers every leg. Projection carries the checked selection into package identity and every Real contraction constructor; omission, target-name selection, and later reselection reject. `FirstProduct` rejects an empty inner domain with the occurrence's D11 provenance; only `PositiveZero` admits it. | target manifests + `rumoca-phase-galec` + package construction | Executable width and signed-zero behavior cannot come from an orchestration hard-code. |
| GAL-045 | Template composition never crosses semantic contexts. A global partial/shared-template registry, support partials, and fallback lookup are forbidden. One checked file plan may explicitly borrow one complete artifact template from a registered built-in owner only with the same artifact kind and `galec` semantic context; construction validates and retains the exact owner/path edge, and built-in plus directory targets consume the owner's identical embedded bytes. Self/unknown/retired/suspended owners and local duplicate bytes reject. Copying Algorithm Code template source into a Solve renderer is forbidden. | target manifests + `rumoca-phase-codegen` | Subtemplates cannot become an AC-to-C compiler path, while the correlated AC representation cannot drift from its standalone canonical spelling. |
| GAL-046 | A packaged Algorithm Code product exists only after target-manifest construction resolves one complete closed role family—package manifest, Algorithm Code manifest, and Algorithm Code source—into one sealed `AlgorithmCodeArtifactLayout`. Construction enforces the standard singleton member paths and whole-family portable-path and case-collision rules once; the layout retains the exact target-issued role/member order without sorting or reconstruction. The packaged renderer accepts only no-path role-bound carriers and derives both the output member and every manifest `File` name/path reference from that retained layout. Generic Algorithm Code rendering is source-only: it cannot admit XML/package roles, mint a package member path, or construct a manifest relation. Missing, duplicate, wrong-kind, wrong-context, partial, or dynamically named roles reject before a checked target exists. Templates perform no path comparison, reconstruction, fallback, or model-name interpolation. The DAE -> Algorithm Code origin session snapshots `input.dae.source_map()` before its higher-ranked lowering callback and consumes one affine session-branded package candidate; an existing raw package, replacement map, or replacement model identity cannot enter Solve or rendering. The origin brand proves candidate/session non-substitution, while the sole phase-galec production call is load-bearing for map-to-block correspondence because Rust does not prove arbitrary closure semantics. Closure rejects missing, out-of-byte-range, or non-UTF-8-boundary exact/nearest provenance and retains a deterministic exact-map content digest for audit. | target manifests + `rumoca-phase-galec` + `rumoca-phase-solve` + `rumoca-phase-codegen` | Make a shipped Algorithm Code manifest incapable of naming bytes other than the correlated member selected by the same construction and trace-origin authority. |

## References

- [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md) — owning GAL-NNN rules,
  conformance ladder, and testing requirements.
- Ground truth: **eFMI Standard 1.0.0 Beta 1** (CC-BY-SA text not reproduced,
  SPEC_0034 GAL-023): §3.1 manifest; §3.2 analyses/signals/builtins;
  App. C reserved names.
