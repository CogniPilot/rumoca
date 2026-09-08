# SPEC_0029: Crate Boundaries as Collaboration Guardrails

## Status
ACCEPTED

## Summary

Crate boundaries are compiler-enforced guardrails. A crate's `Cargo.toml` is
its reading list; illegal coupling should fail before review.

Per-helper and per-layer ownership assignments are catalogued in
[SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md); exact runtime and artifact
carriers are catalogued in [SPEC_0054](SPEC_0054_RUNTIME_LAYERING_CATALOG.md).
Catalog entries are normative only through the affirmative parent rule that
links them.

## Specification

### 1. Bounded Context Per Task

`Cargo.toml` defines what each crate can see. Read dependencies before editing.

### 2. Strict DAG Dependency Graph

No circular dependencies. Dependency tiers form an acyclic graph enforced by the
Rust compiler. See [Dependency Tiers](#dependency-tiers).

### 3. IR Crates and Checked Proof Plans Are Pure Data

`rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`, `rumoca-ir-galec`, and
`rumoca-ir-solve` contain only data types,
display/debug implementations, and their specifically admitted wire
surfaces. No evaluation logic, phase logic, or side effects.

`rumoca-plan-callable` is the sole checked Tier 2 proof-plan crate. It owns the
private-field checked callable semantic-plan aggregate and its closed typed
scalar/tensor/structured-region vocabulary. It contains no DAE traversal,
lowering, evaluation, backend policy, text, target profile, serialization or
deserialization root, public unchecked builder, or side effects. Its plan is
affine, non-wire proof authority rather than a canonical compiler root. Its
sole IR dependency, `rumoca-plan-callable -> rumoca-ir-dae`, is retention-only:
the plan retains the exact checked DAE owner used to construct it and borrows
only its correlated source views; it cannot lend, return, or clone the raw DAE
owner and cannot construct or mutate DAE.
Architecture CI proves that current `rumoca-plan-callable` and
`rumoca-phase-callable` source contains no reference, use, import, or re-export
of DAE construction, decode, or mutation authority. This is a load-bearing
coverage gate, not a construction proof; the typed corridor proof is
lent-view-only access, no owner escape, consumer argument absence, and
drop-on-rebrand.

Every source-language parser, generated grammar, recoverable CST, parser state,
and syntax diagnostic belongs in a `rumoca-phase-parse*` crate. IR crates MUST
NOT contain or feature-gate source parsers. Current-version wire replay through
checked constructors is data integrity, not source parsing.

IR data types own the checked constructors needed to make their local
invariants unrepresentable. `rumoca-ir-dae` also owns private current-version
wire decoding, checked root assembly, and closed root-bound operations that
atomically rebuild invariant-related objects. These are data-integrity APIs,
not semantic analysis: phase crates decide which transformation is valid and
supply its typed proof/input. DAE exposes no public whole-root validator,
unchecked builder, mutable partition callback, or invariant-bearing child
`Deserialize` implementation.

DAE construction enters through `Dae::construct`. Its generatively branded,
sequential semantic-owner closures share one expression arena; expression
insertion requires `expr.at(provenance)` and cannot allocate source-free nodes.

Allowed exception: IR crates MAY provide read-only traversal/query helpers over
their own data when those helpers have no side effects, do not evaluate
expressions, depend on phase crates, or encode backend policy.
Helpers needing modification environments, typechecking state, solver layout,
incidence data, or runtime state are not IR helpers.

IR crates MAY provide rewrite-shape helpers that recursively rebuild their own
IR nodes without semantic state. These helpers are limited to structural
ownership-preserving rewrites such as "visit every expression and allow a
caller-supplied replacement"; they MUST NOT perform name lookup, constant
evaluation, type inference, lowering, balance analysis, backend selection, or
runtime behavior. A multi-object DAE rewrite must be a consuming, root-bound,
non-cloneable checked operation so catalog and expression identity change
atomically. Keep read-only traversal/query helpers and rewrite-shape helpers
separate so reviewers can see observation versus mutation.

### 3a. Foundation Types Live in rumoca-core

`rumoca-core` is the **sole** Tier 1 foundation crate. It owns shared IDs,
source locations, diagnostics/`PhaseError`, shared semantic IR vocabulary, and
small shared helpers.

`SourceId`, `Span`, and `SourceMap` are owned by `rumoca-core`. `SourceId` is a
stable identity derived from a source name, not a source-map slot number. IR and
phase crates must carry `Span` values through transformations and must not add
span-rebasing sidecars.

`VarName` is the shared flattened-variable path identity. It MUST be interned
inside `rumoca-core` and expose compact process-local `VarNameId` for
equality/hash-heavy paths, while preserving string display and serialization as
the stable external representation. Downstream crates must not add sidecar
variable-name interners or serialized ID compatibility layers; if a phase needs
symbol identity beyond `VarNameId`, introduce a phase-specific ID at that
boundary.

The `VarName` interner is process-local, monotonic, and has no public reset.
Hosts serialize/display text, never process-local `VarNameId`.

Do not create `rumoca-ir-core`, `rumoca-foundation`, or another micro-crate for
spans, diagnostics, IDs, or shared IR vocabulary without a spec update.

IR-specific types stay in their matching crate; shared multi-stage primitive
vocabulary belongs in `rumoca-core`. The invariant-bearing, independently
meaningful, multi-consumer callable proof aggregate belongs in
`rumoca-plan-callable`; it does not move into core merely to evade a dependency
boundary.

### 3b. Single-Source Helpers Across the Pipeline

Shared helpers **must** have one designated implementation. The owner of each
shared helper is [SPEC_0041 §1](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#1-single-source-helper-catalog-spec_0029-3b).

Required rules:

- Each helper has exactly one implementation in its listed module; callers MUST
  import that path.
- Do not fork helpers. If ownership creates a forbidden dependency, move it by
  spec update.
- List additions require a spec update.

### 4. Phase Proof Chain and Forward Proof Edges

Each landed successful front-end boundary publishes one opaque proof artifact,
resident in its minting crate with private fields. The current production
compiled-model chain is:

```text
ParsedTree -> ResolvedTree -> raw InstanceOverlay -> TypedInstancedTree -> flat::Model -> Dae
```

Model compilation uses post-instantiation type checking because modifier and
structural-parameter values are available only after instantiation.

Exactly two forward proof edges currently exist inside Tier 3:
`typecheck -> resolve` and `flatten -> typecheck`. Each edge licenses exactly
two things: naming the
predecessor's opaque proof artifact in the successor's own sole mint input
position, and calling that artifact's public immutable query views. An edge
licenses nothing else: no predecessor phase-entry calls, no re-exports, no
trait implementations on predecessor types, and no construction, mutation,
decode, or owned extraction of the predecessor artifact. Any other Tier-3
phase-to-phase dependency remains prohibited.

A proof artifact whose successor consumes it by value is not `Clone`, not
`Default`, and not deserializable, and it exposes no mutable projection;
consuming it transfers the unique phase capability by value, exactly once.
This affine rule currently applies to `TypedInstancedTree`. A phase may
additionally expose a `Clone`-able read-only projection that shares the
immutable payload; a projection carries no phase capability and no phase
accepts one directly in a proof input position. Cloning its raw overlay view
and rerunning the sole Typecheck mint creates a fresh checked proof rather than
recovering or duplicating the original proof.

`ResolvedTree` remains an immutable, `Clone`-able checked root while Instantiate
still takes a borrowed raw `&ClassTree`; that borrowed view is explicitly part
of the unclosed raw Resolve-to-Instantiate migration boundary. `ResolvedTree`
has no consuming payload extraction. The read-only `ResolvedTreeProjection`
is `Clone` only so
Typecheck can retain that exact shared Resolve root; its private field, sole Resolve-owned producer, and lack
of construction, mutation, decode, default, or owned extraction are cataloged
as a separate public semantic boundary.

There is no `TypedTree` proof artifact. The standalone resolved-tree typecheck
entry is a diagnostics query: it returns the checked data and its diagnostics
and mints no proof, and nothing downstream accepts its output as phase
evidence.

Migration state: Instantiate's production entry still accepts a raw
`&ClassTree` and returns a raw `InstanceOverlay`; it therefore has no
`instantiate -> resolve` proof edge and publishes no `InstancedTree` proof.
`InstancedTree` residence in `rumoca-phase-instantiate` with private fields,
the `instantiate -> resolve` proof input, and the `typecheck -> instantiate`
opaque input must land together with the Instantiate construction cutover.
Until then Typecheck consumes the raw instantiation overlay by value while
also naming `ResolvedTree`, and the Flatten edge is closed only for the exact
Resolve-tree identity retained in `TypedInstancedTree`. The raw
`InstanceOverlay` has no Resolve-root stamp yet, so pairing an overlay derived
from tree A with `ResolvedTree` B at the Typecheck mint remains explicit CE-3
migration debt until the Instantiate proof cutover.

### 5. Evaluation Decoupled from Representation

Evaluation crates are aligned to IR ownership: `rumoca-eval-ast`,
`rumoca-eval-flat`, and `rumoca-eval-dae`. `rumoca-eval-solve` evaluates only
checked Solve roots and their shared typed program vocabulary; it MUST NOT
depend on a Tier 4/5 crate. The state
machine and driver stay in `rumoca-solver::runtime`. SPEC_0038 moves the FMI 3
ME master driver to `rumoca-solver::fmi_me`; `runtime` keeps its Solve helpers.
`rumoca-eval-galec` remains an independent Algorithm Code oracle and MUST NOT
delegate to Solve lowering or evaluation.

Phase crates MAY depend on the evaluation crate for the IR they are actively processing
when the phase needs compile-time evaluation of that representation. For example,
`rumoca-phase-flatten` may use `rumoca-eval-flat` for Flat-level constant and shape
evaluation instead of duplicating that logic inside the phase.

`rumoca-phase-structural` owns the checked causal-discrete plan derived from
DAE identities. Solve and GALEC lowering consume that one orientation and
dependency authority; neither may independently choose a discrete target.

### 6. Rules for Adding Dependencies

Before adding a dependency from crate A to crate B:

1. No cycle.
2. Dependency target is lower or equal tier.
3. A `rumoca-core` trait/shared type would not be cleaner.
4. Cross-tier shortcuts are justified by spec, not convenience.

### 7. Rules for Creating New Crates

**Split when:** adding a new IR, checked proof plan, compiler phase, data-only
consumer surface, or separating unrelated concerns. **Keep together when:**
code is small, has one consumer, or always changes as a unit.

An IR crate requires an independently meaningful vocabulary, a checked
construction discipline, a total reference-semantics/proof relation for every
operation, and multiple non-presentation consumers. `rumoca-plan-callable` is
not an IR crate: it is the sole checked proof-plan crate. Its admission is
narrow because the plan is not a canonical root, is not serialized, is not
target-selectable, and cannot authorize code generation. Adding another checked
proof-plan crate or widening any of those properties requires a spec change.

### 8. Import and Re-export Discipline

To keep layer boundaries obvious in code (not only in `Cargo.toml`), use explicit crate namespaces.

In non-IR crates:
- Import IR crates as namespaces:
  - `use rumoca_ir_ast as ast;`
  - `use rumoca_ir_flat as flat;`
  - `use rumoca_ir_dae as dae;`
- Prefer qualified references (`ast::...`, `flat::...`, `dae::...`) over direct type imports.
- Avoid direct IR type imports such as `use rumoca_ir_flat::{Expression, VarName}` outside the owning IR crate.

Re-export guardrails:
- Non-facade crates MUST NOT re-export symbols from other Rumoca crates.
- Downstream crates import the owning crate directly; no intermediate routing.
- Wildcard forwarding is forbidden outside approved facades.
- Only approved facade crates MAY expose selected cross-crate API surfaces:

  | Facade | Scope | Allowed cross-crate exports |
  |---|---|---|
  | `rumoca-compile` | compilation/session | curated compile, parsing, codegen, analysis APIs |
  | `rumoca-sim` | simulation/runtime | solver/reporting/scheduling APIs behind features |
  | `rumoca-codec` | transport-neutral lockstep I/O | `SignalFrame`, codec traits/factories, typed codec config |

  These exports stay curated, namespaced, and documented. CLI/bindings may
  depend on facades but must not add lower-layer forwarding surfaces.
- Root/foundation crates MUST NOT act as compatibility facades for moved symbols.
  If a primitive is owned by a crate, downstream code must import it from the
  owning crate, not via re-export through an intermediate crate.

CI: `architecture_hardening_test::test_no_new_cross_crate_public_exports`
rejects `pub use rumoca_*::...` and `pub type X = rumoca_*::...` in non-facade
crates.

### 9. Session Facade Root API

`rumoca-compile` is the orchestration facade crate for top-level entry points.
Its root API MUST stay minimal:

- Allowed root exports: `Session`, `SessionConfig`.
- Compile result and helper types remain under explicit namespaces such as `rumoca_compile::compile::*`.
- Non-compile helper surfaces remain under explicit namespaces. The current set is `analysis`, `cache`, `codegen`, `parallelism`, `parsing`, `phase_structural`, `scenario`, `source_roots`, `workspace`; adding one is a spec update.

CI enforcement:
- Violations MUST fail CI.
- The workspace test `crates/rumoca/tests/architecture_hardening_test/main.rs::test_session_root_facade_exports_are_minimal`
  enforces this root export policy.

### 10. Session-Owned Source-Root And Class-Graph State

`rumoca-compile` owns IDE/runtime semantic state above the phase crates so
LSP, WASM, and CLI cannot drift into separate cache/invalidation policies. The
per-rule ownership assignments are
[SPEC_0041 §2](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#2-session-owned-source-root-and-class-graph-catalog-spec_0029-10).

Session snapshots are the read-side IDE/binding boundary. They MUST be
detached from the mutable host revision, allow concurrent reads, and reserve
exclusive locking for snapshot creation or query-cache warming.

Dependency fingerprint caches are session-owned. Rebuilt class hashes/edges
invalidate changed classes plus the reverse dependency closure, not every
cached model fingerprint.

### 11. Session Persistence Boundary

`rumoca-compile` MAY persist warm-restore state, scoped to source-root AST/index
plus resolved aggregate inputs. Typed/flat/DAE artifacts are NOT persisted by
default — they rebuild lazily behind dependency fingerprints. The exact
persisted/not-persisted split is
[SPEC_0041 §3](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#3-session-persistence-catalog-spec_0029-11).

Rationale: the warm-restore goal is to skip rebuilding front-end and resolved
dependency inputs on reopen, not to serialize the full downstream pipeline.

### 12. Runtime, Backend, Simulation Session, And Visualization Layering

The exact owner inventory is
[SPEC_0041 §4](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#4-layering-ownership-catalog-spec_0029-12);
the exact closed carriers, alternatives, and enforcement surfaces are
[SPEC_0054](SPEC_0054_RUNTIME_LAYERING_CATALOG.md). Both catalogs are normative
only through these obligations.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Execution adapters consume checked Solve roots or generated artifacts; they are not compiler phases and cannot establish semantics. | execution/runtime boundary | Meaning is established once |
| FMI deployment derives one checked component from completed Solve semantics; lifecycle/ABI text and solver binding occur only in preparation or templates. | Solve/FMI preparation | No parallel FMI authority |
| Each file declares one closed IR context and checked view; one correlated product is constructed without target-wide selectors, defaults, inference, or root mixtures. | target construction | Invalid products are unrepresentable |
| One invariant build-session origin retains semantic input, trace identity, model identity, file facts, and artifact identity through rendering. | `rumoca-compile` | Foreign facts cannot be joined |
| Package membership, producer order, checksum edges, assets, and paths are resolved once into one affine render plan consumed without reordering or revalidation. | package construction | One ordering authority |
| Preparation derives one sealed plan owning legality, ABI, layout, storage, solver, and refinement receipts; renderers receive no candidates or repair authority. | preparation/codegen boundary | Templates remain passive |
| Target syntax, schemas, assets, and template composition remain target-owned; codegen Rust is target-neutral and cannot lower or assemble target language. | target directories + codegen | Presentation cannot acquire semantics |
| Every public residual authority is affine, narrowly scoped, and structurally limited to its catalogued production issuance sites. | architecture CI | Public seams stay auditable |


## Dependency Tiers

Workspace crates use six tiers. Dependencies flow downward.

```
Tier 6 — Binary & bindings: rumoca, bind-python, bind-wasm, contracts
Tier 5 — Integration/runtime: codec/input/solver/sim/opt/viz/tool-lsp families
Tier 4 — Orchestration: rumoca-compile, tool-fmt, tool-lint
Tier 3 — Phases & evaluation: rumoca-phase-*, rumoca-eval-*
Tier 2 — IR data and checked proof plans: rumoca-ir-*, rumoca-plan-callable
Tier 1 — Foundation: rumoca-core
```

Within Tier 3, phases MUST compose through IR/evaluation crates rather than depend on other phases.
The two current forward proof edges of [§4](#4-phase-proof-chain-and-forward-proof-edges)
and shared prerequisite analyses are the sole reasoned, bidirectionally gated
exceptions; test fixtures are outside this production rule.

Input-boundary and simulation-composition ownership is
[SPEC_0041 §5](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#5-input-and-simulation-composition-catalog-spec_0029-dependency-tiers).
