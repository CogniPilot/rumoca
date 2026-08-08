# SPEC_0029: Crate Boundaries as Collaboration Guardrails

## Status
ACCEPTED

## Summary

Crate boundaries are compiler-enforced guardrails. A crate's `Cargo.toml` is
its reading list; illegal coupling should fail before review.

Per-helper and per-layer ownership assignments are catalogued in
[SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md). Every row there is normative
by reference from the section that links it.

## Specification

### 1. Bounded Context Per Task

`Cargo.toml` defines what each crate can see. Read dependencies before editing.

### 2. Strict DAG Dependency Graph

No circular dependencies. Dependency tiers form an acyclic graph enforced by the
Rust compiler. See [Dependency Tiers](#dependency-tiers).

### 3. IR Crates Are Pure Data

`rumoca-ir-ast`, `rumoca-ir-flat`, `rumoca-ir-dae`, and `rumoca-ir-solve`
contain only data types, display/debug implementations, and serde
serialization. No evaluation logic, phase logic, or side effects.

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

IR-specific types stay in their matching crate; shared multi-stage vocabulary
belongs in `rumoca-core`.

### 3b. Single-Source Helpers Across the Pipeline

Shared helpers **must** have one designated implementation. The owner of each
shared helper is [SPEC_0041 §1](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#1-single-source-helper-catalog-spec_0029-3b).

Required rules:

- Each helper has exactly one implementation in its listed module; callers MUST
  import that path.
- Do not fork helpers. If ownership creates a forbidden dependency, move it by
  spec update.
- List additions require a spec update.

### 4. Phase Typing via Newtypes

`ParsedTree`, `ResolvedTree`, `TypedTree`, and `InstancedTree` wrap `ClassTree`.
The type system enforces phase ordering: you can't pass unresolved data to a
phase that requires resolved data. This eliminates pipeline-ordering bugs that
would otherwise require runtime checks or careful documentation.

The production compiled-model path is:

```text
ParsedTree -> ResolvedTree/ClassTree -> InstanceOverlay -> typecheck_instanced -> flat::Model -> Dae
```

`TypedTree` remains the artifact for the standalone resolved-tree typecheck API.
Model compilation uses post-instantiation type checking because modifier and
structural-parameter values are available only after instantiation.

### 5. Evaluation Decoupled from Representation

Evaluation crates are aligned to IR ownership: `rumoca-eval-ast`,
`rumoca-eval-flat`, and `rumoca-eval-dae`. `rumoca-eval-solve` evaluates the
shared typed Solve program vocabulary and both checked Solve roots, including
tensor-kernel selection and `SolveAlgorithmBlock` lifecycle execution; it MUST
NOT depend on a Tier 4/5 crate. The numerical simulation state machine and
driver remain in `rumoca-solver::runtime`. `rumoca-eval-galec` remains an
independent Algorithm Code oracle and MUST NOT delegate to Solve lowering or
evaluation.

Phase crates MAY depend on the evaluation crate for the IR they are actively processing
when the phase needs compile-time evaluation of that representation. For example,
`rumoca-phase-flatten` may use `rumoca-eval-flat` for Flat-level constant and shape
evaluation instead of duplicating that logic inside the phase.

### 6. Rules for Adding Dependencies

Before adding a dependency from crate A to crate B:

1. No cycle.
2. Dependency target is lower or equal tier.
3. A `rumoca-core` trait/shared type would not be cleaner.
4. Cross-tier shortcuts are justified by spec, not convenience.

### 7. Rules for Creating New Crates

**Split when:** adding a new IR, compiler phase, data-only consumer surface, or
separating unrelated concerns. **Keep together when:** code is small, has one
consumer, or always changes as a unit.

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
- Non-compile helper surfaces remain under explicit namespaces (`analysis`, `parsing`, `runtime`, `source_roots`, `project`).

CI enforcement:
- Violations MUST fail CI.
- The workspace test `crates/rumoca/tests/architecture_hardening_test.rs::test_session_root_facade_exports_are_minimal`
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

```
compiler/session → DAE structural → checked Algorithm Code / Solve lowering → checked export/runtime contracts → execution backend → simulation session → reporting → visualization
```

Ownership of each link in that chain is
[SPEC_0041 §4](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#4-layering-ownership-catalog-spec_0029-12).

Execution adapters are not phases. Non-codegen phases must not depend on target
encoders, JITs, toolchains, or device APIs. Textual target policy lives in
`target.toml` and templates; Rust provides generic rendering, validation, and
IR capability probes. Unsupported capabilities report
`unsupported-feature:<feature_id>`. JIT/device adapters consume Solve IR or
generated artifacts through stable execution ABIs and equivalence tests.

FMI deployment is the checked-export case, not a textual projection directly
from DAE or Solve. `rumoca-ir-fmi` owns the private invariant-bearing FMI
component aggregate, `rumoca-phase-fmi` constructs it from checked DAE metadata
and the corresponding checked Solve kernel, and `rumoca-phase-codegen` owns the
generated FMI 2/3 lifecycle and ABI adapter text. Those adapters consume the
checked aggregate and MUST NOT repeat Modelica, DAE, or Solve lowering.

Each target manifest selects one proven-valid canonical or checked export IR.
`rumoca-phase-codegen` exposes a typed, read-only semantic view for each
supported IR and dispatches that view generically. Rendering never performs a
compiler transformation or repairs an artifact. Adding another target over an
existing view MUST require no Rust change; adding support for another IR adds
only its target-neutral semantic view and capability vocabulary. An export IR
selectable by a target remains outside the canonical compiler pipeline.

GALEC Production Code consumes a checked `SolveAlgorithmBlock`, never the
high-level Algorithm Code template view. `rumoca-phase-solve` owns exhaustive
`AlgorithmCodePackage` lowering into typed storage-neutral programs and ordered
lifecycle actions. `rumoca-phase-codegen` exposes the completed root and its
checked correlations; C/H templates may spell the selected ABI but MUST NOT
choose passing mode, storage, scalar/tensor lowering, scope, scheduling,
operation, or failure behavior.

`rumoca-phase-codegen` Rust may derive target-neutral typed contexts, schedules,
shapes, dependency/bounds proofs, symbols, and provenance. It MUST NOT spell or
assemble target-language tokens, expressions, statements, declarations, or
files. Those belong entirely to each target's `target.toml` and MiniJinja
templates, so adding a textual target does not require a Rust dialect or
renderer. Generic template operations consume semantic IR vocabulary and fail
closed; they do not return pre-rendered language fragments.

Target-specific package/schema models, constants, filenames, and artifact
graphs also belong in the owning target directory, not in IR or phase Rust.
Generic documented artifact commands may hash rendered bytes, validate a
declared schema, and assemble the declared graph without understanding eFMI or
another target format. Generic on-disk package assembly is owned by the
`fmu-packaging` feature and MUST NOT depend on scheduled simulation, transports,
input devices, viewers, or process control.

Target assets follow the same ownership rule. Builtin target discovery embeds
arbitrary assets declared beneath a target directory; external targets resolve
declared asset sources relative to their own directory. Rust MUST NOT maintain
a target-format bundle registry or map names such as an eFMI schema bundle to
hardcoded files.

Target-specific semantic lowering is a compiler phase, not code generation.
`rumoca-phase-codegen/src` MUST NOT contain target-named subsystems such as
`galec/`, C lowering, XML manifest models, target manglers, or target dispatch.
It MAY contain small IR-specific adapters under `views/` when they expose only
typed, read-only semantic data. Checked export data and constructors belong to
their `rumoca-ir-*` crate; semantic projection belongs to its
`rumoca-phase-*` crate; all target syntax and presentation belong to the target
directory.

Within `rumoca-phase-codegen`, `src/codegen/` is reserved for the public
MiniJinja extension-command surface. Rendering orchestration belongs in generic
renderer modules and IR adapters belong under `src/views/`. Every registered
command MUST be pure, deterministic, target-neutral, fail closed, and have
documented template syntax, typed inputs/outputs, failure behavior, complexity,
and focused tests. A single registry is the source of truth for registration
and user-facing command documentation. Commands may return semantic values or
checked arithmetic/query results; they MUST NOT return target-language
fragments or perform lowering, name resolution, type repair, target dispatch,
file assembly, or escaping for a particular output language.

Architecture CI MUST reject production `rumoca-phase-codegen` Rust that builds
generated or template-context text with formatting, concatenation, replacement,
writer, or incremental string-assembly APIs. Diagnostic messages and generic
template/file transport are the only string-handling exceptions; their values
must not enter semantic template contexts. Target names are rendered from typed
identity/path segments in templates, not pre-mangled Rust strings.

Steady-state CI rejects reverse dependencies across this chain. `rumoca-compile`
MUST NOT depend on concrete solvers or visualization assets; backend-selection
APIs MUST affect runtime behavior, not only metadata.

## Dependency Tiers

Workspace crates use six tiers. Dependencies flow downward.

```
Tier 6 — Binary & bindings: rumoca, bind-python, bind-wasm, contracts
Tier 5 — Integration/runtime: codec/input/solver/sim/opt/viz/tool-lsp families
Tier 4 — Orchestration: rumoca-compile, tool-fmt, tool-lint
Tier 3 — Phases & evaluation: rumoca-phase-*, rumoca-eval-*
Tier 2 — IR data: rumoca-ir-*
Tier 1 — Foundation: rumoca-core
```

Input-boundary and simulation-composition ownership is
[SPEC_0041 §5](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#5-input-and-simulation-composition-catalog-spec_0029-dependency-tiers).

## Related Specs

- [SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md) — helper, session, and layer ownership catalog.
- [SPEC_0021](SPEC_0021_CODE_COMPLEXITY.md) — maintainability and deterministic-collection rules.
