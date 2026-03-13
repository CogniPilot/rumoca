# AGENTS.md

This file tells coding agents how to work in `rumoca` without drifting from the
Modelica Language Specification (MLS) or Rumoca's design decisions.

## Scope

- Treat active specs in `spec/README.md` as the source of truth.
- Archived specs are historical context only. Do not treat them as normative
  unless the user explicitly asks for archived behavior.
- Spec status matters:
  - `ACCEPTED`: mandatory
  - `REFERENCE`: normative lookup/reference material
  - `DRAFT`: current design direction; preserve it unless the user is
    intentionally changing architecture

## Required Reading Before Editing

Start with `spec/README.md`, then read only the specs relevant to the change.

Minimum default set for compiler work:

- `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md` (use its section index; do not
  load the entire file unless truly necessary)
- `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- `spec/SPEC_0021_CODE_COMPLEXITY.md`

Read additional specs based on the area you touch:

- Name lookup / scopes: `SPEC_0001`, `SPEC_0002`
- Instantiation / flattening: `SPEC_0004`
- DAE representation: `SPEC_0003`, `SPEC_0007`
- Error handling: `SPEC_0008`
- Shared foundation types: `SPEC_0009`
- Ordered public collections: `SPEC_0017`
- Arrays: `SPEC_0019`
- Algorithms / ToDae: `SPEC_0020`
- MLS-to-crate mapping: `SPEC_0023`
- Diagnostics / tracing: `SPEC_0024`
- Source spans / traceability: `SPEC_0026`

## Mandatory Workflow For Agents

1. Identify the owning crate and phase before editing code.
2. Read that crate's `Cargo.toml`; its dependencies are the allowed context.
3. Find the relevant MLS section and spec section before changing Modelica
   semantics.
4. Make the smallest change that preserves spec compliance and crate
   boundaries.
5. Add or update tests that prove the behavior, not just exercise the code.
6. Run the relevant verification commands before claiming the task is done.

If you cannot identify the governing MLS/spec section for a semantic change,
stop and read more before coding.

## MLS Compliance Rules

These rules come primarily from `SPEC_0025` and `SPEC_0022`.

- Any change affecting Modelica semantics must cite the relevant MLS section in
  nearby code comments, tests, or the change explanation.
- Prefer root-cause fixes over model-specific hacks.
- Do not special-case one MSL model if that would violate general MLS
  semantics.
- Preserve Modelica case sensitivity. Do not lowercase identifiers or make
  name lookup case-insensitive.
- When uncertain, implement the general MLS rule, not a local workaround.

Recommended comment style near non-obvious semantic logic:

```rust
// MLS §8.3.4: If-equations may contain any equation kind in each branch.
```

## Rumoca Architecture Rules

These rules come primarily from `SPEC_0004`, `SPEC_0007`, `SPEC_0023`, and
`SPEC_0029`.

- Keep compiler phases separate. Do not merge instantiation, flattening,
  ToDae, or codegen logic into a single cross-phase shortcut.
- IR crates are pure data. Do not add evaluation logic, phase logic, caches, or
  side effects to IR crates.
- Preserve the crate DAG. Do not add dependencies or re-exports that tunnel
  across layers.
- In non-IR crates, prefer namespaced imports such as
  `use rumoca_ir_flat as flat;`.
- Do not add wildcard re-exports or facade shortcuts in low-level crates.
- Use `rumoca-core` for shared IDs, spans, diagnostics, and `PhaseError`.

## Representation Rules

These rules are easy for agents to violate and must be preserved.

- Arrays stay symbolic through flatten and DAE. Do not eagerly scalarize arrays
  or array comprehensions in core IR. See `SPEC_0019`.
- Function algorithms stay structured in `Dae.functions`. Do not lower function
  bodies into solver equation buckets. See `SPEC_0020`.
- Model algorithms may only be lowered when they fit the supported declarative
  subset; otherwise fail explicitly with `ED013`. See `SPEC_0020`.
- Keep the solver-facing DAE lean. Do not store derived data such as incidence
  matrices, BLT orderings, or stale caches in `rumoca-ir-dae`. See `SPEC_0007`.
- Public IR/DAE collections that affect output or serialization must be
  deterministic (`IndexMap`), not `HashMap`. See `SPEC_0017`.

## Diagnostics And Traceability Rules

These rules come primarily from `SPEC_0008`, `SPEC_0024`, and `SPEC_0026`.

- Every phase owns its own error enum and error codes.
- User-facing diagnostics must carry real spans whenever source exists.
- Do not silently drop spans during transformations.
- `Span::DUMMY` is only for true compiler-generated constructs or tightly
  justified placeholders.
- If you add tracing, guard it according to `SPEC_0024` and use explicit
  tracing levels.

## Complexity And Maintainability Rules

These rules come primarily from `SPEC_0021` and `SPEC_0025`.

- Prefer functions under 60 lines; avoid exceeding 100.
- Keep nesting shallow; avoid nesting beyond 3 levels when possible.
- Prefer 0-5 parameters; avoid exceeding 7.
- Do not use `#[allow(clippy::...)]` to bypass maintainability issues except
  for generated code or documented spec-backed exceptions.
- Do not use `include!(...)` as a complexity or file-size escape hatch.
- Use clear names, explicit comments for non-obvious logic, and modules with
  single responsibilities.

## Testing And Verification

Before finishing compiler-facing work, run the narrowest relevant checks plus
the required project gates from `SPEC_0025`.

Standard checks:

```bash
cargo run --bin rum -- ci-parity
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
```

For parser / resolve / typecheck / instantiate / flatten / ToDae / simulator
behavior changes, also run the unified MSL gate:

```bash
cargo test --release --package rumoca-test-msl --test msl_tests \
  balance_pipeline::balance_pipeline_core::test_msl_all -- --ignored
```

If the change updates committed MSL quality data, ensure it was regenerated at
the current commit and promoted using the documented workflow in
`SPEC_0025_PR_REVIEW_PROCESS.md` and `SPEC_0030_COVERAGE_TRIM_PROCESS.md`.

## When To Update Specs

If you introduce a new invariant, cross-cutting pattern, or architectural rule:

- update an existing spec, or
- add a new spec in `spec/`

Do not silently introduce new project rules in code only.

## Prohibited Shortcuts

- No model-specific balance hacks that violate the MLS.
- No silent fallback that changes semantics just to make one test pass.
- No cross-layer crate forwarding to "make imports easier."
- No dropping spans, diagnostics, or source references for convenience.
- No moving behavior into IR crates because it feels simpler locally.
- No speculative architecture changes without checking the relevant spec first.

## Done Means

Do not say a compiler change is done unless:

- the relevant specs were checked,
- MLS-sensitive changes cite the right MLS sections,
- architecture boundaries remain intact,
- tests cover the changed behavior,
- required verification commands were run or an explicit blocker was stated.
