# SPEC_0008: Diagnostics, Traceability, and Phase-Local Errors

## Status
ACCEPTED

## Summary
Each compiler phase defines its own error enum with phase-specific error codes.
Diagnostics, source spans, and optional tracing are owned by the phase that
emits them. Errors are defined close to the code that produces them, not in a
central location.

## Motivation
A monolithic error enum has problems:
- Grows unboundedly as features are added
- Every phase depends on every error type
- Hard to find which phase produces which error
- Error codes become inconsistent

Phase-local errors provide:
- Errors next to the code that emits them
- Clear ownership and responsibility
- Independent evolution per phase
- Consistent error code ranges

## Specification

### Source Identity

`Span.source` is a stable source identity derived from the source name, not a
`SourceMap` insertion index. The parser must assign this identity when it
creates AST spans from lexer locations, and the same identity must be used when
the source text is added to a `SourceMap`.

`SourceMap` is a rendering/lookup table for diagnostics. It must not be relied
on to repair parser spans after parse, merge, or source-root collection. Phase
code may use `Span::DUMMY` only for genuinely source-free diagnostics or
constructs. Compiler-generated IR that is derived from source must use the
nearest honest owner span, such as the rewritten expression, owning equation,
assignment, statement, declaration, or subscript span. Source-backed diagnostics
must carry the original span through AST -> Flat -> DAE -> Solve.

### Fail-Fast Error Semantics

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Unexpected compiler state MUST return a phase error | All compiler phases | Wrong IR must not propagate |
| Unresolved references MUST be hard errors | Resolve/Flat/DAE/Solve | Later defaults hide root causes |
| Default values on error are prohibited | All semantic passes | A guessed value is wrong compiler output |
| Missing semantic data MUST NOT be synthesized | All semantic passes | Garbage in produces garbage out |
| MLS-defined defaults are allowed only when explicitly modeled | Type/instantiate/sim semantics | Language defaults are not recovery |
| Optional serialization defaults require valid absent-field meaning | IR serde boundaries | Compatibility must stay semantic |

Compiler phases MUST fail immediately when required semantic data is missing,
malformed, or unresolved. The phase MUST return a phase-local error carrying
the best available span and diagnostic context. CLIs, workers, and tests MUST
surface that phase error as a failed compilation/model result and MUST NOT
continue the pipeline with invented data.

Recovery by substituting `0`, `1`, `false`, an empty shape, an empty
collection, `Span::DUMMY`, a first enum variant, an arbitrary component, or any
other default is forbidden unless the Modelica Language Specification or an
accepted Rumoca spec defines that value as the actual semantics of the source
construct. Defaults used only to keep the compiler running are bugs.

Defaulting is permitted only for source semantics that genuinely default, for
schema fields whose absence has the same semantic meaning as the default, or
for non-semantic operational configuration. Those cases MUST be documented at
the use site or by the owning spec. They MUST NOT mask errors, unresolved
references, malformed IR, shape/type mismatches, or missing compiler analysis
results.

### Option vs Result in Semantic Code

`Option<T>` is allowed only when absence is a valid semantic outcome or when a
helper is explicitly a non-authoritative pattern recognizer. Examples include
"this expression is not a record constructor", "this optional annotation is not
present", or "this backend display hook has no configured label".

Required semantic data MUST use `Result<T, PhaseError>` or an invariant
`expect(...)`/`panic!(...)` instead of `Option<T>` when absence would mean one
of the following:

- an MLS-mandated construct is malformed;
- name lookup, type information, dimensions, bindings, or function metadata
  were required by the current phase contract;
- an earlier phase promised the IR was resolved, shaped, lowered, or
  structurally consistent;
- continuing would require inventing a scalar shape, zero value, false guard,
  empty collection, dummy span, first enum variant, textual path, or synthetic
  component.

Best-effort helpers MUST make their uncertainty explicit in their name or API
(`try_*`, `maybe_*`, `*_if_present`). Their callers MUST NOT collapse `None`
into a semantic default when the next operation needs the missing data. Instead
split the code into a best-effort probe and a required `Result` path at the
first point where the MLS or IR-stage contract requires the data.

For dimensions, `[]` means scalar only when a prior type/shape fact proves the
expression is scalar. It MUST NOT also mean "unknown shape". Shape inference
APIs that can fail to determine required shape must return a distinct
unknown/error result.

### Error Propagation Mechanism

Three mechanisms are used; choosing the wrong one defeats the fail-fast contract.

| Mechanism | When to use | Phase scope |
|---|---|---|
| `emit()` on `&mut Diagnostics` | User errors in early phases — multiple independent errors exist; collecting all at once gives better IDE diagnostics | parse, resolve, flatten, instantiate |
| `?` (bubble up `Result`) | User errors in late phases, or intra-phase propagation — input is already validated; one error aborts the phase | DAE, structural, solve lowering |
| `panic!` / `expect("invariant")` | Internal compiler invariant violations — a bug in rumoca, not in the user's Modelica; earlier phases must have guaranteed this cannot happen | any phase, any location |
| `debug_assert!` | Hot-loop invariants guaranteed by construction where an always-on check would add measurable overhead | tight loops in structural/solve |

**Classifying an error:**

| Question | Answer → Mechanism |
|---|---|
| Could the user have written Modelica that triggers this? | Yes → `emit()` (early) or `?` (late) |
| Would this only occur if an earlier phase produced wrong output? | Yes → `panic!` / `expect` |
| Is this in a tight loop and the invariant is set up by construction above the loop? | Yes → `debug_assert!` |

**PROHIBITED:**
- `unwrap_or(default)` / `unwrap_or_default()` / `unwrap_or_else(|| fallback)` that substitutes a plausible value when the real value is missing
- Silently skipping work in an `if let … { } // else nothing` when the else branch represents a compiler contract violation
- `or_insert(default)` on a map when a duplicate key is a contract violation

### Error Code Ranges

Error codes use mnemonic prefixes for readability:

| Range | Phase | Mnemonic | Description |
|-------|-------|----------|-------------|
| EP0xx | parse | **P**arse | Syntax errors |
| ER0xx | resolve | **R**esolve | Name resolution errors |
| ET0xx | typecheck | **T**ype | Type errors |
| EI0xx | instantiate | **I**nstantiate | Modification errors |
| EF0xx | flatten | **F**latten | Connection errors |
| ED0xx | todae | **D**AE | Equation errors |
| EC0xx | codegen | **C**odegen | Code generation errors |
| EM0xx | class merge | **M**erge | Class-tree merge errors |
| ES0xx | structural | **S**tructural | Matching/BLT/singularity (`ES001`-`ES002` warnings, `ES01x` errors) |
| EL0xx | solve lowering | so**L**ve | DAE → Solve-IR lowering (`EL001`-`EL011` rows, `EL02x` assembly, `EL03x` overrides) |
| EX0xx | sim runtime | e**X**ecution | Solver, runtime-preparation, parameter-override |
| EG0xx | GALEC IR | **G**ALEC | GALEC IR parse/validation errors |
| EGT0xx | GALEC target projection | **G**ALEC **T**arget | DAE-to-GALEC projection/export errors |
| EFM0xx | eFMI packaging | e**FM**I | eFMI manifest/packaging errors |
| WP/WR/WT/etc | (same) | | Warnings per phase |

The leading letter is the severity: a warning MUST NOT be minted in an `E`
range, nor an error in a `W` range. The stable identity of a diagnostic is its
bare mnemonic (`ED001`); `miette` phases render it as
`rumoca::<phase>::<MNEMONIC>` and others emit the bare form, so consumers MUST
match by mnemonic **suffix**. Contract tests implement this comparison locally
in `crates/rumoca-contracts/src/test_support.rs`. A shipped code is stable:
retire it rather than renumber or reuse.

The former GALEC-target meanings of `ET001`–`ET023` are retired because they
collided with typecheck. GALEC target projection now emits `EGT001`–`EGT023`;
the typecheck meanings of `ET0xx` are unchanged.

**Known drift**, tracked separately: `rumoca-phase-structural` emits
`ES001`/`ES002` at warning severity. For these, severity MUST be read from the
diagnostic's `severity` field, never inferred.

### Acceptance Contract Before Rejection

A rejection is only as good as the acceptance it bounds. A **typed rejection
path** is a new error variant, a newly minted code, or an `Err`/`emit()` on
input a phase previously accepted. Every new one MUST land in the same change as
a written **acceptance contract**: which inputs stay legal, and which owner
handles them.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A new typed rejection ships with its acceptance contract | Author, same change | Unbounded rejection is over-reach |
| The contract names the legal shapes *and* their handling owner | Same change | "Not rejected" is not a design |
| Discharge it as a test asserting the accepted shape, or a checklist/spec bullet | Test suite, or PR notes per SPEC_0025 §3 | Executable evidence preferred, written evidence required |
| Widening an existing rejection to new input is a new rejection path | Author | Scope creep needs the same contract |
| Never discharge it by relaxing an existing fixture or assertion | Test suite | Weakening hides the over-reach it should expose |

**Why:** `EF025` over-reached onto callables that legally select no
implementation — MLS §3.7 predefined operators, record constructors, `type`
conversions — because nothing stated which callables stay legal; the `EI012`
partial-class rejection was absorbed by weakening a fixture instead of naming
the accepted deferred-declaration shape. Rejections whose accepted shape was
designed first landed clean.

**Checklist-item template** — one per new rejection path:

```markdown
- [ ] <CODE> rejects <illegal shape>;
      accepts <legal shapes that stay legal>,
      owned by <crate::module or phase>;
      evidence <test path::name | spec section>
```

### PhaseError Trait

`PhaseError` in `crates/rumoca-core/src/lib.rs` is the common interface:

```rust
pub trait PhaseError {
    /// Convert this error to a diagnostic.
    fn to_diagnostic(&self) -> Diagnostic;
}
```

### Phase Error Pattern

Each phase defines errors in a local `errors.rs`, derives `thiserror::Error` and
`miette::Diagnostic` for the message/code/label, and implements `PhaseError` by
handing the retained spans to `miette_phase_error_to_diagnostic`:

```rust
//! rumoca-phase-resolve/src/errors.rs

#[derive(Debug, Clone, Error, MietteDiagnostic)]
pub enum ResolveError {
    #[error("undefined reference: `{name}` not found")]
    #[diagnostic(
        code(rumoca::resolve::ER002),
        help("check that the name is declared before use")
    )]
    UndefinedReference {
        name: String,
        #[label("not found in scope")]
        span: Span,
    },
    // ER001, ER003, ER004 follow the same shape.
}

impl PhaseError for ResolveError {
    fn to_diagnostic(&self) -> Diagnostic {
        let span = match self {
            Self::UndefinedReference { span, .. } => span,
            // ... one arm per variant
        };
        miette_phase_error_to_diagnostic(self, std::slice::from_ref(span))
    }
}
```

Miette labels carry byte offsets but not source identity, so the phase error
retains the original `Span` values and passes them in `#[label]` order. Callers
emit with `ctx.diags.emit(err.to_diagnostic())` or bubble the `Result`, per the
propagation table above. Typecheck defines `TypeCheckError` in
`crates/rumoca-phase-typecheck/src/lib.rs` rather than in an `errors.rs`.

### Common Diagnostic Infrastructure

`crates/rumoca-core/src/lib.rs` provides the base types:

```rust
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

pub struct Diagnostics {
    diags: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn emit(&mut self, diag: Diagnostic) { ... }
    pub fn has_errors(&self) -> bool { ... }
}
```

`Diagnostic::to_miette(&self, source_name: &str, source: &str) -> MietteReport`
and `to_miette_with_source_map` render a diagnostic for terminal display.

### Exceptions

**CodegenError** does not implement `PhaseError` because:
- Code generation errors occur during template rendering, not source analysis
- They carry at most a rendered-template `SourceSpan`, never a `rumoca_core::Span`
  identifying user Modelica source
- They need to implement `std::error::Error` for Result-based error handling
- They wrap external errors (e.g., minijinja template errors)

`CodegenError` in `crates/rumoca-phase-codegen/src/errors.rs` is a
`thiserror::Error` + `miette::Diagnostic` enum carrying `EC0xx` codes, with
`impl From<minijinja::Error>`; it implements no `PhaseError`.

### Source Traceability

| Rule | Where | Why |
|---|---|---|
| Preserve spans through AST → Flat → DAE → Solve | every transformation | Values originating in source must remain clickable in diagnostics |
| Never silently drop spans | every transformation | Drop = lost user-facing location |
| Generated source-derived IR uses owner/context spans | synthetic equations / generated code | Generated does not mean source-free |
| `Span::DUMMY` only for genuinely source-free constructs | synthetic placeholders / global diagnostics | Absence must be intentional |
| APIs for generated IR require explicit span context | core IR constructors | Callers must choose provenance |
| Diagnostics include primary + secondary labels when useful | error sites | Primary points at the issue; secondary at the related context |
| Source-free constructs explain why no span exists | comment near the synthesis site | Future contributors can't tell intent from absence |

### Diagnostic Instrumentation

| Rule | Why |
|---|---|
| Gate `tracing` imports/calls behind the project's tracing convention | Tracing MUST NOT add production overhead |
| Use explicit tracing levels (no default `#[instrument]`) | Default levels surprise consumers |
| `skip(...)` large context parameters in instrumented functions | Avoid heavy debug formatting |
| Instrument phase entry/exit, eval failures, connection processing, for-range eval | These are the high-value debug points |
| CLI debug/dump syntax is non-normative until `rum` implements it | No spec drift ahead of implementation |

## Rationale

Follows the Rust compiler pattern: each pass owns local errors with
mnemonic-prefixed codes (ER/ET/EI/EF/ED/EC). Codes are grep-discoverable;
phases evolve independently; `PhaseError` enables polymorphic handling.

## References
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) §3 — where a PR records an
  acceptance contract that is not discharged by a test
- Acceptance-contract exemplars:
  `rumoca-phase-flatten/tests/function_selection_identity.rs` and
  `.../pipeline/function_overrides_and_dims/predefined_callables.rs` (`EF025`);
  `rumoca-phase-resolve/src/tests/partial_replaceable.rs` (`EI012`)
- Rust compiler error index: https://doc.rust-lang.org/error_codes/
- miette crate: https://docs.rs/miette
