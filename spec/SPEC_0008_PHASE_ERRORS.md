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
| WP/WR/WT/etc | (same) | | Warnings per phase |

### PhaseError Trait

The `PhaseError` trait in `rumoca-core` provides a common interface for all phase errors:

```rust
//! rumoca-core/src/diag.rs

pub trait PhaseError {
    /// Convert this error to a diagnostic.
    fn to_diagnostic(&self) -> Diagnostic;

    /// Emit this error to a diagnostics collection.
    fn emit_to(&self, diags: &mut Diagnostics) {
        diags.emit(self.to_diagnostic());
    }
}
```

### Phase Error Pattern

Each phase defines errors in a local `errors.rs` and implements `PhaseError`:

```rust
//! rumoca-phase-resolve/src/errors.rs

use rumoca_core::{Diagnostic, Label, PhaseError, Span};

pub enum ResolveError {
    UnresolvedName { name: String, span: Span },
    DuplicateDefinition { name: String, first: Span, second: Span },
    CyclicInheritance { class: String, span: Span },
    InvalidExtends { name: String, span: Span },
}

impl PhaseError for ResolveError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::UnresolvedName { name, span } => {
                Diagnostic::error(format!("cannot find `{name}` in this scope"))
                    .with_code("ER001")
                    .with_label(Label::primary(*span))
            }
            Self::DuplicateDefinition { name, first, second } => {
                Diagnostic::error(format!("`{name}` is defined multiple times"))
                    .with_code("ER002")
                    .with_label(Label::primary(*second).with_message("duplicate"))
                    .with_label(Label::secondary(*first).with_message("first defined here"))
            }
            Self::CyclicInheritance { class, span } => {
                Diagnostic::error(format!("cyclic inheritance involving `{class}`"))
                    .with_code("ER003")
                    .with_label(Label::primary(*span))
            }
            Self::InvalidExtends { name, span } => {
                Diagnostic::error(format!("`{name}` cannot be extended"))
                    .with_code("ER004")
                    .with_label(Label::primary(*span))
            }
        }
    }
}
```

### Typecheck Phase Example

```rust
//! rumoca-phase-typecheck/src/errors.rs

use rumoca_core::{Diagnostic, Label, PhaseError, Span};

pub enum TypecheckError {
    TypeMismatch { expected: String, found: String, span: Span },
    VariabilityViolation { msg: String, span: Span },
    DimensionMismatch { expected: String, found: String, span: Span },
    UnknownType { name: String, span: Span },
}

impl PhaseError for TypecheckError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::TypeMismatch { expected, found, span } => {
                Diagnostic::error(format!("expected `{expected}`, found `{found}`"))
                    .with_code("ET001")
                    .with_label(Label::primary(*span))
            }
            // ...
        }
    }
}
```

### Usage in Phase Implementation

```rust
//! rumoca-phase-resolve/src/lib.rs

mod errors;
use errors::ResolveError;

pub fn resolve(ast: &Ast, ctx: &mut ResolveContext) -> Result<Resolved, ()> {
    let resolver = Resolver::new(ctx);
    
    // Error emission
    if let Some(existing) = resolver.lookup_local(&name) {
        ctx.diags.emit(ResolveError::DuplicateDefinition {
            name: name.clone(),
            first: existing.span,
            second: span,
        }.to_diagnostic());
    }
    
    // ...
}
```

### Common Diagnostic Infrastructure

The shared `rumoca-core` crate provides the base types:

```rust
//! rumoca-core/src/diag.rs

pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

pub struct Diagnostics {
    diags: Vec<Diagnostic>,
    error_count: usize,
}

impl Diagnostics {
    pub fn emit(&mut self, diag: Diagnostic) { ... }
    pub fn has_errors(&self) -> bool { ... }
}
```

### Miette Integration

Errors convert to miette reports for pretty display:

```rust
impl Diagnostic {
    pub fn to_miette(&self, source_name: &str, source: &str) -> MietteReport {
        // ...
    }
}
```

### Exceptions

**CodegenError** does not implement `PhaseError` because:
- Code generation errors occur during template rendering, not source analysis
- They have no `Span` pointing to user source code
- They need to implement `std::error::Error` for Result-based error handling
- They wrap external errors (e.g., minijinja template errors)

```rust
//! rumoca-phase-codegen/src/errors.rs

/// Error during code generation (does NOT implement PhaseError).
pub struct CodegenError {
    pub message: String,
}

impl std::error::Error for CodegenError {}
impl From<minijinja::Error> for CodegenError { ... }
```

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
- Rust compiler error index: https://doc.rust-lang.org/error_codes/
- miette crate: https://docs.rs/miette
