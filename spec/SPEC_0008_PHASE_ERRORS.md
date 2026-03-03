# SPEC_0008: Phase-Local Error Types

## Status
ACCEPTED

## Summary
Each compiler phase defines its own error enum with phase-specific error codes. Errors are defined close to the code that produces them, not in a central location.

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

## Rationale
- Follows Rust compiler pattern (each pass has local errors)
- Error codes are discoverable (grep for ER001, ET001, etc.)
- Mnemonic prefixes make phase origin obvious (ER = Resolve, ET = Type, etc.)
- Phases can add errors without touching other code
- Clear phase ownership prevents "who produces this?" confusion
- `PhaseError` trait enables polymorphic error handling across phases

## References
- Rust compiler error index: https://doc.rust-lang.org/error_codes/
- miette crate: https://docs.rs/miette
