# SPEC_0009: Single Foundation Crate (rumoca-core)

## Status
ACCEPTED

## Summary
Spans, source maps, diagnostics, and shared ID types are combined into a single `rumoca-core` crate rather than split into separate micro-crates.

## Motivation
Early designs considered separate crates (`rumoca-source`, `rumoca-diag`). Problems with micro-crates:
- More Cargo.toml boilerplate than actual code
- Every other crate depends on both anyway
- Extra build overhead, harder to navigate

## Specification

### Crate Contents

Located in `rumoca-core/src/lib.rs` (~900 lines) plus helper modules:

```
rumoca-core/src/
  lib.rs              Main module: ID types, Span, SourceMap, Diagnostics, PhaseError
  eval_lookup.rs      Evaluation lookup helpers
  enum_compare.rs     Enum comparison utilities
  integer_binary.rs   Integer binary operations
  integer_division.rs Integer division semantics
```

### Key Types

**ID types** (all `Copy`, `u32`-based):
```rust
pub struct DefId(pub u32);     // Declaration ID (SPEC_0001)
pub struct TypeId(pub u32);    // Type ID (has TypeId::UNKNOWN sentinel)
pub struct ScopeId(pub u32);   // Scope ID (has ScopeId::GLOBAL sentinel)
pub struct SourceId(pub u32);  // Source file ID
```

**Source location:**
```rust
pub struct BytePos(pub usize);  // Byte offset in source

pub struct Span {
    pub source: SourceId,
    pub start: BytePos,
    pub end: BytePos,
}
// Span::DUMMY for compiler-generated constructs (see SPEC_0026)
```

**Diagnostics:**
```rust
pub enum DiagnosticSeverity { Note, Warning, Error }
pub struct Diagnostic { severity, code, message, labels, notes }
pub struct Diagnostics { diags: Vec<Diagnostic>, error_count: usize }

/// Trait for phase-specific errors (SPEC_0008)
pub trait PhaseError: std::error::Error {
    fn to_diagnostic(&self) -> Diagnostic;
}
```

**Source map:**
```rust
pub struct SourceMap { /* Vec<(name, content)> indexed by SourceId */ }
```

### What Belongs in rumoca-core

**REQUIRED:**
- Types used by 3+ crates (ID types, Span, Diagnostics)
- The `PhaseError` trait
- Shared utility functions (eval helpers, integer operations)

**PROHIBITED:**
- IR-specific types (expressions, equations) — those belong in IR crates
- Phase logic — belongs in phase crates
- Serde-only types — keep serialization details in the crate that owns the data

## Rationale
- Tight coupling: diagnostics reference spans, spans reference sources
- Single import for all foundation types
- Follows "don't over-engineer" principle from SPEC_0029

## References
- SPEC_0001: DefId
- SPEC_0008: Phase-Local Error Types
- SPEC_0026: Source Traceability (Span usage)
