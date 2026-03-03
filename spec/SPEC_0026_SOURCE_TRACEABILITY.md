# SPEC_0026: Source Code Traceability

## Status

Accepted

## Summary

Span and source code location information must be maintained throughout all phases of the compilation process to enable accurate error reporting and debugging.

## Motivation

Accurate error messages require linking errors back to the original source code location. Without traceability:
- Error messages cannot point to the exact source location
- Debugging becomes difficult when generated code or DAE output cannot be traced back
- IDE features like "go to definition" and error highlighting cannot function
- Stack traces in runtime errors lose context

## Specification

### 1. Span Preservation Requirements

**REQUIRED — all IR nodes must carry spans:**
- AST nodes: Every Expression, Statement, and Equation must carry a `Span`
- Flat IR: Expression and Equation must carry spans
- DAE: Equations in the DAE must reference their source

**REQUIRED — transformations must propagate spans:**
- Simplified expressions inherit the original span
- Split/combined equations reference the original equation's span
- Generated code (from algorithm blocks) references the source algorithm

**PROHIBITED:**
- Creating IR nodes without span information (except compiler-generated constructs)
- Silently dropping spans during phase transformations
- Using `Span::DUMMY` in production code without justification

### 2. Span Structure (from rumoca-core)

```rust
pub struct Span {
    pub source: SourceId,  // Source file identifier (see SPEC_0009)
    pub start: BytePos,    // Start byte offset
    pub end: BytePos,      // End byte offset
}

impl Span {
    pub const DUMMY: Span = Span { source: SourceId(0), start: BytePos(0), end: BytePos(0) };

    pub fn merge(self, other: Span) -> Span;
}
```

### 3. Phase-Specific Requirements

**Parse Phase:**
- Parser must attach spans to all AST nodes
- Token spans must be accurate for syntax errors

**Resolve Phase:**
- Preserve spans when resolving references
- def_id resolution should not lose original span

**Instantiate Phase:**
- When instantiating components, preserve span from class definition
- When evaluating array dimensions, preserve span for dimension errors

**Flatten Phase:**
- Flattened variables should reference their declaration span
- Flattened equations should reference their source equation

**Typecheck Phase:**
- Type errors must point to the expression/equation causing the error

**ToDae Phase:**
- DAE equations should reference source equations
- Unknown/state variables should reference their declarations

### 4. Error Messages

All error types must include span information:
```rust
pub enum ResolveError {
    UnresolvedName { name: String, span: Span },
    DuplicateDefinition { name: String, first: Span, second: Span },
    // ...
}
```

Errors without spans (e.g., internal errors) should use `Span::DUMMY` but this should be avoided when possible.

### 5. Code Generation

Generated code should include source mapping comments where applicable:
```c
// From model.mo:42
x_dot[0] = ...;
```

### 6. DUMMY Spans

**PERMITTED uses of `Span::DUMMY`:**
- Compiler-generated constructs with no source equivalent
- Test fixtures where spans are not relevant
- Temporary placeholders during development (MUST be tracked as TODO)

**PROHIBITED uses of `Span::DUMMY`:**
- Any IR node that originated from parsed source code
- Error messages for user-facing diagnostics
- Silent use without a justifying comment

### 7. Verification

- Phase error tests MUST verify spans point to correct locations
- End-to-end tests MUST verify error messages show correct file:line:col
- No production code should silently use DUMMY spans without justification

## Related Specs

- SPEC_0008: Phase Errors (uses spans for error reporting)
- SPEC_0009: Common Crate (defines Span type)
- SPEC_0012: CST/AST (archived/deferred)
- SPEC_0024: Diagnostic Instrumentation (runtime tracing)

## References

- [MLS §19.4](https://specification.modelica.org/master/annotations.html#source-code-locations) - SourceInfo annotation
- [miette crate](https://docs.rs/miette) - Diagnostic reporting library
