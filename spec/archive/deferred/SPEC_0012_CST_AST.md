# SPEC_0012: CST vs AST Distinction

## Status
DEFERRED

> **Note:** CST implementation is deferred. Current compiler uses AST only.
> Comment collection exists in parser but full trivia preservation is not implemented.

## Summary
The compiler has two parse representations: CST (Concrete Syntax Tree) for lossless formatting and AST (Abstract Syntax Tree) for semantic analysis. They serve different purposes and should not be conflated.

## Motivation
Early designs described AST as "preserves source exactly" which is incorrect. A true lossless representation requires:
- Token stream with spans
- Comments and whitespace (trivia)
- Original literal formats (`1.0` vs `1.` vs `1.00`)

AST intentionally discards this information for simpler semantic analysis.

## Specification

### CST (Concrete Syntax Tree)

**Purpose:** Formatter, pretty-printer, source-to-source transformation

**Properties:**
- Lossless: can reconstruct original source exactly
- Contains trivia (comments, whitespace)
- Preserves literal formats
- Token-based representation

```rust
//! rumoca-ir/src/cst.rs

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,
}

pub enum Trivia {
    Whitespace(String),
    LineComment(String),   // // comment
    BlockComment(String),  // /* comment */
    Newline,
}

pub enum TokenKind {
    // Keywords
    Model, Class, Block, Connector, ...
    
    // Literals (preserve original text)
    IntegerLit(i64),
    RealLit(f64),
    StringLit(String),
    Ident(String),
    
    // ...
}
```

### AST (Abstract Syntax Tree)

**Purpose:** Semantic analysis, type checking, compilation

**Properties:**
- Lossy: discards formatting information
- No trivia
- Normalized literals
- Structured tree representation

```rust
//! rumoca-ir/src/ast.rs

pub struct Definition {
    pub kind: DefKind,
    pub name: Ident,
    pub elements: Vec<Element>,
    pub span: Span,  // For error messages only
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub enum ExprKind {
    Integer(i64),      // No original text
    Real(f64),         // No original text
    String(String),
    // ...
}
```

### Pipeline Usage

```
Source
   ↓
   ├─── lex_with_trivia() ───→ CST ───→ Formatter
   │
   └─── parse() ─────────────→ AST ───→ Resolve → Typecheck → ...
```

For compilation, we skip CST and parse directly to AST.
The formatter uses CST for lossless transformation.

### What Each Preserves

| Information | CST | AST |
|-------------|-----|-----|
| Comments | ✓ | ✗ |
| Whitespace | ✓ | ✗ |
| Literal format (`1.0` vs `1.`) | ✓ | ✗ |
| Token spans | ✓ | ✓ (node spans) |
| Semantic structure | ✗ | ✓ |
| Type information | ✗ | ✗ (added in Typed) |

### Formatter Architecture

```rust
//! rumoca-tool-fmt/src/lib.rs

use rumoca_ir::cst::{Token, lex_with_trivia};

pub fn format(source: &str, options: &FormatOptions) -> String {
    // 1. Lex to CST (preserves trivia)
    let tokens = lex_with_trivia(source, source_id);
    
    // 2. Apply formatting rules
    let formatted = reformat_tokens(&tokens, options);
    
    // 3. Reconstruct source
    let output = tokens_to_string(&formatted);
    
    // 4. Validate: formatted must parse to same AST
    debug_assert!(parse(source) == parse(&output));
    
    output
}
```

### Why Two Representations?

**CST advantages:**
- Lossless round-tripping
- Comment preservation
- Minimal diff in version control

**AST advantages:**
- Simpler structure for analysis
- Faster traversal (no trivia)
- Easier pattern matching

**Trade-off:** Maintaining both adds complexity, but each serves a distinct, important use case.

## Rationale
- Clear separation of concerns
- Formatter needs lossless representation
- Compiler needs clean semantic representation
- Matches industry practice (Roslyn, rust-analyzer)

## References
- rust-analyzer: Separate CST and HIR
- Roslyn (C#): Red-green trees for lossless syntax
- Modelica Specification §2: Lexical Structure
