# SPEC_0015: Token-Based Formatter (Pretty-Printer)

## Status
DEFERRED

> **Note:** A simplified line-based formatter exists but full token-based formatting
> with comment preservation requires CST (SPEC_0012), which is also deferred.

## Summary
The formatter operates on CST (token stream with trivia), not AST. It is a **pretty-printer** that produces canonical formatting while preserving comments. Original style choices are not preserved.

## Motivation
Two approaches to formatting:

1. **Format-preserving:** Keeps original style, only fixes violations
2. **Pretty-printer:** Produces canonical output regardless of input style

We choose pretty-printer because:
- Simpler implementation
- Consistent output across team
- No "style drift" over time
- AST-based would lose comments anyway

## Specification

### Architecture

```
Source → Lexer → CST (tokens + trivia) → Formatter → Output
```

The formatter:
1. Lexes source to CST (preserves all tokens and trivia)
2. Applies formatting rules
3. Reconstructs source from tokens
4. Validates by re-parsing

### Token Representation

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
```

### Format Options

```rust
pub struct FormatOptions {
    /// Spaces per indentation level
    pub indent_width: usize,         // default: 2
    
    /// Maximum line width before wrapping
    pub max_line_width: usize,       // default: 100
    
    /// Align annotations across lines
    pub align_annotations: bool,     // default: true
    
    /// Blank lines between sections
    pub section_spacing: usize,      // default: 1
}
```

### Formatting Rules

**Indentation:**
- Class body: +1 level
- Equation/algorithm section: +1 level
- If/for/when body: +1 level
- Continuation lines: +2 levels

**Spacing:**
- Binary operators: `a + b`, `x * y`
- No space before semicolon: `x = 1;`
- No space inside parentheses: `f(x, y)`
- Space after comma: `f(x, y, z)`
- Space after keywords: `if condition then`

**Line breaks:**
- One statement/equation per line
- Blank line between sections (public/protected/equation/algorithm)
- Long expressions wrap at operators

**Comments:**
- Leading comments stay with following token
- Trailing comments stay on same line
- Block comments preserve internal formatting

### Implementation

```rust
//! rumoca-tool-fmt/src/lib.rs

use rumoca_ir::cst::{Token, Trivia, lex_with_trivia};

pub fn format(source: &str, options: &FormatOptions) -> String {
    let source_id = rumoca_core::SourceId(0);
    let tokens = lex_with_trivia(source, source_id);
    let formatted = reformat_tokens(&tokens, options);
    let output = tokens_to_string(&formatted);
    
    // Validation: formatted must parse to same AST
    #[cfg(debug_assertions)]
    {
        let original_ast = rumoca_phase_parse::parse(source, ...);
        let formatted_ast = rumoca_phase_parse::parse(&output, ...);
        assert_eq!(original_ast, formatted_ast, "formatter changed semantics");
    }
    
    output
}

fn reformat_tokens(tokens: &[Token], options: &FormatOptions) -> Vec<Token> {
    let mut result = Vec::new();
    let mut indent_level = 0;
    
    for token in tokens {
        // Adjust trivia based on context
        let mut new_token = token.clone();
        
        // Update leading whitespace for indentation
        new_token.leading_trivia = adjust_leading_trivia(
            &token.leading_trivia,
            indent_level,
            options,
        );
        
        // Track indent level changes
        match token.kind {
            TokenKind::Model | TokenKind::Block | ... => {
                indent_level += 1;
            }
            TokenKind::End => {
                indent_level = indent_level.saturating_sub(1);
            }
            _ => {}
        }
        
        result.push(new_token);
    }
    
    result
}

fn tokens_to_string(tokens: &[Token]) -> String {
    let mut output = String::new();
    
    for token in tokens {
        // Emit leading trivia
        for trivia in &token.leading_trivia {
            match trivia {
                Trivia::Whitespace(s) => output.push_str(s),
                Trivia::LineComment(s) => {
                    output.push_str("// ");
                    output.push_str(s);
                }
                Trivia::BlockComment(s) => {
                    output.push_str("/* ");
                    output.push_str(s);
                    output.push_str(" */");
                }
                Trivia::Newline => output.push('\n'),
            }
        }
        
        // Emit token
        output.push_str(&token.kind.to_string());
        
        // Emit trailing trivia
        for trivia in &token.trailing_trivia {
            // ...
        }
    }
    
    output
}
```

### What Is Preserved

- All comments (may be repositioned)
- Semantic content (verified by re-parsing)
- String literal content

### What Is NOT Preserved

- Original indentation
- Original spacing
- Original line breaks
- Trailing whitespace
- Multiple blank lines (collapsed to one)

### Example

**Input:**
```modelica
model    Test
Real x;Real y;  // position
equation
der(x)=y;der(y)=-9.81; // gravity
end Test;
```

**Output:**
```modelica
model Test
  Real x;
  Real y;  // position
equation
  der(x) = y;
  der(y) = -9.81;  // gravity
end Test;
```

### CLI Integration

```bash
# Format to stdout
rumoca fmt model.mo

# Format in place
rumoca fmt model.mo --write

# Check formatting (exit 1 if changes needed)
rumoca fmt model.mo --check
```

## Rationale
- Token-based preserves comments without AST complexity
- Pretty-printer ensures consistent team-wide style
- Validation by re-parsing catches formatter bugs
- Matches approach of rustfmt, gofmt, prettier

## References
- SPEC_0012: CST vs AST Distinction
- rustfmt: https://github.com/rust-lang/rustfmt
- Prettier: https://prettier.io/docs/en/rationale.html
