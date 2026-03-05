# rumoca-phase-parse

Modelica parser phase producing the initial class-tree representation.

## Role in Rumoca
Converts source text into AST structures and parse diagnostics; serves as the first stage for session-driven compilation.

## Public Surface
- Main parsing APIs: `parse_to_ast`, `parse_to_ast_with_errors`.
- Convenience wrappers: `parse_string`, `parse_file`.
- Public parse diagnostics/errors: `ParseError`, `convert_parol_error`, `format_parse_error`.
- Generated parser/trait re-exports for integration code.

## Inputs
- Modelica source text (single file or merged document streams).

## Outputs
- Parsed class-tree IR and parse diagnostics.

## Design Constraints
- Keep parsing syntax-focused (no resolution/type semantics here).
- Preserve precise source spans for downstream diagnostics.
- Keep grammar integration deterministic.
