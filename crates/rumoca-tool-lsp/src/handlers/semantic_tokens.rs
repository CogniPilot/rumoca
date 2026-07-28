//! Semantic tokens handler for Modelica files (rich syntax highlighting).
//!
//! Ported from the main branch's `src/lsp/handlers/semantic_tokens.rs`.

use rumoca_compile::parsing::{self, ast};
use std::ops::ControlFlow::{self, Continue};

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensResult,
};

use crate::traversal_adapter;

type ClassDef = ast::ClassDef;
type ClassType = rumoca_compile::parsing::ir_core::ClassType;
type Component = ast::Component;
type ComponentReference = ast::ComponentReference;
type Expression = ast::Expression;
type StoredDefinition = ast::StoredDefinition;
type TerminalType = ast::TerminalType;
type Variability = parsing::Variability;

// Token type indices (must match order in get_semantic_token_legend)
const TYPE_NAMESPACE: u32 = 0;
const TYPE_TYPE: u32 = 1;
const TYPE_CLASS: u32 = 2;
const TYPE_PARAMETER: u32 = 3;
const TYPE_VARIABLE: u32 = 4;
const TYPE_PROPERTY: u32 = 5;
const TYPE_FUNCTION: u32 = 6;
const TYPE_KEYWORD: u32 = 7;
const TYPE_STRING: u32 = 9;
const TYPE_NUMBER: u32 = 10;

// Modifier bit flags
const MOD_DECLARATION: u32 = 1 << 0;
const MOD_DEFINITION: u32 = 1 << 1;
const MOD_READONLY: u32 = 1 << 2;

/// Get the semantic token legend for server capabilities.
pub fn get_semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE, // 0: package
            SemanticTokenType::TYPE,      // 1: type
            SemanticTokenType::CLASS,     // 2: class
            SemanticTokenType::PARAMETER, // 3: parameter
            SemanticTokenType::VARIABLE,  // 4: variable
            SemanticTokenType::PROPERTY,  // 5: constant
            SemanticTokenType::FUNCTION,  // 6: function
            SemanticTokenType::KEYWORD,   // 7: keyword
            SemanticTokenType::COMMENT,   // 8: comment
            SemanticTokenType::STRING,    // 9: string
            SemanticTokenType::NUMBER,    // 10: number
            SemanticTokenType::OPERATOR,  // 11: operator
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::MODIFICATION,
        ],
    }
}

/// Handle semantic tokens request - provides rich syntax highlighting.
///
/// Takes a parsed AST from `rumoca-compile` plus the text it was parsed from.
/// The source text is required: LSP semantic-token `start`/`length` are UTF-16
/// code-unit counts, which can only be derived from the token's byte span.
pub fn handle_semantic_tokens(
    ast: &StoredDefinition,
    source: &str,
) -> Option<SemanticTokensResult> {
    let mut collector = SemanticTokenCollector::new(source);
    let _ = traversal_adapter::walk_stored_definition(&mut collector, ast);

    // Sort by line then column
    collector
        .tokens
        .sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    // Convert to delta-encoded semantic tokens
    let mut tokens: Vec<SemanticToken> = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for (line, col, length, token_type, token_modifiers) in collector.tokens {
        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            col - prev_start
        } else {
            col
        };

        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: token_modifiers,
        });

        prev_line = line;
        prev_start = col;
    }

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: tokens,
    }))
}

/// Visitor that collects semantic tokens from the AST.
struct SemanticTokenCollector<'a> {
    /// Source text the AST was parsed from, used to measure UTF-16 columns.
    source: &'a str,
    /// Collected: (line, col, length, token_type, token_modifiers)
    tokens: Vec<(u32, u32, u32, u32, u32)>,
}

impl<'a> SemanticTokenCollector<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
        }
    }

    /// Record a token from its source location.
    ///
    /// The LSP semantic-token encoding is `(line, UTF-16 start, UTF-16 length)`
    /// and has no way to express a token that spans lines, so multi-line spans
    /// (multi-line strings, quoted identifiers containing newlines) are dropped
    /// rather than emitted with a bogus length.
    fn add_token_at(&mut self, loc: &parsing::Location, token_type: u32, modifiers: u32) {
        if loc.start_line == 0 || loc.start_column == 0 {
            return;
        }
        let range = crate::helpers::location_to_range_in_source(self.source, loc);
        if range.end.line != range.start.line || range.end.character <= range.start.character {
            return;
        }
        self.tokens.push((
            range.start.line,
            range.start.character,
            range.end.character - range.start.character,
            token_type,
            modifiers,
        ));
    }

    fn add_class_tokens(&mut self, class: &ClassDef) {
        // Class type keyword (model, class, function, etc.)
        if class.class_type_token.location.start_line > 0 {
            self.add_token_at(&class.class_type_token.location, TYPE_KEYWORD, 0);
        }

        // Class name
        let class_type_idx = match class.class_type {
            ClassType::Package => TYPE_NAMESPACE,
            ClassType::Function => TYPE_FUNCTION,
            ClassType::Type => TYPE_TYPE,
            _ => TYPE_CLASS,
        };
        self.add_token_at(&class.name.location, class_type_idx, MOD_DEFINITION);
    }

    fn add_component_tokens(&mut self, comp: &Component) {
        let (token_type, modifiers) = match (&comp.variability, &comp.causality) {
            (Variability::Parameter(_), _) => (TYPE_PARAMETER, MOD_DECLARATION | MOD_READONLY),
            (Variability::Constant(_), _) => (TYPE_PROPERTY, MOD_DECLARATION | MOD_READONLY),
            _ => (TYPE_VARIABLE, MOD_DECLARATION),
        };

        // Type name
        if let Some(first_token) = comp.type_name.name.first() {
            self.add_token_at(&first_token.location, TYPE_TYPE, 0);
        }

        // Component name
        self.add_token_at(&comp.name_token.location, token_type, modifiers);
    }

    fn add_component_reference_tokens(&mut self, cr: &ComponentReference, token_type: u32) {
        for part in &cr.parts {
            self.add_token_at(&part.ident.location, token_type, 0);
        }
    }

    fn call_token_type(comp: &ComponentReference) -> u32 {
        // Modelica defines operator-like builtins that are spelled as call syntax.
        // Highlight these as keywords, and all other call heads as functions.
        let Some(first) = comp.parts.first() else {
            return TYPE_FUNCTION;
        };
        if comp.parts.len() == 1 && is_modelica_operator_keyword(&first.ident.text) {
            TYPE_KEYWORD
        } else {
            TYPE_FUNCTION
        }
    }

    fn add_call_head_tokens(&mut self, comp: &ComponentReference) {
        self.add_component_reference_tokens(comp, Self::call_token_type(comp));
    }
}

fn is_modelica_operator_keyword(name: &str) -> bool {
    matches!(
        name,
        "der"
            | "initial"
            | "sample"
            | "pre"
            | "edge"
            | "change"
            | "noEvent"
            | "inStream"
            | "actualStream"
            | "reinit"
            | "assert"
            | "terminate"
            | "homotopy"
            | "semiLinear"
            | "spatialDistribution"
            | "delay"
            | "cardinality"
            | "getInstanceName"
    )
}

impl ast::visitor::Visitor for SemanticTokenCollector<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        self.add_class_tokens(class);
        traversal_adapter::walk_class_sections(self, class, true)
    }

    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        self.add_component_tokens(comp);
        traversal_adapter::walk_component_fields(self, comp)
    }

    fn visit_expression(&mut self, expr: &Expression) -> ControlFlow<()> {
        // Handle terminal tokens (numbers, strings, bools)
        if let Expression::Terminal {
            terminal_type,
            token,
            ..
        } = expr
        {
            let tt = match terminal_type {
                TerminalType::UnsignedInteger | TerminalType::UnsignedReal => TYPE_NUMBER,
                TerminalType::String => TYPE_STRING,
                TerminalType::Bool => TYPE_NUMBER,
                TerminalType::Empty | TerminalType::End => return Continue(()),
            };
            self.add_token_at(&token.location, tt, 0);
            return Continue(());
        }

        traversal_adapter::walk_expression_default(self, expr)
    }

    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.visit_expr_function_call_ctx(comp, args, ast::visitor::FunctionCallContext::Expression)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        _ctx: ast::visitor::FunctionCallContext,
    ) -> ControlFlow<()> {
        self.add_call_head_tokens(comp);
        self.visit_each(args, Self::visit_expression)
    }

    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        // Color variable references
        self.add_component_reference_tokens(cr, TYPE_VARIABLE);
        // Visit subscripts
        for part in &cr.parts {
            if let Some(subs) = &part.subs {
                self.visit_each(subs, Self::visit_subscript)?;
            }
        }
        Continue(())
    }

    fn visit_equation_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.add_call_head_tokens(comp);
        self.visit_each(args, Self::visit_expression)
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[Expression],
    ) -> ControlFlow<()> {
        self.add_call_head_tokens(comp);
        self.visit_each(args, Self::visit_expression)?;
        self.visit_each(outputs, Self::visit_expression)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::parsing::parse_source_to_ast;

    fn decode_tokens(tokens: &[SemanticToken]) -> Vec<(u32, u32, u32, u32)> {
        let mut decoded = Vec::with_capacity(tokens.len());
        let mut line = 0u32;
        let mut col = 0u32;
        for token in tokens {
            line += token.delta_line;
            col = if token.delta_line == 0 {
                col + token.delta_start
            } else {
                token.delta_start
            };
            decoded.push((line, col, token.length, token.token_type));
        }
        decoded
    }

    /// Decode a `(line, UTF-16 column, UTF-16 length)` triple back to text, so
    /// assertions read the same units the protocol carries.
    fn lexeme_at(source: &str, line: u32, col: u32, len: u32) -> String {
        let Some(text) = rumoca_lsp_position::line_text(source, line) else {
            return String::new();
        };
        let start = rumoca_lsp_position::utf16_column_to_byte_column(text, col);
        let end = rumoca_lsp_position::utf16_column_to_byte_column(text, col + len);
        text.get(start..end)
            .map(ToString::to_string)
            .unwrap_or_else(String::new)
    }

    fn assert_no_overlaps(source: &str, decoded: &[(u32, u32, u32, u32)]) {
        let mut previous: Option<(u32, u32, u32, u32)> = None;
        for current in decoded {
            if let Some(prev) = previous {
                assert!(
                    current.0 > prev.0 || current.1 >= prev.1 + prev.2,
                    "semantic token overlap in `{}`: previous {:?} `{}`, current {:?} `{}`",
                    source,
                    prev,
                    lexeme_at(source, prev.0, prev.1, prev.2),
                    current,
                    lexeme_at(source, current.0, current.1, current.2),
                );
            }
            previous = Some(*current);
        }
    }

    fn semantic_tokens(source: &str) -> Vec<SemanticToken> {
        let ast = parse_source_to_ast(source, "test.mo").expect("parse should succeed");
        let result =
            handle_semantic_tokens(&ast, source).expect("semantic tokens should be available");
        match result {
            SemanticTokensResult::Tokens(tokens) => tokens.data,
            SemanticTokensResult::Partial(_) => panic!("unexpected partial semantic tokens"),
        }
    }

    #[test]
    fn highlights_reinit_as_keyword_in_when_equation() {
        let source = r#"
model Ball
  Real x(start=1);
  Real v(start=0);
equation
  der(x) = v;
  der(v) = -9.81;
  when x < 0 then
    reinit(v, -0.6 * pre(v));
  end when;
end Ball;
"#;
        let decoded = decode_tokens(&semantic_tokens(source));
        let found_reinit_keyword = decoded.into_iter().any(|(line, col, len, token_type)| {
            token_type == TYPE_KEYWORD && lexeme_at(source, line, col, len) == "reinit"
        });
        assert!(
            found_reinit_keyword,
            "expected `reinit` keyword semantic token"
        );
    }

    #[test]
    fn keeps_regular_function_calls_as_function_tokens() {
        let source = r#"
model M
  Real x;
equation
  x = sin(x);
end M;
"#;
        let decoded = decode_tokens(&semantic_tokens(source));
        let found_sin_function = decoded.into_iter().any(|(line, col, len, token_type)| {
            token_type == TYPE_FUNCTION && lexeme_at(source, line, col, len) == "sin"
        });
        assert!(
            found_sin_function,
            "expected regular call head `sin` to remain a function token"
        );
    }

    #[test]
    fn semantic_tokens_do_not_overlap_for_equation_calls() {
        let source = r#"
model Ball
  Real x(start=10);
  Real v(start=1);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    // terminate("Ball has hit the ground");
    reinit(v, -0.8*pre(v));
  end when;
end Ball;
"#;
        let decoded = decode_tokens(&semantic_tokens(source));
        assert_no_overlaps(source, &decoded);
    }

    #[test]
    fn token_length_counts_utf16_units_not_bytes() {
        // The string literal `"温度"` is 4 UTF-16 units but 8 UTF-8 bytes.
        // Emitting `text.len()` would overrun the line and shift every later
        // token by the delta encoding.
        let source = "model M\n  String s = \"温度\";\n  Real x;\nend M;\n";
        let decoded = decode_tokens(&semantic_tokens(source));
        let literal = decoded
            .iter()
            .copied()
            .find(|&(line, col, len, kind)| {
                kind == TYPE_STRING && lexeme_at(source, line, col, len).starts_with('"')
            })
            .expect("string literal token");
        assert_eq!(
            lexeme_at(source, literal.0, literal.1, literal.2),
            "\"温度\"",
            "decoded token was {literal:?}"
        );
        assert_eq!(literal.2, 4, "expected 4 UTF-16 units, got {}", literal.2);
    }

    #[test]
    fn token_columns_shift_by_utf16_units_after_non_ascii_text() {
        // The `x` component name sits after a two-unit astral character, so its
        // UTF-16 column is one greater than its lexer character column.
        let source = "model M\n  String s = \"𝔸\"; Real x;\nend M;\n";
        let decoded = decode_tokens(&semantic_tokens(source));
        let x_token = decoded
            .iter()
            .find(|&&(line, col, len, _)| line == 1 && lexeme_at(source, line, col, len) == "x")
            .expect("component `x` token");
        let expected = rumoca_lsp_position::byte_offset_to_position(
            source,
            source.rfind("x;").expect("component name present"),
        );
        assert_eq!(x_token.1, expected.character);
        // Guard the actual regression: the lexer's character column would be
        // one short of the UTF-16 column on this line.
        let line = source.lines().nth(1).expect("line 1");
        let name_byte = line.find("x;").expect("component name on line 1");
        let char_column = line[..name_byte].chars().count();
        assert!(
            expected.character as usize > char_column,
            "test line must expose the char-column/UTF-16 divergence \
             (utf16={}, chars={char_column})",
            expected.character
        );
    }
}
