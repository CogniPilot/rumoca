//! Semantic tokens handler for Modelica files (rich syntax highlighting).
//!
//! Ported from the main branch's `src/lsp/handlers/semantic_tokens.rs`.

use rumoca_ir_ast as ast;
use std::ops::ControlFlow::{self, Continue};

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensResult,
};

type ClassDef = ast::ClassDef;
type ClassType = ast::ClassType;
type Component = ast::Component;
type ComponentReference = ast::ComponentReference;
type Expression = ast::Expression;
type StoredDefinition = ast::StoredDefinition;
type TerminalType = ast::TerminalType;
type Variability = ast::Variability;

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
/// Takes a parsed AST from `rumoca-session`.
pub fn handle_semantic_tokens(ast: &StoredDefinition) -> Option<SemanticTokensResult> {
    let mut collector = SemanticTokenCollector::new();
    let _ = ast::visitor::Visitor::visit_stored_definition(&mut collector, ast);

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
struct SemanticTokenCollector {
    /// Collected: (line, col, length, token_type, token_modifiers)
    tokens: Vec<(u32, u32, u32, u32, u32)>,
}

impl SemanticTokenCollector {
    fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    fn add_token(&mut self, line: u32, col: u32, len: u32, token_type: u32, modifiers: u32) {
        if line == 0 || col == 0 || len == 0 {
            return;
        }
        self.tokens.push((
            line.saturating_sub(1),
            col.saturating_sub(1),
            len,
            token_type,
            modifiers,
        ));
    }

    fn add_class_tokens(&mut self, class: &ClassDef) {
        // Class type keyword (model, class, function, etc.)
        if class.class_type_token.location.start_line > 0 {
            self.add_token(
                class.class_type_token.location.start_line,
                class.class_type_token.location.start_column,
                class.class_type_token.text.len() as u32,
                TYPE_KEYWORD,
                0,
            );
        }

        // Class name
        let class_type_idx = match class.class_type {
            ClassType::Package => TYPE_NAMESPACE,
            ClassType::Function => TYPE_FUNCTION,
            ClassType::Type => TYPE_TYPE,
            _ => TYPE_CLASS,
        };
        self.add_token(
            class.name.location.start_line,
            class.name.location.start_column,
            class.name.text.len() as u32,
            class_type_idx,
            MOD_DEFINITION,
        );
    }

    fn add_component_tokens(&mut self, comp: &Component) {
        let (token_type, modifiers) = match (&comp.variability, &comp.causality) {
            (Variability::Parameter(_), _) => (TYPE_PARAMETER, MOD_DECLARATION | MOD_READONLY),
            (Variability::Constant(_), _) => (TYPE_PROPERTY, MOD_DECLARATION | MOD_READONLY),
            _ => (TYPE_VARIABLE, MOD_DECLARATION),
        };

        // Type name
        if let Some(first_token) = comp.type_name.name.first() {
            self.add_token(
                first_token.location.start_line,
                first_token.location.start_column,
                first_token.text.len() as u32,
                TYPE_TYPE,
                0,
            );
        }

        // Component name
        self.add_token(
            comp.name_token.location.start_line,
            comp.name_token.location.start_column,
            comp.name_token.text.len() as u32,
            token_type,
            modifiers,
        );
    }

    fn add_component_reference_tokens(&mut self, cr: &ComponentReference, token_type: u32) {
        for part in &cr.parts {
            self.add_token(
                part.ident.location.start_line,
                part.ident.location.start_column,
                part.ident.text.len() as u32,
                token_type,
                0,
            );
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

impl ast::visitor::Visitor for SemanticTokenCollector {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        self.add_class_tokens(class);

        // Visit children (default behavior)
        for ext in &class.extends {
            self.visit_extend(ext)?;
        }
        for (_, nested) in &class.classes {
            self.visit_class_def(nested)?;
        }
        for (_, comp) in &class.components {
            self.visit_component(comp)?;
        }
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for section in &class.algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        for section in &class.initial_algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        Continue(())
    }

    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        self.add_component_tokens(comp);

        // Visit children (default behavior)
        if !matches!(comp.start, Expression::Empty) {
            self.visit_expression(&comp.start)?;
        }
        for (_, mod_expr) in &comp.modifications {
            self.visit_expression(mod_expr)?;
        }
        if let Some(cond) = &comp.condition {
            self.visit_expression(cond)?;
        }
        self.visit_each(&comp.annotation, Self::visit_expression)
    }

    fn visit_expression(&mut self, expr: &Expression) -> ControlFlow<()> {
        // Handle terminal tokens (numbers, strings, bools)
        if let Expression::Terminal {
            terminal_type,
            token,
        } = expr
        {
            let tt = match terminal_type {
                TerminalType::UnsignedInteger | TerminalType::UnsignedReal => TYPE_NUMBER,
                TerminalType::String => TYPE_STRING,
                TerminalType::Bool => TYPE_NUMBER,
                TerminalType::Empty | TerminalType::End => return Continue(()),
            };
            self.add_token(
                token.location.start_line,
                token.location.start_column,
                token.text.len() as u32,
                tt,
                0,
            );
            return Continue(());
        }

        // Default recursive behavior for all other expression types
        default_visit_expression(self, expr)
    }

    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.add_call_head_tokens(comp);
        // Visit arguments
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

/// Default expression visitor behavior (copied from trait to allow override).
fn default_visit_expression(v: &mut SemanticTokenCollector, expr: &Expression) -> ControlFlow<()> {
    match expr {
        Expression::Empty | Expression::Terminal { .. } => Continue(()),
        Expression::Range { start, step, end } => {
            ast::visitor::Visitor::visit_expression(v, start)?;
            if let Some(s) = step {
                ast::visitor::Visitor::visit_expression(v, s)?;
            }
            ast::visitor::Visitor::visit_expression(v, end)
        }
        Expression::Unary { rhs, .. } => ast::visitor::Visitor::visit_expression(v, rhs),
        Expression::Binary { lhs, rhs, .. } => {
            ast::visitor::Visitor::visit_expression(v, lhs)?;
            ast::visitor::Visitor::visit_expression(v, rhs)
        }
        Expression::ComponentReference(cr) => {
            ast::visitor::Visitor::visit_component_reference(v, cr)
        }
        Expression::FunctionCall { comp, args } => {
            ast::visitor::Visitor::visit_expr_function_call(v, comp, args)
        }
        Expression::ClassModification {
            target,
            modifications,
        } => {
            ast::visitor::Visitor::visit_component_reference(v, target)?;
            ast::visitor::Visitor::visit_each(
                v,
                modifications,
                ast::visitor::Visitor::visit_expression,
            )
        }
        Expression::NamedArgument { value, .. } => {
            ast::visitor::Visitor::visit_expression(v, value)
        }
        Expression::Modification { target, value } => {
            ast::visitor::Visitor::visit_component_reference(v, target)?;
            ast::visitor::Visitor::visit_expression(v, value)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            ast::visitor::Visitor::visit_each(v, elements, ast::visitor::Visitor::visit_expression)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                ast::visitor::Visitor::visit_expression(v, cond)?;
                ast::visitor::Visitor::visit_expression(v, then_expr)?;
            }
            ast::visitor::Visitor::visit_expression(v, else_branch)
        }
        Expression::Parenthesized { inner } => ast::visitor::Visitor::visit_expression(v, inner),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            ast::visitor::Visitor::visit_expression(v, expr)?;
            ast::visitor::Visitor::visit_each(v, indices, ast::visitor::Visitor::visit_for_index)?;
            if let Some(f) = filter {
                ast::visitor::Visitor::visit_expression(v, f)?;
            }
            Continue(())
        }
        Expression::ArrayIndex { base, subscripts } => {
            ast::visitor::Visitor::visit_expression(v, base)?;
            ast::visitor::Visitor::visit_each(v, subscripts, ast::visitor::Visitor::visit_subscript)
        }
        Expression::FieldAccess { base, .. } => ast::visitor::Visitor::visit_expression(v, base),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_phase_parse::parse_to_ast;

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

    fn lexeme_at(source: &str, line: u32, col: u32, len: u32) -> String {
        source
            .lines()
            .nth(line as usize)
            .unwrap_or_default()
            .chars()
            .skip(col as usize)
            .take(len as usize)
            .collect()
    }

    fn semantic_tokens(source: &str) -> Vec<SemanticToken> {
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = handle_semantic_tokens(&ast).expect("semantic tokens should be available");
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
}
