//! Semantic tokens handler for Modelica files (rich syntax highlighting).

use std::collections::HashMap;

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensParams, SemanticTokensResult, Uri,
};

use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Component, ComponentReference, Expression, Variability,
};
use crate::ir::visitor::{Visitable, Visitor};

use crate::lsp::utils::parse_document;

// Token type indices (must match the order in get_semantic_token_legend)
const TYPE_NAMESPACE: u32 = 0;
const TYPE_TYPE: u32 = 1;
const TYPE_CLASS: u32 = 2;
const TYPE_PARAMETER: u32 = 3;
const TYPE_VARIABLE: u32 = 4;
const TYPE_PROPERTY: u32 = 5; // constant
const TYPE_FUNCTION: u32 = 6;
const TYPE_KEYWORD: u32 = 7;
const TYPE_STRING: u32 = 9;
const TYPE_NUMBER: u32 = 10;

// Modifier bit flags
const MOD_DECLARATION: u32 = 1 << 0;
const MOD_DEFINITION: u32 = 1 << 1;
const MOD_READONLY: u32 = 1 << 2;

/// Get the semantic token legend for server capabilities
pub fn get_semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE, // 0: package
            SemanticTokenType::TYPE,      // 1: type (model, block, connector, record)
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
            SemanticTokenModifier::DECLARATION,  // 0: declaration
            SemanticTokenModifier::DEFINITION,   // 1: definition
            SemanticTokenModifier::READONLY,     // 2: readonly (constant/parameter)
            SemanticTokenModifier::MODIFICATION, // 3: modification
        ],
    }
}

/// Visitor that collects semantic tokens from the AST
struct SemanticTokenCollector {
    /// Collected token data: (line, col, length, token_type, token_modifiers)
    tokens: Vec<(u32, u32, u32, u32, u32)>,
    /// Track if we're inside a function call (for coloring references as functions)
    in_function_call: bool,
}

impl SemanticTokenCollector {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            in_function_call: false,
        }
    }

    fn add_token(&mut self, line: u32, col: u32, len: u32, token_type: u32, modifiers: u32) {
        // Skip tokens with invalid locations (line=0 or col=0 means uninitialized/default)
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
}

impl Visitor for SemanticTokenCollector {
    fn enter_class_definition(&mut self, node: &ClassDefinition) {
        // Add class type keyword token (model, class, function, etc.)
        // This ensures "model" is highlighted as a keyword, preventing "mod" from taking precedence
        if node.class_type_token.location.start_line > 0 {
            self.add_token(
                node.class_type_token.location.start_line,
                node.class_type_token.location.start_column,
                node.class_type_token.text.len() as u32,
                TYPE_KEYWORD,
                0,
            );
        }

        // Add class name token
        let class_type_idx = match node.class_type {
            ClassType::Package => TYPE_NAMESPACE,
            ClassType::Function => TYPE_FUNCTION,
            ClassType::Type => TYPE_TYPE,
            _ => TYPE_CLASS,
        };

        self.add_token(
            node.name.location.start_line,
            node.name.location.start_column,
            node.name.text.len() as u32,
            class_type_idx,
            MOD_DEFINITION,
        );
    }

    fn enter_component(&mut self, node: &Component) {
        let (token_type, modifiers) = match (&node.variability, &node.causality) {
            (Variability::Parameter(_), _) => (TYPE_PARAMETER, MOD_DECLARATION | MOD_READONLY),
            (Variability::Constant(_), _) => (TYPE_PROPERTY, MOD_DECLARATION | MOD_READONLY),
            (_, Causality::Input(_)) => (TYPE_VARIABLE, MOD_DECLARATION),
            (_, Causality::Output(_)) => (TYPE_VARIABLE, MOD_DECLARATION),
            _ => (TYPE_VARIABLE, MOD_DECLARATION),
        };

        // Add the type name token
        if let Some(first_token) = node.type_name.name.first() {
            self.add_token(
                first_token.location.start_line,
                first_token.location.start_column,
                first_token.text.len() as u32,
                TYPE_TYPE,
                0,
            );
        }

        // Add component name token using the stored name_token
        self.add_token(
            node.name_token.location.start_line,
            node.name_token.location.start_column,
            node.name_token.text.len() as u32,
            token_type,
            modifiers,
        );
    }

    fn enter_expression(&mut self, node: &Expression) {
        match node {
            Expression::Terminal {
                terminal_type,
                token,
            } => {
                let token_type = match terminal_type {
                    crate::ir::ast::TerminalType::UnsignedInteger => TYPE_NUMBER,
                    crate::ir::ast::TerminalType::UnsignedReal => TYPE_NUMBER,
                    crate::ir::ast::TerminalType::String => TYPE_STRING,
                    crate::ir::ast::TerminalType::Bool => TYPE_NUMBER,
                    crate::ir::ast::TerminalType::Empty | crate::ir::ast::TerminalType::End => {
                        return;
                    }
                };
                self.add_token(
                    token.location.start_line,
                    token.location.start_column,
                    token.text.len() as u32,
                    token_type,
                    0,
                );
            }
            Expression::FunctionCall { .. } => {
                // Set flag so component_reference knows to color as function
                self.in_function_call = true;
            }
            _ => {}
        }
    }

    fn exit_expression(&mut self, node: &Expression) {
        if matches!(node, Expression::FunctionCall { .. }) {
            self.in_function_call = false;
        }
    }

    fn enter_component_reference(&mut self, node: &ComponentReference) {
        let token_type = if self.in_function_call {
            TYPE_FUNCTION
        } else {
            TYPE_VARIABLE
        };

        for part in &node.parts {
            self.add_token(
                part.ident.location.start_line,
                part.ident.location.start_column,
                part.ident.text.len() as u32,
                token_type,
                0,
            );
        }

        // Reset function call flag after processing the function name
        if self.in_function_call {
            self.in_function_call = false;
        }
    }
}

/// Handle semantic tokens request - provides rich syntax highlighting
pub fn handle_semantic_tokens(
    documents: &HashMap<Uri, String>,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let ast = parse_document(text, path)?;

    // Use visitor to collect tokens
    let mut collector = SemanticTokenCollector::new();
    ast.accept(&mut collector);

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
