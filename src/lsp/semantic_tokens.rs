//! Semantic tokens handler for Modelica files (rich syntax highlighting).

use std::collections::HashMap;

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensLegend, SemanticTokensParams, SemanticTokensResult, Uri,
};

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Expression, Variability};

use super::utils::parse_document;

// Token type indices (must match the order in get_semantic_token_legend)
const TYPE_NAMESPACE: u32 = 0;
const TYPE_TYPE: u32 = 1;
const TYPE_CLASS: u32 = 2;
const TYPE_PARAMETER: u32 = 3;
const TYPE_VARIABLE: u32 = 4;
const TYPE_PROPERTY: u32 = 5; // constant
const TYPE_FUNCTION: u32 = 6;
#[allow(dead_code)]
const TYPE_KEYWORD: u32 = 7;
#[allow(dead_code)]
const TYPE_COMMENT: u32 = 8;
const TYPE_STRING: u32 = 9;
const TYPE_NUMBER: u32 = 10;
#[allow(dead_code)]
const TYPE_OPERATOR: u32 = 11;

// Modifier bit flags
const MOD_DECLARATION: u32 = 1 << 0;
const MOD_DEFINITION: u32 = 1 << 1;
const MOD_READONLY: u32 = 1 << 2;

/// Get the semantic token legend for server capabilities
pub fn get_semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE,    // 0: package
            SemanticTokenType::TYPE,         // 1: type (model, block, connector, record)
            SemanticTokenType::CLASS,        // 2: class
            SemanticTokenType::PARAMETER,    // 3: parameter
            SemanticTokenType::VARIABLE,     // 4: variable
            SemanticTokenType::PROPERTY,     // 5: constant
            SemanticTokenType::FUNCTION,     // 6: function
            SemanticTokenType::KEYWORD,      // 7: keyword
            SemanticTokenType::COMMENT,      // 8: comment
            SemanticTokenType::STRING,       // 9: string
            SemanticTokenType::NUMBER,       // 10: number
            SemanticTokenType::OPERATOR,     // 11: operator
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,  // 0: declaration
            SemanticTokenModifier::DEFINITION,   // 1: definition
            SemanticTokenModifier::READONLY,     // 2: readonly (constant/parameter)
            SemanticTokenModifier::MODIFICATION, // 3: modification
        ],
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
    let mut tokens: Vec<SemanticToken> = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    // Collect all tokens from the AST with their positions
    let mut token_data: Vec<(u32, u32, u32, u32, u32)> = Vec::new();

    for class in ast.class_list.values() {
        collect_class_tokens(class, &mut token_data);
    }

    // Sort by line then column
    token_data.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    // Convert to delta-encoded semantic tokens
    for (line, col, length, token_type, token_modifiers) in token_data {
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

/// Collect semantic tokens from a class definition
fn collect_class_tokens(class: &ClassDefinition, tokens: &mut Vec<(u32, u32, u32, u32, u32)>) {
    // Add class name token
    let class_type_idx = match class.class_type {
        ClassType::Package => TYPE_NAMESPACE,
        ClassType::Function => TYPE_FUNCTION,
        ClassType::Type => TYPE_TYPE,
        _ => TYPE_CLASS,
    };

    tokens.push((
        class.name.location.start_line.saturating_sub(1),
        class.name.location.start_column.saturating_sub(1),
        class.name.text.len() as u32,
        class_type_idx,
        MOD_DEFINITION,
    ));

    // Add component tokens
    for (comp_name, comp) in &class.components {
        let (token_type, modifiers) = match (&comp.variability, &comp.causality) {
            (Variability::Parameter(_), _) => (TYPE_PARAMETER, MOD_DECLARATION | MOD_READONLY),
            (Variability::Constant(_), _) => (TYPE_PROPERTY, MOD_DECLARATION | MOD_READONLY),
            (_, Causality::Input(_)) => (TYPE_VARIABLE, MOD_DECLARATION),
            (_, Causality::Output(_)) => (TYPE_VARIABLE, MOD_DECLARATION),
            _ => (TYPE_VARIABLE, MOD_DECLARATION),
        };

        // Get position from type_name location
        if let Some(first_token) = comp.type_name.name.first() {
            // Add the type name token
            tokens.push((
                first_token.location.start_line.saturating_sub(1),
                first_token.location.start_column.saturating_sub(1),
                first_token.text.len() as u32,
                TYPE_TYPE,
                0,
            ));
        }

        // Component name position (estimated)
        if let Some(first_token) = comp.type_name.name.first() {
            let type_end =
                first_token.location.start_column.saturating_sub(1) + first_token.text.len() as u32;
            tokens.push((
                first_token.location.start_line.saturating_sub(1),
                type_end + 1,
                comp_name.len() as u32,
                token_type,
                modifiers,
            ));
        }
    }

    // Add tokens from equations
    for eq in &class.equations {
        collect_equation_tokens(eq, tokens, TYPE_VARIABLE);
    }

    for eq in &class.initial_equations {
        collect_equation_tokens(eq, tokens, TYPE_VARIABLE);
    }

    // Process nested classes
    for nested_class in class.classes.values() {
        collect_class_tokens(nested_class, tokens);
    }
}

/// Collect tokens from an equation
fn collect_equation_tokens(
    eq: &crate::ir::ast::Equation,
    tokens: &mut Vec<(u32, u32, u32, u32, u32)>,
    default_type: u32,
) {
    match eq {
        crate::ir::ast::Equation::Empty => {}
        crate::ir::ast::Equation::Simple { lhs, rhs } => {
            collect_expression_tokens(lhs, tokens, default_type);
            collect_expression_tokens(rhs, tokens, default_type);
        }
        crate::ir::ast::Equation::Connect { lhs, rhs } => {
            collect_component_ref_tokens(lhs, tokens, default_type);
            collect_component_ref_tokens(rhs, tokens, default_type);
        }
        crate::ir::ast::Equation::For { equations, .. } => {
            for sub_eq in equations {
                collect_equation_tokens(sub_eq, tokens, default_type);
            }
        }
        crate::ir::ast::Equation::When(blocks) => {
            for block in blocks {
                collect_expression_tokens(&block.cond, tokens, default_type);
                for sub_eq in &block.eqs {
                    collect_equation_tokens(sub_eq, tokens, default_type);
                }
            }
        }
        crate::ir::ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_tokens(&block.cond, tokens, default_type);
                for sub_eq in &block.eqs {
                    collect_equation_tokens(sub_eq, tokens, default_type);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_equation_tokens(sub_eq, tokens, default_type);
                }
            }
        }
        crate::ir::ast::Equation::FunctionCall { comp, args } => {
            collect_component_ref_tokens(comp, tokens, TYPE_FUNCTION);
            for arg in args {
                collect_expression_tokens(arg, tokens, default_type);
            }
        }
    }
}

/// Collect tokens from a component reference
fn collect_component_ref_tokens(
    comp_ref: &crate::ir::ast::ComponentReference,
    tokens: &mut Vec<(u32, u32, u32, u32, u32)>,
    token_type: u32,
) {
    for part in &comp_ref.parts {
        tokens.push((
            part.ident.location.start_line.saturating_sub(1),
            part.ident.location.start_column.saturating_sub(1),
            part.ident.text.len() as u32,
            token_type,
            0,
        ));
        if let Some(subs) = &part.subs {
            for sub in subs {
                collect_subscript_tokens(sub, tokens, token_type);
            }
        }
    }
}

/// Collect tokens from a subscript
fn collect_subscript_tokens(
    sub: &crate::ir::ast::Subscript,
    tokens: &mut Vec<(u32, u32, u32, u32, u32)>,
    default_type: u32,
) {
    match sub {
        crate::ir::ast::Subscript::Empty => {}
        crate::ir::ast::Subscript::Expression(expr) => {
            collect_expression_tokens(expr, tokens, default_type);
        }
        crate::ir::ast::Subscript::Range { .. } => {}
    }
}

/// Collect tokens from expressions
fn collect_expression_tokens(
    expr: &Expression,
    tokens: &mut Vec<(u32, u32, u32, u32, u32)>,
    default_type: u32,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_component_ref_tokens(comp_ref, tokens, default_type);
        }
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
            tokens.push((
                token.location.start_line.saturating_sub(1),
                token.location.start_column.saturating_sub(1),
                token.text.len() as u32,
                token_type,
                0,
            ));
        }
        Expression::FunctionCall { comp, args } => {
            collect_component_ref_tokens(comp, tokens, TYPE_FUNCTION);
            for arg in args {
                collect_expression_tokens(arg, tokens, default_type);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_expression_tokens(lhs, tokens, default_type);
            collect_expression_tokens(rhs, tokens, default_type);
        }
        Expression::Unary { rhs, .. } => {
            collect_expression_tokens(rhs, tokens, default_type);
        }
        Expression::Array { elements } => {
            for element in elements {
                collect_expression_tokens(element, tokens, default_type);
            }
        }
        Expression::Tuple { elements } => {
            for element in elements {
                collect_expression_tokens(element, tokens, default_type);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_expression_tokens(cond, tokens, default_type);
                collect_expression_tokens(then_expr, tokens, default_type);
            }
            collect_expression_tokens(else_branch, tokens, default_type);
        }
        Expression::Range { start, step, end } => {
            collect_expression_tokens(start, tokens, default_type);
            if let Some(step_expr) = step {
                collect_expression_tokens(step_expr, tokens, default_type);
            }
            collect_expression_tokens(end, tokens, default_type);
        }
    }
}
