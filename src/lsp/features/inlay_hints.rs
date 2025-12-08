//! Inlay Hints handler for Modelica files.
//!
//! Provides inline hints for:
//! - Parameter names in function calls

use std::collections::HashMap;

use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, InlayHintParams, Position, Uri};

use crate::ir::ast::{ClassDefinition, Equation, Expression, Statement};
use crate::ir::transform::constants::{BuiltinFunction, get_builtin_functions};

use crate::lsp::utils::parse_document;

/// Handle inlay hints request
pub fn handle_inlay_hints(
    documents: &HashMap<Uri, String>,
    params: InlayHintParams,
) -> Option<Vec<InlayHint>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();
    let range = params.range;

    let mut hints = Vec::new();
    let builtins: HashMap<&str, &BuiltinFunction> = get_builtin_functions()
        .iter()
        .map(|f| (f.name, f))
        .collect();

    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            collect_class_hints(class, &range, &builtins, &mut hints);
        }
    }

    Some(hints)
}

/// Collect inlay hints from a class
fn collect_class_hints(
    class: &ClassDefinition,
    range: &lsp_types::Range,
    builtins: &HashMap<&str, &BuiltinFunction>,
    hints: &mut Vec<InlayHint>,
) {
    // Collect hints from equations
    for eq in &class.equations {
        collect_equation_hints(eq, range, builtins, hints);
    }

    for eq in &class.initial_equations {
        collect_equation_hints(eq, range, builtins, hints);
    }

    // Collect hints from algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_hints(stmt, range, builtins, hints);
        }
    }

    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_hints(stmt, range, builtins, hints);
        }
    }

    // Recursively process nested classes
    for nested in class.classes.values() {
        collect_class_hints(nested, range, builtins, hints);
    }
}

/// Collect hints from equations
fn collect_equation_hints(
    eq: &Equation,
    range: &lsp_types::Range,
    builtins: &HashMap<&str, &BuiltinFunction>,
    hints: &mut Vec<InlayHint>,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            collect_expression_hints(lhs, range, builtins, hints);
            collect_expression_hints(rhs, range, builtins, hints);
        }
        Equation::For {
            indices: _,
            equations,
        } => {
            for sub_eq in equations {
                collect_equation_hints(sub_eq, range, builtins, hints);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_hints(&block.cond, range, builtins, hints);
                for eq in &block.eqs {
                    collect_equation_hints(eq, range, builtins, hints);
                }
            }
            if let Some(else_eqs) = else_block {
                for eq in else_eqs {
                    collect_equation_hints(eq, range, builtins, hints);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_expression_hints(&block.cond, range, builtins, hints);
                for eq in &block.eqs {
                    collect_equation_hints(eq, range, builtins, hints);
                }
            }
        }
        Equation::FunctionCall { comp: _, args } => {
            for arg in args {
                collect_expression_hints(arg, range, builtins, hints);
            }
        }
        _ => {}
    }
}

/// Collect hints from statements
fn collect_statement_hints(
    stmt: &Statement,
    range: &lsp_types::Range,
    builtins: &HashMap<&str, &BuiltinFunction>,
    hints: &mut Vec<InlayHint>,
) {
    match stmt {
        Statement::Assignment { comp: _, value } => {
            collect_expression_hints(value, range, builtins, hints);
        }
        Statement::For {
            indices: _,
            equations,
        } => {
            for sub_stmt in equations {
                collect_statement_hints(sub_stmt, range, builtins, hints);
            }
        }
        Statement::While(block) => {
            collect_expression_hints(&block.cond, range, builtins, hints);
            for sub_stmt in &block.stmts {
                collect_statement_hints(sub_stmt, range, builtins, hints);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_hints(&block.cond, range, builtins, hints);
                for sub_stmt in &block.stmts {
                    collect_statement_hints(sub_stmt, range, builtins, hints);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    collect_statement_hints(sub_stmt, range, builtins, hints);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                collect_expression_hints(&block.cond, range, builtins, hints);
                for sub_stmt in &block.stmts {
                    collect_statement_hints(sub_stmt, range, builtins, hints);
                }
            }
        }
        Statement::FunctionCall { comp: _, args } => {
            for arg in args {
                collect_expression_hints(arg, range, builtins, hints);
            }
        }
        _ => {}
    }
}

/// Functions where parameter hints are not useful (single obvious parameter)
const SKIP_HINT_FUNCTIONS: &[&str] = &[
    "der",
    "pre",
    "noEvent",
    "edge",
    "change",
    "initial",
    "terminal",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
    "exp",
    "log",
    "log10",
    "sqrt",
    "abs",
    "sign",
    "floor",
    "ceil",
    "sum",
    "product",
    "transpose",
    "ndims",
    "integer",
];

/// Collect hints from expressions (mainly for function call parameter names)
fn collect_expression_hints(
    expr: &Expression,
    range: &lsp_types::Range,
    builtins: &HashMap<&str, &BuiltinFunction>,
    hints: &mut Vec<InlayHint>,
) {
    match expr {
        Expression::FunctionCall { comp, args } => {
            // Get the function name
            let func_name = comp
                .parts
                .first()
                .map(|p| p.ident.text.as_str())
                .unwrap_or("");

            // Skip functions where parameter hints aren't useful
            let should_skip = SKIP_HINT_FUNCTIONS.contains(&func_name);

            // Look up the function in builtins
            if !should_skip && let Some(builtin) = builtins.get(func_name) {
                // Only show hints for functions with multiple parameters
                if builtin.parameters.len() > 1 {
                    // Add parameter name hints for each argument
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(loc) = arg.get_location() {
                            let line = loc.start_line.saturating_sub(1);

                            // Check if in range
                            if line < range.start.line || line > range.end.line {
                                continue;
                            }

                            // Get parameter name from signature
                            if let Some(param_name) =
                                get_param_name_from_signature(builtin.signature, i)
                            {
                                hints.push(InlayHint {
                                    position: Position {
                                        line,
                                        character: loc.start_column.saturating_sub(1),
                                    },
                                    label: InlayHintLabel::String(format!("{}:", param_name)),
                                    kind: Some(InlayHintKind::PARAMETER),
                                    text_edits: None,
                                    tooltip: None,
                                    padding_left: Some(false),
                                    padding_right: Some(true),
                                    data: None,
                                });
                            }
                        }
                    }
                }
            }

            // Recursively check arguments
            for arg in args {
                collect_expression_hints(arg, range, builtins, hints);
            }
        }
        Expression::Binary { lhs, op: _, rhs } => {
            collect_expression_hints(lhs, range, builtins, hints);
            collect_expression_hints(rhs, range, builtins, hints);
        }
        Expression::Unary { op: _, rhs } => {
            collect_expression_hints(rhs, range, builtins, hints);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_expression_hints(elem, range, builtins, hints);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_expression_hints(cond, range, builtins, hints);
                collect_expression_hints(then_expr, range, builtins, hints);
            }
            collect_expression_hints(else_branch, range, builtins, hints);
        }
        _ => {}
    }
}

/// Extract parameter name from a function signature string
fn get_param_name_from_signature(signature: &str, index: usize) -> Option<String> {
    // Parse signature like "sin(x)" or "atan2(y, x)" or "smooth(order, expr)"
    // Handle signatures with return types like "pre(x) -> typeof(x)"
    let start = signature.find('(')?;

    // Find the matching closing parenthesis by counting parens
    let after_open = &signature[start + 1..];
    let mut paren_count = 1;
    let mut end_offset = 0;
    for (i, c) in after_open.char_indices() {
        match c {
            '(' => paren_count += 1,
            ')' => {
                paren_count -= 1;
                if paren_count == 0 {
                    end_offset = i;
                    break;
                }
            }
            _ => {}
        }
    }

    if paren_count != 0 {
        return None; // Unbalanced parentheses
    }

    let params_str = &after_open[..end_offset];
    let params: Vec<&str> = params_str.split(',').map(|s| s.trim()).collect();

    params.get(index).map(|p| {
        // Extract just the parameter name (remove type info if present)
        // Handle formats like "x: Real" or "Real x" or just "x"
        if let Some(colon_pos) = p.find(':') {
            // Format: "x: Real" - take the part before the colon
            p[..colon_pos].trim().to_string()
        } else {
            // Format: "Real x" or just "x" - take the last word
            p.split_whitespace()
                .last()
                .unwrap_or(p)
                .trim_end_matches("...")
                .to_string()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_param_name_simple() {
        assert_eq!(
            get_param_name_from_signature("sin(x)", 0),
            Some("x".to_string())
        );
    }

    #[test]
    fn test_get_param_name_multiple() {
        assert_eq!(
            get_param_name_from_signature("atan2(y, x)", 0),
            Some("y".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("atan2(y, x)", 1),
            Some("x".to_string())
        );
    }

    #[test]
    fn test_get_param_name_with_type() {
        assert_eq!(
            get_param_name_from_signature("smooth(Integer order, Real expr)", 0),
            Some("order".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("smooth(Integer order, Real expr)", 1),
            Some("expr".to_string())
        );
    }

    #[test]
    fn test_get_param_name_with_typeof_return() {
        // pre(x) -> typeof(x) should correctly parse the parameter as "x"
        assert_eq!(
            get_param_name_from_signature("pre(x) -> typeof(x)", 0),
            Some("x".to_string())
        );
        assert_eq!(
            get_param_name_from_signature("noEvent(expr) -> typeof(expr)", 0),
            Some("expr".to_string())
        );
    }

    #[test]
    fn test_get_param_name_colon_format() {
        // Handle "x: Real" format
        assert_eq!(
            get_param_name_from_signature("der(x: Real) -> Real", 0),
            Some("x".to_string())
        );
    }
}
