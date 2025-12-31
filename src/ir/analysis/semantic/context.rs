//! Control flow context validation.
//!
//! Validates break/return usage and dimension matching.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, ClassType, Expression, Statement};

use crate::ir::analysis::type_inference::SymbolType;

use super::types::{has_inferred_dimensions, infer_expression_shape};
use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for break/return statements used outside of valid context.
///
/// This validates that:
/// - `break` is only used inside for/while loops
/// - `return` is only used inside functions
pub fn check_break_return_context(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Determine if this is a function (where return is allowed)
    let is_function = matches!(class.class_type, ClassType::Function);

    // Check all algorithm statements
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_loop_context(stmt, false, is_function, &mut result);
        }
    }

    // Check initial algorithms
    for algorithm_block in &class.initial_algorithms {
        for stmt in algorithm_block {
            check_statement_loop_context(stmt, false, is_function, &mut result);
        }
    }

    // Recursively check nested classes
    for (_name, nested_class) in &class.classes {
        let nested_result = check_break_return_context(nested_class);
        for error in nested_result.errors {
            result.add_error(error);
        }
    }

    result
}

/// Check a statement for break/return context violations
fn check_statement_loop_context(
    stmt: &Statement,
    in_loop: bool,
    is_function: bool,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Break { token } => {
            if !in_loop {
                result.add_error(TypeError::new(
                    token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "'break' may only be used in a while- or for-loop.".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Statement::Return { token } => {
            if !is_function {
                result.add_error(TypeError::new(
                    token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "'return' may only be used in a function.".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Statement::For { equations, .. } => {
            // We're now inside a loop
            for sub_stmt in equations {
                check_statement_loop_context(sub_stmt, true, is_function, result);
            }
        }
        Statement::While(block) => {
            // We're now inside a loop
            for sub_stmt in &block.stmts {
                check_statement_loop_context(sub_stmt, true, is_function, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            // If statements don't change loop context
            for block in cond_blocks {
                for sub_stmt in &block.stmts {
                    check_statement_loop_context(sub_stmt, in_loop, is_function, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_loop_context(sub_stmt, in_loop, is_function, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for sub_stmt in &block.stmts {
                    check_statement_loop_context(sub_stmt, in_loop, is_function, result);
                }
            }
        }
        _ => {}
    }
}

/// Check that start modification dimensions match component dimensions.
///
/// For components with inferred dimensions (using `:`) in their bindings,
/// this checks that the start modification's dimensions match the expected shape.
///
/// Examples that should fail:
/// - `parameter Real x_start[:] = {1, 2}; Real x[3](start = x_start);` - shape `[2]` vs `[3]`
/// - `parameter Real x_start[:,:] = {{1,2,3,4},{5,6,7,8}}; Real x[3](start = x_start[1]);` - shape `[4]` vs `[3]`
pub fn check_start_modification_dimensions(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of parameter names to their inferred shapes
    // This handles parameters with inferred dimensions like `parameter Real x[:] = {1, 2}`
    let mut param_shapes: HashMap<String, Vec<usize>> = HashMap::new();

    for (name, comp) in &class.components {
        // If this parameter has inferred dimensions, infer shape from binding
        if has_inferred_dimensions(&comp.shape_expr) && !matches!(comp.start, Expression::Empty) {
            // For parameters with `:` dimensions, infer shape from their binding
            if let Some(shape) = infer_expression_shape(&comp.start, &HashMap::new()) {
                param_shapes.insert(name.clone(), shape);
            }
        } else if !comp.shape.is_empty() {
            // Use the declared shape for explicit dimensions
            param_shapes.insert(name.clone(), comp.shape.clone());
        }
    }

    // Now check each component's start modification
    for (_name, comp) in &class.components {
        // Only check start modifications (not bindings)
        if !comp.start_is_modification || matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip if component has no explicit shape
        if comp.shape.is_empty() {
            continue;
        }

        // Get expected shape from the component
        let expected_shape = &comp.shape;

        // Infer actual shape from the start expression
        if let Some(actual_shape) = infer_expression_shape(&comp.start, &param_shapes) {
            // Allow scalar values for array components (they are broadcast)
            // E.g., `Real x[3](start = 0)` is valid - 0 is broadcast to all elements
            let is_scalar_broadcast = actual_shape.is_empty() && !expected_shape.is_empty();

            // Check if shapes match (unless it's a valid scalar broadcast)
            if expected_shape != &actual_shape
                && !is_scalar_broadcast
                && let Some(loc) = comp.start.get_location()
            {
                let expected_str = format!(
                    "[{}]",
                    expected_shape
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                let actual_str = format!(
                    "[{}]",
                    actual_shape
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Type mismatch in binding 'start = {}', expected array dimensions {}, got {}.",
                        comp.start, expected_str, actual_str
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }

    result
}
