//! General validation checks (subscripts, bounds, break/return context, dimensions).

use std::collections::HashMap;

use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, OpBinary, OpUnary, Statement,
    Subscript, TerminalType, Variability,
};

use crate::ir::analysis::type_inference::SymbolType;

use super::types::{has_inferred_dimensions, infer_expression_shape};
use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for break/return statements used outside of valid context.
///
/// This validates that:
/// - `break` is only used inside for/while loops
/// - (future: return only in functions)
pub fn check_break_return_context(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check all algorithm statements
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_loop_context(stmt, false, &mut result);
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
fn check_statement_loop_context(stmt: &Statement, in_loop: bool, result: &mut TypeCheckResult) {
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
        Statement::For { equations, .. } => {
            // We're now inside a loop
            for sub_stmt in equations {
                check_statement_loop_context(sub_stmt, true, result);
            }
        }
        Statement::While(block) => {
            // We're now inside a loop
            for sub_stmt in &block.stmts {
                check_statement_loop_context(sub_stmt, true, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            // If statements don't change loop context
            for block in cond_blocks {
                for sub_stmt in &block.stmts {
                    check_statement_loop_context(sub_stmt, in_loop, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_loop_context(sub_stmt, in_loop, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for sub_stmt in &block.stmts {
                    check_statement_loop_context(sub_stmt, in_loop, result);
                }
            }
        }
        _ => {}
    }
}

/// Check for subscripting of scalar variables (like `time[2]`).
///
/// This validates that only array variables are subscripted.
pub fn check_scalar_subscripts(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their dimensions (0 = scalar)
    let mut comp_dims: HashMap<String, usize> = HashMap::new();
    for (name, comp) in &class.components {
        comp_dims.insert(name.clone(), comp.shape.len());
    }

    // Check bindings
    for (_name, comp) in &class.components {
        check_expr_scalar_subscripts(&comp.start, &comp_dims, &mut result);
    }

    // Check equations
    for eq in &class.equations {
        check_equation_scalar_subscripts(eq, &comp_dims, &mut result);
    }

    // Check algorithms
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_stmt_scalar_subscripts(stmt, &comp_dims, &mut result);
        }
    }

    result
}

/// Check an equation for scalar subscript violations
fn check_equation_scalar_subscripts(
    eq: &Equation,
    comp_dims: &HashMap<String, usize>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_expr_scalar_subscripts(lhs, comp_dims, result);
            check_expr_scalar_subscripts(rhs, comp_dims, result);
        }
        Equation::Connect { .. } => {}
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_scalar_subscripts(sub_eq, comp_dims, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expr_scalar_subscripts(&block.cond, comp_dims, result);
                for sub_eq in &block.eqs {
                    check_equation_scalar_subscripts(sub_eq, comp_dims, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expr_scalar_subscripts(&block.cond, comp_dims, result);
                for sub_eq in &block.eqs {
                    check_equation_scalar_subscripts(sub_eq, comp_dims, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_scalar_subscripts(sub_eq, comp_dims, result);
                }
            }
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_scalar_subscripts(arg, comp_dims, result);
            }
        }
        Equation::Empty => {}
    }
}

/// Check a statement for scalar subscript violations
fn check_stmt_scalar_subscripts(
    stmt: &Statement,
    comp_dims: &HashMap<String, usize>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { comp, value } => {
            check_component_ref_scalar_subscripts(comp, comp_dims, result);
            check_expr_scalar_subscripts(value, comp_dims, result);
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                check_stmt_scalar_subscripts(sub_stmt, comp_dims, result);
            }
        }
        Statement::While(block) => {
            check_expr_scalar_subscripts(&block.cond, comp_dims, result);
            for sub_stmt in &block.stmts {
                check_stmt_scalar_subscripts(sub_stmt, comp_dims, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expr_scalar_subscripts(&block.cond, comp_dims, result);
                for sub_stmt in &block.stmts {
                    check_stmt_scalar_subscripts(sub_stmt, comp_dims, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_stmt_scalar_subscripts(sub_stmt, comp_dims, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expr_scalar_subscripts(&block.cond, comp_dims, result);
                for sub_stmt in &block.stmts {
                    check_stmt_scalar_subscripts(sub_stmt, comp_dims, result);
                }
            }
        }
        _ => {}
    }
}

/// Check a component reference for scalar subscripting
fn check_component_ref_scalar_subscripts(
    comp: &ComponentReference,
    _comp_dims: &HashMap<String, usize>,
    result: &mut TypeCheckResult,
) {
    if let Some(first) = comp.parts.first() {
        let var_name = &first.ident.text;
        let num_subscripts = first.subs.as_ref().map_or(0, |s| s.len());

        // Check for built-in scalars like `time`
        // These are always scalars and should never be subscripted
        if var_name == "time" && num_subscripts > 0 {
            result.add_error(TypeError::new(
                first.ident.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Wrong number of subscripts in {}[...] ({} subscripts for 0 dimensions).",
                    var_name, num_subscripts
                ),
                TypeErrorSeverity::Error,
            ));
        }
        // Note: User-defined component dimension checking is deferred to after equation
        // expansion when shapes are fully resolved (parameterized dimensions like x[nx]
        // aren't resolved until then)
    }
}

/// Check an expression for scalar subscript violations
fn check_expr_scalar_subscripts(
    expr: &Expression,
    comp_dims: &HashMap<String, usize>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::ComponentReference(comp) => {
            check_component_ref_scalar_subscripts(comp, comp_dims, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_scalar_subscripts(arg, comp_dims, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expr_scalar_subscripts(lhs, comp_dims, result);
            check_expr_scalar_subscripts(rhs, comp_dims, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expr_scalar_subscripts(rhs, comp_dims, result);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expr_scalar_subscripts(cond, comp_dims, result);
                check_expr_scalar_subscripts(then_expr, comp_dims, result);
            }
            check_expr_scalar_subscripts(else_branch, comp_dims, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expr_scalar_subscripts(elem, comp_dims, result);
            }
        }
        _ => {}
    }
}

/// Check for array subscript out of bounds errors.
///
/// This validates that array subscripts are within the declared array dimensions.
/// For example, `x[4]` where x is `Real x[3]` would be flagged as out of bounds.
pub fn check_array_bounds(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their shapes
    let mut comp_shapes: HashMap<String, Vec<usize>> = HashMap::new();
    for (name, comp) in &class.components {
        if !comp.shape.is_empty() {
            comp_shapes.insert(name.clone(), comp.shape.clone());
        }
    }

    // Build a map of constant parameter values for expression evaluation
    let mut param_values: HashMap<String, i64> = HashMap::new();
    for (name, comp) in &class.components {
        if matches!(
            comp.variability,
            Variability::Parameter(_) | Variability::Constant(_)
        ) && let Some(val) = get_constant_integer(&comp.start, &HashMap::new())
        {
            param_values.insert(name.clone(), val);
        }
    }

    // Check bindings
    for (_name, comp) in &class.components {
        check_expression_bounds(&comp.start, &comp_shapes, &param_values, &mut result);
    }

    // Check equations
    for eq in &class.equations {
        check_equation_bounds(eq, &comp_shapes, &param_values, &mut result);
    }

    // Check algorithms
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_bounds(stmt, &comp_shapes, &param_values, &mut result);
        }
    }

    result
}

/// Check an equation for array bounds violations
fn check_equation_bounds(
    eq: &Equation,
    comp_shapes: &HashMap<String, Vec<usize>>,
    param_values: &HashMap<String, i64>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_expression_bounds(lhs, comp_shapes, param_values, result);
            check_expression_bounds(rhs, comp_shapes, param_values, result);
        }
        Equation::Connect { lhs, rhs } => {
            check_component_ref_bounds(lhs, comp_shapes, param_values, result);
            check_component_ref_bounds(rhs, comp_shapes, param_values, result);
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_bounds(sub_eq, comp_shapes, param_values, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expression_bounds(&block.cond, comp_shapes, param_values, result);
                for sub_eq in &block.eqs {
                    check_equation_bounds(sub_eq, comp_shapes, param_values, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Only check conditions, not bodies - Modelica allows potentially
            // out-of-bounds accesses in conditional branches since they may
            // never be executed at runtime
            for block in cond_blocks {
                check_expression_bounds(&block.cond, comp_shapes, param_values, result);
            }
            // Don't check else_block bodies either
            let _ = else_block; // Suppress unused warning
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_expression_bounds(arg, comp_shapes, param_values, result);
            }
        }
        Equation::Empty => {}
    }
}

/// Check a statement for array bounds violations
fn check_statement_bounds(
    stmt: &Statement,
    comp_shapes: &HashMap<String, Vec<usize>>,
    param_values: &HashMap<String, i64>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { comp, value } => {
            check_component_ref_bounds(comp, comp_shapes, param_values, result);
            check_expression_bounds(value, comp_shapes, param_values, result);
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                check_statement_bounds(sub_stmt, comp_shapes, param_values, result);
            }
        }
        Statement::While(block) => {
            check_expression_bounds(&block.cond, comp_shapes, param_values, result);
            for sub_stmt in &block.stmts {
                check_statement_bounds(sub_stmt, comp_shapes, param_values, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            // Only check conditions, not bodies - Modelica allows potentially
            // out-of-bounds accesses in conditional branches since they may
            // never be executed at runtime
            for block in cond_blocks {
                check_expression_bounds(&block.cond, comp_shapes, param_values, result);
            }
            // Don't check else_block bodies either
            let _ = else_block; // Suppress unused warning
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expression_bounds(&block.cond, comp_shapes, param_values, result);
                for sub_stmt in &block.stmts {
                    check_statement_bounds(sub_stmt, comp_shapes, param_values, result);
                }
            }
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_expression_bounds(arg, comp_shapes, param_values, result);
            }
        }
        _ => {}
    }
}

/// Check an expression for array bounds violations
fn check_expression_bounds(
    expr: &Expression,
    comp_shapes: &HashMap<String, Vec<usize>>,
    param_values: &HashMap<String, i64>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            check_component_ref_bounds(comp_ref, comp_shapes, param_values, result);
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression_bounds(lhs, comp_shapes, param_values, result);
            check_expression_bounds(rhs, comp_shapes, param_values, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression_bounds(rhs, comp_shapes, param_values, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expression_bounds(arg, comp_shapes, param_values, result);
            }
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expression_bounds(elem, comp_shapes, param_values, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression_bounds(cond, comp_shapes, param_values, result);
                check_expression_bounds(then_expr, comp_shapes, param_values, result);
            }
            check_expression_bounds(else_branch, comp_shapes, param_values, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression_bounds(inner, comp_shapes, param_values, result);
        }
        Expression::ArrayComprehension { expr, .. } => {
            check_expression_bounds(expr, comp_shapes, param_values, result);
        }
        _ => {}
    }
}

/// Check a component reference for array bounds violations
fn check_component_ref_bounds(
    comp_ref: &ComponentReference,
    comp_shapes: &HashMap<String, Vec<usize>>,
    param_values: &HashMap<String, i64>,
    result: &mut TypeCheckResult,
) {
    if let Some(first) = comp_ref.parts.first()
        && let Some(shape) = comp_shapes.get(&first.ident.text)
        && let Some(subs) = &first.subs
    {
        let var_name = &first.ident.text;
        for (dim_idx, sub) in subs.iter().enumerate() {
            if dim_idx < shape.len() {
                let dim_size = shape[dim_idx];
                // Check if subscript is a constant integer
                if let Subscript::Expression(sub_expr) = sub
                    && let Some(idx) = get_constant_integer(sub_expr, param_values)
                    && (idx < 1 || idx > dim_size as i64)
                    && let Some(loc) = sub_expr.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Unknown,
                        SymbolType::Unknown,
                        format!(
                            "Subscript '{}' for dimension {} (size = {}) of {} is out of bounds",
                            idx,
                            dim_idx + 1,
                            dim_size,
                            var_name
                        ),
                        TypeErrorSeverity::Error,
                    ));
                }
            }
        }
    }
}

/// Try to get a constant integer value from an expression
fn get_constant_integer(expr: &Expression, param_values: &HashMap<String, i64>) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type,
            token,
        } => {
            if let TerminalType::UnsignedInteger = terminal_type {
                token.text.parse::<i64>().ok()
            } else {
                None
            }
        }
        Expression::Unary { op, rhs } => {
            if matches!(op, OpUnary::Minus(_)) {
                get_constant_integer(rhs, param_values).map(|v| -v)
            } else {
                None
            }
        }
        Expression::ComponentReference(comp_ref) => {
            // Look up parameter values
            if let Some(first) = comp_ref.parts.first() {
                param_values.get(&first.ident.text).copied()
            } else {
                None
            }
        }
        Expression::Binary { lhs, op, rhs } => {
            let lhs_val = get_constant_integer(lhs, param_values)?;
            let rhs_val = get_constant_integer(rhs, param_values)?;
            match op {
                OpBinary::Add(_) => Some(lhs_val + rhs_val),
                OpBinary::Sub(_) => Some(lhs_val - rhs_val),
                OpBinary::Mul(_) => Some(lhs_val * rhs_val),
                OpBinary::Div(_) => {
                    if rhs_val != 0 {
                        Some(lhs_val / rhs_val)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        Expression::Parenthesized { inner } => get_constant_integer(inner, param_values),
        _ => None,
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
