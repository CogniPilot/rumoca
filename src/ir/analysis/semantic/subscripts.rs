//! Array subscript and bounds validation.
//!
//! Validates that array subscripts are valid and within bounds.

use std::collections::HashMap;

use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, OpBinary, OpUnary, Statement,
    Subscript, TerminalType, Variability,
};

use crate::ir::analysis::type_inference::SymbolType;

use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

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

// =============================================================================
// ARRAY DIMENSION MISMATCH CHECK
// =============================================================================

/// Check for array dimension mismatches in equations and operations.
///
/// This validates that:
/// - LHS and RHS of equations have compatible dimensions
/// - Binary operations on arrays have compatible dimensions
pub fn check_array_dimension_mismatch(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their shapes
    let mut comp_shapes: HashMap<String, Vec<usize>> = HashMap::new();
    for (name, comp) in &class.components {
        comp_shapes.insert(name.clone(), comp.shape.clone());
    }

    // Check component bindings
    for (name, comp) in &class.components {
        if !matches!(comp.start, Expression::Empty) {
            let lhs_shape = comp.shape.clone();
            if let Some(rhs_shape) = infer_expression_shape(&comp.start, &comp_shapes)
                && !shapes_compatible(&lhs_shape, &rhs_shape)
            {
                result.add_error(TypeError::new(
                    comp.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Array dimension mismatch in binding for '{}': expected {:?}, got {:?}",
                        name, lhs_shape, rhs_shape
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            // Also check for dimension mismatches within the binding expression itself
            check_expression_dimension_mismatch(&comp.start, &comp_shapes, &mut result);
        }
    }

    // Check equations
    for eq in &class.equations {
        check_equation_dimension_mismatch(eq, &comp_shapes, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_equation_dimension_mismatch(eq, &comp_shapes, &mut result);
    }

    result
}

/// Check if an expression is or contains an array constructor at its root.
/// Array constructors like [a, b] or [a; b] have complex semantics that are
/// difficult to infer dimensions for statically.
fn is_array_constructor(expr: &Expression) -> bool {
    match expr {
        Expression::Array { .. } => true,
        Expression::Parenthesized { inner } => is_array_constructor(inner),
        _ => false,
    }
}

/// Check an equation for dimension mismatches.
fn check_equation_dimension_mismatch(
    eq: &Equation,
    comp_shapes: &HashMap<String, Vec<usize>>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            // Skip dimension check when BOTH sides are array constructors
            // (like [u] = [y1; y2]) since their semantics are complex
            // But still catch cases like x = {1,2,3,4} where only one side is
            if is_array_constructor(lhs) && is_array_constructor(rhs) {
                // Still check for mismatches within sub-expressions
                check_expression_dimension_mismatch(lhs, comp_shapes, result);
                check_expression_dimension_mismatch(rhs, comp_shapes, result);
                return;
            }

            // Check for dimension mismatch between LHS and RHS
            if let (Some(lhs_shape), Some(rhs_shape)) = (
                infer_expression_shape(lhs, comp_shapes),
                infer_expression_shape(rhs, comp_shapes),
            ) && !shapes_compatible(&lhs_shape, &rhs_shape)
                && let Some(loc) = lhs.get_location()
            {
                result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Array dimension mismatch in equation: LHS has shape {:?}, RHS has shape {:?}",
                                lhs_shape, rhs_shape
                            ),
                            TypeErrorSeverity::Error,
                        ));
            }

            // Also check for dimension mismatches within expressions
            check_expression_dimension_mismatch(lhs, comp_shapes, result);
            check_expression_dimension_mismatch(rhs, comp_shapes, result);
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for sub_eq in &block.eqs {
                    check_equation_dimension_mismatch(sub_eq, comp_shapes, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_dimension_mismatch(sub_eq, comp_shapes, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_dimension_mismatch(sub_eq, comp_shapes, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for sub_eq in &block.eqs {
                    check_equation_dimension_mismatch(sub_eq, comp_shapes, result);
                }
            }
        }
        _ => {}
    }
}

/// Check an expression for dimension mismatches in binary operations.
fn check_expression_dimension_mismatch(
    expr: &Expression,
    comp_shapes: &HashMap<String, Vec<usize>>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::Binary { lhs, op, rhs } => {
            // For element-wise operations (+, -, .*, ./, etc.), shapes must match
            // For matrix operations (*, /), shapes must be compatible for matrix ops
            let is_elementwise = matches!(
                op,
                OpBinary::Add(_)
                    | OpBinary::Sub(_)
                    | OpBinary::AddElem(_)
                    | OpBinary::SubElem(_)
                    | OpBinary::MulElem(_)
                    | OpBinary::DivElem(_)
            );

            if is_elementwise
                && let (Some(lhs_shape), Some(rhs_shape)) = (
                    infer_expression_shape(lhs, comp_shapes),
                    infer_expression_shape(rhs, comp_shapes),
                )
            {
                // Both must be arrays with the same shape, or one is scalar
                if !lhs_shape.is_empty()
                    && !rhs_shape.is_empty()
                    && !shapes_compatible(&lhs_shape, &rhs_shape)
                    && let Some(loc) = lhs.get_location()
                {
                    result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Array dimension mismatch in operation: LHS has shape {:?}, RHS has shape {:?}",
                                    lhs_shape, rhs_shape
                                ),
                                TypeErrorSeverity::Error,
                            ));
                }
            }

            // Recursively check sub-expressions
            check_expression_dimension_mismatch(lhs, comp_shapes, result);
            check_expression_dimension_mismatch(rhs, comp_shapes, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression_dimension_mismatch(rhs, comp_shapes, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression_dimension_mismatch(inner, comp_shapes, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expression_dimension_mismatch(arg, comp_shapes, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression_dimension_mismatch(cond, comp_shapes, result);
                check_expression_dimension_mismatch(then_expr, comp_shapes, result);
            }
            check_expression_dimension_mismatch(else_branch, comp_shapes, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expression_dimension_mismatch(elem, comp_shapes, result);
            }
        }
        _ => {}
    }
}

/// Infer the shape of an expression.
/// Returns None if the shape cannot be determined.
fn infer_expression_shape(
    expr: &Expression,
    comp_shapes: &HashMap<String, Vec<usize>>,
) -> Option<Vec<usize>> {
    match expr {
        Expression::Terminal { .. } => Some(vec![]), // Scalars
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                // Get base shape from component
                let base_shape = comp_shapes.get(&first.ident.text)?.clone();

                // If there are subscripts, reduce dimensions
                if let Some(subs) = &first.subs {
                    // Each subscript that is a single value (not a range) reduces dimension by 1
                    let mut result_shape = vec![];
                    for (i, sub) in subs.iter().enumerate() {
                        if i < base_shape.len() {
                            match sub {
                                Subscript::Range { .. } => {
                                    // Keeps the full dimension
                                    result_shape.push(base_shape[i]);
                                }
                                Subscript::Expression(_) => {
                                    // Single index, removes this dimension (scalar result for this dim)
                                }
                                Subscript::Empty => {
                                    // Empty subscript - keep dimension
                                    result_shape.push(base_shape[i]);
                                }
                            }
                        }
                    }
                    // Add remaining dimensions if fewer subscripts than dimensions
                    for dim in base_shape.iter().skip(subs.len()) {
                        result_shape.push(*dim);
                    }
                    Some(result_shape)
                } else {
                    Some(base_shape)
                }
            } else {
                None
            }
        }
        Expression::Array { elements, .. } => {
            // Array literal: shape is [n] where n is number of elements
            // If elements are arrays, shape is [n, inner_shape...]
            if elements.is_empty() {
                return Some(vec![0]);
            }

            let n = elements.len();

            // Check if first element is an array to get inner shape
            if let Some(first) = elements.first() {
                if let Some(inner_shape) = infer_expression_shape(first, comp_shapes) {
                    if inner_shape.is_empty() {
                        // 1D array of scalars
                        Some(vec![n])
                    } else {
                        // Multi-dimensional array
                        let mut shape = vec![n];
                        shape.extend(inner_shape);
                        Some(shape)
                    }
                } else {
                    Some(vec![n])
                }
            } else {
                Some(vec![n])
            }
        }
        Expression::Binary { lhs, op, rhs } => {
            // For element-wise operations, result has same shape as operands
            let lhs_shape = infer_expression_shape(lhs, comp_shapes)?;
            let rhs_shape = infer_expression_shape(rhs, comp_shapes)?;

            match op {
                OpBinary::Add(_)
                | OpBinary::Sub(_)
                | OpBinary::AddElem(_)
                | OpBinary::SubElem(_)
                | OpBinary::MulElem(_)
                | OpBinary::DivElem(_) => {
                    // Element-wise: shapes must match, or one is scalar
                    if lhs_shape.is_empty() {
                        Some(rhs_shape)
                    } else {
                        // rhs is scalar, shapes match, or mismatch (error reported elsewhere)
                        Some(lhs_shape)
                    }
                }
                OpBinary::Mul(_) | OpBinary::Div(_) => {
                    // Scalar multiplication preserves shape
                    if lhs_shape.is_empty() {
                        Some(rhs_shape)
                    } else if rhs_shape.is_empty() {
                        Some(lhs_shape)
                    } else {
                        // Matrix/vector multiplication - infer result shape
                        // A[m,n] * x[n] → y[m]  (matrix-vector)
                        // A[m,n] * B[n,p] → C[m,p]  (matrix-matrix)
                        // x[n] * y[n] → scalar  (vector dot product)
                        if lhs_shape.len() == 2 && rhs_shape.len() == 1 {
                            // Matrix-vector: result is vector with lhs rows
                            Some(vec![lhs_shape[0]])
                        } else if lhs_shape.len() == 2 && rhs_shape.len() == 2 {
                            // Matrix-matrix: result has lhs rows x rhs cols
                            Some(vec![lhs_shape[0], rhs_shape[1]])
                        } else if lhs_shape.len() == 1 && rhs_shape.len() == 1 {
                            // Vector-vector dot product: scalar result
                            Some(vec![])
                        } else if lhs_shape.len() == 1 && rhs_shape.len() == 2 {
                            // Row vector times matrix: result is row vector
                            Some(vec![rhs_shape[1]])
                        } else {
                            // Other cases - just return lhs shape
                            Some(lhs_shape)
                        }
                    }
                }
                _ => {
                    // Comparison operators return scalar Boolean
                    Some(vec![])
                }
            }
        }
        Expression::Unary { rhs, .. } => infer_expression_shape(rhs, comp_shapes),
        Expression::Parenthesized { inner } => infer_expression_shape(inner, comp_shapes),
        Expression::FunctionCall { comp, args, .. } => {
            // Some functions have known output shapes
            let func_name = comp.to_string();
            match func_name.as_str() {
                "zeros" | "ones" | "fill" => {
                    // Output shape determined by arguments
                    // For simplicity, try to infer from first arg
                    args.first()
                        .and_then(|a| infer_expression_shape(a, comp_shapes))
                }
                "size" => Some(vec![]), // Returns Integer
                "sum" | "product" | "min" | "max" => Some(vec![]), // Reduction operations
                _ => None,              // Unknown function
            }
        }
        _ => None,
    }
}

/// Check if two shapes are compatible.
/// Shapes are compatible if they are equal, or if one is a scalar (empty).
fn shapes_compatible(shape1: &[usize], shape2: &[usize]) -> bool {
    if shape1.is_empty() || shape2.is_empty() {
        true // Scalar is compatible with anything
    } else {
        shape1 == shape2
    }
}

// =============================================================================
// ARRAY INDEX TYPE CHECK
// =============================================================================

/// Check that array subscripts are Integer type (not Real).
///
/// MLS §10.1: Array subscripts must be Integer expressions.
/// This validates that expressions like `x[1.5]` are flagged as errors.
pub fn check_array_index_types(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check component bindings
    for (_name, comp) in &class.components {
        if !matches!(comp.start, Expression::Empty) {
            check_index_types_in_expression(&comp.start, &mut result);
        }
    }

    // Check equations
    for eq in &class.equations {
        check_index_types_in_equation(eq, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_index_types_in_equation(eq, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_index_types_in_statement(stmt, &mut result);
        }
    }

    result
}

/// Check index types in an equation.
fn check_index_types_in_equation(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_index_types_in_expression(lhs, result);
            check_index_types_in_expression(rhs, result);
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_index_types_in_expression(arg, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_index_types_in_expression(&block.cond, result);
                for inner_eq in &block.eqs {
                    check_index_types_in_equation(inner_eq, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_index_types_in_expression(&block.cond, result);
                for inner_eq in &block.eqs {
                    check_index_types_in_equation(inner_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_index_types_in_equation(inner_eq, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_index_types_in_equation(inner_eq, result);
            }
        }
        Equation::Connect { .. } | Equation::Empty => {}
    }
}

/// Check index types in a statement.
fn check_index_types_in_statement(stmt: &Statement, result: &mut TypeCheckResult) {
    match stmt {
        Statement::Assignment { comp, value } => {
            check_index_types_in_comp_ref(comp, result);
            check_index_types_in_expression(value, result);
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_index_types_in_expression(arg, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_index_types_in_expression(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_index_types_in_statement(inner_stmt, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_index_types_in_expression(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_index_types_in_statement(inner_stmt, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_index_types_in_statement(inner_stmt, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_index_types_in_statement(inner_stmt, result);
            }
        }
        Statement::While(block) => {
            check_index_types_in_expression(&block.cond, result);
            for inner_stmt in &block.stmts {
                check_index_types_in_statement(inner_stmt, result);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

/// Check index types in an expression.
fn check_index_types_in_expression(expr: &Expression, result: &mut TypeCheckResult) {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            check_index_types_in_comp_ref(comp_ref, result);
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_index_types_in_expression(lhs, result);
            check_index_types_in_expression(rhs, result);
        }
        Expression::Unary { rhs, .. } => {
            check_index_types_in_expression(rhs, result);
        }
        Expression::Parenthesized { inner } => {
            check_index_types_in_expression(inner, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_index_types_in_expression(arg, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_index_types_in_expression(cond, result);
                check_index_types_in_expression(then_expr, result);
            }
            check_index_types_in_expression(else_branch, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_index_types_in_expression(elem, result);
            }
        }
        _ => {}
    }
}

/// Check index types in a component reference.
fn check_index_types_in_comp_ref(comp_ref: &ComponentReference, result: &mut TypeCheckResult) {
    for part in &comp_ref.parts {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let Subscript::Expression(sub_expr) = sub {
                    // Check if the subscript expression is a Real literal
                    if is_real_subscript(sub_expr)
                        && let Some(loc) = sub_expr.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Integer,
                            SymbolType::Real,
                            "Array subscript must be Integer, not Real".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    // Recursively check the subscript expression itself
                    check_index_types_in_expression(sub_expr, result);
                }
            }
        }
    }
}

/// Check if an expression is a Real literal subscript.
fn is_real_subscript(expr: &Expression) -> bool {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            ..
        } => true,
        Expression::Unary { rhs, .. } => is_real_subscript(rhs),
        Expression::Parenthesized { inner } => is_real_subscript(inner),
        _ => false,
    }
}

// =============================================================================
// INCONSISTENT NESTED ARRAY CHECK
// =============================================================================

/// Simple type classification for array element type checking.
#[derive(Debug, Clone, PartialEq, Eq)]
enum ArrayElementType {
    Integer,
    Real,
    Boolean,
    String,
    Array,
    Other,
}

impl ArrayElementType {
    /// Check if two types are compatible in an array literal.
    /// Integer and Real are compatible (Integer promotes to Real).
    fn is_compatible_with(&self, other: &ArrayElementType) -> bool {
        match (self, other) {
            (a, b) if a == b => true,
            // Integer can be promoted to Real
            (ArrayElementType::Integer, ArrayElementType::Real)
            | (ArrayElementType::Real, ArrayElementType::Integer) => true,
            _ => false,
        }
    }
}

/// Get the type category of an expression for array element checking.
fn get_array_element_type(expr: &Expression) -> Option<ArrayElementType> {
    match expr {
        Expression::Terminal { terminal_type, .. } => match terminal_type {
            TerminalType::UnsignedInteger => Some(ArrayElementType::Integer),
            TerminalType::UnsignedReal => Some(ArrayElementType::Real),
            TerminalType::Bool => Some(ArrayElementType::Boolean),
            TerminalType::String => Some(ArrayElementType::String),
            _ => Some(ArrayElementType::Other),
        },
        Expression::Unary { rhs, .. } => get_array_element_type(rhs),
        Expression::Parenthesized { inner } => get_array_element_type(inner),
        Expression::Array { .. } => Some(ArrayElementType::Array),
        _ => None, // Can't determine type for complex expressions
    }
}

/// Check for inconsistent nested array dimensions in array literals.
///
/// MLS §10.4: Array elements must have consistent dimensions.
/// For example, `{{1, 2, 3}, {4, 5}}` is invalid because the rows have
/// different lengths.
pub fn check_inconsistent_nested_arrays(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check component bindings
    for (_name, comp) in &class.components {
        if !matches!(comp.start, Expression::Empty) {
            check_array_consistency(&comp.start, &mut result);
        }
    }

    // Check equations
    for eq in &class.equations {
        check_array_consistency_in_equation(eq, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_array_consistency_in_equation(eq, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_array_consistency_in_statement(stmt, &mut result);
        }
    }

    result
}

/// Check array consistency in an equation.
fn check_array_consistency_in_equation(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_array_consistency(lhs, result);
            check_array_consistency(rhs, result);
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_array_consistency(arg, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_array_consistency(&block.cond, result);
                for inner_eq in &block.eqs {
                    check_array_consistency_in_equation(inner_eq, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_array_consistency(&block.cond, result);
                for inner_eq in &block.eqs {
                    check_array_consistency_in_equation(inner_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_array_consistency_in_equation(inner_eq, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_array_consistency_in_equation(inner_eq, result);
            }
        }
        Equation::Connect { .. } | Equation::Empty => {}
    }
}

/// Check array consistency in a statement.
fn check_array_consistency_in_statement(stmt: &Statement, result: &mut TypeCheckResult) {
    match stmt {
        Statement::Assignment { value, .. } => {
            check_array_consistency(value, result);
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_array_consistency(arg, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_array_consistency(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_array_consistency_in_statement(inner_stmt, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_array_consistency(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_array_consistency_in_statement(inner_stmt, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_array_consistency_in_statement(inner_stmt, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_array_consistency_in_statement(inner_stmt, result);
            }
        }
        Statement::While(block) => {
            check_array_consistency(&block.cond, result);
            for inner_stmt in &block.stmts {
                check_array_consistency_in_statement(inner_stmt, result);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

/// Check an expression for array consistency.
fn check_array_consistency(expr: &Expression, result: &mut TypeCheckResult) {
    match expr {
        Expression::Array { elements, .. } => {
            // Check if this is a multi-dimensional array (elements are also arrays)
            let mut first_inner_len: Option<usize> = None;
            let mut has_dimension_inconsistency = false;

            // Check for mixed types in the array literal
            let mut first_type: Option<ArrayElementType> = None;
            let mut has_type_inconsistency = false;

            for elem in elements {
                // Check for nested array dimension consistency
                if let Expression::Array {
                    elements: inner_elements,
                    ..
                } = elem
                {
                    let inner_len = inner_elements.len();
                    if let Some(expected_len) = first_inner_len {
                        if inner_len != expected_len {
                            has_dimension_inconsistency = true;
                        }
                    } else {
                        first_inner_len = Some(inner_len);
                    }
                }

                // Check for type consistency among scalar elements
                if let Some(elem_type) = get_array_element_type(elem) {
                    if let Some(ref expected_type) = first_type {
                        if !elem_type.is_compatible_with(expected_type) {
                            has_type_inconsistency = true;
                        }
                    } else {
                        first_type = Some(elem_type);
                    }
                }

                // Recursively check elements
                check_array_consistency(elem, result);
            }

            if has_dimension_inconsistency {
                // Get location from the first element if available
                let location = expr
                    .get_location()
                    .cloned()
                    .unwrap_or_else(crate::ir::ast::Location::default);
                result.add_error(TypeError::new(
                    location,
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "Inconsistent dimensions in nested array literal: rows have different lengths"
                        .to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            if has_type_inconsistency {
                let location = expr
                    .get_location()
                    .cloned()
                    .unwrap_or_else(crate::ir::ast::Location::default);
                result.add_error(TypeError::new(
                    location,
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "Mixed types in array literal: all elements must have the same type"
                        .to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_array_consistency(lhs, result);
            check_array_consistency(rhs, result);
        }
        Expression::Unary { rhs, .. } => {
            check_array_consistency(rhs, result);
        }
        Expression::Parenthesized { inner } => {
            check_array_consistency(inner, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_array_consistency(arg, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_array_consistency(cond, result);
                check_array_consistency(then_expr, result);
            }
            check_array_consistency(else_branch, result);
        }
        _ => {}
    }
}
