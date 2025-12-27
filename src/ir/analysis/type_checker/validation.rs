//! General validation checks (subscripts, bounds, break/return context, dimensions).

use std::collections::HashMap;

use crate::ir::ast::{
    ClassDefinition, ClassType, Component, ComponentReference, Connection, Equation, Expression,
    OpBinary, OpUnary, Statement, Subscript, TerminalType, Variability,
};

use crate::ir::analysis::type_inference::SymbolType;

use super::types::{has_inferred_dimensions, infer_expression_shape};
use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that constants have binding expressions.
///
/// MLS §4.4: "A constant shall have a declaration equation."
/// This validates that all components with `constant` variability have a binding.
pub fn check_constant_bindings(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    for (name, comp) in &class.components {
        // Check if this is a constant (not parameter)
        if let Variability::Constant(_) = &comp.variability {
            // Check if it has a real binding (not empty, not a modification, not synthetic default)
            let has_real_binding =
                if matches!(comp.start, Expression::Empty) || comp.start_is_modification {
                    false
                } else {
                    // Check if this is a synthetic default value (location 0,0,"")
                    // Real bindings have actual source locations
                    if let Some(loc) = comp.start.get_location() {
                        !(loc.start_line == 0 && loc.start_column == 0 && loc.file_name.is_empty())
                    } else {
                        true // No location info - assume real binding
                    }
                };

            if !has_real_binding {
                result.add_error(TypeError::new(
                    comp.name_token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Constant '{}' must have a declaration equation (binding expression)",
                        name
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }

    result
}

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

/// Check for variability dependency violations.
///
/// MLS §4.5: Variability constraints on binding equations:
/// - A constant binding cannot depend on parameters or variables
/// - A parameter binding cannot depend on variables
pub fn check_variability_dependencies(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their variability
    let mut comp_variability: HashMap<String, &Variability> = HashMap::new();
    for (name, comp) in &class.components {
        comp_variability.insert(name.clone(), &comp.variability);
    }

    // Check each component's binding expression
    for (name, comp) in &class.components {
        // Skip components without real bindings
        if matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip modifications (we only check declaration equations)
        if comp.start_is_modification {
            continue;
        }

        // Skip synthetic bindings
        if let Some(loc) = comp.start.get_location()
            && loc.start_line == 0
            && loc.start_column == 0
            && loc.file_name.is_empty()
        {
            continue;
        }

        // Collect all component references from the binding
        let mut refs: Vec<(String, crate::ir::ast::Location)> = Vec::new();
        collect_component_refs(&comp.start, &mut refs);

        // Check each referenced component
        for (ref_name, ref_loc) in refs {
            // Skip self-references and built-ins like 'time'
            if ref_name == *name || ref_name == "time" {
                continue;
            }

            // Get the variability of the referenced component
            if let Some(ref_variability) = comp_variability.get(&ref_name) {
                // Check variability constraints
                match &comp.variability {
                    Variability::Constant(_) => {
                        // Constant cannot depend on parameter or variable
                        match ref_variability {
                            Variability::Parameter(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Constant '{}' cannot depend on parameter '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            Variability::Empty | Variability::Discrete(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Constant '{}' cannot depend on variable '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            _ => {}
                        }
                    }
                    Variability::Parameter(_) => {
                        // Parameter cannot depend on variable
                        match ref_variability {
                            Variability::Empty | Variability::Discrete(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Parameter '{}' cannot depend on variable '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    result
}

/// Collect all component references from an expression
fn collect_component_refs(expr: &Expression, refs: &mut Vec<(String, crate::ir::ast::Location)>) {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                refs.push((first.ident.text.clone(), first.ident.location.clone()));
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_component_refs(lhs, refs);
            collect_component_refs(rhs, refs);
        }
        Expression::Unary { rhs, .. } => {
            collect_component_refs(rhs, refs);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_component_refs(arg, refs);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_component_refs(cond, refs);
                collect_component_refs(then_expr, refs);
            }
            collect_component_refs(else_branch, refs);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                collect_component_refs(elem, refs);
            }
        }
        Expression::Parenthesized { inner } => {
            collect_component_refs(inner, refs);
        }
        Expression::ArrayComprehension { expr, .. } => {
            collect_component_refs(expr, refs);
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

// =============================================================================
// WHEN-STATEMENT RESTRICTIONS (MLS §8.3.5, §11.2.7)
// =============================================================================

use std::collections::HashSet;

/// Check all when-statement restrictions for a class.
///
/// MLS §8.3.5 (When-equations) and §11.2.7 (When-statements):
/// - When-statements cannot be nested
/// - When-statements cannot appear in functions
/// - When-equations cannot appear inside if-equations with non-parameter conditions
/// - When-equations cannot appear inside for-equations
/// - When-statements cannot appear inside if-statements in algorithms
/// - When condition must be Boolean
pub fn check_when_restrictions(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check if this is a function - when is not allowed in functions
    let is_function = matches!(class.class_type, ClassType::Function);

    // Build set of parameter/constant variable names for condition checking
    let param_const_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            matches!(
                c.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

    // Check equations for when restrictions
    for eq in &class.equations {
        check_when_in_equation(eq, false, is_function, &param_const_vars, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_when_in_equation(eq, false, is_function, &param_const_vars, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_when_in_statement(stmt, false, false, is_function, &mut result);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            check_when_in_statement(stmt, false, false, is_function, &mut result);
        }
    }

    result
}

/// Check if an expression only references parameters/constants (not variables).
/// Returns true if the expression is "parameter-evaluatable".
fn is_parameter_expression(expr: &Expression, param_const_vars: &HashSet<String>) -> bool {
    match expr {
        Expression::Empty => true,
        Expression::Terminal { terminal_type, .. } => {
            // Literals are always parameter-evaluatable
            matches!(
                terminal_type,
                TerminalType::UnsignedInteger
                    | TerminalType::UnsignedReal
                    | TerminalType::Bool
                    | TerminalType::String
                    | TerminalType::End
            )
        }
        Expression::ComponentReference(comp_ref) => {
            // Check if this is a reference to a parameter/constant
            if let Some(first) = comp_ref.parts.first() {
                param_const_vars.contains(&first.ident.text)
            } else {
                false
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            is_parameter_expression(lhs, param_const_vars)
                && is_parameter_expression(rhs, param_const_vars)
        }
        Expression::Unary { rhs, .. } => is_parameter_expression(rhs, param_const_vars),
        Expression::Parenthesized { inner } => is_parameter_expression(inner, param_const_vars),
        Expression::FunctionCall { args, .. } => {
            // Function calls on parameter arguments are parameter-evaluatable
            args.iter()
                .all(|arg| is_parameter_expression(arg, param_const_vars))
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().all(|(cond, then_expr)| {
                is_parameter_expression(cond, param_const_vars)
                    && is_parameter_expression(then_expr, param_const_vars)
            }) && is_parameter_expression(else_branch, param_const_vars)
        }
        Expression::Array { elements, .. } => elements
            .iter()
            .all(|elem| is_parameter_expression(elem, param_const_vars)),
        Expression::ArrayComprehension { .. } => false, // Conservative
        Expression::Tuple { elements } => elements
            .iter()
            .all(|elem| is_parameter_expression(elem, param_const_vars)),
        Expression::Range { start, step, end } => {
            is_parameter_expression(start, param_const_vars)
                && step
                    .as_ref()
                    .is_none_or(|s| is_parameter_expression(s, param_const_vars))
                && is_parameter_expression(end, param_const_vars)
        }
    }
}

/// Check when restrictions in an equation.
///
/// - `in_when`: true if we're already inside a when-equation (for nested when detection)
/// - `is_function`: true if this is inside a function (when not allowed)
/// - `param_const_vars`: set of parameter/constant variable names for checking if-conditions
fn check_when_in_equation(
    eq: &Equation,
    in_when: bool,
    is_function: bool,
    param_const_vars: &HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::When(blocks) => {
            // When is forbidden in functions
            if is_function
                && let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "When-equations are not allowed in functions".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            // Nested when is forbidden
            if in_when
                && let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "When-equations cannot be nested".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            // Check each block's equations for nested when and connect
            for block in blocks {
                for inner_eq in &block.eqs {
                    // MLS §9.3: connect is not allowed inside when
                    if let Equation::Connect { lhs, .. } = inner_eq
                        && let Some(loc) = lhs.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            "Connect equations are not allowed inside when-equations".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    check_when_in_equation(inner_eq, true, is_function, param_const_vars, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // MLS §8.3.5: When inside if-equations is only allowed if the condition
            // is parameter-evaluatable (depends only on parameters/constants)
            // Check if any branch condition is NOT parameter-evaluatable
            let has_non_param_condition = cond_blocks
                .iter()
                .any(|block| !is_parameter_expression(&block.cond, param_const_vars));

            // Check if when appears inside if-equation with non-parameter condition
            if has_non_param_condition {
                for block in cond_blocks {
                    for inner_eq in &block.eqs {
                        if contains_when_equation(inner_eq) {
                            if let Some(loc) = block.cond.get_location() {
                                result.add_error(TypeError::new(
                                    loc.clone(),
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    "When-equations cannot appear inside if-equations with non-parameter conditions".to_string(),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            break;
                        }
                    }
                }
                if let Some(else_eqs) = else_block {
                    for inner_eq in else_eqs {
                        if contains_when_equation(inner_eq) {
                            result.add_error(TypeError::new(
                                crate::ir::ast::Location::default(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                "When-equations cannot appear inside if-equations with non-parameter conditions".to_string(),
                                TypeErrorSeverity::Error,
                            ));
                            break;
                        }
                    }
                }
            }

            // Continue checking nested equations for other restrictions
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_when_in_equation(
                        inner_eq,
                        in_when,
                        is_function,
                        param_const_vars,
                        result,
                    );
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_when_in_equation(
                        inner_eq,
                        in_when,
                        is_function,
                        param_const_vars,
                        result,
                    );
                }
            }
        }
        Equation::For { equations, .. } => {
            // Check if when appears inside for-equation
            for inner_eq in equations {
                if contains_when_equation(inner_eq) {
                    if let Some(loc) = get_equation_location(inner_eq) {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            "When-equations cannot appear inside for-equations".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    break;
                }
                check_when_in_equation(inner_eq, in_when, is_function, param_const_vars, result);
            }
        }
        _ => {}
    }
}

/// Check when restrictions in a statement.
///
/// - `in_when`: true if we're already inside a when-statement
/// - `in_if`: true if we're inside an if-statement
/// - `is_function`: true if this is inside a function
fn check_when_in_statement(
    stmt: &Statement,
    in_when: bool,
    in_if: bool,
    is_function: bool,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::When(blocks) => {
            // When is forbidden in functions
            if is_function
                && let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "When-statements are not allowed in functions".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            // Nested when is forbidden
            if in_when
                && let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "When-statements cannot be nested".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            // When inside if is forbidden
            if in_if
                && let Some(first_block) = blocks.first()
                && let Some(loc) = first_block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "When-statements cannot appear inside if-statements".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }

            // Check inner statements for nested when
            for block in blocks {
                for inner_stmt in &block.stmts {
                    check_when_in_statement(inner_stmt, true, in_if, is_function, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            // Check statements inside if (with in_if = true)
            for block in cond_blocks {
                for inner_stmt in &block.stmts {
                    check_when_in_statement(inner_stmt, in_when, true, is_function, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_when_in_statement(inner_stmt, in_when, true, is_function, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_when_in_statement(inner_stmt, in_when, in_if, is_function, result);
            }
        }
        Statement::While(block) => {
            for inner_stmt in &block.stmts {
                check_when_in_statement(inner_stmt, in_when, in_if, is_function, result);
            }
        }
        _ => {}
    }
}

/// Check if an equation contains a when-equation (at any depth)
fn contains_when_equation(eq: &Equation) -> bool {
    match eq {
        Equation::When(_) => true,
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks
                .iter()
                .any(|b| b.eqs.iter().any(contains_when_equation))
                || else_block
                    .as_ref()
                    .is_some_and(|eqs| eqs.iter().any(contains_when_equation))
        }
        Equation::For { equations, .. } => equations.iter().any(contains_when_equation),
        _ => false,
    }
}

/// Get the location of an equation (for error reporting)
fn get_equation_location(eq: &Equation) -> Option<&crate::ir::ast::Location> {
    match eq {
        Equation::When(blocks) => blocks.first().and_then(|b| b.cond.get_location()),
        Equation::Simple { lhs, .. } => lhs.get_location(),
        Equation::If { cond_blocks, .. } => cond_blocks.first().and_then(|b| b.cond.get_location()),
        _ => None,
    }
}

// =============================================================================
// FUNCTION BODY RESTRICTIONS (MLS §12.2)
// =============================================================================

/// Built-in operators that are forbidden inside functions.
/// MLS §12.2: "The following built-in operators/functions shall not be used in functions:
/// der, delay, initial, terminal, sample, pre, edge, change, reinit, cardinality"
const FORBIDDEN_FUNCTION_OPERATORS: &[&str] = &[
    "der", "delay", "initial", "terminal", "sample", "pre", "edge", "change",
    "reinit",
    // Note: cardinality is already checked by check_cardinality_context
];

/// Check for forbidden operators inside function bodies.
///
/// MLS §12.2: Certain built-in operators are not allowed in functions because
/// they relate to simulation state (derivatives, discrete events, etc.)
pub fn check_function_forbidden_operators(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check if this is a function
    if !matches!(class.class_type, ClassType::Function) {
        return result;
    }

    // Check algorithm sections
    for algo in &class.algorithms {
        for stmt in algo {
            check_statement_for_forbidden_ops(stmt, &mut result);
        }
    }

    // Check component bindings
    for (_name, comp) in &class.components {
        check_expression_for_forbidden_ops(&comp.start, &mut result);
    }

    result
}

/// Check a statement for forbidden function operators
fn check_statement_for_forbidden_ops(stmt: &Statement, result: &mut TypeCheckResult) {
    match stmt {
        Statement::Assignment { value, .. } => {
            check_expression_for_forbidden_ops(value, result);
        }
        Statement::FunctionCall { args, comp, .. } => {
            // Check if this is a forbidden operator call
            if let Some(first) = comp.parts.first()
                && FORBIDDEN_FUNCTION_OPERATORS.contains(&first.ident.text.as_str())
            {
                result.add_error(TypeError::new(
                    first.ident.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Built-in operator '{}' is not allowed inside functions",
                        first.ident.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            // Check arguments
            for arg in args {
                check_expression_for_forbidden_ops(arg, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression_for_forbidden_ops(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_statement_for_forbidden_ops(inner_stmt, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_statement_for_forbidden_ops(inner_stmt, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_statement_for_forbidden_ops(inner_stmt, result);
            }
        }
        Statement::While(block) => {
            check_expression_for_forbidden_ops(&block.cond, result);
            for inner_stmt in &block.stmts {
                check_statement_for_forbidden_ops(inner_stmt, result);
            }
        }
        Statement::When(blocks) => {
            // When is already forbidden in functions, but check inner expressions too
            for block in blocks {
                check_expression_for_forbidden_ops(&block.cond, result);
                for inner_stmt in &block.stmts {
                    check_statement_for_forbidden_ops(inner_stmt, result);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

/// Check an expression for forbidden function operators
fn check_expression_for_forbidden_ops(expr: &Expression, result: &mut TypeCheckResult) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            // Check if this is a forbidden operator call
            if let Some(first) = comp.parts.first()
                && FORBIDDEN_FUNCTION_OPERATORS.contains(&first.ident.text.as_str())
            {
                result.add_error(TypeError::new(
                    first.ident.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Built-in operator '{}' is not allowed inside functions",
                        first.ident.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            // Check arguments
            for arg in args {
                check_expression_for_forbidden_ops(arg, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression_for_forbidden_ops(lhs, result);
            check_expression_for_forbidden_ops(rhs, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression_for_forbidden_ops(rhs, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression_for_forbidden_ops(inner, result);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression_for_forbidden_ops(cond, result);
                check_expression_for_forbidden_ops(then_expr, result);
            }
            check_expression_for_forbidden_ops(else_branch, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expression_for_forbidden_ops(elem, result);
            }
        }
        Expression::ArrayComprehension { expr, .. } => {
            check_expression_for_forbidden_ops(expr, result);
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_expression_for_forbidden_ops(elem, result);
            }
        }
        Expression::Range { start, step, end } => {
            check_expression_for_forbidden_ops(start, result);
            if let Some(s) = step {
                check_expression_for_forbidden_ops(s, result);
            }
            check_expression_for_forbidden_ops(end, result);
        }
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => {}
    }
}

// =============================================================================
// CONNECT EQUATION VALIDATION (MLS §9.3)
// =============================================================================

/// Check for self-connections in connect equations.
///
/// MLS §9.3: A connector cannot be connected to itself.
/// This detects cases like `connect(c, c)` which are invalid.
pub fn check_self_connections(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check all equations for self-connections
    for eq in &class.equations {
        check_self_connection_in_equation(eq, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_self_connection_in_equation(eq, &mut result);
    }

    result
}

/// Recursively check for self-connections in an equation.
fn check_self_connection_in_equation(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Check if lhs and rhs are the same component reference
            if component_refs_equal(lhs, rhs)
                && let Some(loc) = lhs.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "Self-connection detected: a connector cannot be connected to itself"
                        .to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_self_connection_in_equation(inner_eq, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
        }
        _ => {}
    }
}

/// Check if two component references are equal (same path).
fn component_refs_equal(a: &ComponentReference, b: &ComponentReference) -> bool {
    if a.parts.len() != b.parts.len() {
        return false;
    }

    for (part_a, part_b) in a.parts.iter().zip(b.parts.iter()) {
        // Compare identifiers
        if part_a.ident.text != part_b.ident.text {
            return false;
        }

        // Compare subscripts
        match (&part_a.subs, &part_b.subs) {
            (None, None) => {}
            (Some(subs_a), Some(subs_b)) => {
                if subs_a.len() != subs_b.len() {
                    return false;
                }
                for (sub_a, sub_b) in subs_a.iter().zip(subs_b.iter()) {
                    if !subscripts_equal(sub_a, sub_b) {
                        return false;
                    }
                }
            }
            _ => return false,
        }
    }

    true
}

/// Check if two subscripts are equal.
fn subscripts_equal(a: &Subscript, b: &Subscript) -> bool {
    match (a, b) {
        (Subscript::Empty, Subscript::Empty) => true,
        (Subscript::Range { .. }, Subscript::Range { .. }) => true,
        (Subscript::Expression(expr_a), Subscript::Expression(expr_b)) => {
            expressions_structurally_equal(expr_a, expr_b)
        }
        _ => false,
    }
}

/// Check if two expressions are structurally equal (for subscript comparison).
fn expressions_structurally_equal(a: &Expression, b: &Expression) -> bool {
    match (a, b) {
        (Expression::Empty, Expression::Empty) => true,
        (
            Expression::Terminal {
                terminal_type: type_a,
                token: tok_a,
            },
            Expression::Terminal {
                terminal_type: type_b,
                token: tok_b,
            },
        ) => type_a == type_b && tok_a.text == tok_b.text,
        (Expression::ComponentReference(ref_a), Expression::ComponentReference(ref_b)) => {
            component_refs_equal(ref_a, ref_b)
        }
        (
            Expression::Binary {
                lhs: lhs_a,
                op: op_a,
                rhs: rhs_a,
            },
            Expression::Binary {
                lhs: lhs_b,
                op: op_b,
                rhs: rhs_b,
            },
        ) => {
            std::mem::discriminant(op_a) == std::mem::discriminant(op_b)
                && expressions_structurally_equal(lhs_a, lhs_b)
                && expressions_structurally_equal(rhs_a, rhs_b)
        }
        _ => false,
    }
}

/// Check that connect arguments are connectors (not regular variables).
///
/// MLS §9.1: Both arguments to connect must be connectors.
/// This validates that connect(a, b) where both a and b have connector types.
pub fn check_connect_types(
    class: &ClassDefinition,
    connector_types: &HashSet<String>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their type names
    let comp_types: HashMap<String, String> = class
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.type_name.to_string()))
        .collect();

    // Check all equations for connect type errors
    for eq in &class.equations {
        check_connect_types_in_equation(eq, &comp_types, connector_types, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_connect_types_in_equation(eq, &comp_types, connector_types, &mut result);
    }

    result
}

/// Recursively check connect types in an equation.
fn check_connect_types_in_equation(
    eq: &Equation,
    comp_types: &HashMap<String, String>,
    connector_types: &HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Only check direct connections (single-part paths like `connect(c, x)`)
            // For paths like `connect(a.p, b.p)`, the intermediate components might be
            // models/blocks with connector components, which is valid.
            // We only reject when connecting directly to a built-in type variable.

            // Check lhs - only if it's a single-part path (direct variable)
            if lhs.parts.len() == 1 {
                let lhs_name = &lhs.parts[0].ident.text;
                if let Some(lhs_type) = comp_types.get(lhs_name) {
                    // Check if it's a built-in type (definitely not a connector)
                    if is_builtin_type(lhs_type) {
                        if let Some(loc) = lhs.get_location() {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Connect argument '{}' has type '{}' which is not a connector",
                                    lhs_name, lhs_type
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                    // For non-builtin types, only error if we have full connector info
                    // and this is definitely a model/block/record (not connector)
                    else if !connector_types.is_empty() && !connector_types.contains(lhs_type) {
                        // Check if this type is known to be a model/block/record
                        // by checking our class list (we can't do this here, so skip)
                        // This check would require more context
                    }
                }
            }

            // Check rhs - only if it's a single-part path (direct variable)
            if rhs.parts.len() == 1 {
                let rhs_name = &rhs.parts[0].ident.text;
                if let Some(rhs_type) = comp_types.get(rhs_name) {
                    // Check if it's a built-in type (definitely not a connector)
                    if is_builtin_type(rhs_type)
                        && let Some(loc) = rhs.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Connect argument '{}' has type '{}' which is not a connector",
                                rhs_name, rhs_type
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
        }
        _ => {}
    }
}

/// Check if a type name is a built-in type (not a connector).
fn is_builtin_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "AssertionLevel"
    )
}

use indexmap::IndexMap;

/// Check for flow/non-flow mixing in connect equations.
///
/// MLS §9.3: Flow and non-flow variables cannot be connected directly.
/// This validates that connect(a.v, b.i) where v and i have different flow status is rejected.
pub fn check_flow_compatibility(
    class: &ClassDefinition,
    connector_classes: &IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their type names
    let comp_types: HashMap<String, String> = class
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.type_name.to_string()))
        .collect();

    // Check all equations for flow compatibility errors
    for eq in &class.equations {
        check_flow_in_equation(eq, &comp_types, connector_classes, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_flow_in_equation(eq, &comp_types, connector_classes, &mut result);
    }

    result
}

/// Recursively check flow compatibility in an equation.
fn check_flow_in_equation(
    eq: &Equation,
    comp_types: &HashMap<String, String>,
    connector_classes: &IndexMap<String, ClassDefinition>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Case 1: Connecting individual fields (e.g., connect(p1.v, p2.i))
            // We need to check if both fields have the same flow status
            if lhs.parts.len() >= 2 && rhs.parts.len() >= 2 {
                // Get the connector type for lhs
                let lhs_base = &lhs.parts[0].ident.text;
                let lhs_field = &lhs.parts[1].ident.text;

                // Get the connector type for rhs
                let rhs_base = &rhs.parts[0].ident.text;
                let rhs_field = &rhs.parts[1].ident.text;

                // Look up the connector types
                if let (Some(lhs_type), Some(rhs_type)) =
                    (comp_types.get(lhs_base), comp_types.get(rhs_base))
                {
                    // Look up the connector class definitions
                    if let (Some(lhs_class), Some(rhs_class)) = (
                        connector_classes.get(lhs_type),
                        connector_classes.get(rhs_type),
                    ) {
                        // Get the flow status of each field
                        let lhs_is_flow = get_field_flow_status(lhs_class, lhs_field);
                        let rhs_is_flow = get_field_flow_status(rhs_class, rhs_field);

                        // Check for mixing
                        if lhs_is_flow != rhs_is_flow
                            && let Some(loc) = lhs.get_location()
                        {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Cannot connect {} field '{}' to {} field '{}'",
                                    if lhs_is_flow { "flow" } else { "non-flow" },
                                    lhs_field,
                                    if rhs_is_flow { "flow" } else { "non-flow" },
                                    rhs_field
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                }
            }

            // Case 2: Connecting whole connectors (e.g., connect(a, b))
            // Check that corresponding fields have the same flow status
            if lhs.parts.len() == 1 && rhs.parts.len() == 1 {
                let lhs_name = &lhs.parts[0].ident.text;
                let rhs_name = &rhs.parts[0].ident.text;

                if let (Some(lhs_type), Some(rhs_type)) =
                    (comp_types.get(lhs_name), comp_types.get(rhs_name))
                {
                    // Different connector types - check field compatibility
                    if lhs_type != rhs_type
                        && let (Some(lhs_class), Some(rhs_class)) = (
                            connector_classes.get(lhs_type),
                            connector_classes.get(rhs_type),
                        )
                    {
                        // Check each field in lhs_class against corresponding field in rhs_class
                        for (field_name, lhs_comp) in &lhs_class.components {
                            if let Some(rhs_comp) = rhs_class.components.get(field_name) {
                                let lhs_is_flow =
                                    matches!(lhs_comp.connection, Connection::Flow(_));
                                let rhs_is_flow =
                                    matches!(rhs_comp.connection, Connection::Flow(_));

                                if lhs_is_flow != rhs_is_flow {
                                    if let Some(loc) = lhs.get_location() {
                                        result.add_error(TypeError::new(
                                                loc.clone(),
                                                SymbolType::Unknown,
                                                SymbolType::Unknown,
                                                format!(
                                                    "Incompatible connectors: field '{}' is {} in '{}' but {} in '{}'",
                                                    field_name,
                                                    if lhs_is_flow { "flow" } else { "non-flow" },
                                                    lhs_type,
                                                    if rhs_is_flow { "flow" } else { "non-flow" },
                                                    rhs_type
                                                ),
                                                TypeErrorSeverity::Error,
                                            ));
                                    }
                                    break; // Report only first mismatch
                                }
                            }
                        }
                    }
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
        }
        _ => {}
    }
}

/// Get the flow status of a field in a connector class.
fn get_field_flow_status(class: &ClassDefinition, field_name: &str) -> bool {
    if let Some(comp) = class.components.get(field_name) {
        matches!(comp.connection, Connection::Flow(_))
    } else {
        false
    }
}

/// Check that stream variables are only declared in connectors (MLS §15.1).
///
/// Stream variables can only be declared inside connector classes.
pub fn check_stream_only_in_connector(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check non-connector classes
    if class.class_type == ClassType::Connector {
        return result;
    }

    // Check all components for stream prefix
    for (name, comp) in &class.components {
        if matches!(comp.connection, Connection::Stream(_)) {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Stream variable '{}' can only be declared inside a connector, not in {}",
                    name,
                    match class.class_type {
                        ClassType::Model => "a model",
                        ClassType::Class => "a class",
                        ClassType::Block => "a block",
                        ClassType::Record => "a record",
                        ClassType::Type => "a type",
                        ClassType::Package => "a package",
                        ClassType::Function => "a function",
                        ClassType::Operator => "an operator",
                        ClassType::Connector => "a connector", // unreachable due to early return
                    }
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that stream variables have an associated flow variable (MLS §15.1).
///
/// A connector with stream variables must also have at least one flow variable.
pub fn check_stream_requires_flow(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check connectors
    if class.class_type != ClassType::Connector {
        return result;
    }

    // Check if there are any stream variables
    let stream_vars: Vec<(&String, &Component)> = class
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.connection, Connection::Stream(_)))
        .collect();

    if stream_vars.is_empty() {
        return result;
    }

    // Check if there is at least one flow variable
    let has_flow = class
        .components
        .iter()
        .any(|(_, comp)| matches!(comp.connection, Connection::Flow(_)));

    if !has_flow {
        // Report error on each stream variable
        for (name, comp) in stream_vars {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Stream variable '{}' requires an associated flow variable in the connector",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that expandable connectors do not contain flow variables (MLS §9.1.3).
///
/// Flow variables are not allowed in expandable connectors because the connection
/// set membership is not known at compile time.
pub fn check_expandable_no_flow(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check expandable connectors
    if class.class_type != ClassType::Connector || !class.expandable {
        return result;
    }

    // Check all components for flow prefix
    for (name, comp) in &class.components {
        if matches!(comp.connection, Connection::Flow(_)) {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Flow variable '{}' is not allowed in expandable connector '{}'",
                    name, class.name.text
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

// =============================================================================
// OPERATOR RECORD RESTRICTIONS (MLS §14.3)
// =============================================================================

/// Check that operator records do not use extends (MLS §14.3).
///
/// MLS §14.3: "An operator record cannot extend another class."
/// This is because operator records have special semantics for overloaded operators
/// that don't compose well with inheritance.
pub fn check_operator_record_no_extends(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check operator records
    if !class.operator_record {
        return result;
    }

    // Check if the class has any extends clauses
    if !class.extends.is_empty() {
        for ext in &class.extends {
            result.add_error(TypeError::new(
                ext.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Operator record '{}' cannot extend other classes",
                    class.name.text
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that operator records cannot be extended (MLS §14.3).
///
/// MLS §14.3: "An operator record cannot be used as a base class."
/// This validation requires checking all classes in the stored definition to find
/// any that extend an operator record.
pub fn check_operator_record_cannot_be_extended(
    class: &ClassDefinition,
    all_classes: &IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check if this class extends any operator records
    for ext in &class.extends {
        // Get the base class name
        let base_name = ext.comp.to_string();

        // Look up the base class
        if let Some(base_class) = all_classes.get(&base_name)
            && base_class.operator_record
        {
            result.add_error(TypeError::new(
                ext.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Cannot extend operator record '{}' - operator records cannot be used as base classes",
                    base_name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that operator records cannot be partial (MLS §14.3).
///
/// MLS §14.3: "An operator record shall not be partial."
/// Operator records must be complete definitions with all required operators.
pub fn check_operator_record_no_partial(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check operator records
    if !class.operator_record {
        return result;
    }

    // Check if the class is partial
    if class.partial {
        result.add_error(TypeError::new(
            class.location.clone(),
            SymbolType::Unknown,
            SymbolType::Unknown,
            format!(
                "Operator record '{}' cannot be declared as partial",
                class.name.text
            ),
            TypeErrorSeverity::Error,
        ));
    }

    result
}

// =============================================================================
// STATE MACHINE RESTRICTIONS (MLS §17)
// =============================================================================

/// Check that initialState is called at most once per state machine (MLS §17.1).
///
/// MLS §17.1: "initialState must be called exactly once in a state machine."
/// This validates that there is at most one initialState() call in the equations.
pub fn check_single_initial_state(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    let mut initial_state_calls: Vec<&Equation> = Vec::new();

    // Find all initialState() calls in equations
    for eq in &class.equations {
        if let Equation::FunctionCall { comp, .. } = eq {
            let func_name = comp.to_string();
            if func_name == "initialState" {
                initial_state_calls.push(eq);
            }
        }
    }

    // If there's more than one initialState call, report an error
    if initial_state_calls.len() > 1
        && let Some(Equation::FunctionCall { comp, .. }) = initial_state_calls.get(1)
        && let Some(loc) = comp.get_location()
    {
        result.add_error(TypeError::new(
            loc.clone(),
            SymbolType::Unknown,
            SymbolType::Unknown,
            format!(
                "Multiple initialState() calls in '{}' - only one initial state is allowed per state machine",
                class.name.text
            ),
            TypeErrorSeverity::Error,
        ));
    }

    result
}

/// Check that states passed to initialState/transition are blocks or models (MLS §17.1).
///
/// MLS §17.1: "A state must be a block or model."
/// This validates that arguments to state machine operators reference valid state types.
pub fn check_state_is_block_or_model(
    class: &ClassDefinition,
    all_classes: &indexmap::IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check equations for state machine operator calls
    for eq in &class.equations {
        if let Equation::FunctionCall { comp, args } = eq {
            let func_name = comp.to_string();

            // Check initialState(state) and transition(from, to, ...) calls
            if func_name == "initialState" || func_name == "transition" {
                // Get the state argument(s) to validate
                let state_args: Vec<&Expression> = if func_name == "initialState" {
                    args.iter().take(1).collect()
                } else {
                    // transition(from, to, condition, ...)
                    args.iter().take(2).collect()
                };

                for state_arg in state_args {
                    // The state argument should be a component reference
                    if let Expression::ComponentReference(state_ref) = state_arg {
                        let state_name = state_ref.to_string();

                        // Look up the component in the class
                        if let Some(component) = class.components.get(&state_name) {
                            // Get the type name and look up the class definition
                            let type_name = component.type_name.to_string();

                            // First check nested classes in the current class
                            let class_def = class
                                .classes
                                .get(&type_name)
                                .or_else(|| all_classes.get(&type_name));

                            if let Some(state_class) = class_def
                                && !matches!(
                                    state_class.class_type,
                                    ClassType::Block | ClassType::Model
                                )
                                && let Some(loc) = state_arg.get_location()
                            {
                                result.add_error(TypeError::new(
                                    loc.clone(),
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "State '{}' in {}() must be a block or model, not a {}",
                                        state_name,
                                        func_name,
                                        format!("{:?}", state_class.class_type).to_lowercase()
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    result
}

/// Check that transitions from the same state have unique priorities (MLS §17.2).
///
/// MLS §17.2: "Transitions from a state must have unique priorities."
/// This validates that no two transitions from the same source state have the same priority.
pub fn check_transition_priority_unique(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Collect transitions: (source_state, priority, location)
    let mut transitions: Vec<(String, i64, &Equation)> = Vec::new();

    for eq in &class.equations {
        if let Equation::FunctionCall { comp, args } = eq {
            let func_name = comp.to_string();

            if func_name == "transition" && !args.is_empty() {
                // Get the source state (first argument)
                let source_state =
                    if let Some(Expression::ComponentReference(state_ref)) = args.first() {
                        state_ref.to_string()
                    } else {
                        continue;
                    };

                // Get priority from named argument (default is 1)
                let mut priority: i64 = 1;

                // Look for priority = N in the arguments (after the first 3 positional args)
                for arg in args.iter().skip(3) {
                    if let Expression::Binary { op, lhs, rhs } = arg
                        && matches!(op, OpBinary::Assign(_))
                        && let Expression::ComponentReference(name_ref) = lhs.as_ref()
                        && name_ref.to_string() == "priority"
                        && let Expression::Terminal {
                            terminal_type: TerminalType::UnsignedInteger,
                            token,
                        } = rhs.as_ref()
                        && let Ok(n) = token.text.parse::<i64>()
                    {
                        priority = n;
                    }
                }

                transitions.push((source_state, priority, eq));
            }
        }
    }

    // Check for duplicate priorities from the same source state
    let mut seen: HashMap<(String, i64), &Equation> = HashMap::new();

    for (source, priority, eq) in &transitions {
        let key = (source.clone(), *priority);
        if let Some(prev_eq) = seen.get(&key) {
            // Found duplicate priority - report if we can get the location
            if let Equation::FunctionCall { comp, .. } = eq
                && let Some(loc) = comp.get_location()
            {
                // Get the location of the previous transition for context
                let prev_loc = if let Equation::FunctionCall {
                    comp: prev_comp, ..
                } = prev_eq
                {
                    prev_comp
                        .get_location()
                        .map(|l| format!(" (previous at line {})", l.start_line))
                        .unwrap_or_default()
                } else {
                    String::new()
                };

                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Duplicate transition priority {} from state '{}'{}",
                        priority, source, prev_loc
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        } else {
            seen.insert(key, eq);
        }
    }

    result
}

// =============================================================================
// CLOCK TYPE RESTRICTIONS (MLS §16.1)
// =============================================================================

/// Check that Clock variables don't have flow, stream, or discrete prefixes (MLS §16.1).
///
/// MLS §16.1: "Clock shall not be combined with flow, stream, or discrete."
/// This validates that Clock type variables don't use these incompatible prefixes.
pub fn check_clock_restrictions(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    for (name, comp) in &class.components {
        // Check if this is a Clock type
        let type_name = comp.type_name.to_string();
        if type_name != "Clock" {
            continue;
        }

        // Check for flow prefix
        if let Connection::Flow(token) = &comp.connection {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have flow prefix - Clock is not a physical quantity",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }

        // Check for stream prefix
        if let Connection::Stream(token) = &comp.connection {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have stream prefix - Clock is not a physical quantity",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }

        // Check for discrete prefix
        if let Variability::Discrete(token) = &comp.variability {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have discrete prefix - Clock has its own timing semantics",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}
