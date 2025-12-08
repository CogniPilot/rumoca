//! Equation expansion pass
//!
//! This module expands structured equations into scalar form according to
//! the Modelica specification. After flattening, equations should be expanded:
//!
//! - For-equations are expanded to individual scalar equations
//! - Array equations are expanded to individual element equations
//! - Binding equations in declarations are converted to regular equations
//!
//! This makes balance checking trivial: just count the number of equations.

use crate::ir::ast::{
    ClassDefinition, Component, ComponentRefPart, ComponentReference, Equation, Expression,
    ForIndex, Statement, Subscript, TerminalType, Token,
};
use indexmap::IndexMap;
use std::collections::HashSet;

/// Expand all equations in a class definition to scalar form.
///
/// This includes:
/// - Evaluating parameter-dependent array shapes
/// - Expanding for-loops to individual equations
/// - Expanding array equations to scalar equations
/// - Converting binding equations to regular equations
/// - Converting algorithm sections to equations
pub fn expand_equations(class: &mut ClassDefinition) {
    // First, evaluate any parameter-dependent array shapes
    evaluate_array_shapes(&mut class.components);

    // Expand structured equations first
    let mut expanded = Vec::new();
    for eq in &class.equations {
        expand_equation(eq, &class.components, &mut expanded);
    }

    // Convert algorithm sections to equations
    // Each algorithm section contributes one equation per unique variable assigned
    let algorithm_equations = convert_algorithms_to_equations(&class.algorithms, &class.components);
    expanded.extend(algorithm_equations);

    // Collect binding equations from components (needs expanded equations to find states)
    let binding_equations = extract_binding_equations(&class.components, &expanded);

    // Add binding equations
    expanded.extend(binding_equations);

    class.equations = expanded;

    // Also expand initial equations
    let mut expanded_init = Vec::new();
    for eq in &class.initial_equations {
        expand_equation(eq, &class.components, &mut expanded_init);
    }
    class.initial_equations = expanded_init;
}

/// Evaluate parameter-dependent array shapes.
///
/// For components with `shape_expr` but empty `shape`, try to evaluate
/// the expressions to get the actual array dimensions.
fn evaluate_array_shapes(components: &mut IndexMap<String, Component>) {
    // First, collect parameter values we can look up
    let params: IndexMap<String, Component> = components
        .iter()
        .filter(|(_, c)| matches!(c.variability, crate::ir::ast::Variability::Parameter(_)))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    // Now evaluate shapes
    for (_name, comp) in components.iter_mut() {
        if comp.shape.is_empty() && !comp.shape_expr.is_empty() {
            let mut evaluated_shape = Vec::new();
            let mut all_evaluated = true;

            for sub in &comp.shape_expr {
                match sub {
                    Subscript::Expression(expr) => {
                        if let Some(val) = eval_integer_with_params(expr, &params) {
                            // Allow val >= 0 (including 0 for empty arrays)
                            if val >= 0 {
                                evaluated_shape.push(val as usize);
                            } else {
                                all_evaluated = false;
                                break;
                            }
                        } else {
                            all_evaluated = false;
                            break;
                        }
                    }
                    Subscript::Range { .. } | Subscript::Empty => {
                        // Unbounded dimension or empty - can't evaluate statically
                        all_evaluated = false;
                        break;
                    }
                }
            }

            // Set the shape if all dimensions were evaluated (even if result is empty array [0])
            if all_evaluated {
                comp.shape = evaluated_shape;
            }
        }
    }
}

/// Extract binding equations from component declarations.
///
/// A binding equation is `Real x = expr;` (as opposed to `Real x(start=expr);`).
/// These are identified by `start_is_modification == false` with a non-empty start expression.
///
/// Note: We need to check if the variable is differentiated (appears in der()) -
/// if so, it's a state variable and the binding is an initial value, not an equation.
fn extract_binding_equations(
    components: &IndexMap<String, Component>,
    equations: &[Equation],
) -> Vec<Equation> {
    // First, find all variables that appear in der() calls - these are states
    let states = find_differentiated_variables(equations);

    let mut binding_equations = Vec::new();

    for (name, comp) in components {
        // Skip if this is a modification (start=x) rather than a binding (= x)
        if comp.start_is_modification {
            continue;
        }

        // Skip if there's no start expression (empty binding)
        if matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip if the start value is just a default numeric literal (0, 0.0)
        // These are not actual binding equations, just default values
        if is_default_value(&comp.start) {
            continue;
        }

        // Skip parameters and constants - they don't contribute equations
        if matches!(
            comp.variability,
            crate::ir::ast::Variability::Parameter(_) | crate::ir::ast::Variability::Constant(_)
        ) {
            continue;
        }

        // Skip inputs - they are provided externally
        if matches!(comp.causality, crate::ir::ast::Causality::Input(..)) {
            continue;
        }

        // Skip state variables - their binding is an initial condition, not an equation
        if states.contains(name) {
            continue;
        }

        // Create equation: name = start_expr
        let lhs = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: name.clone(),
                    ..Default::default()
                },
                subs: None,
            }],
        });

        // If the component is an array, we need to expand to scalar equations
        if comp.shape.is_empty() {
            // Scalar binding equation
            binding_equations.push(Equation::Simple {
                lhs,
                rhs: comp.start.clone(),
            });
        } else {
            // Array binding equation - expand to scalars
            expand_array_binding(name, &comp.shape, &comp.start, &mut binding_equations);
        }
    }

    binding_equations
}

/// Convert algorithm sections to equations.
///
/// In Modelica, each algorithm section contributes one equation per unique
/// variable that is assigned. This function finds all assigned variables
/// and creates Simple equations for them.
///
/// For balance checking purposes, we create a simple placeholder equation
/// for each unique assigned variable. The actual algorithm semantics would
/// need more sophisticated handling for simulation.
fn convert_algorithms_to_equations(
    algorithms: &[Vec<Statement>],
    components: &IndexMap<String, Component>,
) -> Vec<Equation> {
    let mut equations = Vec::new();

    for algorithm_section in algorithms {
        // Find all unique variables assigned in this algorithm section
        let assigned_vars = find_assigned_variables(algorithm_section);

        // Create one equation per assigned variable
        for var_name in assigned_vars {
            // Skip if it's an input (inputs don't need equations)
            if let Some(comp) = components.get(&var_name)
                && matches!(comp.causality, crate::ir::ast::Causality::Input(..))
            {
                continue;
            }

            // Create a placeholder equation: var = var (self-assignment)
            // This is a simplification - the actual algorithm semantics are procedural
            let comp_ref = ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: var_name.clone(),
                        ..Default::default()
                    },
                    subs: None,
                }],
            };

            let lhs = Expression::ComponentReference(comp_ref.clone());
            let rhs = Expression::ComponentReference(comp_ref);

            equations.push(Equation::Simple { lhs, rhs });
        }
    }

    equations
}

/// Find all unique variable names assigned in an algorithm section.
fn find_assigned_variables(statements: &[Statement]) -> HashSet<String> {
    let mut assigned = HashSet::new();

    for stmt in statements {
        collect_assigned_variables(stmt, &mut assigned);
    }

    assigned
}

/// Recursively collect assigned variable names from a statement.
fn collect_assigned_variables(stmt: &Statement, assigned: &mut HashSet<String>) {
    match stmt {
        Statement::Assignment { comp, .. } => {
            // Get the base variable name (first part of component reference)
            if let Some(first_part) = comp.parts.first() {
                assigned.insert(first_part.ident.text.clone());
            }
        }
        Statement::For { equations, .. } => {
            for inner in equations {
                collect_assigned_variables(inner, assigned);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner in &block.stmts {
                    collect_assigned_variables(inner, assigned);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    collect_assigned_variables(inner, assigned);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for inner in &block.stmts {
                    collect_assigned_variables(inner, assigned);
                }
            }
        }
        Statement::While(block) => {
            for inner in &block.stmts {
                collect_assigned_variables(inner, assigned);
            }
        }
        Statement::Empty
        | Statement::Return { .. }
        | Statement::Break { .. }
        | Statement::FunctionCall { .. } => {}
    }
}

/// Find all variables that appear inside der() calls (state variables).
fn find_differentiated_variables(equations: &[Equation]) -> std::collections::HashSet<String> {
    let mut states = std::collections::HashSet::new();
    for eq in equations {
        find_der_vars_in_equation(eq, &mut states);
    }
    states
}

fn find_der_vars_in_equation(eq: &Equation, states: &mut std::collections::HashSet<String>) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            find_der_vars_in_expr(lhs, states);
            find_der_vars_in_expr(rhs, states);
        }
        Equation::For { equations, .. } => {
            for inner in equations {
                find_der_vars_in_equation(inner, states);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner in &block.eqs {
                    find_der_vars_in_equation(inner, states);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner in else_eqs {
                    find_der_vars_in_equation(inner, states);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner in &block.eqs {
                    find_der_vars_in_equation(inner, states);
                }
            }
        }
        _ => {}
    }
}

fn find_der_vars_in_expr(expr: &Expression, states: &mut std::collections::HashSet<String>) {
    match expr {
        Expression::FunctionCall { comp, args } => {
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
            {
                // This is a der() call - extract the variable name
                if let Some(Expression::ComponentReference(comp_ref)) = args.first()
                    && let Some(part) = comp_ref.parts.first()
                {
                    states.insert(part.ident.text.clone());
                }
            }
            // Also check arguments recursively
            for arg in args {
                find_der_vars_in_expr(arg, states);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            find_der_vars_in_expr(lhs, states);
            find_der_vars_in_expr(rhs, states);
        }
        Expression::Unary { rhs, .. } => {
            find_der_vars_in_expr(rhs, states);
        }
        Expression::Array { elements } => {
            for e in elements {
                find_der_vars_in_expr(e, states);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, expr) in branches {
                find_der_vars_in_expr(cond, states);
                find_der_vars_in_expr(expr, states);
            }
            find_der_vars_in_expr(else_branch, states);
        }
        _ => {}
    }
}

/// Check if an expression is a default/trivial value (0, 0.0, false)
/// that should not be treated as a binding equation.
fn is_default_value(expr: &Expression) -> bool {
    match expr {
        Expression::Terminal {
            terminal_type,
            token,
        } => {
            match terminal_type {
                TerminalType::UnsignedInteger => token.text == "0",
                TerminalType::UnsignedReal => {
                    // Check for 0, 0.0, 0.0e0, etc.
                    if let Ok(val) = token.text.parse::<f64>() {
                        val == 0.0
                    } else {
                        false
                    }
                }
                TerminalType::Bool => token.text == "false",
                _ => false,
            }
        }
        _ => false,
    }
}

/// Expand an array binding equation to scalar equations.
fn expand_array_binding(
    name: &str,
    shape: &[usize],
    rhs: &Expression,
    equations: &mut Vec<Equation>,
) {
    // For now, handle 1D arrays
    if shape.len() == 1 {
        let size = shape[0];
        for i in 1..=size {
            let lhs = make_subscripted_ref(name, &[i]);

            // If RHS is an array literal, extract the corresponding element
            // Otherwise, subscript the RHS as well
            let rhs_elem = match rhs {
                Expression::Array { elements } => {
                    if i <= elements.len() {
                        elements[i - 1].clone()
                    } else {
                        subscript_expr(rhs.clone(), &[i])
                    }
                }
                _ => subscript_expr(rhs.clone(), &[i]),
            };

            equations.push(Equation::Simple { lhs, rhs: rhs_elem });
        }
    } else {
        // Multi-dimensional arrays - create nested subscripts
        expand_array_binding_nd(name, shape, 0, &[], rhs, equations);
    }
}

/// Recursively expand multi-dimensional array binding.
fn expand_array_binding_nd(
    name: &str,
    shape: &[usize],
    dim: usize,
    indices: &[usize],
    rhs: &Expression,
    equations: &mut Vec<Equation>,
) {
    if dim >= shape.len() {
        // Base case: all dimensions indexed
        let lhs = make_subscripted_ref(name, indices);
        let rhs_elem = subscript_expr_nd(rhs.clone(), indices);
        equations.push(Equation::Simple { lhs, rhs: rhs_elem });
        return;
    }

    for i in 1..=shape[dim] {
        let mut new_indices = indices.to_vec();
        new_indices.push(i);
        expand_array_binding_nd(name, shape, dim + 1, &new_indices, rhs, equations);
    }
}

/// Expand a single equation, recursively handling For and If structures.
fn expand_equation(
    eq: &Equation,
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    match eq {
        Equation::Empty => {}

        Equation::Simple { lhs, rhs } => {
            // Check if this is an array equation that needs expansion
            if let Some(size) = get_equation_array_size(lhs, components) {
                if size == 0 {
                    // Empty array equation (e.g., y[0] = u[0]) - no scalar equations
                    return;
                }
                if size > 1 {
                    expand_array_equation(lhs, rhs, size, components, out);
                    return;
                }
            }
            // Scalar equation - keep as is
            out.push(eq.clone());
        }

        Equation::For { indices, equations } => {
            // Expand for-loop to individual equations
            expand_for_equation(indices, equations, components, out);
        }

        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Try to evaluate conditions at compile time for parameter-based conditions
            let mut selected_branch: Option<&Vec<Equation>> = None;

            for block in cond_blocks {
                if let Some(val) = eval_boolean(&block.cond, components) {
                    if val {
                        selected_branch = Some(&block.eqs);
                        break;
                    }
                    // Condition is false, try next branch
                } else {
                    // Can't evaluate at compile time - keep the structure
                    let mut expanded_cond_blocks = Vec::new();
                    for block in cond_blocks {
                        let mut expanded_eqs = Vec::new();
                        for inner_eq in &block.eqs {
                            expand_equation(inner_eq, components, &mut expanded_eqs);
                        }
                        expanded_cond_blocks.push(crate::ir::ast::EquationBlock {
                            cond: block.cond.clone(),
                            eqs: expanded_eqs,
                        });
                    }

                    let expanded_else = else_block.as_ref().map(|eqs| {
                        let mut expanded = Vec::new();
                        for inner_eq in eqs {
                            expand_equation(inner_eq, components, &mut expanded);
                        }
                        expanded
                    });

                    out.push(Equation::If {
                        cond_blocks: expanded_cond_blocks,
                        else_block: expanded_else,
                    });
                    return;
                }
            }

            // If no true branch found, use else branch
            let eqs_to_expand = selected_branch.or(else_block.as_ref());

            if let Some(eqs) = eqs_to_expand {
                for inner_eq in eqs {
                    expand_equation(inner_eq, components, out);
                }
            }
        }

        Equation::When(blocks) => {
            // Expand equations inside when blocks
            let mut expanded_blocks = Vec::new();
            for block in blocks {
                let mut expanded_eqs = Vec::new();
                for inner_eq in &block.eqs {
                    expand_equation(inner_eq, components, &mut expanded_eqs);
                }
                expanded_blocks.push(crate::ir::ast::EquationBlock {
                    cond: block.cond.clone(),
                    eqs: expanded_eqs,
                });
            }
            out.push(Equation::When(expanded_blocks));
        }

        Equation::Connect { .. } | Equation::FunctionCall { .. } => {
            // Keep these as is
            out.push(eq.clone());
        }
    }
}

/// Expand a for-equation to individual scalar equations.
fn expand_for_equation(
    indices: &[ForIndex],
    equations: &[Equation],
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    if indices.is_empty() {
        // No more indices to expand - expand the inner equations
        for eq in equations {
            expand_equation(eq, components, out);
        }
        return;
    }

    // Get the range for the first index
    let index = &indices[0];
    let range = get_iteration_range(&index.range, components);

    if let Some((start, end, step)) = range {
        // Expand the for-loop
        let index_name = &index.ident.text;
        let mut i = start;
        while (step > 0 && i <= end) || (step < 0 && i >= end) {
            // Substitute the index variable in the inner equations
            for eq in equations {
                let substituted = substitute_index(eq, index_name, i);
                // Recursively expand remaining indices
                expand_for_equation(&indices[1..], &[substituted], components, out);
            }
            i += step;
        }
    } else {
        // Couldn't determine the range (e.g., parameter-dependent)
        // Keep the for-equation as is, but expand inner equations
        let mut expanded_inner = Vec::new();
        for eq in equations {
            expand_equation(eq, components, &mut expanded_inner);
        }
        out.push(Equation::For {
            indices: indices.to_vec(),
            equations: expanded_inner,
        });
    }
}

/// Get the iteration range from a range expression.
/// Returns (start, end, step) if determinable, None otherwise.
fn get_iteration_range(
    expr: &Expression,
    components: &IndexMap<String, Component>,
) -> Option<(i64, i64, i64)> {
    match expr {
        Expression::Range { start, step, end } => {
            let start_val = eval_integer_with_params(start, components)?;
            let end_val = eval_integer_with_params(end, components)?;
            let step_val = step
                .as_ref()
                .map(|s| eval_integer_with_params(s, components))
                .unwrap_or(Some(1))?;
            Some((start_val, end_val, step_val))
        }
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => {
            // Single value means 1:value
            let n: i64 = token.text.parse().ok()?;
            Some((1, n, 1))
        }
        Expression::ComponentReference(_) => {
            // Could be a parameter reference like `n`
            let val = eval_integer_with_params(expr, components)?;
            Some((1, val, 1))
        }
        _ => None,
    }
}

/// Evaluate a boolean expression if possible (for parameter conditions).
fn eval_boolean(expr: &Expression, components: &IndexMap<String, Component>) -> Option<bool> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token,
        } => Some(token.text == "true"),

        Expression::ComponentReference(comp_ref) => {
            // Look up the component to see if it's a parameter with a known value
            if comp_ref.parts.len() == 1 && comp_ref.parts[0].subs.is_none() {
                let name = &comp_ref.parts[0].ident.text;
                if let Some(comp) = components.get(name) {
                    // Only evaluate parameters with known values
                    if matches!(comp.variability, crate::ir::ast::Variability::Parameter(_)) {
                        return eval_boolean(&comp.start, components);
                    }
                }
            }
            None
        }

        Expression::Unary { op, rhs } => {
            let val = eval_boolean(rhs, components)?;
            match op {
                crate::ir::ast::OpUnary::Not(_) => Some(!val),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            // Try boolean operators first
            match op {
                crate::ir::ast::OpBinary::And(_) => {
                    let l = eval_boolean(lhs, components)?;
                    let r = eval_boolean(rhs, components)?;
                    Some(l && r)
                }
                crate::ir::ast::OpBinary::Or(_) => {
                    let l = eval_boolean(lhs, components)?;
                    let r = eval_boolean(rhs, components)?;
                    Some(l || r)
                }
                // Comparison operators - use eval_integer_with_params to handle parameter references
                crate::ir::ast::OpBinary::Eq(_) => {
                    // Try integer comparison with parameter lookup
                    if let (Some(l), Some(r)) = (
                        eval_integer_with_params(lhs, components),
                        eval_integer_with_params(rhs, components),
                    ) {
                        return Some(l == r);
                    }
                    // Try boolean comparison
                    if let (Some(l), Some(r)) =
                        (eval_boolean(lhs, components), eval_boolean(rhs, components))
                    {
                        return Some(l == r);
                    }
                    None
                }
                crate::ir::ast::OpBinary::Neq(_) => {
                    if let (Some(l), Some(r)) = (
                        eval_integer_with_params(lhs, components),
                        eval_integer_with_params(rhs, components),
                    ) {
                        return Some(l != r);
                    }
                    if let (Some(l), Some(r)) =
                        (eval_boolean(lhs, components), eval_boolean(rhs, components))
                    {
                        return Some(l != r);
                    }
                    None
                }
                crate::ir::ast::OpBinary::Lt(_) => {
                    let l = eval_integer_with_params(lhs, components)?;
                    let r = eval_integer_with_params(rhs, components)?;
                    Some(l < r)
                }
                crate::ir::ast::OpBinary::Le(_) => {
                    let l = eval_integer_with_params(lhs, components)?;
                    let r = eval_integer_with_params(rhs, components)?;
                    Some(l <= r)
                }
                crate::ir::ast::OpBinary::Gt(_) => {
                    let l = eval_integer_with_params(lhs, components)?;
                    let r = eval_integer_with_params(rhs, components)?;
                    Some(l > r)
                }
                crate::ir::ast::OpBinary::Ge(_) => {
                    let l = eval_integer_with_params(lhs, components)?;
                    let r = eval_integer_with_params(rhs, components)?;
                    Some(l >= r)
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// Evaluate an expression to an integer, with parameter lookup support.
fn eval_integer_with_params(
    expr: &Expression,
    components: &IndexMap<String, Component>,
) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } => {
            let f: f64 = token.text.parse().ok()?;
            if f.fract() == 0.0 {
                Some(f as i64)
            } else {
                None
            }
        }
        Expression::ComponentReference(comp_ref) => {
            // Look up the component to see if it's a parameter with a known value
            if comp_ref.parts.len() == 1 && comp_ref.parts[0].subs.is_none() {
                let name = &comp_ref.parts[0].ident.text;
                if let Some(comp) = components.get(name) {
                    // Only evaluate parameters with known values
                    if matches!(comp.variability, crate::ir::ast::Variability::Parameter(_)) {
                        return eval_integer_with_params(&comp.start, components);
                    }
                }
            }
            None
        }
        Expression::Unary { op, rhs } => {
            let val = eval_integer_with_params(rhs, components)?;
            match op {
                crate::ir::ast::OpUnary::Minus(_) => Some(-val),
                crate::ir::ast::OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            let l = eval_integer_with_params(lhs, components)?;
            let r = eval_integer_with_params(rhs, components)?;
            match op {
                crate::ir::ast::OpBinary::Add(_) => Some(l + r),
                crate::ir::ast::OpBinary::Sub(_) => Some(l - r),
                crate::ir::ast::OpBinary::Mul(_) => Some(l * r),
                crate::ir::ast::OpBinary::Div(_) => {
                    if r != 0 {
                        Some(l / r)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        Expression::FunctionCall { comp, args } => {
            // Handle size(array, dim) function
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "size"
                && !args.is_empty()
            {
                // Get the array argument
                if let Expression::ComponentReference(array_ref) = &args[0]
                    && array_ref.parts.len() == 1
                    && array_ref.parts[0].subs.is_none()
                {
                    let array_name = &array_ref.parts[0].ident.text;
                    if let Some(array_comp) = components.get(array_name) {
                        // Get the dimension index (default to 1)
                        let dim_index = if args.len() >= 2 {
                            eval_integer_with_params(&args[1], components).unwrap_or(1) as usize
                        } else {
                            1
                        };

                        // First check evaluated shape
                        if !array_comp.shape.is_empty() && dim_index <= array_comp.shape.len() {
                            return Some(array_comp.shape[dim_index - 1] as i64);
                        }

                        // Check if it's an array literal in start
                        if let Expression::Array { elements } = &array_comp.start
                            && dim_index == 1
                        {
                            return Some(elements.len() as i64);
                        }
                    }
                }
            }
            None
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            // Evaluate if-then-else expression for conditional array sizes
            // e.g., if filterType == LowPass then 0 else na
            for (condition, result) in branches {
                if let Some(cond_val) = eval_boolean(condition, components) {
                    if cond_val {
                        return eval_integer_with_params(result, components);
                    }
                } else {
                    // Condition can't be evaluated at compile time
                    return None;
                }
            }
            // All conditions were false, evaluate else branch
            eval_integer_with_params(else_branch, components)
        }
        _ => None,
    }
}

/// Substitute an index variable with a concrete value in an equation.
fn substitute_index(eq: &Equation, index_name: &str, value: i64) -> Equation {
    match eq {
        Equation::Simple { lhs, rhs } => Equation::Simple {
            lhs: substitute_in_expr(lhs, index_name, value),
            rhs: substitute_in_expr(rhs, index_name, value),
        },
        Equation::For { indices, equations } => {
            // Check if this introduces a shadowing variable
            let is_shadowed = indices.iter().any(|idx| idx.ident.text == index_name);
            if is_shadowed {
                eq.clone()
            } else {
                Equation::For {
                    indices: indices.clone(),
                    equations: equations
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => Equation::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|b| crate::ir::ast::EquationBlock {
                    cond: substitute_in_expr(&b.cond, index_name, value),
                    eqs: b
                        .eqs
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                })
                .collect(),
            else_block: else_block.as_ref().map(|eqs| {
                eqs.iter()
                    .map(|e| substitute_index(e, index_name, value))
                    .collect()
            }),
        },
        Equation::When(blocks) => Equation::When(
            blocks
                .iter()
                .map(|b| crate::ir::ast::EquationBlock {
                    cond: substitute_in_expr(&b.cond, index_name, value),
                    eqs: b
                        .eqs
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                })
                .collect(),
        ),
        _ => eq.clone(),
    }
}

/// Substitute an index variable in an expression.
fn substitute_in_expr(expr: &Expression, index_name: &str, value: i64) -> Expression {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            // Check if this is just the index variable itself
            if comp_ref.parts.len() == 1
                && comp_ref.parts[0].subs.is_none()
                && comp_ref.parts[0].ident.text == index_name
            {
                // Replace with literal value
                return Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: value.to_string(),
                        ..Default::default()
                    },
                };
            }

            // Substitute in subscripts
            let new_parts: Vec<ComponentRefPart> = comp_ref
                .parts
                .iter()
                .map(|part| ComponentRefPart {
                    ident: part.ident.clone(),
                    subs: part.subs.as_ref().map(|subs| {
                        subs.iter()
                            .map(|s| match s {
                                Subscript::Expression(e) => {
                                    Subscript::Expression(substitute_in_expr(e, index_name, value))
                                }
                                _ => s.clone(),
                            })
                            .collect()
                    }),
                })
                .collect();

            Expression::ComponentReference(ComponentReference {
                local: comp_ref.local,
                parts: new_parts,
            })
        }

        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_in_expr(lhs, index_name, value)),
            rhs: Box::new(substitute_in_expr(rhs, index_name, value)),
        },

        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_in_expr(rhs, index_name, value)),
        },

        Expression::FunctionCall { comp, args } => Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|a| substitute_in_expr(a, index_name, value))
                .collect(),
        },

        Expression::Array { elements } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_in_expr(e, index_name, value))
                .collect(),
        },

        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(cond, expr)| {
                    (
                        substitute_in_expr(cond, index_name, value),
                        substitute_in_expr(expr, index_name, value),
                    )
                })
                .collect(),
            else_branch: Box::new(substitute_in_expr(else_branch, index_name, value)),
        },

        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(substitute_in_expr(start, index_name, value)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_in_expr(s, index_name, value))),
            end: Box::new(substitute_in_expr(end, index_name, value)),
        },

        Expression::Parenthesized { inner } => Expression::Parenthesized {
            inner: Box::new(substitute_in_expr(inner, index_name, value)),
        },

        _ => expr.clone(),
    }
}

/// Get the array size of an equation's LHS if it's an array variable.
fn get_equation_array_size(
    lhs: &Expression,
    components: &IndexMap<String, Component>,
) -> Option<usize> {
    match lhs {
        Expression::ComponentReference(comp_ref) => {
            if let Some(first_part) = comp_ref.parts.first() {
                // If already subscripted, it's a scalar access
                if first_part
                    .subs
                    .as_ref()
                    .map(|s| !s.is_empty())
                    .unwrap_or(false)
                {
                    return Some(1);
                }

                let name = &first_part.ident.text;
                if let Some(comp) = components.get(name) {
                    if comp.shape.is_empty() {
                        Some(1)
                    } else {
                        Some(comp.shape.iter().product())
                    }
                } else {
                    Some(1)
                }
            } else {
                Some(1)
            }
        }
        Expression::FunctionCall { comp, args } => {
            // Handle der(x) - get size from argument
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
                && let Some(arg) = args.first()
            {
                return get_equation_array_size(arg, components);
            }
            Some(1)
        }
        Expression::Array { elements } => {
            // Array literal like [y] or [u1; u2]
            // Sum up the sizes of all elements (handles concatenation)
            let mut total = 0;
            for elem in elements {
                if let Some(size) = get_equation_array_size(elem, components) {
                    total += size;
                } else {
                    return None;
                }
            }
            Some(total)
        }
        _ => Some(1),
    }
}

/// Expand an array equation to scalar equations.
/// Takes components map for determining array sizes during flattening.
fn expand_array_equation(
    lhs: &Expression,
    rhs: &Expression,
    size: usize,
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    // For 1D arrays, expand to individual equations using flat indexing
    for i in 1..=size {
        let lhs_elem = flatten_and_subscript(lhs, i, components);
        let rhs_elem = flatten_and_subscript(rhs, i, components);
        out.push(Equation::Simple {
            lhs: lhs_elem,
            rhs: rhs_elem,
        });
    }
}

/// Create a subscripted component reference.
fn make_subscripted_ref(name: &str, indices: &[usize]) -> Expression {
    Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: name.to_string(),
                ..Default::default()
            },
            subs: Some(
                indices
                    .iter()
                    .map(|&i| {
                        Subscript::Expression(Expression::Terminal {
                            terminal_type: TerminalType::UnsignedInteger,
                            token: Token {
                                text: i.to_string(),
                                ..Default::default()
                            },
                        })
                    })
                    .collect(),
            ),
        }],
    })
}

/// Add subscripts to an expression.
/// For flat indexing, use subscript_expr_flat which handles concatenated arrays.
fn subscript_expr(expr: Expression, indices: &[usize]) -> Expression {
    match expr {
        Expression::ComponentReference(mut comp_ref) => {
            // Add subscripts to the first part
            if let Some(first_part) = comp_ref.parts.first_mut() {
                let new_subs: Vec<Subscript> = indices
                    .iter()
                    .map(|&i| {
                        Subscript::Expression(Expression::Terminal {
                            terminal_type: TerminalType::UnsignedInteger,
                            token: Token {
                                text: i.to_string(),
                                ..Default::default()
                            },
                        })
                    })
                    .collect();

                first_part.subs = Some(new_subs);
            }
            Expression::ComponentReference(comp_ref)
        }
        Expression::FunctionCall { comp, args } => {
            // Handle der(x) -> der(x[i])
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
                && args.len() == 1
            {
                return Expression::FunctionCall {
                    comp,
                    args: vec![subscript_expr(args[0].clone(), indices)],
                };
            }
            // For other functions, subscript the whole result
            Expression::FunctionCall { comp, args }
        }
        Expression::Unary { op, rhs } => Expression::Unary {
            op,
            rhs: Box::new(subscript_expr(*rhs, indices)),
        },
        Expression::Array { ref elements } => {
            // For array literals, extract the element
            if elements.is_empty() {
                // Empty array - return unchanged
                expr
            } else if indices.len() == 1 && indices[0] > 0 && indices[0] <= elements.len() {
                elements[indices[0] - 1].clone()
            } else {
                subscript_expr(elements[0].clone(), indices)
            }
        }
        _ => expr,
    }
}

/// Flatten an array expression and get element at flat index.
/// For example, [[a, b], [c]] with index 2 returns b, index 3 returns c.
/// This handles matrix concatenation like [y] or [u1; u2].
fn flatten_and_subscript(
    expr: &Expression,
    flat_index: usize,
    components: &IndexMap<String, Component>,
) -> Expression {
    match expr {
        Expression::Array { elements } => {
            // Track cumulative size as we go through elements
            let mut cumulative = 0;
            for elem in elements {
                let elem_size = get_equation_array_size(elem, components).unwrap_or(1);
                if flat_index <= cumulative + elem_size {
                    // The index falls within this element
                    let local_index = flat_index - cumulative;
                    if elem_size == 1 {
                        // Scalar element - return as-is
                        return elem.clone();
                    } else {
                        // Array element - recurse or subscript
                        return flatten_and_subscript(elem, local_index, components);
                    }
                }
                cumulative += elem_size;
            }
            // Fallback: shouldn't reach here if sizes are correct
            expr.clone()
        }
        Expression::ComponentReference(comp_ref) => {
            // Subscript the component reference
            if let Some(first_part) = comp_ref.parts.first() {
                let name = &first_part.ident.text;
                if let Some(comp) = components.get(name)
                    && !comp.shape.is_empty()
                {
                    // It's an array - subscript it
                    return subscript_expr(expr.clone(), &[flat_index]);
                }
            }
            // Scalar - return as-is
            expr.clone()
        }
        _ => expr.clone(),
    }
}

/// Add subscripts to an expression for multi-dimensional arrays.
fn subscript_expr_nd(expr: Expression, indices: &[usize]) -> Expression {
    subscript_expr(expr, indices)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_integer_with_params() {
        let components = IndexMap::new();

        // Test simple integer
        let expr = Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: "3".to_string(),
                ..Default::default()
            },
        };
        assert_eq!(eval_integer_with_params(&expr, &components), Some(3));

        // Test negative
        let expr = Expression::Unary {
            op: crate::ir::ast::OpUnary::Minus(Token::default()),
            rhs: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "5".to_string(),
                    ..Default::default()
                },
            }),
        };
        assert_eq!(eval_integer_with_params(&expr, &components), Some(-5));
    }

    #[test]
    fn test_get_iteration_range() {
        // Test 1:3
        let range = Expression::Range {
            start: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "1".to_string(),
                    ..Default::default()
                },
            }),
            step: None,
            end: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "3".to_string(),
                    ..Default::default()
                },
            }),
        };
        let components = IndexMap::new();
        assert_eq!(get_iteration_range(&range, &components), Some((1, 3, 1)));
    }
}
