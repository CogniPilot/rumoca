//! Balance checking for Modelica models
//!
//! A balanced model has the same number of equations as unknowns.
//! This module provides utilities to count equations and unknowns
//! and check model balance.

use crate::dae::ast::Dae;
use crate::ir::ast::{ClassDefinition, Component, Equation, Expression, Statement, Variability};
use crate::ir::visitor::{Visitable, Visitor};
use indexmap::IndexMap;

/// Result of a balance check
#[derive(Debug, Clone, PartialEq)]
pub struct BalanceCheckResult {
    /// Number of equations in the model
    pub num_equations: usize,
    /// Number of unknown variables (states + algebraic + discrete)
    pub num_unknowns: usize,
    /// Number of state variables (variables that appear in der())
    pub num_states: usize,
    /// Number of algebraic variables
    pub num_algebraic: usize,
    /// Number of parameters
    pub num_parameters: usize,
    /// Number of inputs
    pub num_inputs: usize,
    /// Whether the model is balanced
    pub is_balanced: bool,
}

impl BalanceCheckResult {
    /// Get the difference between equations and unknowns
    pub fn difference(&self) -> i64 {
        self.num_equations as i64 - self.num_unknowns as i64
    }

    /// Get a human-readable description of the balance status
    pub fn status_message(&self) -> String {
        if self.is_balanced {
            format!(
                "Model is balanced: {} equations, {} unknowns ({} states, {} algebraic)",
                self.num_equations, self.num_unknowns, self.num_states, self.num_algebraic
            )
        } else {
            let diff = self.difference();
            if diff > 0 {
                format!(
                    "Model is over-determined: {} equations, {} unknowns ({} extra equations)",
                    self.num_equations,
                    self.num_unknowns,
                    diff.abs()
                )
            } else {
                format!(
                    "Model is under-determined: {} equations, {} unknowns ({} missing equations)",
                    self.num_equations,
                    self.num_unknowns,
                    diff.abs()
                )
            }
        }
    }
}

/// Check if a type name represents a primitive/built-in type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Check balance of a flattened class definition
pub fn check_class_balance(class: &ClassDefinition) -> BalanceCheckResult {
    // Count equations
    let num_equations = count_equations(&class.equations);

    // Count variables by category
    let mut num_algebraic: usize = 0;
    let mut num_parameters: usize = 0;
    let mut num_inputs: usize = 0;

    for (_name, comp) in &class.components {
        let type_name = comp.type_name.to_string();

        // Skip class instances - they have their own balance
        // Only count primitive types (Real, Integer, Boolean, String)
        if !is_primitive_type(&type_name) {
            continue;
        }

        // Skip constants and parameters - they are known
        if matches!(
            comp.variability,
            Variability::Constant(_) | Variability::Parameter(_)
        ) {
            num_parameters += 1;
            continue;
        }

        // Skip inputs - they are known from outside
        if matches!(comp.causality, crate::ir::ast::Causality::Input(_)) {
            num_inputs += 1;
            continue;
        }

        // All other Real variables are unknowns
        // (states are a subset - identified by appearing in der())
        num_algebraic += 1;
    }

    // Find states by looking for der(x) calls in equations
    let states = find_state_variables(&class.equations);
    let num_states = states.len();

    // Algebraic = total unknowns - states
    // But we counted all non-parameter/input as algebraic above
    // So adjust: algebraic = (algebraic we counted) - states found
    // Actually, states are ALSO in the algebraic count, so:
    // total unknowns = algebraic (which already excludes parameters/inputs)
    // states is a subset of those unknowns
    let num_unknowns = num_algebraic;

    // For reporting, separate states from algebraic
    num_algebraic = num_algebraic.saturating_sub(num_states);

    BalanceCheckResult {
        num_equations,
        num_unknowns,
        num_states,
        num_algebraic,
        num_parameters,
        num_inputs,
        is_balanced: num_equations == num_unknowns,
    }
}

/// Count total scalar elements in a collection of components
/// (accounting for array dimensions and filtering conditional components)
fn count_scalar_elements(components: &IndexMap<String, Component>) -> usize {
    count_scalar_elements_with_params(components, &IndexMap::new())
}

/// Count scalar elements with parameter context for evaluating conditions and dimensions
fn count_scalar_elements_with_params(
    components: &IndexMap<String, Component>,
    params: &IndexMap<String, Component>,
) -> usize {
    components
        .values()
        .filter_map(|comp| {
            // Check if this component has a condition
            if let Some(condition) = &comp.condition {
                // Try to evaluate the condition
                match try_evaluate_boolean(condition, params) {
                    Some(true) => {}            // Condition is true, include component
                    Some(false) => return None, // Condition is false, skip component
                    None => {}                  // Can't evaluate, include component (conservative)
                }
            }

            // Calculate scalar count for this component
            let count = if !comp.shape.is_empty() {
                // Shape is already evaluated - use it directly
                // Note: product of [0] is 0, meaning empty array = 0 unknowns
                comp.shape.iter().product::<usize>()
            } else if !comp.shape_expr.is_empty() {
                // Try to evaluate shape_expr with parameter context
                let mut product = 1usize;
                for subscript in &comp.shape_expr {
                    match subscript {
                        crate::ir::ast::Subscript::Expression(dim_expr) => {
                            if let Some(dim) = try_evaluate_integer_with_params(dim_expr, params) {
                                if dim <= 0 {
                                    // Zero or negative dimension means empty array
                                    product = 0;
                                    break;
                                }
                                product *= dim as usize;
                            } else {
                                // Can't evaluate dimension - default to 1
                                // This is conservative (might undercount)
                                product *= 1;
                            }
                        }
                        crate::ir::ast::Subscript::Range { .. } => {
                            // Colon subscript (`:`) means inferred dimension
                            // Can't evaluate without runtime info - default to 1
                            product *= 1;
                        }
                        crate::ir::ast::Subscript::Empty => {}
                    }
                }
                product
            } else {
                1 // Scalar
            };
            // Skip components with zero count (empty arrays)
            if count == 0 {
                return None;
            }
            Some(count)
        })
        .sum()
}

/// Maximum recursion depth for expression evaluation to prevent stack overflow
const MAX_EVAL_DEPTH: usize = 50;

/// Try to evaluate a boolean expression using parameter values
fn try_evaluate_boolean(expr: &Expression, params: &IndexMap<String, Component>) -> Option<bool> {
    try_evaluate_boolean_with_depth(expr, params, &IndexMap::new(), 0)
}

/// Try to evaluate a boolean expression with condition expressions
fn try_evaluate_boolean_with_fc(
    expr: &Expression,
    params: &IndexMap<String, Component>,
    fc: &IndexMap<String, Expression>,
) -> Option<bool> {
    try_evaluate_boolean_with_depth(expr, params, fc, 0)
}

/// Try to evaluate a boolean expression with recursion depth tracking
fn try_evaluate_boolean_with_depth(
    expr: &Expression,
    params: &IndexMap<String, Component>,
    fc: &IndexMap<String, Expression>,
    depth: usize,
) -> Option<bool> {
    // Prevent stack overflow from cyclic references
    if depth > MAX_EVAL_DEPTH {
        return None;
    }

    use crate::ir::ast::{OpBinary, OpUnary, TerminalType};

    match expr {
        // Boolean literal: true or false
        Expression::Terminal {
            token,
            terminal_type: TerminalType::Bool,
        } => match token.text.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },

        // Component reference: look up condition expression or parameter value
        Expression::ComponentReference(cref) => {
            let param_name = cref.to_string();
            // First, check if this is a condition variable with an expression in fc
            if let Some(cond_expr) = fc.get(&param_name) {
                // Evaluate the actual condition expression (e.g., "nx == 0")
                return try_evaluate_boolean_with_depth(cond_expr, params, fc, depth + 1);
            }
            // Fall back to looking up the component's start value
            if let Some(param) = params.get(&param_name) {
                // Get the parameter's start value and evaluate it
                try_evaluate_boolean_with_depth(&param.start, params, fc, depth + 1)
            } else {
                None // Parameter not found
            }
        }

        // Unary not
        Expression::Unary {
            op: OpUnary::Not(_),
            rhs,
        } => try_evaluate_boolean_with_depth(rhs, params, fc, depth + 1).map(|v| !v),

        // Binary operations: and, or, comparisons
        Expression::Binary { lhs, op, rhs } => {
            match op {
                // Boolean operations: and, or
                OpBinary::And(_) | OpBinary::Or(_) => {
                    let l = try_evaluate_boolean_with_depth(lhs, params, fc, depth + 1)?;
                    let r = try_evaluate_boolean_with_depth(rhs, params, fc, depth + 1)?;
                    match op {
                        OpBinary::And(_) => Some(l && r),
                        OpBinary::Or(_) => Some(l || r),
                        _ => None,
                    }
                }
                // Comparison operations: ==, <>, <, <=, >, >=
                OpBinary::Eq(_)
                | OpBinary::Neq(_)
                | OpBinary::Lt(_)
                | OpBinary::Le(_)
                | OpBinary::Gt(_)
                | OpBinary::Ge(_) => {
                    // Try to evaluate operands as integers first
                    let lhs_val = try_evaluate_integer_with_depth(lhs, params, depth + 1);
                    let rhs_val = try_evaluate_integer_with_depth(rhs, params, depth + 1);
                    if let (Some(l), Some(r)) = (lhs_val, rhs_val) {
                        match op {
                            OpBinary::Eq(_) => Some(l == r),
                            OpBinary::Neq(_) => Some(l != r),
                            OpBinary::Lt(_) => Some(l < r),
                            OpBinary::Le(_) => Some(l <= r),
                            OpBinary::Gt(_) => Some(l > r),
                            OpBinary::Ge(_) => Some(l >= r),
                            _ => None,
                        }
                    } else {
                        // Try as boolean comparison (for == and <> on booleans)
                        if let (Some(l), Some(r)) = (
                            try_evaluate_boolean_with_depth(lhs, params, fc, depth + 1),
                            try_evaluate_boolean_with_depth(rhs, params, fc, depth + 1),
                        ) {
                            match op {
                                OpBinary::Eq(_) => Some(l == r),
                                OpBinary::Neq(_) => Some(l != r),
                                _ => None, // <, <=, >, >= don't make sense for booleans
                            }
                        } else {
                            None
                        }
                    }
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// Check balance using the DAE structure (after full compilation)
pub fn check_dae_balance(dae: &Dae) -> BalanceCheckResult {
    // Combine parameters for condition evaluation and range evaluation
    let mut all_params = dae.p.clone();
    all_params.extend(dae.cp.clone());

    // Build a comprehensive map of ALL components for size() lookups
    // This allows evaluating size(x, 1) where x is any component (param, state, output, etc.)
    // Note: We don't include dae.c here - condition variables are looked up via fc instead
    let mut all_components = all_params.clone();
    all_components.extend(dae.x.clone());
    all_components.extend(dae.y.clone());
    all_components.extend(dae.z.clone());
    all_components.extend(dae.m.clone());
    all_components.extend(dae.u.clone());

    // Unknowns from DAE: x (states) + y (algebraic) + z (discrete real) + m (discrete other)
    // Count scalar elements (accounting for array dimensions and conditional components)
    let num_states = count_scalar_elements_with_params(&dae.x, &all_components);
    let num_algebraic = count_scalar_elements_with_params(&dae.y, &all_components);
    let num_discrete = count_scalar_elements_with_params(&dae.z, &all_components)
        + count_scalar_elements_with_params(&dae.m, &all_components);
    let num_unknowns = num_states + num_algebraic + num_discrete;

    // Equations: fx (continuous) + fz (discrete update from when blocks)
    // Use count_equations_with_params_and_fc to properly expand composite equations (if/for)
    // with parameter context for evaluating ranges like `for i in 1:n loop`
    // and fc for evaluating condition expressions like `if nx == 0 then`
    let num_continuous_equations =
        count_equations_with_params_and_fc(&dae.fx, &all_components, &dae.fc);
    let num_discrete_equations =
        count_equations_with_params_and_fc(&dae.fz, &all_components, &dae.fc);

    // Count assignments from when blocks stored in fr
    // Only count assignments to non-state variables (algebraic/discrete)
    // reinit statements target states which already have continuous equations
    let num_event_assignments = dae
        .fr
        .values()
        .filter(|stmt| {
            if let Statement::Assignment { comp, .. } = stmt {
                // Get the variable name being assigned
                let var_name = comp.to_string();
                // Don't count if the target is a state variable (has continuous equation)
                !dae.x.contains_key(&var_name)
            } else {
                false
            }
        })
        .count();

    let num_equations = num_continuous_equations + num_discrete_equations + num_event_assignments;

    // Parameters and inputs (for reporting)
    let num_parameters = count_scalar_elements(&dae.p) + count_scalar_elements(&dae.cp);
    let num_inputs = count_scalar_elements_with_params(&dae.u, &all_components);

    BalanceCheckResult {
        num_equations,
        num_unknowns,
        num_states,
        num_algebraic: num_algebraic + num_discrete,
        num_parameters,
        num_inputs,
        is_balanced: num_equations == num_unknowns,
    }
}

/// Count the number of equations, expanding if/for equations recursively
fn count_equations(equations: &[Equation]) -> usize {
    count_equations_with_params(equations, &IndexMap::new())
}

/// Count the number of equations with parameter context for evaluating ranges
fn count_equations_with_params(
    equations: &[Equation],
    params: &IndexMap<String, Component>,
) -> usize {
    count_equations_with_params_and_fc(equations, params, &IndexMap::new())
}

/// Count the number of equations with parameter and condition expression context
fn count_equations_with_params_and_fc(
    equations: &[Equation],
    params: &IndexMap<String, Component>,
    fc: &IndexMap<String, Expression>,
) -> usize {
    let mut count = 0;
    for eq in equations {
        count += count_single_equation_with_params_and_fc(eq, params, fc);
    }
    count
}

/// Count a single equation (may be composite like if/for)
#[allow(dead_code)]
fn count_single_equation(eq: &Equation) -> usize {
    count_single_equation_with_params_and_fc(eq, &IndexMap::new(), &IndexMap::new())
}

/// Count a single equation with parameter context
#[allow(dead_code)]
fn count_single_equation_with_params(eq: &Equation, params: &IndexMap<String, Component>) -> usize {
    count_single_equation_with_params_and_fc(eq, params, &IndexMap::new())
}

/// Count a single equation with parameter and condition expression context
fn count_single_equation_with_params_and_fc(
    eq: &Equation,
    params: &IndexMap<String, Component>,
    fc: &IndexMap<String, Expression>,
) -> usize {
    match eq {
        Equation::Simple { .. } => 1,
        Equation::Connect { .. } => {
            // Connect equations expand to multiple equations
            // but they should already be expanded at this point
            1
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Try to evaluate each condition with parameters and fc
            // If a condition is true, count only that branch
            for block in cond_blocks {
                let cond_result = try_evaluate_boolean_with_fc(&block.cond, params, fc);
                if let Some(true) = cond_result {
                    return count_equations_with_params_and_fc(&block.eqs, params, fc);
                }
            }

            // Check if any condition is definitively false - if all are false, use else
            let all_conditions_false = cond_blocks.iter().all(|block| {
                matches!(
                    try_evaluate_boolean_with_fc(&block.cond, params, fc),
                    Some(false)
                )
            });

            if all_conditions_false {
                // All conditions are false, use else branch (or 0 if no else)
                if let Some(else_eqs) = else_block {
                    count_equations_with_params_and_fc(else_eqs, params, fc)
                } else {
                    0
                }
            } else {
                // Some conditions couldn't be evaluated - fall back to old behavior:
                // assume all branches have the same count (required by Modelica)
                if let Some(else_eqs) = else_block {
                    count_equations_with_params_and_fc(else_eqs, params, fc)
                } else if let Some(first_block) = cond_blocks.first() {
                    count_equations_with_params_and_fc(&first_block.eqs, params, fc)
                } else {
                    0
                }
            }
        }
        Equation::For { indices, equations } => {
            // For loops expand based on the range
            // Multiply body equations by the product of all index ranges
            let body_count = count_equations_with_params_and_fc(equations, params, fc);

            // Try to evaluate the range sizes with parameter context
            let range_product: usize = indices
                .iter()
                .map(|idx| try_evaluate_range_size_with_params(&idx.range, params).unwrap_or(1))
                .product();

            body_count * range_product
        }
        Equation::When(branches) => {
            // When equations generate discrete equations
            // For continuous balance, these don't count
            // But they do generate fz/fm equations
            let _ = branches; // silence unused warning
            0
        }
        Equation::FunctionCall { .. } => {
            // Function call equations (like assert()) typically don't add to equation count
            0
        }
        Equation::Empty => 0,
    }
}

/// Try to evaluate the size of a range expression (e.g., 1:3 -> 3, 1:2:10 -> 5)
/// Returns None if the range cannot be evaluated at compile time
#[allow(dead_code)]
fn try_evaluate_range_size(expr: &Expression) -> Option<usize> {
    try_evaluate_range_size_with_params(expr, &IndexMap::new())
}

/// Try to evaluate the size of a range expression with parameter context
fn try_evaluate_range_size_with_params(
    expr: &Expression,
    params: &IndexMap<String, Component>,
) -> Option<usize> {
    match expr {
        Expression::Range { start, step, end } => {
            let start_val = try_evaluate_integer_with_params(start, params)?;
            let end_val = try_evaluate_integer_with_params(end, params)?;
            let step_val = if let Some(s) = step {
                try_evaluate_integer_with_params(s, params)?
            } else {
                1
            };

            if step_val == 0 {
                return None;
            }

            // Calculate range size: floor((end - start) / step) + 1
            // But we need to handle the direction properly
            if step_val > 0 && end_val >= start_val {
                Some(((end_val - start_val) / step_val + 1) as usize)
            } else if step_val < 0 && end_val <= start_val {
                Some(((start_val - end_val) / (-step_val) + 1) as usize)
            } else {
                Some(0) // Empty range
            }
        }
        // A single integer means a range of 1 (e.g., `for i in n loop` where n is evaluated)
        _ => try_evaluate_integer_with_params(expr, params).map(|v| v.max(0) as usize),
    }
}

/// Try to evaluate an expression as an integer constant (without parameter context)
#[allow(dead_code)]
fn try_evaluate_integer(expr: &Expression) -> Option<i64> {
    try_evaluate_integer_with_params(expr, &IndexMap::new())
}

/// Try to get the size of an array dimension from an expression (e.g., array literal)
/// For `{1, 2, 3}`, dim_index=1 returns 3
/// For `{{1,2}, {3,4}, {5,6}}`, dim_index=1 returns 3, dim_index=2 returns 2
fn try_get_array_size_from_expr(expr: &Expression, dim_index: usize) -> Option<i64> {
    match expr {
        Expression::Array { elements } => {
            if dim_index == 1 {
                // First dimension is the number of elements
                Some(elements.len() as i64)
            } else if dim_index > 1 && !elements.is_empty() {
                // For higher dimensions, recurse into the first element
                try_get_array_size_from_expr(&elements[0], dim_index - 1)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Try to evaluate an expression as an integer constant with parameter context
fn try_evaluate_integer_with_params(
    expr: &Expression,
    params: &IndexMap<String, Component>,
) -> Option<i64> {
    try_evaluate_integer_with_depth(expr, params, 0)
}

/// Try to evaluate an expression as an integer constant with depth tracking
fn try_evaluate_integer_with_depth(
    expr: &Expression,
    params: &IndexMap<String, Component>,
    depth: usize,
) -> Option<i64> {
    // Prevent stack overflow from cyclic references
    if depth > MAX_EVAL_DEPTH {
        return None;
    }

    use crate::ir::ast::{OpBinary, OpUnary};

    match expr {
        Expression::Terminal { token, .. } => {
            // Try to parse as integer
            token.text.parse::<i64>().ok()
        }
        // Component reference: look up parameter value
        Expression::ComponentReference(cref) => {
            let param_name = cref.to_string();
            if let Some(param) = params.get(&param_name) {
                // Get the parameter's start value and evaluate it recursively
                try_evaluate_integer_with_depth(&param.start, params, depth + 1)
            } else {
                None // Parameter not found
            }
        }
        Expression::Unary { op, rhs } => match op {
            OpUnary::Minus(_) => {
                try_evaluate_integer_with_depth(rhs, params, depth + 1).map(|v| -v)
            }
            OpUnary::Plus(_) => try_evaluate_integer_with_depth(rhs, params, depth + 1),
            _ => None,
        },
        Expression::Binary { lhs, op, rhs } => {
            let l = try_evaluate_integer_with_depth(lhs, params, depth + 1)?;
            let r = try_evaluate_integer_with_depth(rhs, params, depth + 1)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) if r != 0 => Some(l / r),
                _ => None,
            }
        }
        // Function call: handle size() function for array dimensions
        Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "size" && !args.is_empty() {
                // size(arr, dim) - get dimension size of an array
                // First arg: array (ComponentReference)
                // Second arg: dimension index (1-based, optional - defaults to 1)
                if let Expression::ComponentReference(array_ref) = &args[0] {
                    let array_name = array_ref.to_string();

                    // Get dimension index (1-based, default to 1)
                    let dim_index = if args.len() >= 2 {
                        try_evaluate_integer_with_depth(&args[1], params, depth + 1)? as usize
                    } else {
                        1 // Default to first dimension
                    };

                    // Look up the array in params
                    if let Some(array_comp) = params.get(&array_name) {
                        // First try the evaluated shape
                        if !array_comp.shape.is_empty() {
                            // Modelica dimensions are 1-based
                            if dim_index >= 1 && dim_index <= array_comp.shape.len() {
                                return Some(array_comp.shape[dim_index - 1] as i64);
                            }
                        }
                        // Try shape_expr if shape is empty
                        if !array_comp.shape_expr.is_empty()
                            && dim_index >= 1
                            && dim_index <= array_comp.shape_expr.len()
                        {
                            if let crate::ir::ast::Subscript::Expression(expr) =
                                &array_comp.shape_expr[dim_index - 1]
                            {
                                return try_evaluate_integer_with_depth(expr, params, depth + 1);
                            }
                            // Subscript::Range (`:`) can't be evaluated directly
                        }
                        // Fall back to inferring size from start expression (array literal)
                        // For `parameter Real a[:] = {1, 2, 3}`, infer size from the literal
                        if let Some(size) =
                            try_get_array_size_from_expr(&array_comp.start, dim_index)
                        {
                            return Some(size);
                        }
                    }
                }
                None
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Visitor that finds variables that appear in der() calls
struct DerFinder {
    /// Found state variables
    states: Vec<String>,
}

impl DerFinder {
    fn new() -> Self {
        Self { states: Vec::new() }
    }
}

impl Visitor for DerFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node {
            if comp.to_string() == "der" && !args.is_empty() {
                if let Expression::ComponentReference(cref) = &args[0] {
                    let var_name = cref.to_string();
                    if !self.states.contains(&var_name) {
                        self.states.push(var_name);
                    }
                }
            }
        }
    }
}

/// Find variables that appear in der() calls
fn find_state_variables(equations: &[Equation]) -> Vec<String> {
    let mut finder = DerFinder::new();
    for eq in equations {
        eq.accept(&mut finder);
    }
    finder.states
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_check_result_messages() {
        let balanced = BalanceCheckResult {
            num_equations: 3,
            num_unknowns: 3,
            num_states: 1,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            is_balanced: true,
        };
        assert!(balanced.status_message().contains("balanced"));

        let over = BalanceCheckResult {
            num_equations: 5,
            num_unknowns: 3,
            num_states: 1,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            is_balanced: false,
        };
        assert!(over.status_message().contains("over-determined"));
        assert_eq!(over.difference(), 2);

        let under = BalanceCheckResult {
            num_equations: 2,
            num_unknowns: 4,
            num_states: 2,
            num_algebraic: 2,
            num_parameters: 0,
            num_inputs: 0,
            is_balanced: false,
        };
        assert!(under.status_message().contains("under-determined"));
        assert_eq!(under.difference(), -2);
    }
}
