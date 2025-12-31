//! Reinit restrictions and when condition type checks.
//!
//! MLS §8.3.5: reinit() semantics

use std::collections::{HashMap, HashSet};

use crate::ir::ast::{ClassDefinition, Equation, Expression, Statement, Variability};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{SymbolType as InferredType, infer_expression_type};

use super::super::SymbolType;
use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that reinit() is only used inside when-equations.
///
/// MLS §8.3.5: "reinit can only be used in the body of a when-equation."
/// This validates that reinit() calls don't appear in regular equations or if-equations.
pub fn check_reinit_context(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check regular equations (not inside when)
    for eq in &class.equations {
        check_reinit_in_equation(eq, false, &mut result);
    }

    // Check initial equations (reinit is never allowed in initial equations)
    for eq in &class.initial_equations {
        check_reinit_in_equation(eq, false, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_reinit_in_statement(stmt, false, &mut result);
        }
    }

    result
}

/// Check for reinit in an equation, tracking whether we're inside a when.
fn check_reinit_in_equation(eq: &Equation, in_when: bool, result: &mut TypeCheckResult) {
    match eq {
        Equation::FunctionCall { comp, .. } => {
            let func_name = comp.to_string();
            if func_name == "reinit"
                && !in_when
                && let Some(loc) = comp.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "reinit() can only be used inside a when-equation".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Equation::When(blocks) => {
            // Inside when, reinit is allowed
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_reinit_in_equation(inner_eq, true, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // reinit in if-equation (not inside when) is not allowed
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_reinit_in_equation(inner_eq, in_when, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_reinit_in_equation(inner_eq, in_when, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_reinit_in_equation(inner_eq, in_when, result);
            }
        }
        _ => {}
    }
}

/// Check for reinit in a statement, tracking whether we're inside a when.
fn check_reinit_in_statement(stmt: &Statement, in_when: bool, result: &mut TypeCheckResult) {
    match stmt {
        Statement::FunctionCall { comp, .. } => {
            let func_name = comp.to_string();
            if func_name == "reinit"
                && !in_when
                && let Some(loc) = comp.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "reinit() can only be used inside a when-statement".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Statement::When(blocks) => {
            // Inside when, reinit is allowed
            for block in blocks {
                for inner_stmt in &block.stmts {
                    check_reinit_in_statement(inner_stmt, true, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_stmt in &block.stmts {
                    check_reinit_in_statement(inner_stmt, in_when, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_reinit_in_statement(inner_stmt, in_when, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_reinit_in_statement(inner_stmt, in_when, result);
            }
        }
        Statement::While(block) => {
            for inner_stmt in &block.stmts {
                check_reinit_in_statement(inner_stmt, in_when, result);
            }
        }
        _ => {}
    }
}

/// Check that reinit() is only applied to continuous state variables.
///
/// MLS §8.3.5: "reinit can only reinitialize continuous-time Real variables
/// (states or algebraic variables that appear differentiated)."
pub fn check_reinit_variable_type(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build set of state variable names (variables that appear in der())
    // and continuous variables
    let continuous_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            // Include variables with continuous variability (not parameter, constant, or discrete)
            !matches!(
                c.variability,
                Variability::Parameter(_) | Variability::Constant(_) | Variability::Discrete(_)
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

    // Check all equations for reinit variable type
    for eq in &class.equations {
        check_reinit_var_type_in_equation(eq, &continuous_vars, &mut result);
    }

    result
}

/// Check reinit variable type in an equation.
fn check_reinit_var_type_in_equation(
    eq: &Equation,
    continuous_vars: &HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "reinit" && !args.is_empty() {
                // First argument is the variable being reinitialized
                if let Expression::ComponentReference(var_ref) = &args[0] {
                    let var_name = var_ref.to_string();
                    // Extract base name (handle array subscripts like x[1])
                    let base_name = var_name.split('[').next().unwrap_or(&var_name);

                    if !continuous_vars.contains(base_name)
                        && let Some(loc) = var_ref.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "reinit() can only be applied to continuous Real variables, \
                                     but '{}' is discrete or a parameter/constant",
                                var_name
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_reinit_var_type_in_equation(inner_eq, continuous_vars, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_reinit_var_type_in_equation(inner_eq, continuous_vars, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_reinit_var_type_in_equation(inner_eq, continuous_vars, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_reinit_var_type_in_equation(inner_eq, continuous_vars, result);
            }
        }
        _ => {}
    }
}

// =============================================================================
// DUPLICATE REINIT CHECK (MLS §8.3.5)
// =============================================================================

/// Check for duplicate reinit() calls on the same variable within a when-clause.
///
/// MLS §8.3.5: "reinit shall only be applied once to a variable in any when-clause"
pub fn check_duplicate_reinit(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check equations
    for eq in &class.equations {
        check_duplicate_reinit_in_equation(eq, &mut result);
    }

    result
}

/// Check for duplicate reinit in an equation.
fn check_duplicate_reinit_in_equation(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::When(blocks) => {
            // For each when-block, collect all reinit variable names and check for duplicates
            for block in blocks {
                let mut reinit_vars: HashMap<String, crate::ir::ast::Location> = HashMap::new();

                for inner_eq in &block.eqs {
                    collect_reinit_vars_in_equation(inner_eq, &mut reinit_vars, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for sub_eq in &block.eqs {
                    check_duplicate_reinit_in_equation(sub_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_duplicate_reinit_in_equation(sub_eq, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_duplicate_reinit_in_equation(sub_eq, result);
            }
        }
        _ => {}
    }
}

/// Collect reinit variable names and report duplicates.
fn collect_reinit_vars_in_equation(
    eq: &Equation,
    reinit_vars: &mut HashMap<String, crate::ir::ast::Location>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "reinit"
                && !args.is_empty()
                && let Expression::ComponentReference(var_ref) = &args[0]
            {
                let var_name = var_ref.to_string();
                let base_name = var_name.split('[').next().unwrap_or(&var_name).to_string();

                if let Some(first_loc) = reinit_vars.get(&base_name) {
                    // Duplicate reinit found
                    if let Some(loc) = var_ref.get_location() {
                        result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Variable '{}' is reinitialized multiple times in the same when-clause (first at line {})",
                                    var_name, first_loc.start_line
                                ),
                                TypeErrorSeverity::Error,
                            ));
                    }
                } else {
                    // Record first occurrence
                    if let Some(loc) = var_ref.get_location() {
                        reinit_vars.insert(base_name, loc.clone());
                    }
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // For if inside when, we still need to check across the if branches
            for block in cond_blocks {
                for sub_eq in &block.eqs {
                    collect_reinit_vars_in_equation(sub_eq, reinit_vars, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_reinit_vars_in_equation(sub_eq, reinit_vars, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                collect_reinit_vars_in_equation(sub_eq, reinit_vars, result);
            }
        }
        _ => {}
    }
}

// =============================================================================
// WHEN CONDITION TYPE CHECK (MLS §8.3.5)
// =============================================================================

/// Check that when conditions are Boolean expressions.
///
/// MLS §8.3.5: "The condition of a when-equation shall be a discrete-time Boolean
/// expression, or a vector of such expressions."
pub fn check_when_condition_type(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build defined symbols map for type inference
    let defined: HashMap<String, DefinedSymbol> = class
        .components
        .iter()
        .map(|(name, comp)| {
            let sym_type = type_name_to_symbol_type(&comp.type_name.to_string());
            (
                name.clone(),
                DefinedSymbol {
                    name: name.clone(),
                    line: comp.location.start_line,
                    col: comp.location.start_column,
                    is_parameter: matches!(comp.variability, Variability::Parameter(_)),
                    is_constant: matches!(comp.variability, Variability::Constant(_)),
                    is_class: false,
                    has_default: !matches!(comp.start, Expression::Empty),
                    declared_type: sym_type,
                    shape: comp.shape.clone(),
                    function_return: None,
                },
            )
        })
        .collect();

    // Check equations
    for eq in &class.equations {
        check_when_condition_in_equation(eq, &defined, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_when_condition_in_equation(eq, &defined, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_when_condition_in_statement(stmt, &defined, &mut result);
        }
    }

    result
}

/// Convert a type name string to InferredType
fn type_name_to_symbol_type(type_name: &str) -> InferredType {
    match type_name {
        "Real" => InferredType::Real,
        "Integer" => InferredType::Integer,
        "Boolean" => InferredType::Boolean,
        "String" => InferredType::String,
        _ => InferredType::Class(type_name.to_string()),
    }
}

/// Check when condition type in an equation.
fn check_when_condition_in_equation(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::When(blocks) => {
            for block in blocks {
                check_condition_is_boolean(&block.cond, defined, result);
                // Recursively check nested equations
                for inner_eq in &block.eqs {
                    check_when_condition_in_equation(inner_eq, defined, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_when_condition_in_equation(inner_eq, defined, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_when_condition_in_equation(inner_eq, defined, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_when_condition_in_equation(inner_eq, defined, result);
            }
        }
        _ => {}
    }
}

/// Check when condition type in a statement.
fn check_when_condition_in_statement(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::When(blocks) => {
            for block in blocks {
                check_condition_is_boolean(&block.cond, defined, result);
                // Recursively check nested statements
                for inner_stmt in &block.stmts {
                    check_when_condition_in_statement(inner_stmt, defined, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_stmt in &block.stmts {
                    check_when_condition_in_statement(inner_stmt, defined, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_when_condition_in_statement(inner_stmt, defined, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_when_condition_in_statement(inner_stmt, defined, result);
            }
        }
        Statement::While(block) => {
            for inner_stmt in &block.stmts {
                check_when_condition_in_statement(inner_stmt, defined, result);
            }
        }
        _ => {}
    }
}

/// Check that a condition expression is Boolean.
fn check_condition_is_boolean(
    cond: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    let cond_type = infer_expression_type(cond, defined);

    // Allow Boolean, comparison results (which are Boolean), Unknown (can't determine)
    // Don't allow Real, Integer, String, or class types
    match cond_type {
        InferredType::Boolean | InferredType::Unknown => {
            // OK - Boolean condition or unknown (can't determine)
        }
        InferredType::Array(inner, _) => {
            // Allow arrays of Boolean (vector conditions)
            if !matches!(
                inner.as_ref(),
                InferredType::Boolean | InferredType::Unknown
            ) && let Some(loc) = cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!("When condition must be Boolean, got array of {:?}", inner),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        InferredType::Real | InferredType::Integer | InferredType::String => {
            if let Some(loc) = cond.get_location() {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!("When condition must be Boolean, got {:?}", cond_type),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        InferredType::Class(class_name) => {
            if let Some(loc) = cond.get_location() {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!("When condition must be Boolean, got type '{}'", class_name),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        InferredType::Enumeration(enum_name) => {
            if let Some(loc) = cond.get_location() {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "When condition must be Boolean, got enumeration '{}'",
                        enum_name
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }
}

// =============================================================================
// EDGE() ARGUMENT TYPE CHECK (MLS §3.7.3.2)
// =============================================================================

/// Check that edge() is only applied to Boolean expressions.
///
/// MLS §3.7.3.2: "edge(b) is equivalent to (b and not pre(b))"
/// This implies the argument must be Boolean.
pub fn check_edge_argument_type(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build defined symbols map for type inference
    let defined: HashMap<String, DefinedSymbol> = class
        .components
        .iter()
        .map(|(name, comp)| {
            let sym_type = type_name_to_symbol_type(&comp.type_name.to_string());
            (
                name.clone(),
                DefinedSymbol {
                    name: name.clone(),
                    line: comp.location.start_line,
                    col: comp.location.start_column,
                    is_parameter: matches!(comp.variability, Variability::Parameter(_)),
                    is_constant: matches!(comp.variability, Variability::Constant(_)),
                    is_class: false,
                    has_default: !matches!(comp.start, Expression::Empty),
                    declared_type: sym_type,
                    shape: comp.shape.clone(),
                    function_return: None,
                },
            )
        })
        .collect();

    // Check equations
    for eq in &class.equations {
        check_edge_in_equation(eq, &defined, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_edge_in_equation(eq, &defined, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_edge_in_statement(stmt, &defined, &mut result);
        }
    }

    result
}

/// Check edge() argument type in an equation.
fn check_edge_in_equation(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_edge_in_expression(lhs, defined, result);
            check_edge_in_expression(rhs, defined, result);
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_edge_in_expression(arg, defined, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_edge_in_expression(&block.cond, defined, result);
                for inner_eq in &block.eqs {
                    check_edge_in_equation(inner_eq, defined, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_edge_in_expression(&block.cond, defined, result);
                for inner_eq in &block.eqs {
                    check_edge_in_equation(inner_eq, defined, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_edge_in_equation(inner_eq, defined, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_edge_in_equation(inner_eq, defined, result);
            }
        }
        Equation::Connect { .. } | Equation::Empty => {}
    }
}

/// Check edge() argument type in a statement.
fn check_edge_in_statement(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { value, .. } => {
            check_edge_in_expression(value, defined, result);
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_edge_in_expression(arg, defined, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_edge_in_expression(&block.cond, defined, result);
                for inner_stmt in &block.stmts {
                    check_edge_in_statement(inner_stmt, defined, result);
                }
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_edge_in_expression(&block.cond, defined, result);
                for inner_stmt in &block.stmts {
                    check_edge_in_statement(inner_stmt, defined, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner_stmt in else_stmts {
                    check_edge_in_statement(inner_stmt, defined, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for inner_stmt in equations {
                check_edge_in_statement(inner_stmt, defined, result);
            }
        }
        Statement::While(block) => {
            check_edge_in_expression(&block.cond, defined, result);
            for inner_stmt in &block.stmts {
                check_edge_in_statement(inner_stmt, defined, result);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } | Statement::Empty => {}
    }
}

/// Check edge() calls within an expression.
fn check_edge_in_expression(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            let func_name = comp.to_string();
            if func_name == "edge" && !args.is_empty() {
                // Check that the argument is Boolean
                let arg_type = infer_expression_type(&args[0], defined);
                match arg_type {
                    InferredType::Boolean | InferredType::Unknown => {
                        // OK - Boolean argument or unknown (can't determine)
                    }
                    _ => {
                        if let Some(loc) = args[0].get_location() {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!("edge() requires a Boolean argument, got {:?}", arg_type),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                }
            }
            // Recursively check arguments
            for arg in args {
                check_edge_in_expression(arg, defined, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_edge_in_expression(lhs, defined, result);
            check_edge_in_expression(rhs, defined, result);
        }
        Expression::Unary { rhs, .. } => {
            check_edge_in_expression(rhs, defined, result);
        }
        Expression::Parenthesized { inner } => {
            check_edge_in_expression(inner, defined, result);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_edge_in_expression(cond, defined, result);
                check_edge_in_expression(then_expr, defined, result);
            }
            check_edge_in_expression(else_branch, defined, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_edge_in_expression(elem, defined, result);
            }
        }
        Expression::ArrayComprehension { expr, .. } => {
            check_edge_in_expression(expr, defined, result);
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_edge_in_expression(elem, defined, result);
            }
        }
        Expression::Range { start, step, end } => {
            check_edge_in_expression(start, defined, result);
            if let Some(s) = step {
                check_edge_in_expression(s, defined, result);
            }
            check_edge_in_expression(end, defined, result);
        }
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => {}
    }
}
