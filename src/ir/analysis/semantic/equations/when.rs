//! When-statement and when-equation validation checks.
//!
//! MLS §8.3.5 (When-equations) and §11.2.7 (When-statements)

use std::collections::HashSet;

use crate::ir::ast::{
    ClassDefinition, ClassType, Equation, Expression, Statement, TerminalType, Variability,
};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

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

    // Check initial equations - when-equations are forbidden in initial equation sections
    for eq in &class.initial_equations {
        if contains_when_equation(eq)
            && let Some(loc) = get_equation_location(eq)
        {
            result.add_error(TypeError::new(
                loc.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                "When-equations shall not be used in initial equation sections".to_string(),
                TypeErrorSeverity::Error,
            ));
        }
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
