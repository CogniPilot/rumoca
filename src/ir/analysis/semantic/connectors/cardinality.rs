//! Cardinality function checking.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Equation, Expression, Location, Statement};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for cardinality() used outside valid contexts.
///
/// This validates that `cardinality()` is only used in:
/// - The condition of if-statements/equations
/// - Assert statements
pub fn check_cardinality_context(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check component bindings - cardinality not allowed here
    for (_name, comp) in &class.components {
        if !matches!(comp.start, Expression::Empty)
            && let Some(loc) = find_cardinality_call(&comp.start)
        {
            result.add_error(TypeError::new(
                loc.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                "cardinality may only be used in the condition of an if-statement/equation or an assert.".to_string(),
                TypeErrorSeverity::Error,
            ));
        }
    }

    // Check equations
    for eq in &class.equations {
        check_equation_cardinality_context(eq, &mut result);
    }

    // Check algorithms
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_cardinality_context(stmt, &mut result);
        }
    }

    result
}

/// Check an equation for cardinality context violations
fn check_equation_cardinality_context(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            // cardinality not allowed in simple equations
            for expr in [lhs, rhs] {
                if let Some(loc) = find_cardinality_call(expr) {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Unknown,
                        SymbolType::Unknown,
                        "cardinality may only be used in the condition of an if-statement/equation or an assert.".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // cardinality IS allowed in conditions
            // But NOT in the body equations
            for block in cond_blocks {
                for sub_eq in &block.eqs {
                    check_equation_cardinality_context(sub_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_cardinality_context(sub_eq, result);
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_cardinality_context(sub_eq, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for sub_eq in &block.eqs {
                    check_equation_cardinality_context(sub_eq, result);
                }
            }
        }
        Equation::FunctionCall { comp, args, .. } => {
            // Check if this is an assert - cardinality IS allowed in assert
            let is_assert = comp.parts.len() == 1
                && comp.parts.first().is_some_and(|p| p.ident.text == "assert");

            if !is_assert {
                // For other function calls, check arguments for cardinality
                for arg in args {
                    if let Some(loc) = find_cardinality_call(arg) {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            "cardinality may only be used in the condition of an if-statement/equation or an assert.".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }
            }
        }
        _ => {}
    }
}

/// Check a statement for cardinality context violations
fn check_statement_cardinality_context(stmt: &Statement, result: &mut TypeCheckResult) {
    match stmt {
        Statement::Assignment { comp: _, value } => {
            if let Some(loc) = find_cardinality_call(value) {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "cardinality may only be used in the condition of an if-statement/equation or an assert.".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            // cardinality IS allowed in conditions
            // But NOT in the body statements
            for block in cond_blocks {
                for sub_stmt in &block.stmts {
                    check_statement_cardinality_context(sub_stmt, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_cardinality_context(sub_stmt, result);
                }
            }
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                check_statement_cardinality_context(sub_stmt, result);
            }
        }
        Statement::While(block) => {
            // cardinality IS allowed in while conditions
            for sub_stmt in &block.stmts {
                check_statement_cardinality_context(sub_stmt, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for sub_stmt in &block.stmts {
                    check_statement_cardinality_context(sub_stmt, result);
                }
            }
        }
        _ => {}
    }
}

/// Find a cardinality() call in an expression, returning its location if found
fn find_cardinality_call(expr: &Expression) -> Option<Location> {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            // Check if this is a cardinality call
            if comp.parts.len() == 1
                && let Some(first) = comp.parts.first()
                && first.ident.text == "cardinality"
            {
                return Some(first.ident.location.clone());
            }
            // Recurse into arguments
            for arg in args {
                if let Some(loc) = find_cardinality_call(arg) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Binary { lhs, rhs, .. } => {
            find_cardinality_call(lhs).or_else(|| find_cardinality_call(rhs))
        }
        Expression::Unary { rhs, .. } => find_cardinality_call(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                if let Some(loc) = find_cardinality_call(cond) {
                    return Some(loc);
                }
                if let Some(loc) = find_cardinality_call(then_expr) {
                    return Some(loc);
                }
            }
            find_cardinality_call(else_branch)
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                if let Some(loc) = find_cardinality_call(elem) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Parenthesized { inner } => find_cardinality_call(inner),
        _ => None,
    }
}

/// Check that cardinality() arguments are valid.
///
/// This validates that:
/// - The argument is a component reference (not a class name)
/// - The referenced component is a scalar (not an array)
pub fn check_cardinality_arguments(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build maps for lookup
    let component_names: std::collections::HashSet<String> =
        class.components.keys().cloned().collect();
    let class_names: std::collections::HashSet<String> = class.classes.keys().cloned().collect();
    let component_shapes: HashMap<String, Vec<usize>> = class
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.shape.clone()))
        .collect();

    // Check equations
    for eq in &class.equations {
        check_equation_cardinality_args(
            eq,
            &component_names,
            &class_names,
            &component_shapes,
            &mut result,
        );
    }

    // Check algorithms
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_cardinality_args(
                stmt,
                &component_names,
                &class_names,
                &component_shapes,
                &mut result,
            );
        }
    }

    result
}

/// Check cardinality arguments in an equation
fn check_equation_cardinality_args(
    eq: &Equation,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    comp_shapes: &HashMap<String, Vec<usize>>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            check_expr_cardinality_args(lhs, comp_names, class_names, comp_shapes, result);
            check_expr_cardinality_args(rhs, comp_names, class_names, comp_shapes, result);
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expr_cardinality_args(
                    &block.cond,
                    comp_names,
                    class_names,
                    comp_shapes,
                    result,
                );
                for sub_eq in &block.eqs {
                    check_equation_cardinality_args(
                        sub_eq,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_cardinality_args(
                        sub_eq,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_cardinality_args(
                    sub_eq,
                    comp_names,
                    class_names,
                    comp_shapes,
                    result,
                );
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expr_cardinality_args(
                    &block.cond,
                    comp_names,
                    class_names,
                    comp_shapes,
                    result,
                );
                for sub_eq in &block.eqs {
                    check_equation_cardinality_args(
                        sub_eq,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_cardinality_args(arg, comp_names, class_names, comp_shapes, result);
            }
        }
        _ => {}
    }
}

/// Check cardinality arguments in a statement
fn check_statement_cardinality_args(
    stmt: &Statement,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    comp_shapes: &HashMap<String, Vec<usize>>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { value, .. } => {
            check_expr_cardinality_args(value, comp_names, class_names, comp_shapes, result);
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expr_cardinality_args(
                    &block.cond,
                    comp_names,
                    class_names,
                    comp_shapes,
                    result,
                );
                for s in &block.stmts {
                    check_statement_cardinality_args(
                        s,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
            if let Some(else_stmts) = else_block {
                for s in else_stmts {
                    check_statement_cardinality_args(
                        s,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
        }
        Statement::For { equations, .. } => {
            for s in equations {
                check_statement_cardinality_args(s, comp_names, class_names, comp_shapes, result);
            }
        }
        Statement::While(block) => {
            check_expr_cardinality_args(&block.cond, comp_names, class_names, comp_shapes, result);
            for s in &block.stmts {
                check_statement_cardinality_args(s, comp_names, class_names, comp_shapes, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expr_cardinality_args(
                    &block.cond,
                    comp_names,
                    class_names,
                    comp_shapes,
                    result,
                );
                for s in &block.stmts {
                    check_statement_cardinality_args(
                        s,
                        comp_names,
                        class_names,
                        comp_shapes,
                        result,
                    );
                }
            }
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_cardinality_args(arg, comp_names, class_names, comp_shapes, result);
            }
        }
        _ => {}
    }
}

/// Check cardinality arguments in an expression
fn check_expr_cardinality_args(
    expr: &Expression,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    _comp_shapes: &HashMap<String, Vec<usize>>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            // Check if this is a cardinality call
            let is_cardinality = comp.parts.len() == 1
                && comp
                    .parts
                    .first()
                    .is_some_and(|p| p.ident.text == "cardinality");

            if is_cardinality && !args.is_empty() {
                let arg = &args[0];
                let loc = comp.parts.first().unwrap().ident.location.clone();

                // The argument should be a component reference
                if let Expression::ComponentReference(comp_ref) = arg
                    && let Some(first) = comp_ref.parts.first()
                {
                    let first_name = &first.ident.text;

                    // Check if it's a class name instead of a component
                    if class_names.contains(first_name) && !comp_names.contains(first_name) {
                        result.add_error(TypeError::new(
                            loc,
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Expected {} to be a component, but found class instead.",
                                first_name
                            ),
                            TypeErrorSeverity::Error,
                        ));
                        return;
                    }

                    // NOTE: Array connector checking is disabled because subscripts
                    // may be lost during flattening (e.g., a1[1].c becomes a1.c).
                    // This would cause false positives for valid code like cardinality(a1[1].c).
                    // The class name check (CardinalityInvalidArg1) still works for detecting
                    // when cardinality is called with a class name instead of a component.
                }
            }

            // Recurse into arguments for nested cardinality calls
            for arg in args {
                check_expr_cardinality_args(arg, comp_names, class_names, _comp_shapes, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expr_cardinality_args(lhs, comp_names, class_names, _comp_shapes, result);
            check_expr_cardinality_args(rhs, comp_names, class_names, _comp_shapes, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expr_cardinality_args(rhs, comp_names, class_names, _comp_shapes, result);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expr_cardinality_args(cond, comp_names, class_names, _comp_shapes, result);
                check_expr_cardinality_args(
                    then_expr,
                    comp_names,
                    class_names,
                    _comp_shapes,
                    result,
                );
            }
            check_expr_cardinality_args(else_branch, comp_names, class_names, _comp_shapes, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expr_cardinality_args(elem, comp_names, class_names, _comp_shapes, result);
            }
        }
        Expression::Parenthesized { inner } => {
            check_expr_cardinality_args(inner, comp_names, class_names, _comp_shapes, result);
        }
        _ => {}
    }
}
