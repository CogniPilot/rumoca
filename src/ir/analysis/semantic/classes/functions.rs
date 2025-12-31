//! Function body restrictions.
//!
//! MLS §12.2: Function restrictions

use crate::ir::ast::{ClassDefinition, ClassType, Expression, Statement};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

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

/// Check that function components are not of forbidden class types.
///
/// MLS §12.2: "A function shall not contain components of restricted classes:
/// model, block, connector, or expandable connector."
pub fn check_function_component_classes(
    class: &ClassDefinition,
    peer_class_types: &std::collections::HashMap<String, ClassType>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of nested class types
    let mut class_types = peer_class_types.clone();
    for (name, nested_class) in &class.classes {
        class_types.insert(name.clone(), nested_class.class_type.clone());
    }

    // Only check functions
    if matches!(class.class_type, ClassType::Function) {
        // Check each component's type
        for (comp_name, comp) in &class.components {
            let type_name = comp.type_name.to_string();
            if let Some(class_type) = class_types.get(&type_name) {
                match class_type {
                    ClassType::Model => {
                        result.add_error(TypeError::new(
                            comp.name_token.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Function '{}' contains component '{}' of class 'model', which is not allowed",
                                class.name.text, comp_name
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    ClassType::Block => {
                        result.add_error(TypeError::new(
                            comp.name_token.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Function '{}' contains component '{}' of class 'block', which is not allowed",
                                class.name.text, comp_name
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    ClassType::Connector => {
                        result.add_error(TypeError::new(
                            comp.name_token.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Function '{}' contains component '{}' of class 'connector', which is not allowed",
                                class.name.text, comp_name
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    _ => {}
                }
            }
        }
    }

    // Recursively check nested classes
    for (_name, nested_class) in &class.classes {
        let nested_result = check_function_component_classes(nested_class, &class_types);
        result.merge(nested_result);
    }

    result
}

/// Check that functions don't use inner/outer prefixes.
///
/// MLS §12.2: "inner/outer prefixes are not allowed in functions"
pub fn check_function_no_inner_outer(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check functions
    if matches!(class.class_type, ClassType::Function) {
        // Check each component for inner/outer
        for (comp_name, comp) in &class.components {
            if comp.inner {
                result.add_error(TypeError::new(
                    comp.name_token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Component '{}' in function '{}' cannot have 'inner' prefix",
                        comp_name, class.name.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            if comp.outer {
                result.add_error(TypeError::new(
                    comp.name_token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Component '{}' in function '{}' cannot have 'outer' prefix",
                        comp_name, class.name.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }

    // Recursively check nested classes
    for (_name, nested_class) in &class.classes {
        let nested_result = check_function_no_inner_outer(nested_class);
        result.merge(nested_result);
    }

    result
}

/// Check that public components in functions have input or output causality.
///
/// MLS §12.2: "Each public component shall have the prefix input or output"
pub fn check_function_public_components(class: &ClassDefinition) -> TypeCheckResult {
    use crate::ir::ast::Causality;
    let mut result = TypeCheckResult::new();

    // Only check functions
    if matches!(class.class_type, ClassType::Function) {
        // Check each component
        for (comp_name, comp) in &class.components {
            // Skip protected components
            if comp.is_protected {
                continue;
            }
            // Public components must have input or output causality
            if !matches!(comp.causality, Causality::Input(_) | Causality::Output(_)) {
                result.add_error(TypeError::new(
                    comp.name_token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Public component '{}' in function '{}' must have prefix 'input' or 'output'",
                        comp_name, class.name.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }

    // Recursively check nested classes
    for (_name, nested_class) in &class.classes {
        let nested_result = check_function_public_components(nested_class);
        result.merge(nested_result);
    }

    result
}

/// Check that a function has at most one algorithm section.
///
/// MLS §12.2: "A function shall have at most one algorithm section"
pub fn check_function_single_algorithm(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check functions
    if !matches!(class.class_type, ClassType::Function) {
        // Recursively check nested classes
        for (_name, nested_class) in &class.classes {
            let nested_result = check_function_single_algorithm(nested_class);
            result.merge(nested_result);
        }
        return result;
    }

    // Check if there are multiple algorithm sections
    if class.algorithms.len() > 1 {
        result.add_error(TypeError::new(
            class.name.location.clone(),
            SymbolType::Unknown,
            SymbolType::Unknown,
            format!(
                "Function '{}' has {} algorithm sections, but functions may only have at most one algorithm section",
                class.name.text,
                class.algorithms.len()
            ),
            TypeErrorSeverity::Error,
        ));
    }

    // Recursively check nested classes (functions inside functions, etc.)
    for (_name, nested_class) in &class.classes {
        let nested_result = check_function_single_algorithm(nested_class);
        result.merge(nested_result);
    }

    result
}
