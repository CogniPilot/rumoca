//! Class member access checking.

use crate::ir::ast::{ClassDefinition, ClassType, Equation, Expression, Statement};

use crate::ir::analysis::type_inference::SymbolType;

use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for class member access without instance.
///
/// This validates that component references don't try to access members of a class
/// without first instantiating it. For example, `Boolean.x` where `Boolean` is a
/// nested model (not a component instance) is invalid.
///
/// Note: Package constant access (e.g., `MyPackage.constant`) is allowed because
/// packages can have static constants accessed without instantiation.
pub fn check_class_member_access(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build maps for lookup
    let component_names: std::collections::HashSet<String> =
        class.components.keys().cloned().collect();

    // Only include classes that require instantiation (model, class, block, connector)
    // Exclude packages, types, records, and functions since they can have static members
    let class_names: std::collections::HashSet<String> = class
        .classes
        .iter()
        .filter(|(_, c)| {
            matches!(
                c.class_type,
                ClassType::Model | ClassType::Class | ClassType::Block | ClassType::Connector
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

    // Check all component bindings
    for (_, comp) in &class.components {
        if !matches!(comp.start, Expression::Empty) {
            check_expr_class_member_access(
                &comp.start,
                &component_names,
                &class_names,
                &mut result,
            );
        }
    }

    // Check equations
    for eq in &class.equations {
        check_equation_class_member_access(eq, &component_names, &class_names, &mut result);
    }

    // Check algorithms (each algorithm is a Vec<Statement>)
    for algo in &class.algorithms {
        for stmt in algo {
            check_statement_class_member_access(stmt, &component_names, &class_names, &mut result);
        }
    }

    result
}

fn check_equation_class_member_access(
    eq: &Equation,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Simple { lhs, rhs, .. } => {
            check_expr_class_member_access(lhs, comp_names, class_names, result);
            check_expr_class_member_access(rhs, comp_names, class_names, result);
        }
        Equation::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_expr_class_member_access(&block.cond, comp_names, class_names, result);
                for sub_eq in &block.eqs {
                    check_equation_class_member_access(sub_eq, comp_names, class_names, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_class_member_access(sub_eq, comp_names, class_names, result);
                }
            }
        }
        Equation::For {
            indices, equations, ..
        } => {
            // Check range expressions in for indices
            for index in indices {
                check_expr_class_member_access(&index.range, comp_names, class_names, result);
            }
            for sub_eq in equations {
                check_equation_class_member_access(sub_eq, comp_names, class_names, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expr_class_member_access(&block.cond, comp_names, class_names, result);
                for sub_eq in &block.eqs {
                    check_equation_class_member_access(sub_eq, comp_names, class_names, result);
                }
            }
        }
        Equation::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_class_member_access(arg, comp_names, class_names, result);
            }
        }
        _ => {}
    }
}

fn check_statement_class_member_access(
    stmt: &Statement,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { value, .. } => {
            check_expr_class_member_access(value, comp_names, class_names, result);
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_expr_class_member_access(&block.cond, comp_names, class_names, result);
                for s in &block.stmts {
                    check_statement_class_member_access(s, comp_names, class_names, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for s in else_stmts {
                    check_statement_class_member_access(s, comp_names, class_names, result);
                }
            }
        }
        Statement::For {
            indices, equations, ..
        } => {
            // Check range expressions in for indices
            for index in indices {
                check_expr_class_member_access(&index.range, comp_names, class_names, result);
            }
            for s in equations {
                check_statement_class_member_access(s, comp_names, class_names, result);
            }
        }
        Statement::While(block) => {
            check_expr_class_member_access(&block.cond, comp_names, class_names, result);
            for s in &block.stmts {
                check_statement_class_member_access(s, comp_names, class_names, result);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expr_class_member_access(&block.cond, comp_names, class_names, result);
                for s in &block.stmts {
                    check_statement_class_member_access(s, comp_names, class_names, result);
                }
            }
        }
        Statement::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_class_member_access(arg, comp_names, class_names, result);
            }
        }
        _ => {}
    }
}

fn check_expr_class_member_access(
    expr: &Expression,
    comp_names: &std::collections::HashSet<String>,
    class_names: &std::collections::HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            // Check if first part is a class name and there are more parts
            if comp_ref.parts.len() > 1
                && let Some(first) = comp_ref.parts.first()
            {
                let first_name = &first.ident.text;
                // If it's a class name and NOT also a component name
                if class_names.contains(first_name) && !comp_names.contains(first_name) {
                    result.add_error(TypeError::new(
                        first.ident.location.clone(),
                        SymbolType::Unknown,
                        SymbolType::Unknown,
                        format!(
                            "Variable {}.{} not found in scope {}",
                            first_name,
                            comp_ref
                                .parts
                                .get(1)
                                .map(|p| p.ident.text.as_str())
                                .unwrap_or("?"),
                            "the current model"
                        ),
                        TypeErrorSeverity::Error,
                    ));
                }
            }
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_expr_class_member_access(arg, comp_names, class_names, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expr_class_member_access(lhs, comp_names, class_names, result);
            check_expr_class_member_access(rhs, comp_names, class_names, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expr_class_member_access(rhs, comp_names, class_names, result);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expr_class_member_access(cond, comp_names, class_names, result);
                check_expr_class_member_access(then_expr, comp_names, class_names, result);
            }
            check_expr_class_member_access(else_branch, comp_names, class_names, result);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                check_expr_class_member_access(elem, comp_names, class_names, result);
            }
        }
        Expression::Parenthesized { inner } => {
            check_expr_class_member_access(inner, comp_names, class_names, result);
        }
        Expression::Range { start, step, end } => {
            check_expr_class_member_access(start, comp_names, class_names, result);
            if let Some(s) = step {
                check_expr_class_member_access(s, comp_names, class_names, result);
            }
            check_expr_class_member_access(end, comp_names, class_names, result);
        }
        _ => {}
    }
}
