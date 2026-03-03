//! Analysis helpers for variables assigned by when-clauses/statements.

use std::collections::HashSet;

use rumoca_ir_flat as flat;

use crate::path_utils::subscript_fallback_chain;

/// Collect all variables assigned in when-clauses (MLS §8.3.5).
/// Variables that are ONLY assigned in when-clauses should be classified as discrete,
/// not as outputs or algebraics, because they don't contribute to the continuous DAE.
///
/// This includes:
/// - Variables assigned in top-level when equations
/// - Variables assigned in when statements inside algorithms
pub(super) fn collect_when_assigned_vars(flat: &flat::Model) -> HashSet<flat::VarName> {
    let mut when_assigned = HashSet::default();

    // Collect from top-level when equations.
    for when_clause in &flat.when_clauses {
        collect_when_equation_targets(&when_clause.equations, &mut when_assigned);
    }

    // Collect from when statements inside algorithms (MLS §11.2.7).
    for alg in &flat.algorithms {
        collect_when_statement_targets(&alg.statements, &mut when_assigned);
    }

    when_assigned
}

/// Recursively collect variables assigned in when statements inside algorithms.
/// Per MLS §11.2.7, variables assigned in when statements are discrete.
pub(super) fn collect_when_statement_targets(
    statements: &[rumoca_ir_flat::Statement],
    targets: &mut HashSet<flat::VarName>,
) {
    for stmt in statements {
        match stmt {
            flat::Statement::When(blocks) => {
                // Everything inside a when statement is discrete.
                for block in blocks {
                    collect_all_statement_assignments(&block.stmts, targets);
                }
            }
            flat::Statement::For { equations, .. } => {
                // Recurse into for loops to find nested when statements.
                collect_when_statement_targets(equations, targets);
            }
            flat::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                // Recurse into if branches.
                for block in cond_blocks {
                    collect_when_statement_targets(&block.stmts, targets);
                }
                if let Some(else_stmts) = else_block {
                    collect_when_statement_targets(else_stmts, targets);
                }
            }
            flat::Statement::While(block) => {
                collect_when_statement_targets(&block.stmts, targets);
            }
            // Assignment, Return, Break, FunctionCall, Reinit outside when are continuous.
            _ => {}
        }
    }
}

/// Convert a ComponentReference to a flat::VarName (dot-joined).
fn component_ref_to_varname(comp: &rumoca_ir_flat::ComponentReference) -> flat::VarName {
    comp.to_var_name()
}

/// Extract flat::VarName from an expression if it's a ComponentReference.
fn varref_with_subscripts(name: &flat::VarName, subscripts: &[flat::Subscript]) -> flat::VarName {
    if subscripts.is_empty() {
        return name.clone();
    }
    fn render_subscript(subscript: &flat::Subscript) -> String {
        match subscript {
            flat::Subscript::Index(index) => index.to_string(),
            flat::Subscript::Colon => ":".to_string(),
            flat::Subscript::Expr(expr) => format!("{expr:?}"),
        }
    }
    let rendered = subscripts
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(",");
    flat::VarName::new(format!("{}[{rendered}]", name.as_str()))
}

fn extract_output_varname(expr: &rumoca_ir_flat::Expression) -> Option<flat::VarName> {
    if let rumoca_ir_flat::Expression::VarRef { name, subscripts } = expr {
        Some(varref_with_subscripts(name, subscripts))
    } else {
        None
    }
}

/// Collect ALL assignment targets from statements (used inside when blocks).
fn collect_all_statement_assignments(
    statements: &[rumoca_ir_flat::Statement],
    targets: &mut HashSet<flat::VarName>,
) {
    for stmt in statements {
        collect_statement_assignment(stmt, targets);
    }
}

/// Process a single statement for assignment targets.
fn collect_statement_assignment(
    stmt: &rumoca_ir_flat::Statement,
    targets: &mut HashSet<flat::VarName>,
) {
    match stmt {
        flat::Statement::Assignment { comp, .. } => {
            let target = component_ref_to_varname(comp);
            insert_when_target(targets, &target);
        }
        flat::Statement::FunctionCall { outputs, .. } => {
            for output in outputs.iter().filter_map(extract_output_varname) {
                insert_when_target(targets, &output);
            }
        }
        flat::Statement::Reinit { variable, .. } => {
            let target = component_ref_to_varname(variable);
            insert_when_target(targets, &target);
        }
        flat::Statement::For { equations, .. } => {
            collect_all_statement_assignments(equations, targets);
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_all_statement_assignments(&block.stmts, targets);
            }
            if let Some(else_stmts) = else_block {
                collect_all_statement_assignments(else_stmts, targets);
            }
        }
        flat::Statement::While(block) => {
            collect_all_statement_assignments(&block.stmts, targets);
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                collect_all_statement_assignments(&block.stmts, targets);
            }
        }
        flat::Statement::Return
        | flat::Statement::Break
        | flat::Statement::Empty
        | flat::Statement::Assert { .. } => {}
    }
}

/// Recursively collect assignment targets from when-equations.
fn collect_when_equation_targets(
    equations: &[flat::WhenEquation],
    targets: &mut HashSet<flat::VarName>,
) {
    for eq in equations {
        match eq {
            flat::WhenEquation::Assign { target, .. } => {
                insert_when_target(targets, target);
            }
            flat::WhenEquation::Reinit { state, .. } => {
                // Reinit targets are state variables, not discrete,
                // but track them anyway as they are when-assigned.
                insert_when_target(targets, state);
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, .. } => {
                for out in outputs {
                    insert_when_target(targets, out);
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_cond, branch_eqs) in branches {
                    collect_when_equation_targets(branch_eqs, targets);
                }
                collect_when_equation_targets(else_branch, targets);
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

/// Insert a when-equation target, also adding the base name (without subscripts).
/// When equations for array elements produce targets like `k[1]`, but the flat
/// variable uses just the base name `k`. We insert both to ensure correct matching.
fn insert_when_target(targets: &mut HashSet<flat::VarName>, target: &flat::VarName) {
    targets.insert(target.clone());
    // Keep progressively less-indexed aliases during migration from embedded
    // subscript strings to structured references.
    for fallback in subscript_fallback_chain(target) {
        targets.insert(fallback);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_when_target_adds_base_name_for_subscripted_target() {
        let mut targets = HashSet::default();
        let target = flat::VarName::new("k[1]");

        insert_when_target(&mut targets, &target);

        assert!(targets.contains(&flat::VarName::new("k[1]")));
        assert!(targets.contains(&flat::VarName::new("k")));
    }

    #[test]
    fn test_insert_when_target_adds_all_subscript_fallback_layers() {
        let mut targets = HashSet::default();
        let target = flat::VarName::new("a[1].b[2].c[3]");

        insert_when_target(&mut targets, &target);

        for expected in ["a[1].b[2].c[3]", "a[1].b[2].c", "a[1].b.c", "a.b.c"] {
            assert!(targets.contains(&flat::VarName::new(expected)));
        }
    }

    #[test]
    fn test_collect_when_statement_targets_keeps_function_call_output_subscripts() {
        let mut targets = HashSet::default();
        let when_stmt = flat::Statement::When(vec![flat::StatementBlock {
            cond: flat::Expression::Literal(flat::Literal::Boolean(true)),
            stmts: vec![flat::Statement::FunctionCall {
                comp: flat::ComponentReference {
                    local: false,
                    parts: vec![flat::ComponentRefPart {
                        ident: "superSample".to_string(),
                        subs: vec![],
                    }],
                    def_id: None,
                },
                args: vec![flat::Expression::VarRef {
                    name: flat::VarName::new("u"),
                    subscripts: vec![],
                }],
                outputs: vec![flat::Expression::VarRef {
                    name: flat::VarName::new("arr"),
                    subscripts: vec![flat::Subscript::Colon],
                }],
            }],
        }]);

        collect_when_statement_targets(&[when_stmt], &mut targets);

        assert!(targets.contains(&flat::VarName::new("arr[:]")));
    }
}
