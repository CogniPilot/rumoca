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
pub(super) fn collect_when_assigned_vars(flat: &flat::Model) -> HashSet<rumoca_core::VarName> {
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
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<rumoca_core::VarName>,
) {
    for stmt in statements {
        match stmt {
            rumoca_core::Statement::When { blocks, .. } => {
                // Everything inside a when statement is discrete.
                for block in blocks {
                    collect_all_statement_assignments(&block.stmts, targets);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                // Recurse into for loops to find nested when statements.
                collect_when_statement_targets(equations, targets);
            }
            rumoca_core::Statement::If {
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
            rumoca_core::Statement::While { block, .. } => {
                collect_when_statement_targets(&block.stmts, targets);
            }
            // Non-when statements do not make targets discrete here. DAE algorithm
            // lowering rejects unsupported statements such as bare reinit().
            _ => {}
        }
    }
}

fn extract_output_varname(comp: &rumoca_core::ComponentReference) -> Option<rumoca_core::VarName> {
    Some(comp.to_var_name())
}

/// Collect ALL assignment targets from statements (used inside when blocks).
fn collect_all_statement_assignments(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<rumoca_core::VarName>,
) {
    for stmt in statements {
        collect_statement_assignment(stmt, targets);
    }
}

/// Process a single statement for assignment targets.
fn collect_statement_assignment(
    stmt: &rumoca_core::Statement,
    targets: &mut HashSet<rumoca_core::VarName>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { comp, .. } => {
            let target = comp.to_var_name();
            insert_when_target(targets, &target);
        }
        rumoca_core::Statement::FunctionCall { outputs, .. } => {
            for output in outputs.iter().filter_map(extract_output_varname) {
                insert_when_target(targets, &output);
            }
        }
        rumoca_core::Statement::Reinit { variable, .. } => {
            let target = variable.to_var_name();
            insert_when_target(targets, &target);
        }
        rumoca_core::Statement::For { equations, .. } => {
            collect_all_statement_assignments(equations, targets);
        }
        rumoca_core::Statement::If {
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
        rumoca_core::Statement::While { block, .. } => {
            collect_all_statement_assignments(&block.stmts, targets);
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_all_statement_assignments(&block.stmts, targets);
            }
        }
        rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. }
        | rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Assert { .. } => {}
    }
}

/// Recursively collect assignment targets from when-equations.
fn collect_when_equation_targets(
    equations: &[flat::WhenEquation],
    targets: &mut HashSet<rumoca_core::VarName>,
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
fn insert_when_target(targets: &mut HashSet<rumoca_core::VarName>, target: &rumoca_core::VarName) {
    targets.insert(target.clone());
    // Keep progressively less-indexed aliases during migration from embedded
    // subscript strings to structured references.
    for fallback in subscript_fallback_chain(target.as_str()) {
        targets.insert(fallback);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("when_analysis_fixture.mo"),
            1,
            2,
        )
    }

    #[test]
    fn test_insert_when_target_adds_base_name_for_subscripted_target() {
        let mut targets = HashSet::default();
        let target = rumoca_core::VarName::new("k[1]");

        insert_when_target(&mut targets, &target);

        assert!(targets.contains(&rumoca_core::VarName::new("k[1]")));
        assert!(targets.contains(&rumoca_core::VarName::new("k")));
    }

    #[test]
    fn test_insert_when_target_adds_all_subscript_fallback_layers() {
        let mut targets = HashSet::default();
        let target = rumoca_core::VarName::new("a[1].b[2].c[3]");

        insert_when_target(&mut targets, &target);

        for expected in ["a[1].b[2].c[3]", "a[1].b[2].c", "a[1].b.c", "a.b.c"] {
            assert!(targets.contains(&rumoca_core::VarName::new(expected)));
        }
    }

    #[test]
    fn test_collect_when_statement_targets_keeps_function_call_output_subscripts() {
        let mut targets = HashSet::default();
        let span = test_span();
        let when_stmt = rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span,
                },
                stmts: vec![rumoca_core::Statement::FunctionCall {
                    comp: rumoca_core::ComponentReference {
                        local: false,
                        span,
                        parts: vec![rumoca_core::ComponentRefPart {
                            ident: "superSample".to_string(),
                            span,
                            subs: vec![],
                        }],
                        def_id: None,
                    },
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span,
                    }],
                    outputs: vec![rumoca_core::ComponentReference {
                        local: false,
                        span,
                        parts: vec![rumoca_core::ComponentRefPart {
                            ident: "arr".to_string(),
                            span,
                            subs: vec![rumoca_core::Subscript::generated_colon(span)],
                        }],
                        def_id: None,
                    }],
                    span,
                }],
            }],
            span,
        };

        collect_when_statement_targets(&[when_stmt], &mut targets);

        assert!(targets.contains(&rumoca_core::VarName::new("arr[:]")));
    }
}
