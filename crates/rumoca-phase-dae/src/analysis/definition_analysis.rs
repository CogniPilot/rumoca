//! Helpers for identifying variables already defined by algorithms/record equations.

use super::variable_analysis::{InternalInputIndex, filter_state_variables, is_continuous_unknown};
use crate::ToDaeError;
use crate::path_utils::subscript_fallback_chain;
use indexmap::IndexSet;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::HashSet;

/// Collect all variables that are defined by algorithm outputs.
///
/// Per MLS §11.1, algorithm sections contribute equations for variables they assign.
/// When an algorithm assigns to a record variable (like `vp := func(...)`), it defines
/// ALL fields of that record. This function expands record outputs to their individual
/// fields so we can skip binding equations for these variables.
///
/// Returns a set of variable names that should NOT have binding equations generated,
/// because they are already defined by algorithm sections.
pub(crate) fn collect_algorithm_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) -> HashSet<rumoca_core::VarName> {
    let mut defined = HashSet::default();

    for alg in &flat.algorithms {
        for out in &alg.outputs {
            // Check if this output is a record that expands to multiple fields.
            if let Some(fields) = prefix_children.get(out.as_str()) {
                // This is a record output - add all matching fields.
                defined.extend(fields.iter().cloned());
            }

            // Also add the output itself (for scalar outputs or the record itself).
            defined.insert(out.var_name().clone());
        }
    }

    defined
}

fn algorithm_output_target_name(
    comp: &rumoca_core::ComponentReference,
) -> Option<rumoca_core::VarName> {
    Some(comp.to_var_name())
}

fn insert_algorithm_defined_target(
    defined: &mut HashSet<rumoca_core::VarName>,
    target: &rumoca_core::VarName,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) {
    defined.insert(target.clone());
    for fallback in subscript_fallback_chain(target.as_str()) {
        defined.insert(fallback);
    }
    if let Some(fields) = prefix_children.get(target.as_str()) {
        for field in fields {
            defined.insert(field.clone());
            for fallback in subscript_fallback_chain(field.as_str()) {
                defined.insert(fallback);
            }
        }
    }
}

fn collect_continuous_algorithm_statement_targets(
    statements: &[rumoca_core::Statement],
    defined: &mut HashSet<rumoca_core::VarName>,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                let target = comp.to_var_name();
                insert_algorithm_defined_target(defined, &target, prefix_children);
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                for output in outputs.iter().filter_map(algorithm_output_target_name) {
                    insert_algorithm_defined_target(defined, &output, prefix_children);
                }
            }
            rumoca_core::Statement::Reinit { variable, .. } => {
                let target = variable.to_var_name();
                insert_algorithm_defined_target(defined, &target, prefix_children);
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_continuous_algorithm_statement_targets(equations, defined, prefix_children);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_continuous_algorithm_statement_targets(
                        &block.stmts,
                        defined,
                        prefix_children,
                    );
                }
                if let Some(else_stmts) = else_block {
                    collect_continuous_algorithm_statement_targets(
                        else_stmts,
                        defined,
                        prefix_children,
                    );
                }
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_continuous_algorithm_statement_targets(
                    &block.stmts,
                    defined,
                    prefix_children,
                );
            }
            // When-statements are discrete partitions and must not suppress
            // when-only classification.
            rumoca_core::Statement::When { blocks: _, .. } => {}
            rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. }
            | rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Assert { .. } => {}
        }
    }
}

pub(super) fn collect_continuous_algorithm_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) -> HashSet<rumoca_core::VarName> {
    let mut defined = HashSet::default();
    for algorithm in &flat.algorithms {
        collect_continuous_algorithm_statement_targets(
            &algorithm.statements,
            &mut defined,
            prefix_children,
        );
    }
    defined
}

/// Collect variables that are fields of record-type LHS variables in flat equations
/// (MLS §8.4, §12.2).
///
/// When a flat equation defines a record variable (e.g., `cc = func(...)`), all expanded
/// fields (e.g., `cc.idrain`, `cc.iBD`, etc.) are implicitly defined by that equation.
/// Binding equations for those fields should not be generated to avoid double-counting.
///
/// Returns a set of variable names that are defined by record equations.
pub(crate) fn collect_record_equation_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
) -> Result<HashSet<rumoca_core::VarName>, ToDaeError> {
    let mut defined = HashSet::default();
    let internal_inputs = InternalInputIndex::new(flat)?;
    let state_vars = filter_state_variables(
        super::classification::find_state_variables(flat),
        flat,
        &internal_inputs,
    );
    let continuous_unknown_prefixes = build_continuous_unknown_prefix_set(flat, &state_vars)?;

    for eq in &flat.equations {
        // Extract LHS variable from residual pattern: lhs - rhs = 0.
        let rumoca_core::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }

        // Get the variable name from the LHS.
        let lhs_var_name = match lhs.as_ref() {
            rumoca_core::Expression::VarRef { name, .. } => Some(name),
            _ => None,
        };

        let Some(var_name) = lhs_var_name else {
            continue;
        };

        // Check if this variable is a record type (has expanded fields in flat.variables).
        let Some(field_names) = prefix_children.get(var_name.as_str()) else {
            continue;
        };

        // Only add fields that have bindings which DON'T reference continuous
        // unknowns. If a binding references unknowns (e.g., `Phi.re = port_p.Phi.re`),
        // it provides an independent constraint separate from the record equation
        // (e.g., `Phi = Complex(0,0)`) and must be kept for correct balance (MLS §4.7).
        for field_name in field_names {
            if is_dominated_field_binding(
                flat,
                field_name,
                &state_vars,
                &continuous_unknown_prefixes,
            ) {
                defined.insert(field_name.clone());
            }
        }
    }

    Ok(defined)
}

fn is_dominated_field_binding(
    flat: &flat::Model,
    field_name: &rumoca_core::VarName,
    state_vars: &IndexSet<rumoca_core::VarName>,
    continuous_unknown_prefixes: &IndexSet<String>,
) -> bool {
    let Some(field_var) = flat.variables.get(field_name) else {
        return false;
    };
    let Some(binding) = &field_var.binding else {
        return false;
    };

    // Keep binding if it references continuous unknowns; otherwise dominated
    // by parent record equation and safe to skip.
    let binding_refs = crate::collect_var_refs(binding);
    !binding_refs_contain_unknowns(&binding_refs, flat, state_vars, continuous_unknown_prefixes)
}

fn binding_refs_contain_unknowns(
    refs: &HashSet<rumoca_core::VarName>,
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
    continuous_unknown_prefixes: &IndexSet<String>,
) -> bool {
    refs.iter().any(|r| {
        is_continuous_unknown_var(flat, state_vars, r)
            || has_continuous_unknown_fields(continuous_unknown_prefixes, r)
    })
}

fn is_continuous_unknown_var(
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
    name: &rumoca_core::VarName,
) -> bool {
    is_continuous_unknown(flat, state_vars, name)
}

/// Returns true if `name` is a parent record prefix with at least one
/// continuous unknown field (e.g., `vcf` when unknowns include `vcf.pin_p.v`).
fn has_continuous_unknown_fields(
    continuous_unknown_prefixes: &IndexSet<String>,
    name: &rumoca_core::VarName,
) -> bool {
    continuous_unknown_prefixes.contains(name.as_str())
}

/// Build a set of all parent prefixes for continuous unknown variable names.
///
/// Example: for unknown `vcf.pin_p.v`, inserts `vcf.pin_p` and `vcf`.
/// For `a.b.c.d`, inserts `a.b.c`, `a.b`, and `a`.
///
/// This allows O(1) lookups in `has_continuous_unknown_fields`.
fn build_continuous_unknown_prefix_set(
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
) -> Result<IndexSet<String>, ToDaeError> {
    let mut prefixes = IndexSet::new();

    for (name, variable) in &flat.variables {
        if !is_continuous_unknown(flat, state_vars, name) {
            continue;
        }

        let path = rumoca_core::ComponentPath::from_flat_path(name.as_str());
        for i in 1..path.len() {
            let Some(prefix) = path.prefix(i) else {
                return Err(ToDaeError::runtime_contract_violation_at(
                    format!(
                        "continuous unknown `{name}` could not produce prefix {i} \
                         from {} path segments",
                        path.len()
                    ),
                    variable.source_span,
                ));
            };
            prefixes.insert(prefix.to_flat_string());
        }
    }

    Ok(prefixes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_span() -> Span {
        Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    #[test]
    fn test_collect_algorithm_defined_vars_expands_record_outputs() {
        let mut flat = flat::Model::default();
        let mut alg = flat::Algorithm::new(Vec::new(), test_span(), "test");
        alg.outputs.push(rumoca_core::VarName::new("rec").into());
        alg.outputs.push(rumoca_core::VarName::new("x").into());
        flat.algorithms.push(alg);

        let mut prefix_children = FxHashMap::default();
        prefix_children.insert(
            "rec".to_string(),
            vec![
                rumoca_core::VarName::new("rec.a"),
                rumoca_core::VarName::new("rec.b"),
            ],
        );

        let defined = collect_algorithm_defined_vars(&flat, &prefix_children);

        assert!(defined.contains(&rumoca_core::VarName::new("rec")));
        assert!(defined.contains(&rumoca_core::VarName::new("rec.a")));
        assert!(defined.contains(&rumoca_core::VarName::new("rec.b")));
        assert!(defined.contains(&rumoca_core::VarName::new("x")));
    }

    #[test]
    fn test_collect_continuous_algorithm_defined_vars_skips_when_only_assignments() {
        let mut flat = flat::Model::default();
        let mut alg = flat::Algorithm::new(Vec::new(), test_span(), "test");
        alg.statements.push(rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: test_span(),
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: rumoca_core::ComponentReference {
                        local: false,
                        span: test_span(),
                        parts: vec![rumoca_core::ComponentRefPart {
                            ident: "x".to_string(),
                            span: test_span(),
                            subs: vec![],
                        }],
                        def_id: None,
                    },
                    value: rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: test_span(),
                    },
                    span: test_span(),
                }],
            }],
            span: test_span(),
        });
        flat.algorithms.push(alg);

        let prefix_children = FxHashMap::default();
        let defined = collect_continuous_algorithm_defined_vars(&flat, &prefix_children);

        assert!(
            !defined.contains(&rumoca_core::VarName::new("x")),
            "when-only algorithm targets must not be treated as continuous definitions"
        );
    }

    #[test]
    fn test_build_continuous_unknown_prefix_set_ignores_dot_inside_subscript() {
        let mut flat = flat::Model::default();
        flat.add_variable(
            rumoca_core::VarName::new("bus[data.medium].pin.v"),
            flat::Variable {
                name: rumoca_core::VarName::new("bus[data.medium].pin.v"),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let state_vars = IndexSet::new();
        let prefixes = build_continuous_unknown_prefix_set(&flat, &state_vars)
            .expect("continuous unknown prefix set should build");
        assert!(prefixes.contains("bus[data.medium]"));
        assert!(prefixes.contains("bus[data.medium].pin"));
        assert!(!prefixes.contains("bus[data"));
    }
}
