//! Helpers for identifying variables already defined by algorithms/record equations.

use super::{filter_state_variables, is_continuous_unknown};
use crate::path_utils::{split_path_with_indices, subscript_fallback_chain};
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
pub(super) fn collect_algorithm_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) -> HashSet<flat::VarName> {
    let mut defined = HashSet::default();

    for alg in &flat.algorithms {
        for out in &alg.outputs {
            // Check if this output is a record that expands to multiple fields.
            if let Some(fields) = prefix_children.get(out.as_str()) {
                // This is a record output - add all matching fields.
                defined.extend(fields.iter().cloned());
            }

            // Also add the output itself (for scalar outputs or the record itself).
            defined.insert(out.clone());
        }
    }

    defined
}

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

fn algorithm_output_target_name(expr: &flat::Expression) -> Option<flat::VarName> {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            Some(varref_with_subscripts(name, subscripts))
        }
        flat::Expression::FieldAccess { base, field } => {
            if let flat::Expression::VarRef { name, subscripts } = base.as_ref() {
                let base = varref_with_subscripts(name, subscripts);
                Some(flat::VarName::new(format!("{}.{}", base.as_str(), field)))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn insert_algorithm_defined_target(
    defined: &mut HashSet<flat::VarName>,
    target: &flat::VarName,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) {
    defined.insert(target.clone());
    for fallback in subscript_fallback_chain(target) {
        defined.insert(fallback);
    }
    if let Some(fields) = prefix_children.get(target.as_str()) {
        for field in fields {
            defined.insert(field.clone());
            for fallback in subscript_fallback_chain(field) {
                defined.insert(fallback);
            }
        }
    }
}

fn collect_continuous_algorithm_statement_targets(
    statements: &[flat::Statement],
    defined: &mut HashSet<flat::VarName>,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) {
    for statement in statements {
        match statement {
            flat::Statement::Assignment { comp, .. } => {
                let target = comp.to_var_name();
                insert_algorithm_defined_target(defined, &target, prefix_children);
            }
            flat::Statement::FunctionCall { outputs, .. } => {
                for output in outputs.iter().filter_map(algorithm_output_target_name) {
                    insert_algorithm_defined_target(defined, &output, prefix_children);
                }
            }
            flat::Statement::Reinit { variable, .. } => {
                let target = variable.to_var_name();
                insert_algorithm_defined_target(defined, &target, prefix_children);
            }
            flat::Statement::For { equations, .. } => {
                collect_continuous_algorithm_statement_targets(equations, defined, prefix_children);
            }
            flat::Statement::If {
                cond_blocks,
                else_block,
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
            flat::Statement::While(block) => {
                collect_continuous_algorithm_statement_targets(
                    &block.stmts,
                    defined,
                    prefix_children,
                );
            }
            // When-statements are discrete partitions and must not suppress
            // when-only classification.
            flat::Statement::When(_) => {}
            flat::Statement::Return
            | flat::Statement::Break
            | flat::Statement::Empty
            | flat::Statement::Assert { .. } => {}
        }
    }
}

pub(super) fn collect_continuous_algorithm_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) -> HashSet<flat::VarName> {
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
pub(super) fn collect_record_equation_defined_vars(
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<flat::VarName>>,
) -> HashSet<flat::VarName> {
    let mut defined = HashSet::default();
    let continuous_unknown_prefixes = build_continuous_unknown_prefix_set(flat);

    for eq in &flat.equations {
        // Extract LHS variable from residual pattern: lhs - rhs = 0.
        let flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_ast::OpBinary::Sub(_)) {
            continue;
        }

        // Get the variable name from the LHS.
        let lhs_var_name = match lhs.as_ref() {
            flat::Expression::VarRef { name, .. } => Some(name),
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
            if is_dominated_field_binding(flat, field_name, &continuous_unknown_prefixes) {
                defined.insert(field_name.clone());
            }
        }
    }

    defined
}

fn is_dominated_field_binding(
    flat: &flat::Model,
    field_name: &flat::VarName,
    continuous_unknown_prefixes: &HashSet<String>,
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
    !binding_refs_contain_unknowns(&binding_refs, flat, continuous_unknown_prefixes)
}

fn binding_refs_contain_unknowns(
    refs: &HashSet<flat::VarName>,
    flat: &flat::Model,
    continuous_unknown_prefixes: &HashSet<String>,
) -> bool {
    refs.iter().any(|r| {
        is_continuous_unknown_var(flat, r)
            || has_continuous_unknown_fields(continuous_unknown_prefixes, r)
    })
}

fn is_continuous_unknown_var(flat: &flat::Model, name: &flat::VarName) -> bool {
    let state_vars =
        filter_state_variables(crate::classification::find_state_variables(flat), flat);
    is_continuous_unknown(flat, &state_vars, name)
}

/// Returns true if `name` is a parent record prefix with at least one
/// continuous unknown field (e.g., `vcf` when unknowns include `vcf.pin_p.v`).
fn has_continuous_unknown_fields(
    continuous_unknown_prefixes: &HashSet<String>,
    name: &flat::VarName,
) -> bool {
    continuous_unknown_prefixes.contains(name.as_str())
}

/// Build a set of all parent prefixes for continuous unknown variable names.
///
/// Example: for unknown `vcf.pin_p.v`, inserts `vcf.pin_p` and `vcf`.
/// For `a.b.c.d`, inserts `a.b.c`, `a.b`, and `a`.
///
/// This allows O(1) lookups in `has_continuous_unknown_fields`.
fn build_continuous_unknown_prefix_set(flat: &flat::Model) -> HashSet<String> {
    let mut prefixes = HashSet::default();
    let state_vars =
        filter_state_variables(crate::classification::find_state_variables(flat), flat);

    for name in flat.variables.keys() {
        if !is_continuous_unknown(flat, &state_vars, name) {
            continue;
        }

        let parts = split_path_with_indices(name.as_str());
        // Insert all proper prefixes (exclude full name).
        for i in 1..parts.len() {
            prefixes.insert(parts[..i].join("."));
        }
    }

    prefixes
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    #[test]
    fn test_collect_algorithm_defined_vars_expands_record_outputs() {
        let mut flat = flat::Model::default();
        let mut alg = flat::Algorithm::new(Vec::new(), Span::DUMMY, "test");
        alg.outputs.push(flat::VarName::new("rec"));
        alg.outputs.push(flat::VarName::new("x"));
        flat.algorithms.push(alg);

        let mut prefix_children = FxHashMap::default();
        prefix_children.insert(
            "rec".to_string(),
            vec![flat::VarName::new("rec.a"), flat::VarName::new("rec.b")],
        );

        let defined = collect_algorithm_defined_vars(&flat, &prefix_children);

        assert!(defined.contains(&flat::VarName::new("rec")));
        assert!(defined.contains(&flat::VarName::new("rec.a")));
        assert!(defined.contains(&flat::VarName::new("rec.b")));
        assert!(defined.contains(&flat::VarName::new("x")));
    }

    #[test]
    fn test_collect_continuous_algorithm_defined_vars_skips_when_only_assignments() {
        let mut flat = flat::Model::default();
        let mut alg = flat::Algorithm::new(Vec::new(), Span::DUMMY, "test");
        alg.statements
            .push(flat::Statement::When(vec![flat::StatementBlock {
                cond: flat::Expression::Literal(flat::Literal::Boolean(true)),
                stmts: vec![flat::Statement::Assignment {
                    comp: flat::ComponentReference {
                        local: false,
                        parts: vec![flat::ComponentRefPart {
                            ident: "x".to_string(),
                            subs: vec![],
                        }],
                        def_id: None,
                    },
                    value: flat::Expression::Literal(flat::Literal::Real(1.0)),
                }],
            }]));
        flat.algorithms.push(alg);

        let prefix_children = FxHashMap::default();
        let defined = collect_continuous_algorithm_defined_vars(&flat, &prefix_children);

        assert!(
            !defined.contains(&flat::VarName::new("x")),
            "when-only algorithm targets must not be treated as continuous definitions"
        );
    }

    #[test]
    fn test_build_continuous_unknown_prefix_set_ignores_dot_inside_subscript() {
        let mut flat = flat::Model::default();
        flat.add_variable(
            flat::VarName::new("bus[data.medium].pin.v"),
            flat::Variable {
                name: flat::VarName::new("bus[data.medium].pin.v"),
                variability: rumoca_ir_ast::Variability::Empty,
                is_primitive: true,
                ..Default::default()
            },
        );

        let prefixes = build_continuous_unknown_prefix_set(&flat);
        assert!(prefixes.contains("bus[data.medium]"));
        assert!(prefixes.contains("bus[data.medium].pin"));
        assert!(!prefixes.contains("bus[data"));
    }
}
