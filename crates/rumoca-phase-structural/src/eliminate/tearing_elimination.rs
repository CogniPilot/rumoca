use std::collections::HashSet;

use indexmap::IndexSet;

use crate::{EquationRef, UnknownId, tear_algebraic_loop};

use super::{
    Dae, DerivativeNameMatcher, Substitution, VarName, algebraic_or_output_unknown,
    apply_substitutions_in_order, can_eliminate_scalar_unknown, can_use_equation_for_elimination,
    equation_has_state_derivative, expr_contains_var, normalize_unknown_for_dae,
    stable_solution_for_unknown, substitution_for_var,
};

#[allow(clippy::too_many_arguments)]
pub(super) fn tear_and_eliminate_loop_block(
    dae: &Dae,
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    runtime_protected_unknowns: &IndexSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    substitutions: &mut Vec<Substitution>,
    eliminated_eq_indices: &mut Vec<usize>,
    eliminated_eq_flags: &mut [bool],
    eliminated_var_names: &mut Vec<VarName>,
) {
    let Some(var_names) = loop_elimination_unknowns(dae, unknowns, runtime_protected_unknowns)
    else {
        return;
    };
    let eq_indices: Vec<usize> = equations.iter().map(|eq| eq.0).collect();
    if !loop_equations_can_be_torn(dae, &eq_indices, state_derivative_matcher) {
        return;
    }

    let local_eq_unknowns = loop_local_incidence(dae, &eq_indices, &var_names, substitutions);
    let Some(tearing) = tear_algebraic_loop(var_names.len(), &local_eq_unknowns) else {
        return;
    };

    let mut loop_substitutions = Vec::new();
    let mut trial_substitutions = substitutions.clone();
    for (local_eq, local_var) in tearing.causal_sequence {
        let eq_idx = eq_indices[local_eq];
        debug_assert!(
            eq_idx < eliminated_eq_flags.len(),
            "eq_idx {eq_idx} out of bounds (len={}): tearing BLT index invariant violated",
            eliminated_eq_flags.len()
        );
        if eliminated_eq_flags[eq_idx] {
            return;
        }
        let var_name = var_names[local_var].clone();
        let eq_rhs = apply_substitutions_in_order(
            &dae.continuous.equations[eq_idx].rhs,
            &trial_substitutions,
        );
        let Some(solution) = stable_solution_for_unknown(dae, &eq_rhs, &var_name) else {
            return;
        };
        trial_substitutions.push(substitution_for_var(
            dae,
            var_name.clone(),
            solution.clone(),
        ));
        loop_substitutions.push((eq_idx, var_name, solution));
    }

    for (eq_idx, var_name, solution) in loop_substitutions {
        substitutions.push(substitution_for_var(dae, var_name.clone(), solution));
        eliminated_eq_indices.push(eq_idx);
        eliminated_eq_flags[eq_idx] = true;
        eliminated_var_names.push(var_name);
    }
}

fn loop_elimination_unknowns(
    dae: &Dae,
    unknowns: &[UnknownId],
    runtime_protected_unknowns: &IndexSet<String>,
) -> Option<Vec<VarName>> {
    unknowns
        .iter()
        .map(|unknown| {
            let var_name = normalize_unknown_for_dae(dae, algebraic_or_output_unknown(unknown)?);
            can_eliminate_scalar_unknown(dae, &var_name, runtime_protected_unknowns)
                .then_some(var_name)
        })
        .collect()
}

fn loop_equations_can_be_torn(
    dae: &Dae,
    eq_indices: &[usize],
    state_derivative_matcher: &DerivativeNameMatcher,
) -> bool {
    eq_indices
        .iter()
        .all(|&eq_idx| can_use_equation_for_elimination(dae, eq_idx))
        && !eq_indices
            .iter()
            .any(|&eq_idx| equation_has_state_derivative(dae, eq_idx, state_derivative_matcher))
}

fn loop_local_incidence(
    dae: &Dae,
    eq_indices: &[usize],
    var_names: &[VarName],
    substitutions: &[Substitution],
) -> Vec<HashSet<usize>> {
    eq_indices
        .iter()
        .map(|&eq_idx| {
            let rhs =
                apply_substitutions_in_order(&dae.continuous.equations[eq_idx].rhs, substitutions);
            var_names
                .iter()
                .enumerate()
                .filter_map(|(idx, name)| expr_contains_var(&rhs, name).then_some(idx))
                .collect()
        })
        .collect()
}
