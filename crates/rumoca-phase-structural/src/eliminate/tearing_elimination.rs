use std::collections::HashSet;

use indexmap::IndexSet;

use crate::{EquationRef, StructuralError, UnknownId, tear_algebraic_loop};

use super::{
    Dae, DerivativeNameMatcher, Substitution, VarName, algebraic_or_output_unknown,
    apply_substitutions_in_order, can_eliminate_scalar_unknown, can_use_equation_for_elimination,
    equation_has_state_derivative, expr_contains_var, stable_solution_for_unknown,
    substitution_for_var,
};

#[cfg(feature = "tracing")]
macro_rules! trace_tearing {
    ($($tokens:tt)*) => {
        tracing::debug!(target: "rumoca_phase_structural::tearing", $($tokens)*);
    };
}

#[cfg(not(feature = "tracing"))]
macro_rules! trace_tearing {
    ($($tokens:tt)*) => {};
}

pub(super) struct EliminationOutputs<'a> {
    pub(super) substitutions: &'a mut Vec<Substitution>,
    pub(super) eliminated_eq_indices: &'a mut Vec<usize>,
    pub(super) eliminated_eq_flags: &'a mut [bool],
    pub(super) eliminated_var_names: &'a mut Vec<VarName>,
}

pub(super) fn tear_and_eliminate_loop_block(
    dae: &Dae,
    equations: &[EquationRef],
    unknowns: &[UnknownId],
    runtime_protected_unknowns: &IndexSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    outputs: EliminationOutputs<'_>,
) -> Result<(), StructuralError> {
    let EliminationOutputs {
        substitutions,
        eliminated_eq_indices,
        eliminated_eq_flags,
        eliminated_var_names,
    } = outputs;
    let Some(var_names) = loop_elimination_unknowns(dae, unknowns, runtime_protected_unknowns)?
    else {
        trace_tearing!(
            unknowns = unknowns.len(),
            "algebraic loop is not wholly eligible for symbolic tearing"
        );
        return Ok(());
    };
    let eq_indices: Vec<usize> = equations.iter().map(|eq| eq.0).collect();
    if !loop_equations_can_be_torn(dae, &eq_indices, state_derivative_matcher) {
        trace_tearing!(
            equations = eq_indices.len(),
            "algebraic loop contains an equation outside symbolic tearing scope"
        );
        return Ok(());
    }

    let local_eq_unknowns = loop_local_incidence(dae, &eq_indices, &var_names, substitutions)?;
    let Some(tearing) = tear_algebraic_loop(var_names.len(), &local_eq_unknowns) else {
        trace_tearing!(
            unknowns = var_names.len(),
            "incidence tearing did not reduce the algebraic loop"
        );
        return Ok(());
    };
    trace_tearing!(
        unknowns = var_names.len(),
        tear_variables = tearing.tear_var_local_indices.len(),
        causal_steps = tearing.causal_sequence.len(),
        "attempting symbolic algebraic-loop tearing"
    );
    let _causal_step_count = tearing.causal_sequence.len();
    let _tear_variable_count = tearing.tear_var_local_indices.len();

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
            return Ok(());
        }
        let var_name = var_names[local_var].clone();
        let eq_rhs = apply_substitutions_in_order(
            dae,
            &dae.continuous.equations[eq_idx].rhs,
            &trial_substitutions,
        )?;
        let Some(solution) = stable_solution_for_unknown(dae, &eq_rhs, &var_name)? else {
            trace_tearing!(
                variable = var_name.as_str(),
                equation = eq_idx,
                "symbolic tearing could not isolate a causal variable"
            );
            return Ok(());
        };
        trial_substitutions.push(substitution_for_var(
            dae,
            var_name.clone(),
            solution.clone(),
        )?);
        loop_substitutions.push((eq_idx, var_name, solution));
    }

    for (eq_idx, var_name, solution) in loop_substitutions {
        substitutions.push(substitution_for_var(dae, var_name.clone(), solution)?);
        eliminated_eq_indices.push(eq_idx);
        eliminated_eq_flags[eq_idx] = true;
        eliminated_var_names.push(var_name);
    }
    trace_tearing!(
        eliminated = _causal_step_count,
        retained_tears = _tear_variable_count,
        "symbolic algebraic-loop tearing succeeded"
    );
    Ok(())
}

fn loop_elimination_unknowns(
    dae: &Dae,
    unknowns: &[UnknownId],
    runtime_protected_unknowns: &IndexSet<String>,
) -> Result<Option<Vec<VarName>>, StructuralError> {
    let mut var_names = Vec::with_capacity(unknowns.len());
    for unknown in unknowns {
        let Some(raw_var_name) = algebraic_or_output_unknown(unknown) else {
            trace_tearing!(
                unknown = ?unknown,
                "symbolic tearing requires algebraic or output unknowns"
            );
            return Ok(None);
        };
        let var_name = raw_var_name.clone();
        if !can_eliminate_scalar_unknown(dae, &var_name, runtime_protected_unknowns)? {
            trace_tearing!(
                variable = var_name.as_str(),
                runtime_protected = runtime_protected_unknowns.contains(var_name.as_str()),
                "symbolic tearing cannot eliminate loop unknown"
            );
            return Ok(None);
        }
        var_names.push(var_name);
    }
    Ok(Some(var_names))
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
) -> Result<Vec<HashSet<usize>>, StructuralError> {
    eq_indices
        .iter()
        .map(|&eq_idx| {
            let rhs = apply_substitutions_in_order(
                dae,
                &dae.continuous.equations[eq_idx].rhs,
                substitutions,
            )?;
            Ok(var_names
                .iter()
                .enumerate()
                .filter_map(|(idx, name)| expr_contains_var(&rhs, name).then_some(idx))
                .collect())
        })
        .collect()
}
