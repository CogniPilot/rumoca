//! Seeds for holonomic constraints that reach their states only indirectly.
//!
//! [`super::dummy_derivative_group::holonomic_constraint_groups`] pairs a state
//! with a constraint row by *textual* occurrence: the row has to mention the
//! state (or one of its scalar components) to be differentiated against it.
//!
//! Position-level (index-3) kinematic constraints do not have that shape. A
//! multibody cut joint writes its constraint against an intermediate algebraic
//! quantity, e.g. `Joints.RevolutePlanarLoopConstraint`:
//!
//! ```text
//! r_rel_a = Frames.resolve2(frame_a.R, frame_b.r_0 - frame_a.r_0);
//! 0 = ex_a*r_rel_a;
//! 0 = ey_a*r_rel_a;
//! ```
//!
//! The two constraint rows mention no state at all — the joint angles they
//! constrain are several algebraic definitions away — and they mention no
//! constraint force either, so a maximum matching leaves one row unmatched per
//! constraint and structural analysis reports a singular system.
//!
//! This module recovers the missing seeds. A row is a *constraint row* when it
//! contains no `der` and determines nothing on its own: every continuous
//! unknown it reads already has an explicit defining equation somewhere else.
//! Differentiating such a row once, with the relaxed derivative map that
//! resolves algebraic definitions transitively, exposes exactly the state
//! derivatives the constraint acts on. Those states are the seeds this module
//! returns; the existing dummy-derivative machinery does the rest, and the
//! caller still commits a candidate only when the DAE maximum matching proves
//! it strictly reduces the matching deficiency.

use super::*;

/// One `(state, constraint row)` seed for holonomic differentiation.
pub(super) struct IndirectConstraintSeed {
    pub(super) state_name: VarName,
    pub(super) equation_index: usize,
    rank: u8,
}

/// Enumerate `(state, constraint row)` pairs the textual scan cannot see.
///
/// Only scalar rows and scalar states take part: a vector holonomic group is
/// assembled from consecutive rows of one origin that jointly cover the whole
/// state, and that assembly has no meaning for a state the rows never name.
pub(super) fn indirect_constraint_seeds(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    demoted_derivatives: &HashMap<String, Expression>,
) -> Result<Vec<IndirectConstraintSeed>, StructuralError> {
    let state_names: Vec<VarName> = dae
        .variables
        .states
        .iter()
        .filter(|(_, variable)| variable.dims.is_empty())
        .filter(|(_, variable)| variable.state_select != rumoca_core::StateSelect::Always)
        .map(|(name, _)| name.clone())
        .collect();
    if state_names.is_empty() {
        return Ok(Vec::new());
    }
    let mut state_name_set: HashSet<String> = dae
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    state_name_set.extend(demoted_derivatives.keys().cloned());

    let mut seeds = Vec::new();
    for index in constraint_row_indices(dae, defining_expr_index, structural_bindings) {
        if !constraint_row_is_differentiable(
            dae,
            defining_expr_index,
            index,
            &state_name_set,
            demoted_derivatives,
        )? {
            continue;
        }
        collect_row_seeds(dae, defining_expr_index, index, &state_names, &mut seeds);
    }
    seeds.sort_by(|lhs, rhs| {
        lhs.rank
            .cmp(&rhs.rank)
            .then_with(|| lhs.state_name.cmp(&rhs.state_name))
            .then_with(|| lhs.equation_index.cmp(&rhs.equation_index))
    });
    Ok(seeds)
}

/// Scalar rows that constrain the solution manifold without determining a value.
fn constraint_row_indices(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
) -> Vec<usize> {
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(index, equation)| {
            equation.scalar_count == 1
                && !expression_contains_any_der_call(&equation.rhs)
                && row_determines_nothing(dae, defining_expr_index, *index, &equation.rhs)
                && state_row_reduction::expression_is_smooth_for_index_reduction(
                    &equation.rhs,
                    dae,
                    structural_bindings,
                )
        })
        .map(|(index, _)| index)
        .collect()
}

/// True when every continuous unknown the row reads is defined by another row.
///
/// A row that can be solved for one of its own unknowns is that unknown's
/// defining equation, not a constraint: differentiating it would replace a
/// definition by its derivative and lose the value the rest of the system
/// reads. Parameters, constants, `time` and states are known columns for the
/// structural matching, so they never make a row determining.
fn row_determines_nothing(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    index: usize,
    rhs: &Expression,
) -> bool {
    let equation = &dae.continuous.equations[index];
    if equation.lhs.is_some() {
        return false;
    }
    collect_rhs_var_refs(rhs).iter().all(|name| {
        if name.as_str() == "time"
            || dae.variables.parameters.contains_key(name)
            || dae.variables.constants.contains_key(name)
            || dae.variables.states.contains_key(name)
        {
            return true;
        }
        defining_expr_index
            .get(name.as_str())
            .is_some_and(|candidates| {
                candidates
                    .iter()
                    .any(|candidate| candidate.equation_index != index)
            })
    })
}

/// Whether one constraint row differentiates to a closed expression.
///
/// The relaxed derivative map walks the algebraic defining expressions, so a
/// successful derivative is expressed in states, their ODE right-hand sides and
/// known columns alone. A row whose derivative still mentions `der` of a
/// non-state is not closed, and a row the differentiator cannot handle at all
/// can never produce a usable candidate; neither is worth seeding.
fn constraint_row_is_differentiable(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    index: usize,
    state_name_set: &HashSet<String>,
    demoted_derivatives: &HashMap<String, Expression>,
) -> Result<bool, StructuralError> {
    let seed_exprs = [dae.continuous.equations[index].rhs.clone()];
    let derivative_map = build_relaxed_derivative_map_for_exprs_with_index(
        dae,
        defining_expr_index,
        &seed_exprs,
        RelaxedDerivativeMapOptions {
            canonical_state_derivative: None,
            rejected_state_derivative: None,
            excluded_equations: &[index],
            selected_derivatives: Some(demoted_derivatives),
        },
    )?;
    let Some(differentiated) =
        symbolic_time_derivative(&dae.continuous.equations[index].rhs, dae, &derivative_map)
    else {
        return Ok(false);
    };
    let differentiated = crate::eliminate::simplify_arithmetic_identities(differentiated);
    Ok(!expr_contains_der_of_non_state(
        &differentiated,
        state_name_set,
    ))
}

/// Record the states the constraint row reaches through algebraic definitions.
///
/// The row's own time derivative is not a usable seed list: the relaxed
/// derivative map substitutes each state's ODE right-hand side for `der(state)`
/// (`der(phi)` becomes `w`), so the differentiated row names velocities, not
/// derivatives. The dependency closure — the same transitive walk over defining
/// expressions that the chain rule uses — is what identifies the states the
/// constraint acts on.
///
/// States the row already names are left to the textual scan, which builds the
/// same group; seeding them twice would only duplicate trial candidates.
fn collect_row_seeds(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    index: usize,
    state_names: &[VarName],
    seeds: &mut Vec<IndirectConstraintSeed>,
) {
    let row = &dae.continuous.equations[index];
    let closure =
        derivative_closure_names(dae, defining_expr_index, std::slice::from_ref(&row.rhs));
    for state_name in state_names {
        if !closure.contains(state_name) {
            continue;
        }
        if dummy_derivative_group::contains_exact_reference(&row.rhs, state_name.as_str()) {
            continue;
        }
        if constrained_dummy_derivative::state_has_overlapping_event_update(dae, state_name) {
            continue;
        }
        let Some(variable) = dae.variables.states.get(state_name) else {
            continue;
        };
        crate::structural_trace!(
            "[sim-trace] indirect holonomic seed state={} constraint={} origin='{}'",
            state_name.as_str(),
            index,
            row.origin
        );
        seeds.push(IndirectConstraintSeed {
            rank: state_select_rank(variable.state_select),
            state_name: state_name.clone(),
            equation_index: index,
        });
    }
}
