use rumoca_ir_dae as dae;

use crate::simulation::dae_prepare::expr_contains_der_of;
use crate::simulation::diagnostics::sim_introspect_enabled;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
/// Errors returned while reordering equations for solver ODE rows.
pub enum EquationReorderError {
    #[error(
        "equation/variable mismatch: {n_equations} equations but {n_states} states + {n_algebraics} algebraics = {} unknowns",
        n_states + n_algebraics
    )]
    EquationMismatch {
        n_equations: usize,
        n_states: usize,
        n_algebraics: usize,
    },

    #[error(
        "no ODE equation found for state variable '{0}': every state needs an equation containing der({0})"
    )]
    MissingStateEquation(String),
}

fn count_states(dae_model: &dae::Dae) -> usize {
    dae_model.states.values().map(|v| v.size()).sum()
}

fn try_match_state_to_row(
    state_idx: usize,
    state_to_rows: &[Vec<usize>],
    row_to_state: &mut [Option<usize>],
    seen_rows: &mut [bool],
) -> bool {
    for &row_idx in &state_to_rows[state_idx] {
        if seen_rows[row_idx] {
            continue;
        }
        seen_rows[row_idx] = true;
        if let Some(other_state_idx) = row_to_state[row_idx] {
            if try_match_state_to_row(other_state_idx, state_to_rows, row_to_state, seen_rows) {
                row_to_state[row_idx] = Some(state_idx);
                return true;
            }
            continue;
        }
        row_to_state[row_idx] = Some(state_idx);
        return true;
    }
    false
}

fn match_primary_derivative_rows(
    state_to_rows: &[Vec<usize>],
    n_rows: usize,
) -> Vec<Option<usize>> {
    let mut state_order: Vec<usize> = (0..state_to_rows.len()).collect();
    state_order.sort_by_key(|idx| state_to_rows[*idx].len());

    let mut row_to_state: Vec<Option<usize>> = vec![None; n_rows];
    for state_idx in state_order {
        if state_to_rows[state_idx].is_empty() {
            continue;
        }
        let mut seen_rows = vec![false; n_rows];
        let _ = try_match_state_to_row(state_idx, state_to_rows, &mut row_to_state, &mut seen_rows);
    }

    let mut state_to_row = vec![None; state_to_rows.len()];
    for (row_idx, state_idx) in row_to_state.into_iter().enumerate() {
        if let Some(state_idx) = state_idx {
            state_to_row[state_idx] = Some(row_idx);
        }
    }
    state_to_row
}

fn trim_state_sizes_to_available_derivative_rows(dae_model: &mut dae::Dae) -> usize {
    let state_names: Vec<dae::VarName> = dae_model.states.keys().cloned().collect();
    let mut trimmed_scalars = 0usize;

    for state_name in state_names {
        let Some(current_var) = dae_model.states.get(&state_name) else {
            continue;
        };
        let current_size = current_var.size();
        if current_size <= 1 {
            continue;
        }

        let derivative_rows = dae_model
            .f_x
            .iter()
            .filter(|eq| expr_contains_der_of(&eq.rhs, &state_name))
            .count()
            .min(current_size);
        if derivative_rows == current_size {
            continue;
        }

        if derivative_rows == 0 {
            if let Some(var) = dae_model.states.shift_remove(&state_name) {
                trimmed_scalars += current_size;
                dae_model.algebraics.insert(state_name, var);
            }
            continue;
        }

        let Some(var) = dae_model.states.get_mut(&state_name) else {
            continue;
        };
        if var.dims.len() == 1 {
            var.dims[0] = derivative_rows as i64;
            trimmed_scalars += current_size - derivative_rows;
        }
    }

    trimmed_scalars
}

/// Reorder `f_x` so ODE rows are placed first and aligned with state ordering.
///
/// This routine is backend-agnostic and mutates the DAE in place.
pub fn reorder_equations_for_solver(dae_model: &mut dae::Dae) -> Result<(), EquationReorderError> {
    let mut n_x = count_states(dae_model);
    let n_eq = dae_model.f_x.len();

    if n_eq < n_x && trim_state_sizes_to_available_derivative_rows(dae_model) > 0 {
        n_x = count_states(dae_model);
    }
    if n_eq < n_x {
        return Err(EquationReorderError::EquationMismatch {
            n_equations: n_eq,
            n_states: n_x,
            n_algebraics: n_eq.saturating_sub(n_x),
        });
    }

    let state_entries: Vec<(dae::VarName, usize)> = dae_model
        .states
        .iter()
        .map(|(state_name, state_var)| (state_name.clone(), state_var.size()))
        .collect();
    let state_to_rows: Vec<Vec<usize>> = state_entries
        .iter()
        .map(|(state_name, _)| {
            dae_model
                .f_x
                .iter()
                .enumerate()
                .filter_map(|(idx, eq)| expr_contains_der_of(&eq.rhs, state_name).then_some(idx))
                .collect()
        })
        .collect();
    let primary_rows = match_primary_derivative_rows(&state_to_rows, n_eq);

    let mut used = vec![false; n_eq];
    let mut ordered: Vec<dae::Equation> = Vec::with_capacity(n_eq);

    for (state_idx, (state_name, n_scalars)) in state_entries.iter().enumerate() {
        let n_scalars = *n_scalars;
        let candidates = &state_to_rows[state_idx];
        let mut selected = Vec::new();

        if let Some(primary) = primary_rows[state_idx] {
            selected.push(primary);
            used[primary] = true;
        }

        if selected.is_empty() {
            if sim_introspect_enabled() {
                eprintln!(
                    "[sim-introspect] reorder state={} size={} der_rows_all={:?} selected=[]",
                    state_name, n_scalars, candidates
                );
            }
            return Err(EquationReorderError::MissingStateEquation(
                state_name.as_str().to_string(),
            ));
        }

        for &row_idx in candidates {
            if selected.len() >= n_scalars {
                break;
            }
            if used[row_idx] {
                continue;
            }
            selected.push(row_idx);
            used[row_idx] = true;
        }

        if sim_introspect_enabled() {
            eprintln!(
                "[sim-introspect] reorder state={} size={} der_rows_all={:?} selected={:?}",
                state_name, n_scalars, candidates, selected
            );
        }

        ordered.extend(selected.iter().map(|idx| dae_model.f_x[*idx].clone()));
    }

    for (idx, eq) in dae_model.f_x.iter().enumerate() {
        if !used[idx] {
            ordered.push(eq.clone());
        }
    }

    dae_model.f_x = ordered;
    Ok(())
}
