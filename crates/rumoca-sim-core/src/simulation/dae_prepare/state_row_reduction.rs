use super::*;

fn state_has_any_equation_reference(dae: &Dae, state_name: &VarName) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, state_name))
}

fn state_has_any_derivative_reference(dae: &Dae, state_name: &VarName) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_contains_der_of(&eq.rhs, state_name))
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

fn states_with_assignable_derivative_rows(dae: &Dae, state_names: &[VarName]) -> HashSet<usize> {
    let state_to_rows: Vec<Vec<usize>> = state_names
        .iter()
        .map(|state_name| {
            dae.f_x
                .iter()
                .enumerate()
                .filter_map(|(row_idx, eq)| {
                    expr_contains_der_of(&eq.rhs, state_name).then_some(row_idx)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut state_order: Vec<usize> = (0..state_names.len()).collect();
    state_order.sort_by_key(|idx| state_to_rows[*idx].len());

    let mut row_to_state: Vec<Option<usize>> = vec![None; dae.f_x.len()];
    for state_idx in state_order {
        if state_to_rows[state_idx].is_empty() {
            continue;
        }
        let mut seen_rows = vec![false; dae.f_x.len()];
        let _ =
            try_match_state_to_row(state_idx, &state_to_rows, &mut row_to_state, &mut seen_rows);
    }

    row_to_state.into_iter().flatten().collect()
}

/// Demote states that are no longer referenced by any continuous equation.
///
/// Trivial elimination may remove an alias/binding equation that was the only
/// remaining reference to a misclassified state-like variable. Such orphan
/// states cannot have valid ODE rows and should be treated as algebraics.
pub fn demote_orphan_states_without_equation_refs(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_equation_reference(dae, &name) {
            continue;
        }
        if let Some(var) = dae.states.shift_remove(&name) {
            dae.algebraics.insert(name, var);
            demoted += 1;
        }
    }
    demoted
}

/// Demote state variables that have no `der(state)` occurrence in any equation.
///
/// Promotion of algebraics used in `der(...)` expressions can temporarily mark
/// variables as states even if later structural passes remove all derivative
/// occurrences for that variable. Such variables cannot be solved as states and
/// must remain algebraic.
pub fn demote_states_without_derivative_refs(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_derivative_reference(dae, &name) {
            continue;
        }
        if let Some(var) = dae.states.shift_remove(&name) {
            dae.algebraics.insert(name, var);
            demoted += 1;
        }
    }
    demoted
}

/// Demote states that cannot be assigned a unique derivative row.
///
/// The simulator's ODE row ordering needs at least one assignable derivative
/// equation per retained state. We compute a maximum bipartite matching between
/// states and derivative-bearing rows; unmatched states are demoted to
/// algebraics.
pub fn demote_states_without_assignable_derivative_rows(dae: &mut Dae) -> usize {
    let mut total_demoted = 0usize;

    loop {
        let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
        if state_names.is_empty() {
            break;
        }

        let matched_states = states_with_assignable_derivative_rows(dae, &state_names);
        let to_demote: Vec<VarName> = state_names
            .iter()
            .enumerate()
            .filter_map(|(idx, name)| (!matched_states.contains(&idx)).then_some(name.clone()))
            .collect();

        if to_demote.is_empty() {
            break;
        }

        let mut demoted_this_round = 0usize;
        for name in to_demote {
            if let Some(var) = dae.states.shift_remove(&name) {
                dae.algebraics.insert(name, var);
                demoted_this_round += 1;
            }
        }
        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
    }

    total_demoted
}

/// Phase-1 structural index reduction.
///
/// For each state without a `der(state)` equation, find a non-ODE constraint
/// referencing that state and differentiate it once with symbolic chain-rule.
/// The differentiated equation must explicitly contain `der(state)` to be
/// accepted; otherwise it is discarded.
pub fn index_reduce_missing_state_derivatives_once(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    if state_names.is_empty() {
        return 0;
    }
    let state_name_set: HashSet<String> = state_names
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();

    let der_map = build_relaxed_derivative_map(dae);
    let mut changed = 0usize;
    let mut used_eq = HashSet::new();

    for state_name in &state_names {
        if state_has_standalone_der_equation(dae, state_name, &state_names) {
            continue;
        }

        let candidate_indices: Vec<usize> = dae
            .f_x
            .iter()
            .enumerate()
            .filter_map(|(idx, eq)| {
                if used_eq.contains(&idx) {
                    return None;
                }
                if eq_contains_any_state_der(&eq.rhs, &state_names) {
                    return None;
                }
                expr_contains_var(&eq.rhs, state_name).then_some(idx)
            })
            .collect();

        for idx in candidate_indices {
            let differentiated = symbolic_time_derivative(&dae.f_x[idx].rhs, dae, &der_map);
            let Some(new_rhs) = differentiated else {
                continue;
            };
            let der_states = derivative_states_in_eq(&new_rhs, &state_names);
            if der_states.len() != 1 || der_states[0] != *state_name {
                continue;
            }
            if expr_contains_der_of_non_state(&new_rhs, &state_name_set) {
                continue;
            }

            let old_origin = dae.f_x[idx].origin.clone();
            dae.f_x[idx].rhs = new_rhs;
            dae.f_x[idx].origin = if old_origin.is_empty() {
                format!("index_reduction:d_dt_for_{}", state_name.as_str())
            } else {
                format!(
                    "{}|index_reduction:d_dt_for_{}",
                    old_origin,
                    state_name.as_str()
                )
            };
            used_eq.insert(idx);
            changed += 1;
            break;
        }
    }

    changed
}

pub fn index_reduce_missing_state_derivatives(dae: &mut Dae) -> usize {
    let max_rounds = dae.states.len().clamp(1, 8);
    let mut total_changed = 0usize;
    for _round in 0..max_rounds {
        let changed = index_reduce_missing_state_derivatives_once(dae);
        if changed == 0 {
            break;
        }
        total_changed += changed;
    }
    total_changed
}

/// Regularisation epsilon levels to try, from most accurate to least.
///
/// The larger fallback values help stiff, switch-heavy MSL examples that can
/// otherwise fail early with very small accepted timesteps.
pub const REGULARIZATION_LEVELS: &[f64] = &[1e-8, 1e-6, 1e-4, 1e-3, 1e-2, 1e-1];

/// Determine the sign of `der(state)` in an expression by tracking negations.
///
/// Returns +1 if der(state) appears with positive coefficient, -1 if negative, 0 if absent.
/// Tracks sign flips through subtraction (RHS negated) and unary minus.
pub fn der_sign_in_expr(expr: &Expression, state_name: &VarName, current_sign: i32) -> i32 {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
        } if args.len() == 1 && expr_refers_to_var(&args[0], state_name) => current_sign,
        Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::Add(_) | OpBinary::AddElem(_) => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            OpBinary::Sub(_) | OpBinary::SubElem(_) => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, -current_sign)
            }
            OpBinary::Mul(_) | OpBinary::MulElem(_) => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            _ => 0,
        },
        Expression::Unary { op, rhs } => match op {
            OpUnary::Minus(_) | OpUnary::DotMinus(_) => {
                der_sign_in_expr(rhs, state_name, -current_sign)
            }
            _ => der_sign_in_expr(rhs, state_name, current_sign),
        },
        Expression::If {
            branches,
            else_branch,
        } => {
            for (_, v) in branches {
                let s = der_sign_in_expr(v, state_name, current_sign);
                if s != 0 {
                    return s;
                }
            }
            der_sign_in_expr(else_branch, state_name, current_sign)
        }
        _ => 0,
    }
}

/// Normalize ODE equation signs so that `der(state)` has positive coefficient.
///
/// The mass-matrix formulation `M * y' = f` with `f = -eval(equation)` for ODE
/// rows requires `der(state)` to appear with coefficient +1 in the residual.
/// Equations like `0 = v - der(s)` (from `v = der(s)` in Modelica) have
/// coefficient -1 and produce the wrong sign.
///
/// This pass negates equations where `der(state)` has negative coefficient.
pub fn normalize_ode_equation_signs(dae: &mut Dae) {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    for (i, state_name) in state_names.iter().enumerate() {
        if i >= dae.f_x.len() {
            break;
        }
        let sign = der_sign_in_expr(&dae.f_x[i].rhs, state_name, 1);
        if sign < 0 {
            let old_rhs = dae.f_x[i].rhs.clone();
            dae.f_x[i].rhs = Expression::Unary {
                op: OpUnary::Minus(Default::default()),
                rhs: Box::new(old_rhs),
            };
        }
    }
}

/// After ODE row selection, non-ODE residual rows must not keep standalone
/// `der(state)` calls because compiled residual evaluation lowers `der(...)`
/// to zero outside the mass-matrix rows. Substitute any duplicate standalone
/// state derivative that can be resolved from the selected ODE rows.
pub fn substitute_standalone_state_derivatives_in_non_ode_rows(dae: &mut Dae) -> usize {
    let n_x: usize = dae.states.values().map(Variable::size).sum();
    if n_x == 0 || dae.f_x.len() <= n_x {
        return 0;
    }

    let der_map = build_der_value_map(dae);
    if der_map.is_empty() {
        return 0;
    }

    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut rewritten_rows = 0usize;

    for eq in dae.f_x.iter_mut().skip(n_x) {
        let mut rewritten = false;
        for state_name in &state_names {
            let Some(replacement) = der_map.get(state_name.as_str()) else {
                continue;
            };
            if expression_contains_any_der_call(replacement) {
                continue;
            }
            if !expr_contains_der_of(&eq.rhs, state_name) {
                continue;
            }
            eq.rhs = substitute_der_of_state(&eq.rhs, state_name, replacement);
            rewritten = true;
        }
        rewritten_rows += usize::from(rewritten);
    }

    rewritten_rows
}
