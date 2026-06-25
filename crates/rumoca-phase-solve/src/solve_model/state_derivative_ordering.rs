use std::sync::Arc;

use rumoca_eval_dae::eval::{EvalRuntimeState, eval_expr};
use rumoca_ir_dae as dae;

use super::{
    SolveModelLowerError, build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    derivative_coefficient_expr, reserve_solve_model_capacity, runtime_tail_error, scalar_names,
    solve_model_contract_violation, solve_model_vec_with_capacity,
};

pub(super) fn order_state_derivative_rows(
    dae_model: &mut dae::Dae,
    state_count: usize,
    params: &[f64],
    runtime: Arc<EvalRuntimeState>,
) -> Result<(), SolveModelLowerError> {
    if state_count == 0 || dae_model.continuous.equations.len() <= state_count {
        return Ok(());
    }
    let Some(first_state) = dae_model.variables.states.values().next() else {
        return Ok(());
    };
    let span = first_state.source_span;
    let mut state_names = solve_model_vec_with_capacity(
        dae_model.variables.states.len(),
        "state derivative name count",
        span,
    )?;
    for (name, var) in &dae_model.variables.states {
        let names = scalar_names(name.as_str(), var)?;
        reserve_solve_model_capacity(
            &mut state_names,
            names.len(),
            "state derivative name count",
            var.source_span,
        )?;
        state_names.extend(names);
    }
    let env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    let equation_count = dae_model.continuous.equations.len();
    let span = dae_model.continuous.equations[0].span;
    let mut used = reserve_state_derivative_order_flags(equation_count, span)?;
    let mut ordered = Vec::new();
    reserve_state_derivative_order_capacity(&mut ordered, equation_count, span)?;

    for state_name in state_names.iter().take(state_count) {
        let Some((row_idx, _)) = dae_model
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(idx, _)| !used[*idx])
            .filter_map(|(idx, equation)| {
                derivative_coefficient_expr(&equation.rhs, state_name, equation.span)
                    .ok()
                    .and_then(|expr| eval_expr::<f64>(&expr, &env).ok().map(|value| (idx, value)))
            })
            .find(|(_, coeff)| coeff.abs() > 1.0e-15)
        else {
            continue;
        };
        used[row_idx] = true;
        ordered.push(dae_model.continuous.equations[row_idx].clone());
    }

    if ordered.len() != state_count {
        return Ok(());
    }
    ordered.extend(
        dae_model
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(idx, _)| !used[*idx])
            .map(|(_, equation)| equation.clone()),
    );
    dae_model.continuous.equations = ordered;
    Ok(())
}

pub(super) fn reserve_state_derivative_order_flags(
    count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<bool>, SolveModelLowerError> {
    let mut used = Vec::new();
    reserve_state_derivative_order_capacity(&mut used, count, span)?;
    used.resize(count, false);
    Ok(used)
}

pub(super) fn reserve_state_derivative_order_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        solve_model_contract_violation(
            "state derivative row ordering capacity overflows".to_string(),
            span,
        )
    })
}
