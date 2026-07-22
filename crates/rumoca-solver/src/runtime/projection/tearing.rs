use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::{
    AlgebraicProjectionArgs, ImplicitProjectionModel, ProjectionBlockUpdate, RuntimeSolveError,
    algebraic_selected_residual_norm, implicit_selected_jacobian_v_rows,
    implicit_selected_residuals, residual_converged,
};

pub(super) fn project_torn_algebraic_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<Option<ProjectionBlockUpdate>, RuntimeSolveError> {
    let Some(plan) = block.tearing.as_ref() else {
        return Ok(None);
    };
    if plan.residual_rows.is_empty()
        || plan.residual_rows.len() != plan.tear_y_indices.len()
        || plan.causal_steps.is_empty()
    {
        return Ok(None);
    }

    let original = y.to_vec();
    if !reconstruct_causal_values(model, plan, y, p, t)? {
        y.copy_from_slice(&original);
        return Ok(None);
    }
    let residual = implicit_selected_residuals(
        model,
        y,
        p,
        t,
        &plan.residual_rows,
        "torn algebraic projection",
    )?;
    if residual_converged(&residual, tol) {
        tracing::debug!(
            target: "rumoca_solver::projection",
            tear_unknowns = plan.tear_y_indices.len(),
            "torn block converged after causal reconstruction"
        );
        return Ok(Some(block_update(&original, y, block, tol, true)));
    }
    if !residual.iter().all(|value| value.is_finite()) {
        y.copy_from_slice(&original);
        return Ok(None);
    }

    let jacobian = reduced_jacobian(model, plan, y, p, t)?;
    let rhs = DVector::from_iterator(residual.len(), residual.iter().map(|value| -*value));
    let delta = jacobian
        .clone()
        .lu()
        .solve(&rhs)
        .or_else(|| jacobian.svd(true, true).solve(&rhs, tol).ok());
    let Some(delta) = delta else {
        y.copy_from_slice(&original);
        return Ok(None);
    };
    let baseline = y.to_vec();
    if !accept_reduced_delta(
        model,
        plan,
        y,
        ReducedDeltaContext {
            parameters: p,
            time: t,
            baseline: &baseline,
            delta: delta.as_slice(),
            tolerance: tol,
        },
    )? {
        y.copy_from_slice(&original);
        return Ok(None);
    }
    let settled = algebraic_selected_residual_norm(model, y, p, t, &plan.residual_rows)? <= tol;
    tracing::debug!(
        target: "rumoca_solver::projection",
        tear_unknowns = plan.tear_y_indices.len(),
        settled,
        "torn block accepted reduced Newton update"
    );
    Ok(Some(block_update(&original, y, block, tol, settled)))
}

pub(super) fn project_torn_algebraic_seed_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    args: AlgebraicProjectionArgs<'_>,
    block: &solve::AlgebraicProjectionBlock,
    seed: &mut [f64],
    unit_seed: &mut [f64],
) -> Result<bool, RuntimeSolveError> {
    let Some(plan) = block.tearing.as_ref() else {
        return Ok(false);
    };
    if plan.residual_rows.is_empty()
        || plan.residual_rows.len() != plan.tear_y_indices.len()
        || plan.causal_steps.is_empty()
    {
        return Ok(false);
    }
    for &index in &plan.tear_y_indices {
        seed[index] = 0.0;
    }
    propagate_causal_seed(model, plan, y, args.parameters, args.time, seed)?;
    let base = implicit_selected_jacobian_v_rows(
        model,
        y,
        args.parameters,
        args.time,
        seed,
        &plan.residual_rows,
        "torn algebraic seed residual",
    )?;
    let jacobian = reduced_seed_jacobian(model, plan, y, args.parameters, args.time, unit_seed)?;
    let rhs = DVector::from_iterator(base.len(), base.into_iter().map(|value| -value));
    let Some(solution) = jacobian.lu().solve(&rhs) else {
        return Ok(false);
    };
    for (&index, value) in plan.tear_y_indices.iter().zip(solution.iter().copied()) {
        if !value.is_finite() {
            return Ok(false);
        }
        seed[index] = value;
    }
    propagate_causal_seed(model, plan, y, args.parameters, args.time, seed)?;
    Ok(true)
}

fn reconstruct_causal_values<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicTearingPlan,
    y: &mut [f64],
    p: &[f64],
    t: f64,
) -> Result<bool, RuntimeSolveError> {
    if model.try_apply_implicit_causal_values(&plan.causal_steps, y, p, t)? {
        return Ok(true);
    }
    let mut unit_seed = vec![0.0; y.len()];
    for step in &plan.causal_steps {
        let value =
            match model.eval_implicit_target_value(step.row, step.target_y_index, y, p, t)? {
                Some(value) => value,
                None => {
                    let Some(value) = affine_causal_value(model, step, y, p, t, &mut unit_seed)?
                    else {
                        return Ok(false);
                    };
                    value
                }
            };
        if !value.is_finite() {
            return Ok(false);
        }
        y[step.target_y_index] = value;
    }
    Ok(true)
}

fn affine_causal_value<M: ImplicitProjectionModel>(
    model: &M,
    step: &solve::AlgebraicCausalStep,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    unit_seed: &mut [f64],
) -> Result<Option<f64>, RuntimeSolveError> {
    let previous = y[step.target_y_index];
    y[step.target_y_index] = 0.0;
    let result = (|| {
        let offset = implicit_selected_residuals(
            model,
            y,
            p,
            t,
            &[step.row],
            "affine causal tearing offset",
        )?[0];
        unit_seed.fill(0.0);
        unit_seed[step.target_y_index] = 1.0;
        let coefficient = implicit_selected_jacobian_v_rows(
            model,
            y,
            p,
            t,
            unit_seed,
            &[step.row],
            "affine causal tearing coefficient",
        )?[0];
        Ok(
            (coefficient.is_finite() && coefficient != 0.0 && offset.is_finite())
                .then_some(-offset / coefficient),
        )
    })();
    y[step.target_y_index] = previous;
    result
}

fn reduced_jacobian<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicTearingPlan,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let n = plan.tear_y_indices.len();
    let mut jacobian = DMatrix::zeros(n, n);
    let mut seed = vec![0.0; y.len()];
    for (column, &tear_y_index) in plan.tear_y_indices.iter().enumerate() {
        seed.fill(0.0);
        seed[tear_y_index] = 1.0;
        propagate_causal_seed(model, plan, y, p, t, &mut seed)?;
        let column_values = implicit_selected_jacobian_v_rows(
            model,
            y,
            p,
            t,
            &seed,
            &plan.residual_rows,
            "torn algebraic projection Jacobian",
        )?;
        for (row, value) in column_values.into_iter().enumerate() {
            jacobian[(row, column)] = value;
        }
    }
    Ok(jacobian)
}

fn reduced_seed_jacobian<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicTearingPlan,
    y: &[f64],
    p: &[f64],
    t: f64,
    unit_seed: &mut [f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let n = plan.tear_y_indices.len();
    let mut jacobian = DMatrix::zeros(n, n);
    for (column, &tear_y_index) in plan.tear_y_indices.iter().enumerate() {
        unit_seed.fill(0.0);
        unit_seed[tear_y_index] = 1.0;
        propagate_causal_seed(model, plan, y, p, t, unit_seed)?;
        let values = implicit_selected_jacobian_v_rows(
            model,
            y,
            p,
            t,
            unit_seed,
            &plan.residual_rows,
            "torn algebraic seed Jacobian",
        )?;
        for (row, value) in values.into_iter().enumerate() {
            jacobian[(row, column)] = value;
        }
    }
    unit_seed.fill(0.0);
    Ok(jacobian)
}

pub(super) fn propagate_causal_seed<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicTearingPlan,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    if model.try_propagate_implicit_causal_seed(&plan.causal_steps, y, p, t, seed)? {
        return Ok(());
    }
    let mut active_indices = seed
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(index, value)| (value != 0.0).then_some(index))
        .collect::<Vec<_>>();
    for step in &plan.causal_steps {
        active_indices.retain(|&index| index != step.target_y_index);
        seed[step.target_y_index] = 0.0;
        if active_indices
            .iter()
            .all(|&index| !model.implicit_jacobian_v_row_depends_on(step.row, index))
        {
            continue;
        }
        let offset = implicit_selected_jacobian_v_rows(
            model,
            y,
            p,
            t,
            seed,
            &[step.row],
            "causal tearing sensitivity offset",
        )?[0];
        let coefficient = match step.target_residual_coefficient {
            Some(coefficient) => coefficient,
            None => runtime_target_residual_coefficient(model, step, y, p, t, seed, offset)?,
        };
        if !coefficient.is_finite() || coefficient == 0.0 {
            return Err(RuntimeSolveError::solve_ir(format!(
                "causal tearing row {} has singular target coefficient for y[{}]",
                step.row, step.target_y_index
            )));
        }
        seed[step.target_y_index] = -offset / coefficient;
        if seed[step.target_y_index] != 0.0 {
            active_indices.push(step.target_y_index);
        }
    }
    Ok(())
}

fn runtime_target_residual_coefficient<M: ImplicitProjectionModel>(
    model: &M,
    step: &solve::AlgebraicCausalStep,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: &mut [f64],
    offset: f64,
) -> Result<f64, RuntimeSolveError> {
    seed[step.target_y_index] = 1.0;
    let unit = implicit_selected_jacobian_v_rows(
        model,
        y,
        p,
        t,
        seed,
        &[step.row],
        "causal tearing sensitivity coefficient",
    )?[0];
    Ok(unit - offset)
}

struct ReducedDeltaContext<'a> {
    parameters: &'a [f64],
    time: f64,
    baseline: &'a [f64],
    delta: &'a [f64],
    tolerance: f64,
}

fn accept_reduced_delta<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicTearingPlan,
    y: &mut [f64],
    context: ReducedDeltaContext<'_>,
) -> Result<bool, RuntimeSolveError> {
    let ReducedDeltaContext {
        parameters,
        time,
        baseline,
        delta,
        tolerance,
    } = context;
    let before =
        algebraic_selected_residual_norm(model, baseline, parameters, time, &plan.residual_rows)?;
    let mut alpha = 1.0;
    loop {
        y.copy_from_slice(baseline);
        for (&index, &step) in plan.tear_y_indices.iter().zip(delta) {
            y[index] += alpha * step;
        }
        if reconstruct_causal_values(model, plan, y, parameters, time)? {
            let after =
                algebraic_selected_residual_norm(model, y, parameters, time, &plan.residual_rows)?;
            if after.is_finite() && (after <= tolerance || after < before) {
                tracing::debug!(
                    target: "rumoca_solver::projection",
                    alpha,
                    before,
                    after,
                    "accepted torn reduced step"
                );
                return Ok(true);
            }
        }
        let next_alpha = alpha * 0.5;
        if next_alpha == 0.0 || next_alpha == alpha {
            break;
        }
        alpha = next_alpha;
    }
    y.copy_from_slice(baseline);
    Ok(false)
}

fn block_update(
    before: &[f64],
    after: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
    settled: bool,
) -> ProjectionBlockUpdate {
    let changed = block
        .y_indices
        .iter()
        .any(|&index| (before[index] - after[index]).abs() > tol);
    ProjectionBlockUpdate { changed, settled }
}
