use std::collections::HashSet;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;

const ALGEBRAIC_PROJECTION_MAX_ITERS: usize = 32;

pub trait AlgebraicProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn initial_residual_len(&self) -> usize;
    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot>;
    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot>;
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan;
    fn has_explicit_initial_targets(&self) -> bool;
    fn target_name_for_row(&self, row_idx: usize) -> Option<&str>;

    fn eval_implicit_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }

    fn eval_initial_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }

    /// Evaluate one logical initialization residual without evaluating the
    /// entire initialization block. Models may return `None` when the row
    /// cannot be isolated safely.
    fn eval_initial_residual_row(
        &self,
        _row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }
}

pub fn implicit_residual_is_zero_through_interval<M: AlgebraicProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t_start: f64,
    t_end: f64,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    if t_end <= t_start {
        return Ok(true);
    }
    let midpoint = t_start + 0.5 * (t_end - t_start);
    for t in [t_start, midpoint, t_end] {
        if !implicit_residual_is_zero(model, y, p, t, tol)? {
            return Ok(false);
        }
    }
    Ok(true)
}

pub fn implicit_residual_is_zero<M: AlgebraicProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let mut rhs = vec![0.0; y.len()];
    model.eval_residual(y, p, t, &mut rhs)?;
    Ok(rhs.iter().all(|value| value.abs() <= tol))
}

pub fn project_algebraics<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let algebraic_count = algebraic_tail_len(y.len(), state_count, "project algebraics")?;
    if algebraic_count == 0 {
        return Ok(());
    }
    project_algebraics_with_plan(
        model,
        model.algebraic_projection_plan(),
        y,
        p,
        t,
        state_count,
        tol,
    )
}

pub fn project_algebraics_and_detect_changes<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let before = y.to_vec();
    project_algebraics(model, y, p, t, state_count, tol)?;
    Ok(before
        .iter()
        .zip(y.iter())
        .any(|(old, new)| (old - new).abs() > tol))
}

fn project_algebraics_with_plan<M: AlgebraicProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let mut rhs = vec![0.0; y.len()];
    let mut last_residual = Vec::new();
    for _ in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        seed_nonfinite_algebraics(y, state_count);
        model.eval_residual(y, p, t, &mut rhs)?;
        let residual = projection_residual_tail(&rhs, plan, state_count)?;
        if residual_converged(&residual, tol) {
            return Ok(());
        }
        last_residual = residual.clone();
        let mut changed = false;
        let mut settled = true;
        for block in &plan.blocks {
            let update = project_algebraic_block(model, y, p, t, block, tol)?;
            changed |= update.changed;
            settled &= update.settled;
        }
        if !changed {
            if settled {
                return Ok(());
            }
            break;
        }
    }
    Err(projection_error(
        model,
        state_count,
        "algebraic projection did not converge at event boundary",
        &last_residual,
    ))
}

fn projection_residual_tail(
    rhs: &[f64],
    plan: &solve::AlgebraicProjectionPlan,
    state_count: usize,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut residual =
        vec![0.0; algebraic_tail_len(rhs.len(), state_count, "projection residual tail")?];
    for row in plan.blocks.iter().flat_map(|block| {
        block
            .rows
            .iter()
            .copied()
            .chain(block.causal_steps.iter().map(|step| step.row))
    }) {
        if row < state_count {
            continue;
        }
        let value = residual_at(rhs, row, "projection residual tail")?;
        if let Some(slot) = residual.get_mut(row - state_count) {
            *slot = value;
        }
    }
    Ok(residual)
}

fn algebraic_tail_len(
    total: usize,
    state_count: usize,
    context: &'static str,
) -> Result<usize, RuntimeSolveError> {
    total.checked_sub(state_count).ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "{context} state count {state_count} exceeds vector length {total}"
        ))
    })
}

fn project_algebraic_block<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let mut changed = apply_causal_steps(model, y, p, t, &block.causal_steps, tol)?;
    if block.rows.is_empty() || block.y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    if changed {
        changed |= apply_causal_steps(model, y, p, t, &block.causal_steps, tol)?;
    }
    if block.rows.is_empty() || block.y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    let mut rhs = vec![0.0; y.len()];
    model.eval_residual(y, p, t, &mut rhs)?;
    let residual = block
        .rows
        .iter()
        .map(|row| residual_at(&rhs, *row, "algebraic projection block"))
        .collect::<Result<Vec<_>, _>>()?;
    if residual_converged(&residual, tol) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }
    if !residual.iter().all(|value| value.is_finite()) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    }
    let jacobian = algebraic_block_jacobian(model, y, p, t, &block.rows, &block.y_indices)?;
    let rhs = DVector::from_vec(residual.into_iter().map(|value| -value).collect());
    let delta = if jacobian.nrows() == jacobian.ncols() {
        jacobian.clone().lu().solve(&rhs)
    } else {
        None
    }
    .or_else(|| jacobian.svd(true, true).solve(&rhs, tol).ok());
    let Some(delta) = delta else {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    };

    let mut applied_delta = false;
    for (y_idx, value) in block.y_indices.iter().copied().zip(delta.iter().copied()) {
        if !value.is_finite() {
            return Ok(ProjectionBlockUpdate {
                changed,
                settled: false,
            });
        }
        if value.abs() <= tol {
            continue;
        }
        if let Some(slot) = y.get_mut(y_idx) {
            *slot += value;
            changed = true;
            applied_delta = true;
        }
    }
    if !applied_delta {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }
    if changed {
        changed |= apply_causal_steps(model, y, p, t, &block.causal_steps, tol)?;
    }
    Ok(ProjectionBlockUpdate {
        changed,
        settled: false,
    })
}

#[derive(Debug, Clone, Copy)]
struct ProjectionBlockUpdate {
    changed: bool,
    settled: bool,
}

fn apply_causal_steps<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    steps: &[solve::AlgebraicProjectionStep],
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let mut changed = false;
    for step in steps {
        changed |= project_causal_step(model, y, p, t, step, tol)?;
    }
    Ok(changed)
}

fn project_causal_step<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    step: &solve::AlgebraicProjectionStep,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    if step.y_index >= y.len() {
        return Err(RuntimeSolveError::solve_ir(format!(
            "causal projection step references y index {}, but the model has only {} variables",
            step.y_index,
            y.len()
        )));
    }
    let mut rhs = vec![0.0; y.len()];
    let mut seed = vec![0.0; y.len()];
    let mut jv = vec![0.0; y.len()];
    let mut changed = false;
    for _ in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        model.eval_residual(y, p, t, &mut rhs)?;
        let residual = residual_at(&rhs, step.row, "causal projection step")?;
        if !residual.is_finite() {
            return Ok(changed);
        }
        if residual.abs() <= tol {
            return Ok(changed);
        }
        seed[step.y_index] = 1.0;
        model.eval_jacobian_v(y, p, t, &seed, &mut jv)?;
        seed[step.y_index] = 0.0;
        let derivative = residual_at(&jv, step.row, "causal projection jacobian-vector product")?;
        if !derivative.is_finite() || derivative.abs() <= 1.0e-15 {
            return Ok(changed);
        }
        let delta = -residual / derivative;
        if !delta.is_finite() || delta.abs() <= tol {
            return Ok(changed);
        }
        y[step.y_index] += delta;
        changed = true;
    }
    Ok(changed)
}

pub fn project_initial_algebraics<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let projection_indices = (state_count..y.len()).collect::<Vec<_>>();
    project_initial_variables(model, y, p, t, &projection_indices, tol)
}

pub fn project_initial_variables<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    projection_indices: &[usize],
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    project_initial_variables_with_plan(
        model,
        y,
        p,
        t,
        projection_indices,
        &solve::AlgebraicProjectionPlan::default(),
        tol,
    )
}

pub fn project_initial_variables_with_plan<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    projection_indices: &[usize],
    plan: &solve::AlgebraicProjectionPlan,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    if model.initial_residual_len() == 0 {
        return Ok(());
    }
    if !plan.is_empty() {
        return project_initial_variables_by_plan(model, y, p, t, plan, tol);
    }
    if projection_indices.is_empty() {
        return Ok(());
    }
    let mut last_selected_residual = Vec::new();
    let mut last_selected_rows = Vec::new();
    for _ in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        seed_nonfinite_projection_values(y, projection_indices);
        let mut residual = vec![0.0; model.initial_residual_len()];
        model.eval_initial_residual(y, p, t, &mut residual)?;
        let jacobian = initial_projection_jacobian(model, y, p, t, projection_indices, &residual)?;
        let selected = projectable_initial_rows(model, &residual, &jacobian, projection_indices);
        if selected.is_empty() {
            return Ok(());
        }
        let selected_residual =
            DVector::from_iterator(selected.len(), selected.iter().map(|row| residual[*row]));
        if residual_converged(selected_residual.as_slice(), tol) {
            return Ok(());
        }
        last_selected_residual = selected_residual.as_slice().to_vec();
        last_selected_rows.clone_from(&selected);
        let initial_projection = InitialVariableProjection {
            model,
            p,
            t,
            tol,
            selected_rows: &selected,
            residual: &residual,
            jacobian: &jacobian,
            projection_indices,
        };
        if relax_initial_variables_from_row_targets(&initial_projection, y)? {
            continue;
        }
        let selected_jacobian =
            DMatrix::from_fn(selected.len(), projection_indices.len(), |row, col| {
                jacobian[(selected[row], col)]
            });
        let solve_rhs = -selected_residual;
        let delta = if selected_jacobian.nrows() == selected_jacobian.ncols() {
            selected_jacobian.clone().lu().solve(&solve_rhs)
        } else {
            None
        }
        .or_else(|| {
            selected_jacobian
                .svd(true, true)
                .solve(&solve_rhs, tol)
                .ok()
        });
        let Some(delta) = delta else {
            return Err(RuntimeSolveError::solve_ir(
                "failed to project initial variables",
            ));
        };
        for (idx, value) in delta.iter().enumerate() {
            y[projection_indices[idx]] += value;
        }
    }
    Err(initial_projection_error(
        "initial variable projection did not converge",
        &last_selected_rows,
        &last_selected_residual,
    ))
}

fn seed_nonfinite_algebraics(y: &mut [f64], state_count: usize) {
    for value in &mut y[state_count..] {
        if !value.is_finite() {
            *value = 0.0;
        }
    }
}

fn projection_error<M: AlgebraicProjectionModel>(
    model: &M,
    state_count: usize,
    message: &str,
    residual: &[f64],
) -> RuntimeSolveError {
    let worst = residual
        .iter()
        .copied()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| residual_sort_key(*lhs).total_cmp(&residual_sort_key(*rhs)));
    match worst {
        Some((row, value)) => {
            let target = model
                .target_name_for_row(state_count + row)
                .map_or(String::new(), |name| format!(" target={name}"));
            RuntimeSolveError::solve_ir(format!(
                "{message}: max residual row={row}{target} value={value:.6e} norm={:.6e}",
                residual_norm(residual)
            ))
        }
        None => RuntimeSolveError::solve_ir(message),
    }
}

fn project_initial_variables_by_plan<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let mut residual = vec![0.0; model.initial_residual_len()];
    let mut last_residual = Vec::new();
    let projection_indices = initial_plan_projection_indices(plan);
    for _ in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        seed_nonfinite_projection_values(y, &projection_indices);
        model.eval_initial_residual(y, p, t, &mut residual)?;
        let selected = initial_plan_residual(&residual, plan)?;
        if residual_converged(&selected, tol) {
            return Ok(());
        }
        last_residual = selected;
        let mut changed = false;
        let mut settled = true;
        for block in &plan.blocks {
            let update = project_initial_block(model, y, p, t, block, tol)?;
            changed |= update.changed;
            settled &= update.settled;
        }
        if settled {
            return Ok(());
        }
        if !changed {
            break;
        }
    }
    Err(initial_projection_error(
        "initial variable projection plan did not converge",
        &initial_plan_rows(plan),
        &last_residual,
    ))
}

fn initial_plan_projection_indices(plan: &solve::AlgebraicProjectionPlan) -> Vec<usize> {
    let mut indices = plan
        .blocks
        .iter()
        .flat_map(|block| {
            block
                .y_indices
                .iter()
                .copied()
                .chain(block.causal_steps.iter().map(|step| step.y_index))
        })
        .collect::<Vec<_>>();
    indices.sort_unstable();
    indices.dedup();
    indices
}

fn initial_plan_residual(
    residual: &[f64],
    plan: &solve::AlgebraicProjectionPlan,
) -> Result<Vec<f64>, RuntimeSolveError> {
    initial_plan_rows(plan)
        .into_iter()
        .map(|row| initial_residual_at(residual, row, "algebraic projection plan"))
        .collect()
}

fn initial_residual_at(
    residual: &[f64],
    row: usize,
    context: &str,
) -> Result<f64, RuntimeSolveError> {
    residual.get(row).copied().ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "{context} references residual row {row}, but the model has only {} initial residual rows",
            residual.len()
        ))
    })
}

fn initial_plan_rows(plan: &solve::AlgebraicProjectionPlan) -> Vec<usize> {
    plan.blocks
        .iter()
        .flat_map(|block| {
            block
                .rows
                .iter()
                .copied()
                .chain(block.causal_steps.iter().map(|step| step.row))
        })
        .collect()
}

fn project_initial_block<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let (mut changed, unresolved_steps) =
        apply_initial_causal_steps(model, y, p, t, &block.causal_steps, tol)?;
    let mut rows = block.rows.clone();
    rows.extend(unresolved_steps.iter().map(|step| step.row));
    rows.sort_unstable();
    rows.dedup();
    let mut y_indices = block.y_indices.clone();
    y_indices.extend(unresolved_steps.iter().map(|step| step.y_index));
    y_indices.sort_unstable();
    y_indices.dedup();
    if rows.is_empty() || y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    if let Some(update) = project_initial_singleton_assignment(
        InitialBlockDeltaCtx {
            model,
            p,
            t,
            rows: &rows,
            y_indices: &y_indices,
            tol,
        },
        y,
        changed,
    )? {
        return Ok(update);
    }
    let mut residual = vec![0.0; model.initial_residual_len()];
    model.eval_initial_residual(y, p, t, &mut residual)?;
    let selected = rows
        .iter()
        .map(|row| initial_residual_at(&residual, *row, "algebraic projection block"))
        .collect::<Result<Vec<_>, _>>()?;
    if residual_converged(&selected, tol) || !selected.iter().all(|value| value.is_finite()) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: selected.iter().all(|value| value.is_finite()),
        });
    }
    let delta_ctx = InitialBlockDeltaCtx {
        model,
        p,
        t,
        rows: &rows,
        y_indices: &y_indices,
        tol,
    };
    if let Some(update) =
        project_initial_full_residual_singleton_assignment(&delta_ctx, y, &selected, changed)?
    {
        return Ok(update);
    }
    let jacobian = initial_block_jacobian(model, y, p, t, &rows, &y_indices, &residual)?;
    if rows.len() == 1 && relax_initial_block_from_row_targets(delta_ctx, y, &selected, &jacobian)?
    {
        return Ok(ProjectionBlockUpdate {
            changed: true,
            settled: false,
        });
    }
    let solve_rhs = DVector::from_vec(selected.into_iter().map(|value| -value).collect());
    let delta = if jacobian.nrows() == jacobian.ncols() {
        jacobian.clone().lu().solve(&solve_rhs)
    } else {
        None
    }
    .or_else(|| jacobian.svd(true, true).solve(&solve_rhs, tol).ok());
    let Some(delta) = delta else {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    };
    let update = accept_initial_block_delta(
        InitialBlockDeltaCtx {
            model,
            p,
            t,
            rows: &rows,
            y_indices: &y_indices,
            tol,
        },
        y,
        delta.as_slice(),
    )?;
    changed |= update.changed;
    Ok(ProjectionBlockUpdate {
        changed,
        settled: update.settled,
    })
}

fn project_initial_singleton_assignment<M: AlgebraicProjectionModel>(
    ctx: InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    changed: bool,
) -> Result<Option<ProjectionBlockUpdate>, RuntimeSolveError> {
    let ([row], [y_index]) = (ctx.rows, ctx.y_indices) else {
        return Ok(None);
    };
    let Some(before) = ctx.model.eval_initial_residual_row(*row, y, ctx.p, ctx.t)? else {
        return Ok(None);
    };
    if before.abs() <= ctx.tol || !before.is_finite() {
        return Ok(Some(ProjectionBlockUpdate {
            changed,
            settled: before.is_finite(),
        }));
    }
    let Some(value) = ctx
        .model
        .eval_initial_target_value(*row, *y_index, y, ctx.p, ctx.t)?
    else {
        return Ok(None);
    };
    if !value.is_finite() {
        return Ok(None);
    }
    let previous = y[*y_index];
    y[*y_index] = value;
    let after = ctx.model.eval_initial_residual_row(*row, y, ctx.p, ctx.t)?;
    if let Some(after) =
        after.filter(|after| after.is_finite() && after.abs() + ctx.tol < before.abs())
    {
        return Ok(Some(ProjectionBlockUpdate {
            changed: changed || (previous - value).abs() > ctx.tol,
            settled: after.abs() <= ctx.tol,
        }));
    }
    y[*y_index] = previous;
    Ok(None)
}

fn project_initial_full_residual_singleton_assignment<M: AlgebraicProjectionModel>(
    ctx: &InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    selected: &[f64],
    changed: bool,
) -> Result<Option<ProjectionBlockUpdate>, RuntimeSolveError> {
    let ([row], [y_index], [before]) = (ctx.rows, ctx.y_indices, selected) else {
        return Ok(None);
    };
    let Some(value) = ctx
        .model
        .eval_initial_target_value(*row, *y_index, y, ctx.p, ctx.t)?
    else {
        return Ok(None);
    };
    if !value.is_finite() {
        return Ok(None);
    }
    let previous = y[*y_index];
    y[*y_index] = value;
    let mut residual_after = vec![0.0; ctx.model.initial_residual_len()];
    ctx.model
        .eval_initial_residual(y, ctx.p, ctx.t, &mut residual_after)?;
    let after = initial_residual_at(
        &residual_after,
        *row,
        "initial singleton assignment validation",
    )?;
    if after.is_finite() && after.abs() + ctx.tol < before.abs() {
        return Ok(Some(ProjectionBlockUpdate {
            changed: changed || (previous - value).abs() > ctx.tol,
            settled: after.abs() <= ctx.tol,
        }));
    }
    y[*y_index] = previous;
    Ok(None)
}

fn apply_initial_causal_steps<'a, M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    steps: &'a [solve::AlgebraicProjectionStep],
    tol: f64,
) -> Result<(bool, Vec<&'a solve::AlgebraicProjectionStep>), RuntimeSolveError> {
    let mut changed = false;
    let mut unresolved = Vec::new();
    for step in steps {
        if step.y_index >= y.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "initial causal projection step references y index {}, but the model has only {} variables",
                step.y_index,
                y.len()
            )));
        }
        match model.eval_initial_target_value(step.row, step.y_index, y, p, t)? {
            Some(value) if value.is_finite() => {
                if (y[step.y_index] - value).abs() > tol {
                    y[step.y_index] = value;
                    changed = true;
                }
            }
            Some(_) => unresolved.push(step),
            None => unresolved.push(step),
        }
    }
    Ok((changed, unresolved))
}

#[derive(Clone, Copy)]
struct InitialBlockDeltaCtx<'a, M: AlgebraicProjectionModel> {
    model: &'a M,
    p: &'a [f64],
    t: f64,
    rows: &'a [usize],
    y_indices: &'a [usize],
    tol: f64,
}

fn accept_initial_block_delta<M: AlgebraicProjectionModel>(
    ctx: InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    delta: &[f64],
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let snapshot = y.to_vec();
    let before = initial_selected_residual_norm(ctx.model, y, ctx.p, ctx.t, ctx.rows)?;
    let mut alpha = 1.0;
    for _ in 0..12 {
        y.copy_from_slice(&snapshot);
        let mut changed = false;
        for (y_idx, value) in ctx.y_indices.iter().copied().zip(delta.iter().copied()) {
            let step = alpha * value;
            if !step.is_finite() {
                y.copy_from_slice(&snapshot);
                return Ok(ProjectionBlockUpdate {
                    changed: false,
                    settled: false,
                });
            }
            if step.abs() <= ctx.tol {
                continue;
            }
            if let Some(slot) = y.get_mut(y_idx) {
                *slot += step;
                changed = true;
            }
        }
        if !changed {
            y.copy_from_slice(&snapshot);
            return Ok(ProjectionBlockUpdate {
                changed: false,
                settled: true,
            });
        }
        let after = initial_selected_residual_norm(ctx.model, y, ctx.p, ctx.t, ctx.rows)?;
        if after.is_finite() && after + ctx.tol < before {
            return Ok(ProjectionBlockUpdate {
                changed: true,
                settled: false,
            });
        }
        alpha *= 0.5;
    }
    y.copy_from_slice(&snapshot);
    Ok(ProjectionBlockUpdate {
        changed: false,
        settled: false,
    })
}

fn relax_initial_block_from_row_targets<M: AlgebraicProjectionModel>(
    ctx: InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    residual: &[f64],
    jacobian: &DMatrix<f64>,
) -> Result<bool, RuntimeSolveError> {
    let snapshot = y.to_vec();
    let mut updated_rows = Vec::new();
    let mut used_columns = HashSet::new();
    for (row_pos, row) in ctx.rows.iter().copied().enumerate() {
        let Some(residual_value) = residual.get(row_pos).copied() else {
            continue;
        };
        if !residual_value.is_finite() {
            continue;
        }
        let Some(column) = initial_projection_target_column(ctx.model, row, ctx.y_indices) else {
            continue;
        };
        if !used_columns.insert(column) {
            continue;
        }
        let derivative = jacobian[(row_pos, column)];
        if !derivative.is_finite() || derivative.abs() <= 1.0e-15 {
            continue;
        }
        let delta = -residual_value / derivative;
        if !delta.is_finite() || delta.abs() <= ctx.tol {
            continue;
        }
        y[ctx.y_indices[column]] += delta;
        updated_rows.push((row, residual_value.abs()));
    }

    if updated_rows.is_empty() {
        return Ok(false);
    }

    let mut residual_after = vec![0.0; ctx.model.initial_residual_len()];
    ctx.model
        .eval_initial_residual(y, ctx.p, ctx.t, &mut residual_after)?;
    let target_rows_improved = updated_rows.iter().all(|(row, before)| {
        residual_after
            .get(*row)
            .copied()
            .is_some_and(|after| after.is_finite() && after.abs() + ctx.tol < *before)
    });
    if target_rows_improved {
        Ok(true)
    } else {
        y.copy_from_slice(&snapshot);
        Ok(false)
    }
}

fn initial_selected_residual_norm<M: AlgebraicProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
) -> Result<f64, RuntimeSolveError> {
    let mut residual = vec![0.0; model.initial_residual_len()];
    model.eval_initial_residual(y, p, t, &mut residual)?;
    let mut norm = 0.0;
    for row in rows {
        let value = initial_residual_at(&residual, *row, "selected initial projection rows")?;
        if !value.is_finite() {
            return Ok(f64::INFINITY);
        }
        norm = f64::max(norm, value.abs());
    }
    Ok(norm)
}

fn initial_projection_error(
    message: &str,
    selected_rows: &[usize],
    residual: &[f64],
) -> RuntimeSolveError {
    let worst = residual
        .iter()
        .copied()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| residual_sort_key(*lhs).total_cmp(&residual_sort_key(*rhs)));
    match worst {
        Some((row, value)) => {
            let original_row = selected_rows.get(row).copied().unwrap_or(row);
            RuntimeSolveError::solve_ir(format!(
                "{message}: max selected residual row={row} original_row={original_row} value={value:.6e} norm={:.6e}",
                residual_norm(residual)
            ))
        }
        None => RuntimeSolveError::solve_ir(message),
    }
}

fn residual_sort_key(value: f64) -> f64 {
    if value.is_finite() {
        value.abs()
    } else {
        f64::INFINITY
    }
}

fn residual_converged(residual: &[f64], tol: f64) -> bool {
    residual
        .iter()
        .all(|value| value.is_finite() && value.abs() <= tol)
}

fn residual_norm(residual: &[f64]) -> f64 {
    residual
        .iter()
        .copied()
        .map(f64::abs)
        .try_fold(0.0, |acc, value| {
            if value.is_finite() {
                Some(f64::max(acc, value))
            } else {
                None
            }
        })
        .unwrap_or(f64::INFINITY)
}

struct InitialVariableProjection<'a> {
    model: &'a dyn AlgebraicProjectionModel,
    p: &'a [f64],
    t: f64,
    tol: f64,
    selected_rows: &'a [usize],
    residual: &'a [f64],
    jacobian: &'a DMatrix<f64>,
    projection_indices: &'a [usize],
}

fn relax_initial_variables_from_row_targets(
    projection: &InitialVariableProjection<'_>,
    y: &mut [f64],
) -> Result<bool, RuntimeSolveError> {
    let snapshot = y.to_vec();
    let mut updated_rows = Vec::new();
    let mut used_columns = HashSet::new();
    for row in projection.selected_rows.iter().copied() {
        let Some(residual_value) = projection.residual.get(row).copied() else {
            continue;
        };
        if !residual_value.is_finite() {
            continue;
        }
        let Some(column) =
            initial_projection_target_column(projection.model, row, projection.projection_indices)
        else {
            continue;
        };
        if !used_columns.insert(column) {
            continue;
        }
        let derivative = projection.jacobian[(row, column)];
        if !derivative.is_finite() || derivative.abs() <= 1.0e-15 {
            continue;
        }
        let delta = -residual_value / derivative;
        if !delta.is_finite() || delta.abs() <= projection.tol {
            continue;
        }
        y[projection.projection_indices[column]] += delta;
        updated_rows.push((row, residual_value.abs()));
    }

    if updated_rows.is_empty() {
        return Ok(false);
    }

    let mut residual_after = vec![0.0; projection.model.initial_residual_len()];
    projection
        .model
        .eval_initial_residual(y, projection.p, projection.t, &mut residual_after)?;
    let target_rows_improved = updated_rows.iter().any(|(row, before)| {
        residual_after
            .get(*row)
            .copied()
            .is_some_and(|after| after.is_finite() && after.abs() + projection.tol < *before)
    });
    if target_rows_improved {
        Ok(true)
    } else {
        y.copy_from_slice(&snapshot);
        Ok(false)
    }
}

fn initial_projection_target_column(
    model: &dyn AlgebraicProjectionModel,
    row_idx: usize,
    projection_indices: &[usize],
) -> Option<usize> {
    let solve::ScalarSlot::Y { index, .. } = model.initial_target(row_idx)? else {
        return None;
    };
    projection_indices
        .iter()
        .position(|projection_index| *projection_index == index)
}

fn algebraic_block_jacobian(
    model: &dyn AlgebraicProjectionModel,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    y_indices: &[usize],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::<f64>::zeros(rows.len(), y_indices.len());
    let mut seed = vec![0.0; y.len()];
    let mut jv = vec![0.0; y.len()];
    for (col, y_idx) in y_indices.iter().copied().enumerate() {
        if y_idx >= seed.len() {
            continue;
        }
        seed[y_idx] = 1.0;
        model.eval_jacobian_v(y, p, t, &seed, &mut jv)?;
        for (row, residual_idx) in rows.iter().copied().enumerate() {
            jacobian[(row, col)] =
                residual_at(&jv, residual_idx, "algebraic block jacobian-vector product")?;
        }
        seed[y_idx] = 0.0;
    }
    Ok(jacobian)
}

fn initial_block_jacobian(
    model: &dyn AlgebraicProjectionModel,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    y_indices: &[usize],
    base_residual: &[f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::<f64>::zeros(rows.len(), y_indices.len());
    for (col, y_idx) in y_indices.iter().copied().enumerate() {
        if y_idx >= y.len() {
            continue;
        }
        let step = f64::EPSILON.sqrt() * y[y_idx].abs().max(1.0);
        let mut perturbed_y = y.to_vec();
        perturbed_y[y_idx] += step;
        let mut perturbed = vec![0.0; base_residual.len()];
        model.eval_initial_residual(&perturbed_y, p, t, &mut perturbed)?;
        for (row_idx, residual_idx) in rows.iter().copied().enumerate() {
            let base =
                initial_residual_at(base_residual, residual_idx, "initial block base residual")?;
            let value =
                initial_residual_at(&perturbed, residual_idx, "initial block perturbed residual")?;
            jacobian[(row_idx, col)] = (value - base) / step;
        }
    }
    Ok(jacobian)
}

fn projectable_initial_rows(
    model: &dyn AlgebraicProjectionModel,
    residual: &[f64],
    jacobian: &DMatrix<f64>,
    projection_indices: &[usize],
) -> Vec<usize> {
    let mut used_target_columns = HashSet::new();
    let mut rows = Vec::new();
    for row in 0..residual.len() {
        let Some(column) = initial_projection_target_column(model, row, projection_indices) else {
            continue;
        };
        if !used_target_columns.insert(column) || !row_is_projectable(residual, jacobian, row) {
            continue;
        }
        rows.push(row);
    }
    let has_explicit_targets = model.has_explicit_initial_targets();
    if rows.is_empty() && !has_explicit_targets && !projection_indices.is_empty() {
        let existing_rows = rows.iter().copied().collect::<HashSet<_>>();
        let fallback_rows = (0..residual.len())
            .filter(|row| !existing_rows.contains(row))
            .filter(|row| row_is_projectable(residual, jacobian, *row));
        rows.extend(fallback_rows);
    }
    rows
}

fn residual_at(residual: &[f64], row: usize, context: &str) -> Result<f64, RuntimeSolveError> {
    residual.get(row).copied().ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "{context} references residual row {row}, but the model evaluated only {} residual rows",
            residual.len()
        ))
    })
}

fn row_is_projectable(residual: &[f64], jacobian: &DMatrix<f64>, row: usize) -> bool {
    residual[row].is_finite()
        && (0..jacobian.ncols()).any(|column| {
            let value = jacobian[(row, column)];
            value.is_finite() && value.abs() > 1.0e-15
        })
}

fn initial_projection_jacobian(
    model: &dyn AlgebraicProjectionModel,
    y: &[f64],
    p: &[f64],
    t: f64,
    projection_indices: &[usize],
    base_residual: &[f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::<f64>::zeros(base_residual.len(), projection_indices.len());
    for (col, y_idx) in projection_indices.iter().copied().enumerate() {
        let step = f64::EPSILON.sqrt() * y[y_idx].abs().max(1.0);
        let mut perturbed_y = y.to_vec();
        perturbed_y[y_idx] += step;
        let mut perturbed = vec![0.0; base_residual.len()];
        model.eval_initial_residual(&perturbed_y, p, t, &mut perturbed)?;
        for row in 0..base_residual.len() {
            jacobian[(row, col)] = (perturbed[row] - base_residual[row]) / step;
        }
    }
    Ok(jacobian)
}

fn seed_nonfinite_projection_values(y: &mut [f64], projection_indices: &[usize]) {
    for idx in projection_indices.iter().copied() {
        if !y[idx].is_finite() {
            y[idx] = 0.0;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use super::*;

    struct BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan,
        initial_residual_len: usize,
    }

    impl AlgebraicProjectionModel for BlockProjectionModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] - 2.0;
            out[1] = y[1] - 3.0;
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out.copy_from_slice(v);
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            self.initial_residual_len
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            None
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            &self.plan
        }

        fn has_explicit_initial_targets(&self) -> bool {
            false
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            None
        }
    }

    struct RectInitialProjectionModel;

    struct InitialCausalAssignmentModel {
        initial_residual_calls: Cell<usize>,
        initial_residual_row_calls: Cell<usize>,
        plan: solve::AlgebraicProjectionPlan,
    }

    impl AlgebraicProjectionModel for InitialCausalAssignmentModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] - 5.0;
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.initial_residual_calls
                .set(self.initial_residual_calls.get() + 1);
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = v[0];
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            1
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            &self.plan
        }

        fn has_explicit_initial_targets(&self) -> bool {
            true
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            Some("x")
        }

        fn eval_initial_target_value(
            &self,
            _row_idx: usize,
            _target_y_index: usize,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
        ) -> Result<Option<f64>, RuntimeSolveError> {
            Ok(Some(5.0))
        }

        fn eval_initial_residual_row(
            &self,
            _row_idx: usize,
            y: &[f64],
            _p: &[f64],
            _t: f64,
        ) -> Result<Option<f64>, RuntimeSolveError> {
            self.initial_residual_row_calls
                .set(self.initial_residual_row_calls.get() + 1);
            Ok(Some(y[0] - 5.0))
        }
    }

    impl AlgebraicProjectionModel for RectInitialProjectionModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] - 2.0;
            out[1] = 2.0 * y[0] - 4.0;
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = v[0];
            out[1] = 2.0 * v[0];
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            2
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            None
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
                std::sync::OnceLock::new();
            PLAN.get_or_init(solve::AlgebraicProjectionPlan::default)
        }

        fn has_explicit_initial_targets(&self) -> bool {
            false
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            None
        }
    }

    struct TargetedInitialProjectionModel;

    impl AlgebraicProjectionModel for TargetedInitialProjectionModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] + y[1] - 2.0;
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = v[0] + v[1];
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            1
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(0))
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
                std::sync::OnceLock::new();
            PLAN.get_or_init(solve::AlgebraicProjectionPlan::default)
        }

        fn has_explicit_initial_targets(&self) -> bool {
            true
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            Some("target")
        }
    }

    struct CoupledTargetedInitialProjectionModel;

    impl AlgebraicProjectionModel for CoupledTargetedInitialProjectionModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] + y[1] - 1.0;
            out[1] = y[0] - y[1];
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = v[0] + v[1];
            out[1] = v[0] - v[1];
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            2
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
                std::sync::OnceLock::new();
            PLAN.get_or_init(|| solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1],
                    y_indices: vec![0, 1],
                    causal_steps: Vec::new(),
                }],
            })
        }

        fn has_explicit_initial_targets(&self) -> bool {
            true
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            Some("target")
        }
    }

    #[test]
    fn project_algebraics_uses_solve_projection_plan_blocks() {
        let model = BlockProjectionModel {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        causal_steps: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        causal_steps: Vec::new(),
                    },
                ],
            },
            initial_residual_len: 0,
        };
        let mut y = vec![0.0, 0.0];

        project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
            .expect("block projection should converge");

        assert_eq!(y, vec![2.0, 3.0]);
    }

    #[test]
    fn initial_causal_assignment_uses_target_value_without_dense_jacobian() {
        let model = InitialCausalAssignmentModel {
            initial_residual_calls: Cell::new(0),
            initial_residual_row_calls: Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: Vec::new(),
                    y_indices: Vec::new(),
                    causal_steps: vec![solve::AlgebraicProjectionStep { row: 0, y_index: 0 }],
                }],
            },
        };
        let mut y = vec![0.0];

        project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &[0], &model.plan, 1.0e-12)
            .expect("causal initial assignment should project");

        assert_eq!(y, vec![5.0]);
        assert_eq!(
            model.initial_residual_calls.get(),
            2,
            "the causal fast path needs only the before/after convergence checks"
        );
    }

    #[test]
    fn initial_singleton_assignment_validates_only_its_residual_row() {
        let model = InitialCausalAssignmentModel {
            initial_residual_calls: Cell::new(0),
            initial_residual_row_calls: Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    causal_steps: Vec::new(),
                }],
            },
        };
        let mut y = vec![0.0];

        project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &[0], &model.plan, 1.0e-12)
            .expect("singleton initial assignment should project");

        assert_eq!(y, vec![5.0]);
        assert_eq!(model.initial_residual_calls.get(), 1);
        assert_eq!(model.initial_residual_row_calls.get(), 2);
    }

    #[test]
    fn project_algebraics_rejects_state_count_past_y_length() {
        let model = BlockProjectionModel {
            plan: solve::AlgebraicProjectionPlan::default(),
            initial_residual_len: 0,
        };
        let mut y = vec![1.0];

        let err = project_algebraics(&model, &mut y, &[], 0.0, 2, 1.0e-12)
            .expect_err("state count beyond y length should fail");

        assert!(
            err.to_string()
                .contains("state count 2 exceeds vector length 1")
        );
    }

    #[test]
    fn projection_residual_tail_rejects_state_count_past_rhs_length() {
        let err = projection_residual_tail(&[0.0], &solve::AlgebraicProjectionPlan::default(), 2)
            .expect_err("state count beyond residual length should fail");

        assert!(
            err.to_string()
                .contains("state count 2 exceeds vector length 1")
        );
    }

    #[test]
    fn project_algebraic_block_uses_svd_for_rectangular_jacobian() {
        let model = BlockProjectionModel {
            plan: solve::AlgebraicProjectionPlan::default(),
            initial_residual_len: 0,
        };
        let block = solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0, 1],
            causal_steps: Vec::new(),
        };
        let mut y = vec![0.0, 0.0];

        let update = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
            .expect("rectangular projection should use least-squares solve without panicking");

        assert!(update.changed);
        assert!(!update.settled);
        assert!((y[0] - 2.0).abs() < 1.0e-9);
    }

    #[test]
    fn project_algebraic_block_rejects_row_outside_residual_vector() {
        let model = BlockProjectionModel {
            plan: solve::AlgebraicProjectionPlan::default(),
            initial_residual_len: 0,
        };
        let block = solve::AlgebraicProjectionBlock {
            rows: vec![2],
            y_indices: vec![0],
            causal_steps: Vec::new(),
        };
        let mut y = vec![0.0, 0.0];

        let err = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
            .expect_err("invalid projection row should bubble a runtime error");

        assert!(
            err.to_string()
                .contains("references residual row 2, but the model evaluated only 2")
        );
    }

    #[test]
    fn project_causal_step_rejects_row_outside_residual_vector() {
        let model = BlockProjectionModel {
            plan: solve::AlgebraicProjectionPlan::default(),
            initial_residual_len: 0,
        };
        let mut y = vec![0.0, 0.0];
        let step = solve::AlgebraicProjectionStep { row: 2, y_index: 0 };

        let err = project_causal_step(&model, &mut y, &[], 0.0, &step, 1.0e-12)
            .expect_err("invalid causal projection row should bubble a runtime error");

        assert!(
            err.to_string()
                .contains("references residual row 2, but the model evaluated only 2")
        );
    }

    #[test]
    fn project_algebraics_accepts_scaled_residual_with_sub_tolerance_correction() {
        let model = ScaledResidualProjectionModel;
        let mut y = vec![0.0];

        project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
            .expect("projection should settle when remaining correction is below tolerance");

        assert_eq!(y, vec![0.0]);
    }

    #[test]
    fn project_initial_variables_accepts_scaled_residual_with_sub_tolerance_correction() {
        let model = ScaledResidualProjectionModel;
        let mut y = vec![0.0];

        project_initial_variables_with_plan(
            &model,
            &mut y,
            &[],
            0.0,
            &[0],
            model.algebraic_projection_plan(),
            1.0e-6,
        )
        .expect("initial projection should settle when remaining correction is below tolerance");

        assert_eq!(y, vec![0.0]);
    }

    struct ScaledResidualProjectionModel;

    impl AlgebraicProjectionModel for ScaledResidualProjectionModel {
        fn eval_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = 1.0e3 * y[0] + 1.0e-4;
            Ok(())
        }

        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.eval_residual(y, p, t, out)
        }

        fn eval_jacobian_v(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = 1.0e3 * v[0];
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            1
        }

        fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(row_idx))
        }

        fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            None
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
                std::sync::OnceLock::new();
            PLAN.get_or_init(|| solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    causal_steps: Vec::new(),
                }],
            })
        }

        fn has_explicit_initial_targets(&self) -> bool {
            false
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            None
        }
    }

    #[test]
    fn project_initial_block_uses_svd_for_rectangular_jacobian() {
        let model = RectInitialProjectionModel;
        let block = solve::AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0],
            causal_steps: Vec::new(),
        };
        let mut y = vec![0.0, 0.0];

        let update = project_initial_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
            .expect("rectangular initial projection should use least-squares without panicking");

        assert!(update.changed);
        assert!(!update.settled);
        assert!((y[0] - 2.0).abs() < 1.0e-9);
    }

    #[test]
    fn project_initial_block_honors_row_targets_before_rectangular_solve() {
        let model = TargetedInitialProjectionModel;
        let block = solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0, 1],
            causal_steps: Vec::new(),
        };
        let mut y = vec![0.0, 0.0];

        let update = project_initial_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
            .expect("targeted row relaxation should be accepted before least-squares fallback");

        assert!(update.changed);
        assert!(!update.settled);
        assert!((y[0] - 2.0).abs() < 1.0e-9);
        assert_eq!(y[1], 0.0);
    }

    #[test]
    fn project_initial_variables_solves_coupled_targeted_block_as_block() {
        let model = CoupledTargetedInitialProjectionModel;
        let mut y = vec![0.0, 0.0];

        project_initial_variables_with_plan(
            &model,
            &mut y,
            &[],
            0.0,
            &[0, 1],
            model.algebraic_projection_plan(),
            1.0e-12,
        )
        .expect("coupled targeted block should use the coupled solve, not greedy row relaxation");

        assert!((y[0] - 0.5).abs() < 1.0e-9);
        assert!((y[1] - 0.5).abs() < 1.0e-9);
    }

    #[test]
    fn project_initial_variables_runs_plan_with_empty_projection_indices() {
        let model = CoupledTargetedInitialProjectionModel;
        let mut y = vec![0.0, 0.0];

        project_initial_variables_with_plan(
            &model,
            &mut y,
            &[],
            0.0,
            &[],
            model.algebraic_projection_plan(),
            1.0e-12,
        )
        .expect("non-empty plan should run even without projection indices");

        assert!((y[0] - 0.5).abs() < 1.0e-9);
        assert!((y[1] - 0.5).abs() < 1.0e-9);
    }

    #[test]
    fn project_initial_variables_rejects_plan_rows_outside_residual_vector() {
        let model = CoupledTargetedInitialProjectionModel;
        let plan = solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![2],
                y_indices: vec![0],
                causal_steps: Vec::new(),
            }],
        };
        let mut y = vec![0.0, 0.0];

        let err =
            project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &[], &plan, 1.0e-12)
                .expect_err("invalid plan row must not default to zero residual");

        assert!(
            err.to_string()
                .contains("references residual row 2, but the model has only 2")
        );
    }
}
