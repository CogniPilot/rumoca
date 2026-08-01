use std::collections::HashSet;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;
use scaling::{
    algebraic_block_scales, algebraic_plan_row_scales, initial_block_fallback_scales,
    initial_residual_scales, jacobian_row_scales, model_variable_scale, scaled_newton_delta,
    scaled_residual_converged, scaled_residual_norm, scaled_tolerance,
};
use singleton::{SingletonAssignmentStep, initial_row_target_name, singleton_assignment_improves};
use step_limit::StepLimit;

mod homotopy;
mod manifold;
mod plan;
mod retry;
mod scaling;
mod singleton;
mod step_limit;

pub use manifold::{ManifoldProjectionModel, project_state_manifold};

use plan::{
    algebraic_tail_len, require_square_projection_block, validate_algebraic_projection_plan,
    validate_initial_projection_plan,
};

const ALGEBRAIC_PROJECTION_MAX_ITERS: usize = 32;

#[derive(Clone, Copy)]
pub struct AlgebraicProjectionArgs<'a> {
    pub parameters: &'a [f64],
    pub time: f64,
    pub state_count: usize,
    pub tolerance: f64,
}

pub trait ImplicitProjectionModel {
    fn eval_residual(
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

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot>;
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan;
    fn target_name_for_row(&self, row_idx: usize) -> Option<&str>;

    /// Return the diagnostic name for a solver variable. Implementations may
    /// omit names without changing projection semantics.
    fn variable_name_for_y_index(&self, _y_index: usize) -> Option<&str> {
        None
    }

    /// Return a finite positive characteristic scale for one solver variable.
    ///
    /// Implementations backed by Solve IR should combine the declared
    /// `nominal` attribute with the variable's start magnitude. The default
    /// preserves unit scaling for third-party projection models.
    fn variable_scale_for_y_index(&self, _y_index: usize) -> f64 {
        1.0
    }

    /// Evaluate one logical implicit residual without evaluating the complete
    /// residual block. Models may return `None` when the row has no scalar view.
    fn eval_implicit_residual_row(
        &self,
        _row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }

    /// Evaluate one logical implicit Jacobian-vector product row without
    /// evaluating the complete JVP block. Models may return `None` when the
    /// row has no scalar view.
    fn eval_implicit_jacobian_v_row(
        &self,
        _row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }

    /// Evaluate the complete gradient of one scalar implicit residual with
    /// respect to solver `y`. Returning `false` keeps the exact forward-JVP
    /// construction available for models without reverse-row support.
    fn eval_implicit_jacobian_row(
        &self,
        _row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        Ok(false)
    }

    /// Report exact structural dependence of one residual JVP row on a seed
    /// column. The conservative default keeps third-party models correct.
    fn implicit_jacobian_v_row_depends_on(&self, _row_idx: usize, _seed_index: usize) -> bool {
        true
    }

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
}

pub trait AlgebraicProjectionModel: ImplicitProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn initial_residual_len(&self) -> usize;
    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot>;

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

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

pub fn implicit_residual_is_zero_through_interval<M: ImplicitProjectionModel>(
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

pub fn implicit_residual_is_zero<M: ImplicitProjectionModel>(
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

pub fn project_algebraics<M: ImplicitProjectionModel>(
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
        AlgebraicProjectionArgs {
            parameters: p,
            time: t,
            state_count,
            tolerance: tol,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
}

pub fn project_algebraics_and_detect_changes<M: ImplicitProjectionModel>(
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

pub fn project_algebraic_seed_with_plan<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &[f64],
    args: AlgebraicProjectionArgs<'_>,
    seed: &mut [f64],
    unit_seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    validate_algebraic_projection_plan(plan, args.state_count, y.len())?;
    if seed.len() < y.len() || unit_seed.len() < seed.len() {
        return Err(RuntimeSolveError::solve_ir(format!(
            "algebraic projection seed buffers have lengths {} and {}, but require at least {}",
            seed.len(),
            unit_seed.len(),
            y.len()
        )));
    }
    let snapshot = projection_unknown_values(plan, seed);
    let result = project_algebraic_seed_with_plan_inner(model, plan, y, args, seed, unit_seed);
    if result.is_err() {
        restore_projection_unknown_values(plan, seed, &snapshot);
    }
    result
}

fn project_algebraic_seed_with_plan_inner<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &[f64],
    args: AlgebraicProjectionArgs<'_>,
    seed: &mut [f64],
    unit_seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    for block in &plan.blocks {
        for &y_index in &block.y_indices {
            seed[y_index] = 0.0;
        }
    }
    for block in &plan.blocks {
        let block_residual = implicit_selected_jacobian_v_rows(
            model,
            y,
            args.parameters,
            args.time,
            seed,
            &block.rows,
            "algebraic seed projection",
        )?;
        let rhs = DVector::from_iterator(
            block.rows.len(),
            block_residual.into_iter().map(|value| -value),
        );
        let jacobian =
            algebraic_seed_block_jacobian(model, y, args.parameters, args.time, block, unit_seed)?;
        let Some(solution) = jacobian.lu().solve(&rhs) else {
            return Err(RuntimeSolveError::solve_ir(
                "algebraic projection sensitivity matrix is singular".to_string(),
            ));
        };
        for (y_index, value) in block
            .y_indices
            .iter()
            .copied()
            .zip(solution.iter().copied())
        {
            if !value.is_finite() {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "algebraic projection produced a non-finite sensitivity for y[{y_index}]"
                )));
            }
            seed[y_index] = value;
        }
    }
    let rows = projection_rows(plan);
    let residual = implicit_selected_jacobian_v_rows(
        model,
        y,
        args.parameters,
        args.time,
        seed,
        &rows,
        "algebraic projection sensitivity",
    )?;
    if residual_converged(&residual, args.tolerance) {
        return Ok(());
    }
    Err(projection_error_for_rows(
        model,
        "algebraic projection sensitivity did not satisfy the selected residual system",
        &rows,
        &residual,
    ))
}

fn algebraic_seed_block_jacobian<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    unit_seed: &mut [f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::zeros(block.rows.len(), block.y_indices.len());
    for (column, y_index) in block.y_indices.iter().copied().enumerate() {
        unit_seed.fill(0.0);
        unit_seed[y_index] = 1.0;
        for (row_pos, row) in block.rows.iter().copied().enumerate() {
            if !model.implicit_jacobian_v_row_depends_on(row, y_index) {
                continue;
            }
            let Some(value) = model.eval_implicit_jacobian_v_row(row, y, p, t, unit_seed)? else {
                let mut jvp = vec![0.0; y.len()];
                model.eval_jacobian_v(y, p, t, unit_seed, &mut jvp)?;
                fill_jacobian_column_from_jvp(
                    &mut jacobian,
                    column,
                    &block.rows,
                    &jvp,
                    None,
                    "algebraic seed projection Jacobian",
                )?;
                break;
            };
            jacobian[(row_pos, column)] = value;
        }
    }
    unit_seed.fill(0.0);
    Ok(jacobian)
}

pub fn project_algebraics_with_plan<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    validate_algebraic_projection_plan(plan, args.state_count, y.len())?;
    retry::project_with_step_limited_retry(model, plan, y, args, max_iters)
}

fn project_algebraics_with_plan_inner<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
    step_limit: StepLimit,
) -> Result<(), RuntimeSolveError> {
    let rows = projection_rows(plan);
    for iteration in 0..max_iters {
        seed_nonfinite_projection_unknowns(y, plan);
        let mut changed = false;
        let mut all_settled = true;
        for block in &plan.blocks {
            let update = project_algebraic_block(
                model,
                y,
                args.parameters,
                args.time,
                block,
                args.tolerance,
                step_limit,
            )?;
            changed |= update.changed;
            all_settled &= update.settled;
        }
        if all_settled {
            tracing::debug!(
                target: "rumoca_solver::projection",
                iteration,
                "algebraic projection converged"
            );
            return Ok(());
        }
        if !changed {
            tracing::debug!(
                target: "rumoca_solver::projection",
                iteration,
                "algebraic projection made no accepted update"
            );
            break;
        }
    }
    seed_nonfinite_projection_unknowns(y, plan);
    let residual = implicit_selected_residuals(
        model,
        y,
        args.parameters,
        args.time,
        &rows,
        "selected algebraic projection",
    )?;
    let row_scales = algebraic_plan_row_scales(model, y, args.parameters, args.time, plan)?;
    if scaled_residual_converged(&residual, &row_scales, args.tolerance) {
        return Ok(());
    }
    Err(projection_error_for_rows(
        model,
        "algebraic projection did not converge at event boundary",
        &rows,
        &residual,
    ))
}

fn project_algebraic_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
    step_limit: StepLimit,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    require_square_projection_block(block.rows.len(), block.y_indices.len(), "algebraic")?;
    let mut changed = false;
    if block.rows.is_empty() || block.y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    if let Some(update) = project_algebraic_singleton_assignment(model, y, p, t, block, tol)? {
        return Ok(update);
    }
    let mut residual =
        implicit_selected_residuals(model, y, p, t, &block.rows, "algebraic projection block")?;
    if !residual.iter().all(|value| value.is_finite()) {
        changed |= seed_algebraic_block_assignments(model, y, p, t, block, tol)?;
        residual = implicit_selected_residuals(
            model,
            y,
            p,
            t,
            &block.rows,
            "seeded algebraic projection block",
        )?;
    }
    if !residual.iter().all(|value| value.is_finite()) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    }
    let jacobian = algebraic_block_jacobian(model, y, p, t, &block.rows, &block.y_indices)?;
    let (row_scales, variable_scales) = algebraic_block_scales(model, block, &jacobian);
    if scaled_residual_converged(&residual, &row_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }
    let before_norm = scaled_residual_norm(&residual, &row_scales);
    let delta = scaled_newton_delta(&jacobian, &residual, &row_scales, &variable_scales, tol);
    let Some(delta) = delta else {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    };

    let update = accept_algebraic_block_delta(
        AlgebraicBlockDeltaContext {
            model,
            parameters: p,
            time: t,
            block,
            before: before_norm,
            tolerance: tol,
            row_scales: &row_scales,
            variable_scales: &variable_scales,
            step_limit,
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

fn seed_algebraic_block_assignments<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let mut seeded_rows = vec![false; block.rows.len()];
    let mut seeded_targets = vec![false; block.y_indices.len()];
    let mut changed = false;
    let context = AlgebraicSeedContext {
        model,
        parameters: p,
        time: t,
        y_indices: &block.y_indices,
        tolerance: tol,
    };
    for _ in 0..block.rows.len() {
        let mut seeded_in_pass = false;
        for (row_pos, row) in block.rows.iter().copied().enumerate() {
            if seeded_rows[row_pos] {
                continue;
            }
            let Some((target_pos, target_changed)) =
                seed_algebraic_row_assignment(&context, y, &seeded_targets, row)?
            else {
                continue;
            };
            changed |= target_changed;
            seeded_rows[row_pos] = true;
            seeded_targets[target_pos] = true;
            seeded_in_pass = true;
        }
        if !seeded_in_pass {
            break;
        }
    }
    Ok(changed)
}

struct AlgebraicSeedContext<'a, M> {
    model: &'a M,
    parameters: &'a [f64],
    time: f64,
    y_indices: &'a [usize],
    tolerance: f64,
}

fn seed_algebraic_row_assignment<M: ImplicitProjectionModel>(
    context: &AlgebraicSeedContext<'_, M>,
    y: &mut [f64],
    seeded_targets: &[bool],
    row: usize,
) -> Result<Option<(usize, bool)>, RuntimeSolveError> {
    for (target_pos, y_index) in context.y_indices.iter().copied().enumerate() {
        if seeded_targets[target_pos] {
            continue;
        }
        let Some(changed) = try_seed_algebraic_target(context, y, row, y_index)? else {
            continue;
        };
        return Ok(Some((target_pos, changed)));
    }
    Ok(None)
}

fn try_seed_algebraic_target<M: ImplicitProjectionModel>(
    context: &AlgebraicSeedContext<'_, M>,
    y: &mut [f64],
    row: usize,
    y_index: usize,
) -> Result<Option<bool>, RuntimeSolveError> {
    let Some(value) = context.model.eval_implicit_target_value(
        row,
        y_index,
        y,
        context.parameters,
        context.time,
    )?
    else {
        return Ok(None);
    };
    if !value.is_finite() {
        return Ok(None);
    }
    let previous = y[y_index];
    y[y_index] = value;
    let accepted = context
        .model
        .eval_implicit_residual_row(row, y, context.parameters, context.time)?
        .is_some_and(|after| {
            after.is_finite()
                && after.abs()
                    <= scaled_tolerance(
                        context.tolerance,
                        model_variable_scale(context.model, y_index),
                    )
        });
    if accepted {
        return Ok(Some(previous != value));
    }
    y[y_index] = previous;
    Ok(None)
}

fn project_algebraic_singleton_assignment<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<Option<ProjectionBlockUpdate>, RuntimeSolveError> {
    let ([row], [y_index]) = (block.rows.as_slice(), block.y_indices.as_slice()) else {
        return Ok(None);
    };
    let Some(before) = model.eval_implicit_residual_row(*row, y, p, t)? else {
        return Ok(None);
    };
    let scale = model_variable_scale(model, *y_index);
    let variable_tol = scaled_tolerance(tol, scale);
    if !before.is_finite() {
        return Ok(Some(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        }));
    }
    let Some(value) = model
        .eval_implicit_target_value(*row, *y_index, y, p, t)?
        .filter(|value| value.is_finite())
    else {
        return Ok(None);
    };
    let previous = y[*y_index];
    y[*y_index] = value;
    let after = model.eval_implicit_residual_row(*row, y, p, t)?;
    if let Some(after) =
        after.filter(|after| after.is_finite() && after.abs() + variable_tol < before.abs())
    {
        return Ok(Some(ProjectionBlockUpdate {
            changed: (previous - value).abs() > variable_tol,
            settled: after.abs() <= variable_tol,
        }));
    }
    y[*y_index] = previous;
    Ok(None)
}

struct AlgebraicBlockDeltaContext<'a, M> {
    model: &'a M,
    parameters: &'a [f64],
    time: f64,
    block: &'a solve::AlgebraicProjectionBlock,
    before: f64,
    tolerance: f64,
    row_scales: &'a [f64],
    variable_scales: &'a [f64],
    step_limit: StepLimit,
}

fn accept_algebraic_block_delta<M: ImplicitProjectionModel>(
    context: AlgebraicBlockDeltaContext<'_, M>,
    y: &mut [f64],
    delta: &[f64],
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let AlgebraicBlockDeltaContext {
        model,
        parameters,
        time,
        block,
        before,
        tolerance,
        row_scales,
        variable_scales,
        step_limit,
    } = context;
    let snapshot = y.to_vec();
    if !before.is_finite() {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    }
    let mut alpha = step_limit.initial_alpha(y, &block.y_indices, delta, variable_scales);
    loop {
        y.copy_from_slice(&snapshot);
        let mut changed = false;
        let mut step_at_resolution = true;
        for (y_idx, value) in block.y_indices.iter().copied().zip(delta.iter().copied()) {
            let step = alpha * value;
            if !step.is_finite() {
                y.copy_from_slice(&snapshot);
                return Ok(ProjectionBlockUpdate {
                    changed: false,
                    settled: false,
                });
            }
            let Some(slot) = y.get_mut(y_idx) else {
                y.copy_from_slice(&snapshot);
                return Err(RuntimeSolveError::solve_ir(format!(
                    "algebraic projection references y index {y_idx}, but the model has only {} variables",
                    snapshot.len()
                )));
            };
            let candidate = *slot + step;
            if !candidate.is_finite() {
                y.copy_from_slice(&snapshot);
                return Ok(ProjectionBlockUpdate {
                    changed: false,
                    settled: false,
                });
            }
            changed |= candidate != *slot;
            step_at_resolution &= algebraic_step_at_resolution(*slot, candidate);
            *slot = candidate;
        }
        if !changed {
            y.copy_from_slice(&snapshot);
            return Ok(ProjectionBlockUpdate {
                changed: false,
                settled: false,
            });
        }
        let after =
            algebraic_selected_residual_norm(model, y, parameters, time, &block.rows, row_scales)?;
        if after.is_finite() && (after <= tolerance || (!step_at_resolution && after < before)) {
            return Ok(ProjectionBlockUpdate {
                changed: true,
                settled: after <= tolerance,
            });
        }
        if step_at_resolution {
            break;
        }
        let next_alpha = alpha * 0.5;
        if next_alpha == 0.0 || next_alpha == alpha {
            break;
        }
        alpha = next_alpha;
    }
    y.copy_from_slice(&snapshot);
    Ok(ProjectionBlockUpdate {
        changed: false,
        settled: false,
    })
}

fn algebraic_step_at_resolution(current: f64, candidate: f64) -> bool {
    candidate == current || candidate == current.next_up() || candidate == current.next_down()
}

fn algebraic_selected_residual_norm<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    row_scales: &[f64],
) -> Result<f64, RuntimeSolveError> {
    let residual =
        implicit_selected_residuals(model, y, p, t, rows, "selected algebraic projection rows")?;
    Ok(scaled_residual_norm(&residual, row_scales))
}

fn implicit_selected_residuals<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    context: &str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut selected = Vec::with_capacity(rows.len());
    for row in rows {
        let Some(value) = model.eval_implicit_residual_row(*row, y, p, t)? else {
            let mut residual = vec![0.0; y.len()];
            model.eval_residual(y, p, t, &mut residual)?;
            return rows
                .iter()
                .map(|row| residual_at(&residual, *row, context))
                .collect();
        };
        selected.push(value);
    }
    Ok(selected)
}

fn implicit_selected_jacobian_v_rows<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    rows: &[usize],
    context: &str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut selected = Vec::with_capacity(rows.len());
    for row in rows {
        let Some(value) = model.eval_implicit_jacobian_v_row(*row, y, p, t, v)? else {
            let mut jvp = vec![0.0; y.len()];
            model.eval_jacobian_v(y, p, t, v, &mut jvp)?;
            return rows
                .iter()
                .map(|row| residual_at(&jvp, *row, context))
                .collect();
        };
        selected.push(value);
    }
    Ok(selected)
}

#[derive(Debug, Clone, Copy)]
struct ProjectionBlockUpdate {
    changed: bool,
    settled: bool,
}

struct CombinedInitializationProjectionModel<'a, M> {
    model: &'a M,
    y_len: usize,
    parameter_scales: Vec<f64>,
}

impl<M> CombinedInitializationProjectionModel<'_, M> {
    fn split_values<'a>(
        &self,
        values: &'a [f64],
    ) -> Result<(&'a [f64], &'a [f64]), RuntimeSolveError> {
        if values.len() < self.y_len {
            return Err(RuntimeSolveError::solve_ir(format!(
                "combined initialization vector has {} values, but Y requires {}",
                values.len(),
                self.y_len
            )));
        }
        Ok(values.split_at(self.y_len))
    }
}

impl<M: AlgebraicProjectionModel> ImplicitProjectionModel
    for CombinedInitializationProjectionModel<'_, M>
{
    fn eval_residual(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_residual(y, p, t, out)
    }

    fn eval_jacobian_v(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        let y_seed = v.get(..self.y_len).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "combined initialization seed has {} values, but Y requires {}",
                v.len(),
                self.y_len
            ))
        })?;
        self.model.eval_jacobian_v(y, p, t, y_seed, out)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.model.implicit_target(row_idx)
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        self.model.algebraic_projection_plan()
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        self.model.target_name_for_row(row_idx)
    }

    fn variable_name_for_y_index(&self, index: usize) -> Option<&str> {
        (index < self.y_len)
            .then(|| self.model.variable_name_for_y_index(index))
            .flatten()
    }

    fn variable_scale_for_y_index(&self, index: usize) -> f64 {
        if index < self.y_len {
            self.model.variable_scale_for_y_index(index)
        } else {
            self.parameter_scales
                .get(index - self.y_len)
                .copied()
                .unwrap_or(1.0)
        }
    }
}

impl<M: AlgebraicProjectionModel> AlgebraicProjectionModel
    for CombinedInitializationProjectionModel<'_, M>
{
    fn eval_initial_residual(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_residual(y, p, t, out)
    }

    fn initial_residual_len(&self) -> usize {
        self.model.initial_residual_len()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.model.initial_target(row_idx)
    }

    fn eval_initial_jacobian_v(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_jacobian_v(y, p, t, v, out)
    }

    fn eval_initial_target_value(
        &self,
        row_idx: usize,
        target_index: usize,
        values: &[f64],
        _p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if target_index >= self.y_len {
            return Ok(None);
        }
        let (y, p) = self.split_values(values)?;
        self.model
            .eval_initial_target_value(row_idx, target_index, y, p, t)
    }

    fn eval_initial_residual_row(
        &self,
        row_idx: usize,
        values: &[f64],
        _p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_residual_row(row_idx, y, p, t)
    }
}

pub fn project_initial_variables_with_plan<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    plan: &solve::InitializationProjectionPlan,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let combined_len = y.len().checked_add(p.len()).ok_or_else(|| {
        RuntimeSolveError::solve_ir(
            "combined initialization Y/P vector length exceeds host index range".to_string(),
        )
    })?;
    let combined_plan = combined_initial_projection_plan(plan, y.len(), p.len())?;
    validate_initial_projection_plan(&combined_plan, model.initial_residual_len(), combined_len)?;
    if model.initial_residual_len() == 0 {
        return Ok(());
    }
    let mut values = Vec::with_capacity(combined_len);
    values.extend_from_slice(y);
    values.extend_from_slice(p);
    let combined_model = CombinedInitializationProjectionModel {
        model,
        y_len: y.len(),
        parameter_scales: p
            .iter()
            .map(|value| {
                if value.is_finite() {
                    value.abs().max(1.0)
                } else {
                    1.0
                }
            })
            .collect(),
    };
    project_initial_variables_by_plan(&combined_model, &mut values, &[], t, &combined_plan, tol)?;
    let (projected_y, projected_p) = values.split_at(y.len());
    y.copy_from_slice(projected_y);
    p.copy_from_slice(projected_p);
    Ok(())
}

fn combined_initial_projection_plan(
    plan: &solve::InitializationProjectionPlan,
    y_len: usize,
    p_len: usize,
) -> Result<solve::AlgebraicProjectionPlan, RuntimeSolveError> {
    let mut blocks = Vec::with_capacity(plan.blocks.len());
    for block in &plan.blocks {
        let mut indices = Vec::with_capacity(block.unknowns.len());
        for unknown in &block.unknowns {
            let index = match *unknown {
                solve::ScalarSlot::Y { index, .. } if index < y_len => index,
                solve::ScalarSlot::P { index, .. } if index < p_len => {
                    combined_parameter_seed_index(y_len, index)?
                }
                _ => {
                    return Err(RuntimeSolveError::solve_ir(format!(
                        "initial projection contains invalid unknown {unknown:?} for \
                         Y/P lengths {y_len}/{p_len}"
                    )));
                }
            };
            indices.push(index);
        }
        blocks.push(solve::AlgebraicProjectionBlock {
            rows: block.rows.clone(),
            y_indices: indices,
        });
    }
    Ok(solve::AlgebraicProjectionPlan { blocks })
}

fn combined_parameter_seed_index(
    y_len: usize,
    parameter_index: usize,
) -> Result<usize, RuntimeSolveError> {
    y_len.checked_add(parameter_index).ok_or_else(|| {
        RuntimeSolveError::solve_ir(
            "initial projection P-slot seed index exceeds host index range".to_string(),
        )
    })
}

/// The initialization system a homotopy continuation sweeps.
///
/// `homotopy_parameter_index` is the hidden λ slot; `None` means the model
/// carries no `homotopy(...)` and the plan is projected once, as-is.
pub struct InitialHomotopySystem<'a, M> {
    pub model: &'a M,
    pub t: f64,
    pub plan: &'a solve::InitializationProjectionPlan,
    pub homotopy_parameter_index: Option<usize>,
    pub tol: f64,
}

/// Drive the initialization homotopy continuation.
///
/// `continuation_dependents` re-solves every system outside `system.plan` that
/// the sweep must carry (see `homotopy::project_initial_variables_with_homotopy`
/// for the acceptance contract). Callers with no such system pass a step that
/// does nothing, and must have proven that the plan alone owns every unknown the
/// continuation parameter reaches.
pub fn project_initial_variables_with_homotopy<M, F>(
    system: InitialHomotopySystem<'_, M>,
    y: &mut [f64],
    p: &mut [f64],
    continuation_dependents: F,
) -> Result<(), RuntimeSolveError>
where
    M: AlgebraicProjectionModel,
    F: FnMut(&mut [f64], &[f64]) -> Result<(), RuntimeSolveError>,
{
    homotopy::project_initial_variables_with_homotopy(system, y, p, continuation_dependents)
}

fn projection_rows(plan: &solve::AlgebraicProjectionPlan) -> Vec<usize> {
    plan.blocks
        .iter()
        .flat_map(|block| block.rows.iter().copied())
        .collect()
}

fn projection_unknown_values(plan: &solve::AlgebraicProjectionPlan, y: &[f64]) -> Vec<f64> {
    plan.blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().map(|&index| y[index]))
        .collect()
}

fn restore_projection_unknown_values(
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    values: &[f64],
) {
    for (index, value) in plan
        .blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
        .zip(values.iter().copied())
    {
        y[index] = value;
    }
}

fn seed_nonfinite_projection_unknowns(y: &mut [f64], plan: &solve::AlgebraicProjectionPlan) {
    for block in &plan.blocks {
        for &index in &block.y_indices {
            if !y[index].is_finite() {
                y[index] = 0.0;
            }
        }
    }
}

fn projection_error_for_rows<M: ImplicitProjectionModel>(
    model: &M,
    message: &str,
    rows: &[usize],
    residual: &[f64],
) -> RuntimeSolveError {
    let worst = residual
        .iter()
        .copied()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| residual_sort_key(*lhs).total_cmp(&residual_sort_key(*rhs)));
    match worst {
        Some((offset, value)) => {
            let row = rows.get(offset).copied().unwrap_or(offset);
            let target = model
                .target_name_for_row(row)
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
    let projection_indices = initial_plan_projection_indices(plan);
    let projection_rows = initial_plan_rows(plan);
    for iteration in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        seed_nonfinite_projection_values(y, &projection_indices);
        model.eval_initial_residual(y, p, t, &mut residual)?;
        if iteration > 0 && residual_converged(&residual, tol) {
            return Ok(());
        }
        let selected = initial_plan_residual(&residual, plan)?;
        if tracing::enabled!(target: "rumoca_solver::projection", tracing::Level::DEBUG) {
            let worst_row = residual
                .iter()
                .enumerate()
                .max_by(|(_, lhs), (_, rhs)| {
                    residual_sort_key(**lhs).total_cmp(&residual_sort_key(**rhs))
                })
                .map(|(row, _)| row);
            let worst_block = worst_row
                .and_then(|row| plan.blocks.iter().find(|block| block.rows.contains(&row)));
            let worst_initial_target = worst_row
                .and_then(|row| model.initial_target(row))
                .and_then(y_index_for_slot)
                .and_then(|index| model.variable_name_for_y_index(index));
            tracing::debug!(
                target: "rumoca_solver::projection",
                iteration,
                full_norm = residual_norm(&residual),
                selected_norm = residual_norm(&selected),
                worst_row,
                worst_row_selected = worst_row.is_some_and(|row| projection_rows.contains(&row)),
                worst_target = worst_initial_target,
                worst_slot = ?worst_row.and_then(|row| model.initial_target(row)),
                worst_block_rows = ?worst_block.map(|block| block.rows.as_slice()),
                worst_block_y_indices = ?worst_block.map(|block| block.y_indices.as_slice()),
                blocks = plan.blocks.len(),
                projected_variables = projection_indices.len(),
                "initial algebraic projection iteration"
            );
        }
        if selected.is_empty() {
            break;
        }
        let mut changed = false;
        for block in &plan.blocks {
            let update = project_initial_block(model, y, p, t, block, tol)?;
            changed |= update.changed;
        }
        if !changed {
            break;
        }
    }
    seed_nonfinite_projection_values(y, &projection_indices);
    model.eval_initial_residual(y, p, t, &mut residual)?;
    if residual_converged(&residual, tol) {
        return Ok(());
    }
    let residual_scales = initial_residual_scales(model, y, p, t, plan)?;
    if scaled_residual_converged(&residual, &residual_scales, tol) {
        return Ok(());
    }
    let rows = (0..residual.len()).collect::<Vec<_>>();
    Err(initial_projection_error(
        model,
        "initial variable projection did not satisfy the complete residual system",
        &rows,
        &residual,
    ))
}

fn initial_plan_projection_indices(plan: &solve::AlgebraicProjectionPlan) -> Vec<usize> {
    let mut indices = plan
        .blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
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
        .flat_map(|block| block.rows.iter().copied())
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
    let mut changed = false;
    let rows = &block.rows;
    let y_indices = &block.y_indices;
    let variable_scales = y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index))
        .collect::<Vec<_>>();
    let fallback_scales = initial_block_fallback_scales(model, block, &variable_scales);
    let assignment_context = InitialBlockDeltaCtx {
        model,
        p,
        t,
        rows,
        y_indices,
        tol,
        row_scales: &fallback_scales,
        variable_scales: &variable_scales,
    };
    require_square_projection_block(rows.len(), y_indices.len(), "initial")?;
    if rows.is_empty() || y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    if let Some(update) = project_initial_singleton_assignment(assignment_context, y, changed)? {
        return Ok(update);
    }
    let mut residual = vec![0.0; model.initial_residual_len()];
    model.eval_initial_residual(y, p, t, &mut residual)?;
    let selected = rows
        .iter()
        .map(|row| initial_residual_at(&residual, *row, "algebraic projection block"))
        .collect::<Result<Vec<_>, _>>()?;
    if !selected.iter().all(|value| value.is_finite()) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    }
    let jacobian = initial_block_jacobian(model, y, p, t, rows, y_indices, &residual)?;
    let row_scales = jacobian_row_scales(&jacobian, &variable_scales, &fallback_scales);
    let context = InitialBlockDeltaCtx {
        row_scales: &row_scales,
        ..assignment_context
    };
    if scaled_residual_converged(&selected, &row_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: true,
        });
    }
    tracing::debug!(
        target: "rumoca_solver::projection",
        rows = ?rows,
        y_indices = ?y_indices,
        residual_norm = residual_norm(&selected),
        "solving coupled initial projection block"
    );
    if let Some(update) =
        project_initial_full_residual_singleton_assignment(&context, y, &selected, changed)?
    {
        return Ok(update);
    }
    trace_initial_projection_block(model, rows, y_indices, &selected, &jacobian, tol);
    if rows.len() == 1 && relax_initial_block_from_row_targets(context, y, &selected, &jacobian)? {
        return Ok(ProjectionBlockUpdate {
            changed: true,
            settled: false,
        });
    }
    let update = solve_coupled_initial_block(context, y, &selected, jacobian)?;
    changed |= update.changed;
    Ok(ProjectionBlockUpdate {
        changed,
        settled: update.settled,
    })
}

fn solve_coupled_initial_block<M: AlgebraicProjectionModel>(
    context: InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    residual: &[f64],
    jacobian: DMatrix<f64>,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let delta = scaled_newton_delta(
        &jacobian,
        residual,
        context.row_scales,
        context.variable_scales,
        context.tol,
    );
    let Some(delta) = delta else {
        tracing::debug!(
            target: "rumoca_solver::projection",
            rows = ?context.rows,
            y_indices = ?context.y_indices,
            "coupled initial projection block Jacobian is unsolvable"
        );
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    };
    let update = accept_initial_block_delta(context, y, delta.as_slice())?;
    tracing::debug!(
        target: "rumoca_solver::projection",
        rows = ?context.rows,
        y_indices = ?context.y_indices,
        changed = update.changed,
        settled = update.settled,
        "coupled initial projection block update"
    );
    Ok(update)
}

fn trace_initial_projection_block<M: AlgebraicProjectionModel>(
    model: &M,
    rows: &[usize],
    y_indices: &[usize],
    residual: &[f64],
    jacobian: &DMatrix<f64>,
    tolerance: f64,
) {
    if !tracing::enabled!(target: "rumoca_solver::projection", tracing::Level::DEBUG) {
        return;
    }
    let variables = y_indices
        .iter()
        .map(|&index| {
            model
                .variable_name_for_y_index(index)
                .unwrap_or("<unnamed>")
        })
        .collect::<Vec<_>>();
    let targets = rows
        .iter()
        .map(|&row| {
            model
                .initial_target(row)
                .and_then(y_index_for_slot)
                .and_then(|index| model.variable_name_for_y_index(index))
                .unwrap_or("<none>")
        })
        .collect::<Vec<_>>();
    let decomposition = jacobian.clone().svd(true, true);
    let singular_values = &decomposition.singular_values;
    let largest = singular_values.iter().copied().fold(0.0_f64, f64::max);
    let rank_threshold = tolerance.max(f64::EPSILON * largest * rows.len() as f64);
    let numerical_rank = singular_values
        .iter()
        .filter(|value| value.is_finite() && **value > rank_threshold)
        .count();
    tracing::debug!(
        target: "rumoca_solver::projection",
        rows = ?rows,
        variables = ?variables,
        row_targets = ?targets,
        residual = ?residual,
        singular_values = ?singular_values.as_slice(),
        numerical_rank,
        rank_threshold,
        "coupled initial projection block diagnostics"
    );
    if numerical_rank < rows.len().min(y_indices.len()) {
        trace_initial_projection_nullspace(
            model,
            rows,
            &variables,
            decomposition.u.as_ref(),
            decomposition.v_t.as_ref(),
        );
    }
}

fn trace_initial_projection_nullspace<M: AlgebraicProjectionModel>(
    model: &M,
    rows: &[usize],
    variables: &[&str],
    left_vectors: Option<&DMatrix<f64>>,
    right_vectors_transposed: Option<&DMatrix<f64>>,
) {
    let null_index = rows.len().min(variables.len()).saturating_sub(1);
    let left_null = left_vectors.map(|vectors| {
        rows.iter()
            .enumerate()
            .map(|(index, &row)| {
                let target = model
                    .initial_target(row)
                    .and_then(y_index_for_slot)
                    .and_then(|y_index| model.variable_name_for_y_index(y_index));
                (row, target, vectors[(index, null_index)])
            })
            .collect::<Vec<_>>()
    });
    let right_null = right_vectors_transposed.map(|vectors| {
        variables
            .iter()
            .enumerate()
            .map(|(index, &variable)| (variable, vectors[(null_index, index)]))
            .collect::<Vec<_>>()
    });
    tracing::debug!(
        target: "rumoca_solver::projection",
        left_null = ?left_null,
        right_null = ?right_null,
        "rank-deficient initial projection block nullspace"
    );
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
    let row_tol = scaled_tolerance(ctx.tol, ctx.row_scales[0]);
    let variable_tol = scaled_tolerance(ctx.tol, ctx.variable_scales[0]);
    if !before.is_finite() {
        return Ok(Some(ProjectionBlockUpdate {
            changed,
            settled: false,
        }));
    }
    let Some(value) = ctx
        .model
        .eval_initial_target_value(*row, *y_index, y, ctx.p, ctx.t)?
        .filter(|value| value.is_finite())
    else {
        return Ok(None);
    };
    let previous = y[*y_index];
    y[*y_index] = value;
    let after = ctx.model.eval_initial_residual_row(*row, y, ctx.p, ctx.t)?;
    if let Some(after) = after.filter(|after| {
        singleton_assignment_improves(SingletonAssignmentStep {
            before,
            after: *after,
            step: previous - value,
            row_tol,
            variable_tol,
        })
    }) {
        return Ok(Some(ProjectionBlockUpdate {
            changed: changed || (previous - value).abs() > variable_tol,
            settled: after.abs() <= row_tol,
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
    let row_tol = scaled_tolerance(ctx.tol, ctx.row_scales[0]);
    let variable_tol = scaled_tolerance(ctx.tol, ctx.variable_scales[0]);
    if after.is_finite() && after.abs() + row_tol < before.abs() {
        return Ok(Some(ProjectionBlockUpdate {
            changed: changed || (previous - value).abs() > variable_tol,
            settled: after.abs() <= row_tol,
        }));
    }
    y[*y_index] = previous;
    Ok(None)
}

struct InitialBlockDeltaCtx<'a, M: AlgebraicProjectionModel> {
    model: &'a M,
    p: &'a [f64],
    t: f64,
    rows: &'a [usize],
    y_indices: &'a [usize],
    tol: f64,
    row_scales: &'a [f64],
    variable_scales: &'a [f64],
}

impl<M: AlgebraicProjectionModel> Copy for InitialBlockDeltaCtx<'_, M> {}

impl<M: AlgebraicProjectionModel> Clone for InitialBlockDeltaCtx<'_, M> {
    fn clone(&self) -> Self {
        *self
    }
}

fn accept_initial_block_delta<M: AlgebraicProjectionModel>(
    ctx: InitialBlockDeltaCtx<'_, M>,
    y: &mut [f64],
    delta: &[f64],
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let snapshot = y.to_vec();
    let before =
        initial_selected_residual_norm(ctx.model, y, ctx.p, ctx.t, ctx.rows, ctx.row_scales)?;
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
            let Some(slot) = y.get_mut(y_idx) else {
                y.copy_from_slice(&snapshot);
                return Err(RuntimeSolveError::solve_ir(format!(
                    "initial projection references y index {y_idx}, but the model has only {} variables",
                    snapshot.len()
                )));
            };
            let candidate = *slot + step;
            changed |= candidate != *slot;
            *slot = candidate;
        }
        if !changed {
            y.copy_from_slice(&snapshot);
            return Ok(ProjectionBlockUpdate {
                changed: false,
                settled: false,
            });
        }
        let after =
            initial_selected_residual_norm(ctx.model, y, ctx.p, ctx.t, ctx.rows, ctx.row_scales)?;
        if after.is_finite() && (after <= ctx.tol || after < before) {
            return Ok(ProjectionBlockUpdate {
                changed: true,
                settled: after <= ctx.tol,
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
        let variable_tol = scaled_tolerance(ctx.tol, ctx.variable_scales[column]);
        if !delta.is_finite() || delta.abs() <= variable_tol {
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
        let row_pos = ctx.rows.iter().position(|candidate| candidate == row);
        let row_tol = row_pos
            .and_then(|position| ctx.row_scales.get(position).copied())
            .map_or(ctx.tol, |scale| scaled_tolerance(ctx.tol, scale));
        residual_after
            .get(*row)
            .copied()
            .is_some_and(|after| after.is_finite() && after.abs() + row_tol < *before)
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
    row_scales: &[f64],
) -> Result<f64, RuntimeSolveError> {
    let mut residual = vec![0.0; model.initial_residual_len()];
    model.eval_initial_residual(y, p, t, &mut residual)?;
    let mut selected = Vec::with_capacity(rows.len());
    for row in rows {
        let value = initial_residual_at(&residual, *row, "selected initial projection rows")?;
        if !value.is_finite() {
            return Ok(f64::INFINITY);
        }
        selected.push(value);
    }
    Ok(scaled_residual_norm(&selected, row_scales))
}

fn initial_projection_error<M: AlgebraicProjectionModel>(
    model: &M,
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
            // Naming the row's target turns "row 51 is NaN" into the variable a
            // model author can act on; the row index alone is meaningless
            // outside the lowered IR.
            let target = initial_row_target_name(model, original_row)
                .map_or(String::new(), |name| format!(" target={name}"));
            RuntimeSolveError::solve_ir(format!(
                "{message}: max selected residual row={row} original_row={original_row}{target} value={value:.6e} norm={:.6e}",
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

fn y_index_for_slot(slot: solve::ScalarSlot) -> Option<usize> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some(index),
        _ => None,
    }
}

fn algebraic_block_jacobian(
    model: &dyn ImplicitProjectionModel,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    y_indices: &[usize],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::<f64>::zeros(rows.len(), y_indices.len());
    let mut reverse_gradient = vec![0.0; y.len()];
    let mut needs_forward_jvp = vec![true; rows.len()];
    for (row, residual_idx) in rows.iter().copied().enumerate() {
        if !model.eval_implicit_jacobian_row(residual_idx, y, p, t, &mut reverse_gradient)? {
            continue;
        }
        needs_forward_jvp[row] = false;
        for (col, y_idx) in y_indices.iter().copied().enumerate() {
            if model.implicit_jacobian_v_row_depends_on(residual_idx, y_idx) {
                jacobian[(row, col)] = reverse_gradient[y_idx];
            }
        }
    }
    if needs_forward_jvp.iter().all(|needs_forward| !needs_forward) {
        return Ok(jacobian);
    }

    let mut seed = vec![0.0; y.len()];
    for (col, y_idx) in y_indices.iter().copied().enumerate() {
        if y_idx >= seed.len() {
            continue;
        }
        seed[y_idx] = 1.0;
        let mut selected_complete = true;
        for (row, residual_idx) in rows.iter().copied().enumerate() {
            if !needs_forward_jvp[row]
                || !model.implicit_jacobian_v_row_depends_on(residual_idx, y_idx)
            {
                continue;
            }
            let Some(value) = model.eval_implicit_jacobian_v_row(residual_idx, y, p, t, &seed)?
            else {
                selected_complete = false;
                break;
            };
            jacobian[(row, col)] = value;
        }
        if !selected_complete {
            let mut jv = vec![0.0; y.len()];
            model.eval_jacobian_v(y, p, t, &seed, &mut jv)?;
            fill_jacobian_column_from_jvp(
                &mut jacobian,
                col,
                rows,
                &jv,
                Some(&needs_forward_jvp),
                "algebraic block jacobian-vector product",
            )?;
        }
        seed[y_idx] = 0.0;
    }
    Ok(jacobian)
}

fn fill_jacobian_column_from_jvp(
    jacobian: &mut DMatrix<f64>,
    column: usize,
    rows: &[usize],
    jvp: &[f64],
    selected_rows: Option<&[bool]>,
    context: &str,
) -> Result<(), RuntimeSolveError> {
    for (row, residual_idx) in rows.iter().copied().enumerate() {
        if selected_rows.is_some_and(|selected| !selected[row]) {
            continue;
        }
        jacobian[(row, column)] = residual_at(jvp, residual_idx, context)?;
    }
    Ok(())
}

fn initial_block_jacobian(
    model: &dyn AlgebraicProjectionModel,
    y: &[f64],
    p: &[f64],
    t: f64,
    rows: &[usize],
    y_indices: &[usize],
    _base_residual: &[f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::<f64>::zeros(rows.len(), y_indices.len());
    let mut seed = vec![0.0; y.len()];
    let mut jvp = vec![0.0; model.initial_residual_len()];
    for (col, y_idx) in y_indices.iter().copied().enumerate() {
        if y_idx >= seed.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "initial projection Jacobian references y index {y_idx}, but the model has only {} variables",
                y.len()
            )));
        }
        seed[y_idx] = 1.0;
        model.eval_initial_jacobian_v(y, p, t, &seed, &mut jvp)?;
        for (row_idx, residual_idx) in rows.iter().copied().enumerate() {
            jacobian[(row_idx, col)] =
                initial_residual_at(&jvp, residual_idx, "initial block Jacobian-vector product")?;
        }
        seed[y_idx] = 0.0;
    }
    Ok(jacobian)
}

fn residual_at(residual: &[f64], row: usize, context: &str) -> Result<f64, RuntimeSolveError> {
    residual.get(row).copied().ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "{context} references residual row {row}, but the model evaluated only {} residual rows",
            residual.len()
        ))
    })
}

fn seed_nonfinite_projection_values(y: &mut [f64], projection_indices: &[usize]) {
    for idx in projection_indices.iter().copied() {
        if !y[idx].is_finite() {
            y[idx] = 0.0;
        }
    }
}

#[cfg(test)]
#[path = "projection/tests.rs"]
mod tests;
