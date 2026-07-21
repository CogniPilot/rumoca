use std::collections::HashSet;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;

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
    let snapshot = seed[args.state_count..y.len()].to_vec();
    let result = project_algebraic_seed_with_plan_inner(model, plan, y, args, seed, unit_seed);
    if result.is_err() {
        seed[args.state_count..y.len()].copy_from_slice(&snapshot);
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
    seed[args.state_count..y.len()].fill(0.0);
    let mut jvp = vec![0.0; y.len()];
    for block in &plan.blocks {
        model.eval_jacobian_v(y, args.parameters, args.time, seed, &mut jvp)?;
        let rhs = DVector::from_iterator(
            block.rows.len(),
            block
                .rows
                .iter()
                .map(|row| residual_at(&jvp, *row, "algebraic seed projection").map(|v| -v))
                .collect::<Result<Vec<_>, _>>()?,
        );
        let jacobian = algebraic_seed_block_jacobian(
            model,
            y,
            args.parameters,
            args.time,
            block,
            unit_seed,
            &mut jvp,
        )?;
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
    model.eval_jacobian_v(y, args.parameters, args.time, seed, &mut jvp)?;
    let residual = projection_residual_tail(&jvp, args.state_count)?;
    if residual_converged(&residual, args.tolerance) {
        return Ok(());
    }
    Err(projection_error(
        model,
        args.state_count,
        "algebraic projection sensitivity did not satisfy the complete residual system",
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
    jvp: &mut [f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let mut jacobian = DMatrix::zeros(block.rows.len(), block.y_indices.len());
    for (column, y_index) in block.y_indices.iter().copied().enumerate() {
        unit_seed.fill(0.0);
        unit_seed[y_index] = 1.0;
        model.eval_jacobian_v(y, p, t, unit_seed, jvp)?;
        for (row_pos, row) in block.rows.iter().copied().enumerate() {
            jacobian[(row_pos, column)] =
                residual_at(jvp, row, "algebraic seed projection Jacobian")?;
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
    let snapshot = y[args.state_count..].to_vec();
    let result = project_algebraics_with_plan_inner(model, plan, y, args, max_iters);
    if result.is_err() {
        y[args.state_count..].copy_from_slice(&snapshot);
    }
    result
}

fn project_algebraics_with_plan_inner<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    let mut rhs = vec![0.0; y.len()];
    for _ in 0..max_iters {
        seed_nonfinite_algebraics(y, args.state_count);
        model.eval_residual(y, args.parameters, args.time, &mut rhs)?;
        let residual = projection_residual_tail(&rhs, args.state_count)?;
        if residual_converged(&residual, args.tolerance) {
            return Ok(());
        }
        let mut changed = false;
        for block in &plan.blocks {
            let update = project_algebraic_block(
                model,
                y,
                args.parameters,
                args.time,
                block,
                args.tolerance,
            )?;
            changed |= update.changed;
        }
        if !changed {
            break;
        }
    }
    seed_nonfinite_algebraics(y, args.state_count);
    model.eval_residual(y, args.parameters, args.time, &mut rhs)?;
    let residual = projection_residual_tail(&rhs, args.state_count)?;
    if residual_converged(&residual, args.tolerance) {
        return Ok(());
    }
    Err(projection_error(
        model,
        args.state_count,
        "algebraic projection did not converge at event boundary",
        &residual,
    ))
}

fn projection_residual_tail(
    rhs: &[f64],
    state_count: usize,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let _ = algebraic_tail_len(rhs.len(), state_count, "projection residual tail")?;
    Ok(rhs[state_count..].to_vec())
}

fn validate_algebraic_projection_plan(
    plan: &solve::AlgebraicProjectionPlan,
    state_count: usize,
    solver_count: usize,
) -> Result<(), RuntimeSolveError> {
    let algebraic_count =
        algebraic_tail_len(solver_count, state_count, "algebraic projection plan")?;
    let mut row_seen = vec![false; algebraic_count];
    let mut y_seen = vec![false; algebraic_count];
    for block in &plan.blocks {
        require_square_projection_block(block.rows.len(), block.y_indices.len(), "algebraic")?;
        mark_projection_indices(
            &block.rows,
            state_count,
            solver_count,
            &mut row_seen,
            "algebraic projection",
            "residual row",
        )?;
        mark_projection_indices(
            &block.y_indices,
            state_count,
            solver_count,
            &mut y_seen,
            "algebraic projection",
            "unknown",
        )?;
    }
    Ok(())
}

fn validate_initial_projection_plan(
    plan: &solve::AlgebraicProjectionPlan,
    residual_count: usize,
    solver_count: usize,
) -> Result<(), RuntimeSolveError> {
    let mut row_seen = vec![false; residual_count];
    let mut y_seen = vec![false; solver_count];
    for block in &plan.blocks {
        require_square_projection_block(block.rows.len(), block.y_indices.len(), "initial")?;
        mark_projection_indices(
            &block.rows,
            0,
            residual_count,
            &mut row_seen,
            "initial projection",
            "residual row",
        )?;
        mark_projection_indices(
            &block.y_indices,
            0,
            solver_count,
            &mut y_seen,
            "initial projection",
            "unknown",
        )?;
    }
    Ok(())
}

fn require_square_projection_block(
    row_count: usize,
    unknown_count: usize,
    kind: &str,
) -> Result<(), RuntimeSolveError> {
    if row_count == unknown_count {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "{kind} projection block has {row_count} residual rows but {unknown_count} unknowns"
    )))
}

fn mark_projection_indices(
    indices: &[usize],
    lower_bound: usize,
    upper_bound: usize,
    seen: &mut [bool],
    context: &str,
    role: &str,
) -> Result<(), RuntimeSolveError> {
    for &index in indices {
        if index < lower_bound || index >= upper_bound {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} {role} {index} is outside {lower_bound}..{upper_bound}"
            )));
        }
        let slot = &mut seen[index - lower_bound];
        if *slot {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} {role} {index} appears more than once"
            )));
        }
        *slot = true;
    }
    Ok(())
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

fn project_algebraic_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
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
    let solve_rhs = DVector::from_vec(residual.into_iter().map(|value| -value).collect());
    let delta = jacobian
        .clone()
        .lu()
        .solve(&solve_rhs)
        .or_else(|| jacobian.clone().svd(true, true).solve(&solve_rhs, tol).ok());
    let Some(delta) = delta else {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    };

    let update = accept_algebraic_block_delta(model, y, p, t, block, delta.as_slice(), tol)?;
    changed |= update.changed;
    Ok(ProjectionBlockUpdate {
        changed,
        settled: update.settled,
    })
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
    if before.abs() <= tol || !before.is_finite() {
        return Ok(Some(ProjectionBlockUpdate {
            changed: false,
            settled: before.is_finite(),
        }));
    }
    let Some(value) = model.eval_implicit_target_value(*row, *y_index, y, p, t)? else {
        return Ok(None);
    };
    if !value.is_finite() {
        return Ok(None);
    }
    let previous = y[*y_index];
    y[*y_index] = value;
    let after = model.eval_implicit_residual_row(*row, y, p, t)?;
    if let Some(after) = after.filter(|after| after.is_finite() && after.abs() + tol < before.abs())
    {
        return Ok(Some(ProjectionBlockUpdate {
            changed: (previous - value).abs() > tol,
            settled: after.abs() <= tol,
        }));
    }
    y[*y_index] = previous;
    Ok(None)
}

fn accept_algebraic_block_delta<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    delta: &[f64],
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let snapshot = y.to_vec();
    let before = algebraic_selected_residual_norm(model, y, p, t, &block.rows)?;
    if !before.is_finite() {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    }
    let mut alpha = 1.0;
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
        let after = algebraic_selected_residual_norm(model, y, p, t, &block.rows)?;
        if after.is_finite() && (after <= tol || (!step_at_resolution && after < before)) {
            return Ok(ProjectionBlockUpdate {
                changed: true,
                settled: after <= tol,
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
) -> Result<f64, RuntimeSolveError> {
    let mut residual = vec![0.0; y.len()];
    model.eval_residual(y, p, t, &mut residual)?;
    rows.iter().try_fold(0.0_f64, |norm, row| {
        residual_at(&residual, *row, "selected algebraic projection rows").map(|value| {
            if value.is_finite() {
                norm.max(value.abs())
            } else {
                f64::INFINITY
            }
        })
    })
}

#[derive(Debug, Clone, Copy)]
struct ProjectionBlockUpdate {
    changed: bool,
    settled: bool,
}

pub fn project_initial_variables_with_plan<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    validate_initial_projection_plan(plan, model.initial_residual_len(), y.len())?;
    if model.initial_residual_len() == 0 {
        return Ok(());
    }
    let projection_indices = initial_plan_projection_indices(plan);
    let snapshot = projection_indices
        .iter()
        .copied()
        .map(|index| {
            y.get(index).copied().map(|value| (index, value)).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "initial projection plan references y index {index}, but the model has only {} variables",
                    y.len()
                ))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let result = project_initial_variables_by_plan(model, y, p, t, plan, tol);
    if result.is_err() {
        for (index, value) in snapshot {
            y[index] = value;
        }
    }
    result
}

fn seed_nonfinite_algebraics(y: &mut [f64], state_count: usize) {
    for value in &mut y[state_count..] {
        if !value.is_finite() {
            *value = 0.0;
        }
    }
}

fn projection_error<M: ImplicitProjectionModel>(
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
    let projection_indices = initial_plan_projection_indices(plan);
    for _ in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
        seed_nonfinite_projection_values(y, &projection_indices);
        model.eval_initial_residual(y, p, t, &mut residual)?;
        if residual_converged(&residual, tol) {
            return Ok(());
        }
        let selected = initial_plan_residual(&residual, plan)?;
        if selected.is_empty() || residual_converged(&selected, tol) {
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
    let rows = (0..residual.len()).collect::<Vec<_>>();
    Err(initial_projection_error(
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
    require_square_projection_block(rows.len(), y_indices.len(), "initial")?;
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
            rows,
            y_indices,
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
        rows,
        y_indices,
        tol,
    };
    if let Some(update) =
        project_initial_full_residual_singleton_assignment(&delta_ctx, y, &selected, changed)?
    {
        return Ok(update);
    }
    let jacobian = initial_block_jacobian(model, y, p, t, rows, y_indices, &residual)?;
    if rows.len() == 1 && relax_initial_block_from_row_targets(delta_ctx, y, &selected, &jacobian)?
    {
        return Ok(ProjectionBlockUpdate {
            changed: true,
            settled: false,
        });
    }
    let solve_rhs = DVector::from_vec(selected.iter().map(|value| -*value).collect());
    let delta = jacobian
        .clone()
        .lu()
        .solve(&solve_rhs)
        .or_else(|| jacobian.clone().svd(true, true).solve(&solve_rhs, tol).ok());
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
            rows,
            y_indices,
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
        let after = initial_selected_residual_norm(ctx.model, y, ctx.p, ctx.t, ctx.rows)?;
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
    model: &dyn ImplicitProjectionModel,
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
