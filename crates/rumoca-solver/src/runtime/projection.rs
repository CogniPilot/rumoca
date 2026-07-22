use std::collections::HashSet;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;

mod tearing;

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

    /// Try to apply a compiler-certified causal value schedule as one prepared
    /// evaluator operation. The default preserves compatibility with models
    /// that only provide row-at-a-time evaluation.
    fn try_apply_implicit_causal_values(
        &self,
        _steps: &[solve::AlgebraicCausalStep],
        _y: &mut [f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        Ok(false)
    }

    /// Try to propagate a tangent seed through a causal schedule as one
    /// prepared evaluator operation. The default uses the general row path.
    fn try_propagate_implicit_causal_seed(
        &self,
        _steps: &[solve::AlgebraicCausalStep],
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _seed: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        Ok(false)
    }

    /// Evaluate an exact affine pivot certified by the compiler-owned tearing
    /// plan. The default leaves execution to the residual/JVP fallback.
    fn eval_implicit_affine_target_value(
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
        if block.tearing.is_some() {
            let seed_snapshot = seed.to_vec();
            match tearing::project_torn_algebraic_seed_block(model, y, args, block, seed, unit_seed)
            {
                Ok(true) => continue,
                Ok(false) | Err(_) => seed.copy_from_slice(&seed_snapshot),
            }
        }
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
    let snapshot = projection_unknown_values(plan, y);
    let result = project_algebraics_with_plan_inner(model, plan, y, args, max_iters);
    if result.is_err() {
        restore_projection_unknown_values(plan, y, &snapshot);
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
    if residual_converged(&residual, args.tolerance) {
        return Ok(());
    }
    Err(projection_error_for_rows(
        model,
        "algebraic projection did not converge at event boundary",
        &rows,
        &residual,
    ))
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
        validate_algebraic_tearing_plan(block)?;
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
        if block.tearing.is_some() {
            return Err(RuntimeSolveError::solve_ir(
                "initial projection blocks cannot carry continuous tearing metadata".to_string(),
            ));
        }
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

fn validate_algebraic_tearing_plan(
    block: &solve::AlgebraicProjectionBlock,
) -> Result<(), RuntimeSolveError> {
    let Some(plan) = block.tearing.as_ref() else {
        return Ok(());
    };
    if plan.residual_rows.is_empty()
        || plan.residual_rows.len() != plan.tear_y_indices.len()
        || plan.causal_steps.is_empty()
    {
        return Err(RuntimeSolveError::solve_ir(
            "algebraic tearing plan must have equally many nonempty residual rows and tear variables plus at least one causal step".to_string(),
        ));
    }
    let full_rows = block.rows.iter().copied().collect::<HashSet<_>>();
    let full_y = block.y_indices.iter().copied().collect::<HashSet<_>>();
    let mut reduced_rows = HashSet::new();
    let mut reduced_y = HashSet::new();
    for &row in &plan.residual_rows {
        if !full_rows.contains(&row) || !reduced_rows.insert(row) {
            return Err(invalid_tearing_inventory("residual row", row));
        }
    }
    for &index in &plan.tear_y_indices {
        if !full_y.contains(&index) || !reduced_y.insert(index) {
            return Err(invalid_tearing_inventory("tear variable", index));
        }
    }
    for step in &plan.causal_steps {
        if !full_rows.contains(&step.row) || !reduced_rows.insert(step.row) {
            return Err(invalid_tearing_inventory("causal row", step.row));
        }
        if !full_y.contains(&step.target_y_index) || !reduced_y.insert(step.target_y_index) {
            return Err(invalid_tearing_inventory(
                "causal target",
                step.target_y_index,
            ));
        }
    }
    if reduced_rows != full_rows || reduced_y != full_y {
        return Err(RuntimeSolveError::solve_ir(
            "algebraic tearing plan does not partition its full block inventory".to_string(),
        ));
    }
    Ok(())
}

fn invalid_tearing_inventory(role: &str, index: usize) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "algebraic tearing {role} {index} is duplicated or outside its full block"
    ))
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
    if let Some(update) = tearing::project_torn_algebraic_block(model, y, p, t, block, tol)? {
        return Ok(update);
    }
    let residual =
        implicit_selected_residuals(model, y, p, t, &block.rows, "algebraic projection block")?;
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
    let before_norm = residual_norm(&residual);
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

    let update = accept_algebraic_block_delta(
        AlgebraicBlockDeltaContext {
            model,
            parameters: p,
            time: t,
            block,
            before: before_norm,
            tolerance: tol,
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

struct AlgebraicBlockDeltaContext<'a, M> {
    model: &'a M,
    parameters: &'a [f64],
    time: f64,
    block: &'a solve::AlgebraicProjectionBlock,
    before: f64,
    tolerance: f64,
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
    } = context;
    let snapshot = y.to_vec();
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
        let after = algebraic_selected_residual_norm(model, y, parameters, time, &block.rows)?;
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
) -> Result<f64, RuntimeSolveError> {
    let residual =
        implicit_selected_residuals(model, y, p, t, rows, "selected algebraic projection rows")?;
    Ok(residual.into_iter().fold(0.0_f64, |norm, value| {
        if value.is_finite() {
            norm.max(value.abs())
        } else {
            f64::INFINITY
        }
    }))
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
        if residual_converged(&residual, tol) {
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
    let context = InitialBlockDeltaCtx {
        model,
        p,
        t,
        rows,
        y_indices,
        tol,
    };
    require_square_projection_block(rows.len(), y_indices.len(), "initial")?;
    if rows.is_empty() || y_indices.is_empty() {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: !changed,
        });
    }
    if let Some(update) = project_initial_singleton_assignment(context, y, changed)? {
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
    let jacobian = initial_block_jacobian(model, y, p, t, rows, y_indices, &residual)?;
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
    let solve_rhs = DVector::from_vec(residual.iter().map(|value| -*value).collect());
    let delta = jacobian
        .clone()
        .lu()
        .solve(&solve_rhs)
        .or_else(|| jacobian.svd(true, true).solve(&solve_rhs, context.tol).ok());
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

struct InitialBlockDeltaCtx<'a, M: AlgebraicProjectionModel> {
    model: &'a M,
    p: &'a [f64],
    t: f64,
    rows: &'a [usize],
    y_indices: &'a [usize],
    tol: f64,
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
