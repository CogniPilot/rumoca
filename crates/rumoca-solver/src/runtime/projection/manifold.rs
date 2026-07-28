use nalgebra::DMatrix;
use rumoca_ir_solve as solve;

use super::{
    ProjectionBlockUpdate, RuntimeSolveError, algebraic_step_at_resolution, jacobian_row_scales,
    scaled_newton_delta, scaled_residual_converged, scaled_residual_norm,
};

const MANIFOLD_PROJECTION_MAX_ITERS: usize = 16;

/// Runtime view of lower-order constraints retained by structural index
/// reduction.
///
/// Unlike algebraic projection, manifold projection changes state coordinates.
/// A block may therefore contain more coordinates than residual rows; the
/// runtime computes the minimum-norm scaled Newton correction.
pub trait ManifoldProjectionModel {
    fn eval_manifold_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn eval_manifold_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn manifold_residual_len(&self) -> usize;
    fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan;

    fn manifold_variable_scale(&self, _y_index: usize) -> f64 {
        1.0
    }
}

/// Project participating state coordinates onto every retained index-reduction
/// constraint. Returns whether any state value changed.
pub fn project_state_manifold<M: ManifoldProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let plan = model.manifold_projection_plan();
    let residual_len = model.manifold_residual_len();
    validate_manifold_projection_plan(plan, residual_len, state_count, y.len())?;
    if plan.blocks.is_empty() {
        return Ok(false);
    }
    let snapshot = y.to_vec();
    let result =
        project_state_manifold_inner(model, plan, y, p, t, tol, MANIFOLD_PROJECTION_MAX_ITERS);
    if result.is_err() {
        y.copy_from_slice(&snapshot);
    }
    result.map(|()| {
        snapshot
            .iter()
            .zip(y.iter())
            .take(state_count)
            .any(|(before, after)| before != after)
    })
}

fn validate_manifold_projection_plan(
    plan: &solve::AlgebraicProjectionPlan,
    residual_len: usize,
    state_count: usize,
    y_len: usize,
) -> Result<(), RuntimeSolveError> {
    if state_count > y_len {
        return Err(RuntimeSolveError::solve_ir(format!(
            "manifold projection state count {state_count} exceeds Y length {y_len}"
        )));
    }
    let mut rows_seen = vec![false; residual_len];
    let mut states_seen = vec![false; state_count];
    for block in &plan.blocks {
        if block.rows.is_empty()
            || block.y_indices.is_empty()
            || block.rows.len() > block.y_indices.len()
        {
            return Err(RuntimeSolveError::solve_ir(format!(
                "manifold projection block has {} residual rows and {} state coordinates",
                block.rows.len(),
                block.y_indices.len()
            )));
        }
        for &row in &block.rows {
            let Some(seen) = rows_seen.get_mut(row) else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "manifold projection row {row} is outside 0..{residual_len}"
                )));
            };
            if std::mem::replace(seen, true) {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "manifold projection row {row} appears more than once"
                )));
            }
        }
        for &state in &block.y_indices {
            let Some(seen) = states_seen.get_mut(state) else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "manifold projection state Y[{state}] is outside 0..{state_count}"
                )));
            };
            if std::mem::replace(seen, true) {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "manifold projection state Y[{state}] appears in multiple blocks"
                )));
            }
        }
    }
    if rows_seen.iter().any(|seen| !seen) {
        return Err(RuntimeSolveError::solve_ir(
            "manifold projection plan does not cover every retained residual row",
        ));
    }
    Ok(())
}

fn project_state_manifold_inner<M: ManifoldProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tol: f64,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    for _ in 0..max_iters {
        let mut settled = true;
        let mut changed = false;
        for block in &plan.blocks {
            let update = project_manifold_block(model, y, p, t, block, tol)?;
            settled &= update.settled;
            changed |= update.changed;
        }
        if settled {
            return Ok(());
        }
        if !changed {
            break;
        }
    }
    let mut residual = vec![0.0; model.manifold_residual_len()];
    model.eval_manifold_residual(y, p, t, &mut residual)?;
    let worst = residual
        .iter()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| lhs.abs().total_cmp(&rhs.abs()))
        .map(|(row, value)| (row, *value));
    Err(RuntimeSolveError::solve_ir(match worst {
        Some((row, value)) => format!(
            "index-reduction manifold projection did not converge; residual row {row} is {value:e}"
        ),
        None => "index-reduction manifold projection did not converge".to_string(),
    }))
}

fn project_manifold_block<M: ManifoldProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let residual_len = model.manifold_residual_len();
    let mut full_residual = vec![0.0; residual_len];
    model.eval_manifold_residual(y, p, t, &mut full_residual)?;
    let residual = block
        .rows
        .iter()
        .map(|&row| full_residual[row])
        .collect::<Vec<_>>();
    let mut jacobian = DMatrix::zeros(block.rows.len(), block.y_indices.len());
    let mut seed = vec![0.0; y.len()];
    let mut jvp = vec![0.0; residual_len];
    for (column, state) in block.y_indices.iter().copied().enumerate() {
        seed[state] = 1.0;
        model.eval_manifold_jacobian_v(y, p, t, &seed, &mut jvp)?;
        for (row_position, row) in block.rows.iter().copied().enumerate() {
            jacobian[(row_position, column)] = jvp[row];
        }
        seed[state] = 0.0;
        jvp.fill(0.0);
    }
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&state| model.manifold_variable_scale(state))
        .collect::<Vec<_>>();
    let row_scales = jacobian_row_scales(&jacobian, &variable_scales, &vec![1.0; block.rows.len()]);
    if scaled_residual_converged(&residual, &row_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: true,
        });
    }
    let before = scaled_residual_norm(&residual, &row_scales);
    let Some(delta) = scaled_newton_delta(&jacobian, &residual, &row_scales, &variable_scales, tol)
    else {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    };
    let snapshot = y.to_vec();
    let mut alpha = 1.0;
    loop {
        y.copy_from_slice(&snapshot);
        let mut changed = false;
        let mut at_resolution = true;
        let mut finite_step = true;
        for (state, correction) in block.y_indices.iter().copied().zip(delta.iter().copied()) {
            let step = alpha * correction;
            let candidate = snapshot[state] + step;
            if !candidate.is_finite() {
                finite_step = false;
                break;
            }
            changed |= candidate != snapshot[state];
            at_resolution &= algebraic_step_at_resolution(snapshot[state], candidate);
            y[state] = candidate;
        }
        if finite_step && changed {
            model.eval_manifold_residual(y, p, t, &mut full_residual)?;
            let after_residual = block
                .rows
                .iter()
                .map(|&row| full_residual[row])
                .collect::<Vec<_>>();
            let after = scaled_residual_norm(&after_residual, &row_scales);
            if after.is_finite() && (after <= tol.abs() || (!at_resolution && after < before)) {
                return Ok(ProjectionBlockUpdate {
                    changed: true,
                    settled: after <= tol.abs(),
                });
            }
        }
        if at_resolution {
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
