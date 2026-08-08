use std::collections::HashSet;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;
use initial_diagnostics::initial_projection_error;
pub(crate) use scaling::{
    SparseNewtonCache, scaled_newton_delta, scaled_newton_delta_with_cache, scaled_unique_delta,
};
use scaling::{
    algebraic_block_scales, algebraic_plan_row_scales, initial_block_fallback_scales,
    initial_residual_scales, jacobian_row_scales, model_variable_scale,
    scaled_correction_converged, scaled_residual_converged, scaled_residual_norm, scaled_tolerance,
};
use singleton::{SingletonAssignmentStep, initial_row_target_name, singleton_assignment_improves};
use step_limit::StepLimit;

mod branch_continuity;
mod homotopy;
mod initial_diagnostics;
mod manifold;
mod plan;
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

    /// Whether this plan is a projection of the constructor-validated Solve
    /// aggregate rather than a third-party runtime plan.
    fn algebraic_projection_plan_is_validated(&self) -> bool {
        false
    }

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

    /// Compiler-derived structure for one block of the algebraic projection
    /// plan. Absence selects the conservative dense policy for third-party
    /// projection models.
    fn algebraic_projection_block_structure(
        &self,
        _block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        None
    }

    /// Constructor-derived proof that changing this block can invalidate a
    /// residual row belonging to an earlier projection block.
    fn algebraic_projection_block_invalidates_earlier(&self, _block_index: usize) -> bool {
        true
    }

    /// Whether construction proved that every residual in this block is affine
    /// in solver-Y. Affine blocks have no nonlinear branch to preserve and may
    /// take the complete Newton correction.
    fn algebraic_projection_block_is_affine(&self, _block_index: usize) -> bool {
        false
    }

    #[allow(clippy::too_many_arguments)]
    fn solve_algebraic_newton_delta(
        &self,
        _block_index: usize,
        jacobian: &DMatrix<f64>,
        residual: &[f64],
        row_scales: &[f64],
        variable_scales: &[f64],
        structure: Option<&solve::StructuralPattern>,
        tolerance: f64,
    ) -> Option<DVector<f64>> {
        scaled_newton_delta(
            jacobian,
            residual,
            row_scales,
            variable_scales,
            structure,
            tolerance,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn solve_algebraic_sensitivity_delta(
        &self,
        _block_index: usize,
        jacobian: &DMatrix<f64>,
        residual: &[f64],
        row_scales: &[f64],
        variable_scales: &[f64],
        structure: Option<&solve::StructuralPattern>,
        tolerance: f64,
    ) -> Option<DVector<f64>> {
        scaled_unique_delta(
            jacobian,
            residual,
            row_scales,
            variable_scales,
            structure,
            tolerance,
            None,
        )
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

    /// Whether the constructor proved that evaluating the target isolator and
    /// writing its value satisfies this scalar residual exactly.
    fn implicit_target_assignment_is_exact(&self, _row_idx: usize, _target_y_index: usize) -> bool {
        false
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

    /// Compiler-derived structure for one initialization projection block.
    fn initial_projection_block_structure(
        &self,
        _block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        None
    }

    /// What the initialization projection does with one row, when the lowered
    /// model records it. Diagnostics only; projection semantics never read it.
    ///
    /// The default is `None` so a third-party projection model keeps working —
    /// it then gets a diagnostic that says the role is unrecorded rather than one
    /// that guesses which of the two very different readings applies.
    fn initial_row_role(&self, _row_idx: usize) -> Option<solve::InitializationRowRole> {
        None
    }

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
    validate_projection_plan_if_needed(model, plan, args.state_count, y.len())?;
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
        let jacobian =
            algebraic_seed_block_jacobian(model, y, args.parameters, args.time, block, unit_seed)?;
        let rhs = DVector::from_iterator(
            block.rows.len(),
            block_residual.into_iter().map(|value| -value),
        );
        let Some(solution) = jacobian.lu().solve(&rhs) else {
            return Err(RuntimeSolveError::DirectionalDerivativeUnavailable {
                reason: "algebraic projection sensitivity matrix is singular".to_string(),
            });
        };
        for (y_index, value) in block
            .y_indices
            .iter()
            .copied()
            .zip(solution.iter().copied())
        {
            if !value.is_finite() {
                return Err(RuntimeSolveError::DirectionalDerivativeUnavailable {
                    reason: format!(
                        "algebraic projection produced a non-finite sensitivity for y[{y_index}]"
                    ),
                });
            }
            seed[y_index] = value;
        }
    }
    unit_seed.fill(0.0);
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
    let row_scales = algebraic_plan_row_scales(model, y, args.parameters, args.time, plan)?;
    if scaled_residual_converged(&residual, &row_scales, args.tolerance) {
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
    validate_projection_plan_if_needed(model, plan, args.state_count, y.len())?;
    branch_continuity::project_with_branch_continuity(model, plan, y, args, max_iters, false)
}

pub fn project_algebraics_with_plan_certified<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    validate_projection_plan_if_needed(model, plan, args.state_count, y.len())?;
    branch_continuity::project_with_branch_continuity(model, plan, y, args, max_iters, true)
}

fn validate_projection_plan_if_needed<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    state_count: usize,
    solver_count: usize,
) -> Result<(), RuntimeSolveError> {
    if model.algebraic_projection_plan_is_validated() {
        Ok(())
    } else {
        validate_algebraic_projection_plan(plan, state_count, solver_count)
    }
}

fn project_algebraics_with_plan_inner<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
    step_limit: StepLimit,
    certify_coordinates: bool,
) -> Result<(), RuntimeSolveError> {
    for iteration in 0..max_iters {
        seed_nonfinite_projection_unknowns(y, plan);
        let mut changed = false;
        let mut all_settled = true;
        let mut earlier_row_invalidated = false;
        for (block_index, block) in plan.blocks.iter().enumerate() {
            let update = project_algebraic_block(
                model,
                y,
                args.parameters,
                args.time,
                block,
                block_index,
                AlgebraicBlockProjectionPolicy {
                    tolerance: args.tolerance,
                    step_limit,
                    certify_coordinates,
                },
            )?;
            if update.changed && block_index != 0 && !earlier_row_invalidated {
                earlier_row_invalidated =
                    model.algebraic_projection_block_invalidates_earlier(block_index);
            }
            changed |= update.changed;
            all_settled &= update.settled;
        }
        // A block is settled at the point where it is visited. A later block
        // may still change one of that row's dependencies. The model exposes
        // the compiler-proven row sparsity, so only repeat a sweep when such a
        // reverse dependency was actually invalidated; causal plans retain
        // their one-sweep fast path.
        if all_settled && !earlier_row_invalidated {
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
    let rows = projection_rows(plan);
    let residual = implicit_selected_residuals(
        model,
        y,
        args.parameters,
        args.time,
        &rows,
        "selected algebraic projection",
    )?;
    let row_scales = algebraic_plan_row_scales(model, y, args.parameters, args.time, plan)?;
    if certify_coordinates {
        return Err(projection_error_for_rows(
            model,
            "algebraic projection did not establish coordinate convergence",
            &rows,
            &residual,
        ));
    }
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
    block_index: usize,
    policy: AlgebraicBlockProjectionPolicy,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let AlgebraicBlockProjectionPolicy {
        tolerance: tol,
        mut step_limit,
        certify_coordinates,
    } = policy;
    if model.algebraic_projection_block_is_affine(block_index) {
        step_limit = StepLimit::None;
    }
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
    // Exact zero satisfies every positive scaled tolerance, independent of
    // Jacobian-derived row scaling. Runtime callers frequently project an
    // already canonical algebraic view (for example around event queries), so
    // avoid rebuilding the full reverse-mode Jacobian merely to prove that
    // zero is zero. This is an exact semantic shortcut: nonzero residuals keep
    // the existing scaled convergence and correction path unchanged.
    if !certify_coordinates && residual.iter().all(|value| *value == 0.0) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }
    let structure = model.algebraic_projection_block_structure(block_index);
    let jacobian =
        algebraic_block_jacobian(model, y, p, t, &block.rows, &block.y_indices, structure)?;
    let pattern = structure.map(solve::JacobianStructure::pattern);
    let (row_scales, variable_scales) = algebraic_block_scales(model, block, &jacobian, pattern);
    let residual_converged = scaled_residual_converged(&residual, &row_scales, tol);
    if residual_converged && !certify_coordinates {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }
    let before_norm = scaled_residual_norm(&residual, &row_scales);
    let delta = model.solve_algebraic_newton_delta(
        block_index,
        &jacobian,
        &residual,
        &row_scales,
        &variable_scales,
        pattern,
        tol,
    );
    let Some(delta) = delta else {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: false,
        });
    };
    if residual_converged && scaled_correction_converged(delta.as_slice(), &variable_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed,
            settled: true,
        });
    }

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
    let Some(before) =
        context
            .model
            .eval_implicit_residual_row(row, y, context.parameters, context.time)?
    else {
        return Ok(None);
    };
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
            let (row_tol, _) = assignment_tolerances(
                context.model,
                y_index,
                before,
                after,
                previous,
                value,
                context.tolerance,
            );
            after.is_finite() && after.abs() <= row_tol
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
    if model.implicit_target_assignment_is_exact(*row, *y_index) {
        let value = model
            .eval_implicit_target_value(*row, *y_index, y, p, t)?
            .filter(|value| value.is_finite());
        let Some(value) = value else {
            return Ok(Some(ProjectionBlockUpdate {
                changed: false,
                settled: false,
            }));
        };
        let previous = y[*y_index];
        y[*y_index] = value;
        return Ok(Some(ProjectionBlockUpdate {
            changed: previous != value,
            settled: true,
        }));
    }
    let Some(before) = model.eval_implicit_residual_row(*row, y, p, t)? else {
        return Ok(None);
    };
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
    if let Some(after) = after.filter(|after| after.is_finite()) {
        let (row_tol, variable_tol) =
            assignment_tolerances(model, *y_index, before, after, previous, value, tol);
        if singleton_assignment_improves(SingletonAssignmentStep {
            before,
            after,
            step: previous - value,
            row_tol,
            variable_tol,
        }) {
            // An accepted exact assignment is semantic progress even when its
            // coordinate delta is below the solver tolerance. Downstream rows
            // may amplify that small coordinate, so suppressing `changed`
            // would let the outer sweep stop with a stale dependent value.
            return Ok(Some(ProjectionBlockUpdate {
                changed: previous != value,
                settled: after.abs() <= row_tol,
            }));
        }
    }
    y[*y_index] = previous;
    Ok(None)
}

/// Tolerances for one isolated assignment, preserving the distinction between
/// the residual row's units and the target coordinate's units.
///
/// The target evaluator proves a scalar assignment for this row. Its observed
/// residual change over the target step therefore supplies the row/coordinate
/// scale conversion without invoking a second Jacobian evaluation on the hot
/// singleton path.
fn assignment_tolerances<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y_index: usize,
    before: f64,
    after: f64,
    previous: f64,
    value: f64,
    tol: f64,
) -> (f64, f64) {
    let variable_scale = model_variable_scale(model, y_index);
    let step = (previous - value).abs();
    let residual_change = (before - after).abs();
    let row_scale = if step.is_finite() && step > 0.0 && residual_change.is_finite() {
        let scale = residual_change / step * variable_scale;
        if scale.is_finite() && scale > 0.0 {
            scale
        } else {
            variable_scale
        }
    } else {
        variable_scale
    };
    (
        scaled_tolerance(tol, row_scale),
        scaled_tolerance(tol, variable_scale),
    )
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
        let Some(next_alpha) = next_scaled_backtrack(alpha, delta, variable_scales, tolerance)
        else {
            break;
        };
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

/// Return the next halved step while at least one correction remains larger
/// than its variable's declared solver accuracy.
///
/// Once every correction is within its scaled tolerance, every later halving is
/// smaller still. The block cannot make a solution-significant state update on
/// this direction, so the caller returns it unsettled to the existing retry or
/// typed failure path instead of searching down to floating-point underflow.
fn next_scaled_backtrack(
    current_alpha: f64,
    delta: &[f64],
    variable_scales: &[f64],
    tolerance: f64,
) -> Option<f64> {
    let next_alpha = current_alpha * 0.5;
    if !(next_alpha > 0.0 && next_alpha < current_alpha) || delta.len() != variable_scales.len() {
        return None;
    }
    delta
        .iter()
        .copied()
        .zip(variable_scales.iter().copied())
        .any(|(correction, scale)| {
            (next_alpha * correction).abs() > scaled_tolerance(tolerance, scale)
        })
        .then_some(next_alpha)
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

#[derive(Clone, Copy)]
struct AlgebraicBlockProjectionPolicy {
    tolerance: f64,
    step_limit: StepLimit,
    certify_coordinates: bool,
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

    fn algebraic_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.model.algebraic_projection_block_structure(block_index)
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        self.model.algebraic_projection_plan_is_validated()
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        self.model
            .algebraic_projection_block_invalidates_earlier(block_index)
    }

    fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.model.algebraic_projection_block_is_affine(block_index)
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        jacobian: &DMatrix<f64>,
        residual: &[f64],
        row_scales: &[f64],
        variable_scales: &[f64],
        structure: Option<&solve::StructuralPattern>,
        tolerance: f64,
    ) -> Option<DVector<f64>> {
        self.model.solve_algebraic_newton_delta(
            block_index,
            jacobian,
            residual,
            row_scales,
            variable_scales,
            structure,
            tolerance,
        )
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.model
            .implicit_target_assignment_is_exact(row_idx, target_y_index)
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

    fn initial_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.model.initial_projection_block_structure(block_index)
    }

    fn initial_row_role(&self, row_idx: usize) -> Option<solve::InitializationRowRole> {
        self.model.initial_row_role(row_idx)
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

mod initial;
#[cfg(test)]
use initial::project_initial_block;
use initial::*;
pub use initial::{
    InitialHomotopySystem, project_initial_variables_with_homotopy,
    project_initial_variables_with_plan,
};
#[cfg(test)]
#[path = "projection/tests.rs"]
mod tests;
