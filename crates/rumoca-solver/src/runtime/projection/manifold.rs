#[cfg(test)]
mod selected_tests;

use nalgebra::DMatrix;
#[cfg(test)]
use rumoca_eval_solve::dense_basis::DenseStageMatrix;
use rumoca_ir_solve as solve;

use super::{
    ProjectionBlockUpdate, RuntimeSolveError, ScaledNewtonSystem, algebraic_step_at_resolution,
    jacobian_row_scales, next_scaled_backtrack, scaled_newton_delta, scaled_residual_converged,
    scaled_residual_norm,
};

const MANIFOLD_PROJECTION_MAX_ITERS: usize = 16;

/// Runtime view of lower-order constraints retained by structural index
/// reduction.
///
/// Unlike algebraic projection, manifold projection changes state coordinates.
/// A block may therefore contain more coordinates than residual rows; the
/// runtime computes the minimum-norm scaled Newton correction.
pub(crate) trait ManifoldProjectionModel {
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

    fn eval_manifold_jacobian_outputs(
        &self,
        _selection: &solve::ProjectionOutputSelection,
        _inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        _out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        Ok(false)
    }

    fn manifold_projection_block_structure(
        &self,
        _block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        None
    }

    fn manifold_variable_scale(&self, _y_index: usize) -> f64 {
        1.0
    }
}

/// Project participating state coordinates onto every retained index-reduction
/// constraint. Returns whether any state value changed.
pub(crate) fn project_state_manifold<M: ManifoldProjectionModel>(
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

/// Safety multiple on the singular threshold at which a chart is reported to
/// be approaching a fold while its dependent reconstruction is still regular.
///
/// A chart crosses `folding` only when its reciprocal conditioning reaches the
/// rank-test threshold, which is near machine epsilon; by then the fixed
/// reconstruction has already collapsed. `margin` widens the boundary so the
/// approach is observable one step earlier, while the active chart still pins
/// the branch. It changes no behavior on its own: the probe reports it and
/// takes no action.
///
/// The probe is instrumentation for the runtime coordinate re-selection that
/// consumes [`solve::AlgebraicProjectionBlock::alternate_charts`]. Until a
/// caller reads it, it is compiled only under test.
#[cfg(test)]
const CHART_MARGIN_MULTIPLE: f64 = 1.0e6;

/// Read-only conditioning of one manifold reconstruction chart at an accepted
/// continuous point.
///
/// A chart partitions the block's state coordinates into dependent columns it
/// reconstructs (its tear and causal unknowns) and independent columns it
/// integrates. `rcond` is the reciprocal conditioning of the dependent
/// Jacobian `g_d` relative to the whole block Jacobian's pivot scale, obtained
/// through the same column-pivoted QR the coordinate selection uses. `folding`
/// marks a chart whose `rcond` has reached the singular threshold the rank test
/// rejects; `margin` marks the earlier approach, an `rcond` under a safety
/// multiple of that threshold.
#[cfg(test)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct ChartConditioning {
    pub block_index: usize,
    pub rcond: f64,
    pub folding: bool,
    pub margin: bool,
}

/// Estimate the conditioning of `chart` for manifold block `block` at the
/// accepted point `(y, p, t)` without changing any state.
///
/// This is a query only. It evaluates the block Jacobian of `manifold_residual`
/// (reusing [`manifold_block_jacobian`]), restricts it to the chart's residual
/// rows and dependent reconstruction columns, and estimates the dependent
/// block's reciprocal conditioning against the whole block Jacobian's pivot
/// scale through [`DenseStageMatrix::dependent_conditioning`]. It performs no
/// projection, mutates no `y`, and requests no chart change.
#[cfg(test)]
pub(crate) fn probe_chart_conditioning<M: ManifoldProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
    chart: &solve::BlockTearing,
) -> Result<ChartConditioning, RuntimeSolveError> {
    let dependent_columns = chart_dependent_columns(block, chart)?;
    let residual_row_positions = chart
        .residual_rows
        .iter()
        .map(|row| {
            block
                .rows
                .iter()
                .position(|&candidate| candidate == *row)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "chart residual row {row} is not a row of manifold block {block_index}"
                    ))
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if residual_row_positions.len() != dependent_columns.len() {
        return Err(RuntimeSolveError::solve_ir(format!(
            "chart for manifold block {block_index} reconstructs {} columns over {} residual rows",
            dependent_columns.len(),
            residual_row_positions.len()
        )));
    }
    let jacobian = manifold_block_jacobian(model, y, p, t, block, block_index)?;
    let columns = block.y_indices.len();
    let mut stage_values = Vec::with_capacity(residual_row_positions.len() * columns);
    for &row_position in &residual_row_positions {
        for column in 0..columns {
            stage_values.push(jacobian[(row_position, column)]);
        }
    }
    let stage = DenseStageMatrix::new(residual_row_positions.len(), columns, &stage_values)
        .map_err(|error| {
            RuntimeSolveError::solve_ir(format!(
                "manifold block {block_index} chart Jacobian is not a finite stage matrix: {error:?}"
            ))
        })?;
    let conditioning = stage
        .dependent_conditioning(&dependent_columns)
        .map_err(|error| {
            RuntimeSolveError::solve_ir(format!(
                "manifold block {block_index} chart conditioning is undefined: {error:?}"
            ))
        })?;
    Ok(ChartConditioning {
        block_index,
        rcond: conditioning.rcond,
        folding: conditioning.rcond <= conditioning.singular_threshold,
        margin: conditioning.rcond <= CHART_MARGIN_MULTIPLE * conditioning.singular_threshold,
    })
}

/// Column positions within `block.y_indices` of the coordinates `chart`
/// reconstructs: its tear unknowns and its causal back-substitution unknowns.
#[cfg(test)]
fn chart_dependent_columns(
    block: &solve::AlgebraicProjectionBlock,
    chart: &solve::BlockTearing,
) -> Result<Vec<usize>, RuntimeSolveError> {
    let mut columns = Vec::with_capacity(chart.tear_y_indices.len() + chart.causal_steps.len());
    let dependent_ys = chart
        .tear_y_indices
        .iter()
        .copied()
        .chain(chart.causal_steps.iter().map(|step| step.y_index));
    for y_index in dependent_ys {
        let position = block
            .y_indices
            .iter()
            .position(|&candidate| candidate == y_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "chart dependent coordinate Y[{y_index}] is not a state of this manifold block"
                ))
            })?;
        if columns.contains(&position) {
            return Err(RuntimeSolveError::solve_ir(format!(
                "chart reconstructs coordinate Y[{y_index}] more than once"
            )));
        }
        columns.push(position);
    }
    Ok(columns)
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
        for (block_index, block) in plan.blocks.iter().enumerate() {
            let update = project_manifold_block(model, y, p, t, block, block_index, tol)?;
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

struct ManifoldBlockEvaluation {
    full_residual: Vec<f64>,
    residual: Vec<f64>,
    jacobian: DMatrix<f64>,
    row_scales: Vec<f64>,
    variable_scales: Vec<f64>,
}

fn evaluate_manifold_block<M: ManifoldProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
) -> Result<ManifoldBlockEvaluation, RuntimeSolveError> {
    let residual_len = model.manifold_residual_len();
    let mut full_residual = vec![0.0; residual_len];
    model.eval_manifold_residual(y, p, t, &mut full_residual)?;
    let residual = block
        .rows
        .iter()
        .map(|&row| full_residual[row])
        .collect::<Vec<_>>();
    let jacobian = manifold_block_jacobian(model, y, p, t, block, block_index)?;
    let structure = model
        .manifold_projection_block_structure(block_index)
        .map(solve::JacobianStructure::pattern);
    let (row_scales, variable_scales) = manifold_block_scales(model, block, &jacobian, structure);
    Ok(ManifoldBlockEvaluation {
        full_residual,
        residual,
        jacobian,
        row_scales,
        variable_scales,
    })
}

fn manifold_block_jacobian<M: ManifoldProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    if let Some(structure) = model.manifold_projection_block_structure(block_index)
        && let Some(jacobian) = selected_manifold_jacobian(model, y, p, t, block, structure)?
    {
        return Ok(jacobian);
    }
    let mut jacobian = DMatrix::zeros(block.rows.len(), block.y_indices.len());
    let mut seed = vec![0.0; y.len()];
    let mut jvp = vec![0.0; model.manifold_residual_len()];
    for (column, state) in block.y_indices.iter().copied().enumerate() {
        seed[state] = 1.0;
        model.eval_manifold_jacobian_v(y, p, t, &seed, &mut jvp)?;
        for (row_position, row) in block.rows.iter().copied().enumerate() {
            jacobian[(row_position, column)] = jvp[row];
        }
        seed[state] = 0.0;
        jvp.fill(0.0);
    }
    Ok(jacobian)
}

fn selected_manifold_jacobian<M: ManifoldProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    structure: &solve::JacobianStructure,
) -> Result<Option<DMatrix<f64>>, RuntimeSolveError> {
    let mut jacobian = DMatrix::zeros(block.rows.len(), block.y_indices.len());
    let column_rows = structure.pattern().column_rows();
    let mut seed = vec![0.0; y.len()];
    let mut values = vec![0.0; block.rows.len()];
    for (color, group) in structure.coloring().groups().iter().enumerate() {
        let Some(selection) = structure
            .output_evaluation(color)
            .and_then(|owner| owner.solver_y())
        else {
            return Ok(None);
        };
        for &column in group.iter() {
            seed[block.y_indices[column as usize]] = 1.0;
        }
        if !model.eval_manifold_jacobian_outputs(
            selection,
            rumoca_eval_solve::JacobianEvalInputs {
                y,
                p,
                t,
                seed: &seed,
            },
            &mut values,
        )? {
            return Ok(None);
        }
        for &column in group.iter() {
            for &row in &column_rows[column as usize] {
                jacobian[(row, column as usize)] = values[row];
            }
            seed[block.y_indices[column as usize]] = 0.0;
        }
    }
    Ok(Some(jacobian))
}

/// Certify a settled initialization point without obtaining mutable state storage.
pub(crate) fn certify_state_manifold<M: ManifoldProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    for (index, block) in model.manifold_projection_plan().blocks.iter().enumerate() {
        let evaluated = evaluate_manifold_block(model, y, p, t, block, index)?;
        if !scaled_residual_converged(&evaluated.residual, &evaluated.row_scales, tol) {
            return Err(RuntimeSolveError::solve_ir(format!(
                "initial state manifold is inconsistent in constraint block {index}; settled initial states cannot be corrected"
            )));
        }
    }
    Ok(())
}

fn project_manifold_block<M: ManifoldProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    let ManifoldBlockEvaluation {
        mut full_residual,
        residual,
        jacobian,
        row_scales,
        variable_scales,
    } = evaluate_manifold_block(model, y, p, t, block, block_index)?;
    let structure = model
        .manifold_projection_block_structure(block_index)
        .map(solve::JacobianStructure::pattern);
    if scaled_residual_converged(&residual, &row_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: true,
        });
    }
    let before = scaled_residual_norm(&residual, &row_scales);
    let Some(delta) = scaled_newton_delta(ScaledNewtonSystem {
        jacobian: &jacobian,
        residual: &residual,
        row_scales: &row_scales,
        variable_scales: &variable_scales,
        structure,
        tolerance: tol,
    }) else {
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
        let Some(next_alpha) =
            next_scaled_backtrack(alpha, delta.as_slice(), &variable_scales, tol)
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

fn manifold_block_scales<M: ManifoldProjectionModel>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&state| model.manifold_variable_scale(state))
        .collect::<Vec<_>>();
    let row_scales = jacobian_row_scales(
        jacobian,
        &variable_scales,
        &vec![1.0; block.rows.len()],
        structure,
    );
    (row_scales, variable_scales)
}

#[cfg(test)]
mod conditioning_tests {
    use super::*;

    /// The `CircleChart` orientation manifold: the first-integral norm
    /// `q[1]^2 + q[2]^2 = 1` retained by index reduction. Its dependent
    /// reconstruction Jacobian for a chart that keeps `q[i]` dependent is
    /// `2 * q[i]`, which collapses as that coordinate crosses zero.
    struct CircleChartManifold {
        plan: solve::AlgebraicProjectionPlan,
    }

    impl ManifoldProjectionModel for CircleChartManifold {
        fn eval_manifold_residual(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] * y[0] + y[1] * y[1] - 1.0;
            Ok(())
        }

        fn eval_manifold_jacobian_v(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
            v: &[f64],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = 2.0 * y[0] * v[0] + 2.0 * y[1] * v[1];
            Ok(())
        }

        fn manifold_residual_len(&self) -> usize {
            1
        }

        fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            &self.plan
        }
    }

    // Chart A: reconstruct q[1] (dependent), integrate q[2] (independent).
    fn chart_reconstructing_q1() -> solve::BlockTearing {
        solve::BlockTearing {
            tear_y_indices: vec![0],
            residual_rows: vec![0],
            causal_steps: vec![],
        }
    }

    // Chart B: reconstruct q[2] (dependent), integrate q[1] (independent).
    fn chart_reconstructing_q2() -> solve::BlockTearing {
        solve::BlockTearing {
            tear_y_indices: vec![1],
            residual_rows: vec![0],
            causal_steps: vec![],
        }
    }

    fn circle_block() -> solve::AlgebraicProjectionBlock {
        solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0, 1],
            tearing: Some(chart_reconstructing_q1()),
            alternate_charts: vec![chart_reconstructing_q2()],
        }
    }

    fn model() -> CircleChartManifold {
        CircleChartManifold {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![circle_block()],
            },
        }
    }

    #[test]
    fn chart_folds_as_its_dependent_coordinate_passes_through_zero() {
        let model = model();
        let block = circle_block();
        let chart = chart_reconstructing_q1();

        // Away from the fold: q[1] is well clear of zero, so g_d = 2*q[1] is a
        // well-conditioned dependent reconstruction.
        let regular =
            probe_chart_conditioning(&model, &[0.6, 0.8], &[], 0.0, &block, 0, &chart).unwrap();
        assert!(
            !regular.folding,
            "chart must be regular away from the fold: {regular:?}"
        );
        assert!(
            !regular.margin,
            "chart must clear the margin away from the fold: {regular:?}"
        );
        assert!(
            regular.rcond > 1.0e-3,
            "expected a well-conditioned chart: {regular:?}"
        );

        // The quarter turn: q[1] -> 0, so g_d = 2*q[1] collapses toward zero.
        let folded =
            probe_chart_conditioning(&model, &[1.0e-18, 1.0], &[], 0.0, &block, 0, &chart).unwrap();
        assert!(
            folded.folding,
            "chart must report folding as q[1] -> 0: {folded:?}"
        );
        assert!(
            folded.margin,
            "a folding chart is also within the margin: {folded:?}"
        );
        assert!(
            folded.rcond < regular.rcond,
            "the fold must be worse conditioned than the regular point: {folded:?} vs {regular:?}"
        );
    }

    #[test]
    fn margin_fires_before_folding_on_the_approach() {
        let model = model();
        let block = circle_block();
        let chart = chart_reconstructing_q1();

        // Approaching the fold: still full rank by the pivot test, but the
        // reciprocal conditioning has fallen under the safety margin.
        let approaching =
            probe_chart_conditioning(&model, &[1.0e-12, 1.0], &[], 0.0, &block, 0, &chart).unwrap();
        assert!(
            !approaching.folding,
            "the chart is still regular on approach: {approaching:?}"
        );
        assert!(
            approaching.margin,
            "the margin must fire before the fold: {approaching:?}"
        );
    }

    #[test]
    fn alternate_chart_stays_regular_across_the_fold() {
        let model = model();
        let block = circle_block();
        let alternate = chart_reconstructing_q2();

        // At the same quarter turn the alternate chart reconstructs q[2], whose
        // g_d = 2*q[2] ~ 2 is far from singular, so it neither folds nor margins.
        let conditioning =
            probe_chart_conditioning(&model, &[1.0e-18, 1.0], &[], 0.0, &block, 0, &alternate)
                .unwrap();
        assert!(
            !conditioning.folding,
            "the alternate chart must remain regular at the fold: {conditioning:?}"
        );
        assert!(
            !conditioning.margin,
            "the alternate chart must clear the margin at the fold: {conditioning:?}"
        );
    }
}
