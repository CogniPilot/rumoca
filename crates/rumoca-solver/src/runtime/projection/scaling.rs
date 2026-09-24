use super::JacobianMatrix;
use faer::{
    Col,
    prelude::Solve,
    sparse::{SparseColMat, Triplet, linalg::solvers::Lu},
};
use nalgebra::{DMatrix, DVector};
use rumoca_eval_solve::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};
use rumoca_ir_solve as solve;

use super::{
    AlgebraicProjectionModel, ImplicitProjectionModel, RuntimeSolveError, SparseNewtonCache,
    algebraic_block_jacobian_storage, initial_block_jacobian, y_index_for_slot,
};

#[cfg(test)]
mod dense_ownership_tests;

#[cfg(test)]
thread_local! {
    static SCALED_DENSE_ALLOCATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
pub(super) fn scaled_dense_allocation_count() -> usize {
    SCALED_DENSE_ALLOCATIONS.get()
}

pub(super) fn scaled_residual_converged(residual: &[f64], scales: &[f64], tol: f64) -> bool {
    residual.len() == scales.len()
        && residual
            .iter()
            .copied()
            .zip(scales.iter().copied())
            .all(|(value, scale)| value.is_finite() && value.abs() <= scaled_tolerance(tol, scale))
}

pub(super) fn scaled_correction_converged(correction: &[f64], scales: &[f64], tol: f64) -> bool {
    correction.len() == scales.len()
        && correction
            .iter()
            .copied()
            .zip(scales.iter().copied())
            .all(|(value, scale)| value.is_finite() && value.abs() <= scaled_tolerance(tol, scale))
}

pub(super) fn scaled_residual_norm(residual: &[f64], scales: &[f64]) -> f64 {
    if residual.len() != scales.len() {
        return f64::INFINITY;
    }
    residual
        .iter()
        .copied()
        .zip(scales.iter().copied())
        .map(|(value, scale)| value.abs() / valid_variable_scale(scale))
        .try_fold(0.0, |acc, value| {
            value.is_finite().then(|| f64::max(acc, value))
        })
        .unwrap_or(f64::INFINITY)
}

pub(super) fn scaled_tolerance(tol: f64, scale: f64) -> f64 {
    let scaled = tol.abs() * valid_variable_scale(scale);
    if scaled.is_finite() {
        scaled.max(f64::MIN_POSITIVE)
    } else {
        f64::MAX
    }
}

pub(super) fn valid_variable_scale(scale: f64) -> f64 {
    if scale.is_finite() && scale > 0.0 {
        scale
    } else {
        1.0
    }
}

pub(super) fn model_variable_scale<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    index: usize,
    current_value: f64,
) -> f64 {
    let current_magnitude = if current_value.is_finite() {
        current_value.abs()
    } else {
        0.0
    };
    valid_variable_scale(model.variable_scale_for_y_index(index)).max(current_magnitude)
}

pub(super) fn algebraic_block_scales<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &dyn JacobianMatrix,
    structure: Option<&solve::StructuralPattern>,
) -> Result<(Vec<f64>, Vec<f64>), RuntimeSolveError> {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();
    let fallback_scales = block
        .rows
        .iter()
        .enumerate()
        .map(|(offset, &row)| {
            model
                .implicit_target(row)
                .and_then(y_index_for_slot)
                .map_or_else(
                    || variable_scales.get(offset).copied().unwrap_or(1.0),
                    |index| model_variable_scale(model, index, y[index]),
                )
        })
        .collect::<Vec<_>>();
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure)?;
    Ok((row_scales, variable_scales))
}

fn initial_block_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &dyn JacobianMatrix,
    structure: Option<&solve::StructuralPattern>,
) -> Result<(Vec<f64>, Vec<f64>), RuntimeSolveError> {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();
    let fallback_scales = initial_block_fallback_scales(model, y, block, &variable_scales);
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure)?;
    Ok((row_scales, variable_scales))
}

pub(super) fn initial_block_fallback_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    variable_scales: &[f64],
) -> Vec<f64> {
    block
        .rows
        .iter()
        .enumerate()
        .map(|(offset, &row)| {
            model
                .initial_target(row)
                .and_then(y_index_for_slot)
                .map_or_else(
                    || variable_scales.get(offset).copied().unwrap_or(1.0),
                    |index| model_variable_scale(model, index, y[index]),
                )
        })
        .collect()
}

pub(super) fn jacobian_row_scales(
    jacobian: &dyn JacobianMatrix,
    variable_scales: &[f64],
    fallback_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
) -> Result<Vec<f64>, RuntimeSolveError> {
    if let Some(layout) = jacobian.value_layout()
        && structure.is_some_and(|pattern| pattern != layout.pattern())
    {
        return Err(RuntimeSolveError::solve_ir(
            "Jacobian scaling pattern differs from stored source",
        ));
    }
    if let Some(layout) = jacobian.value_layout() {
        return sparse_jacobian_row_scales(
            jacobian,
            variable_scales,
            fallback_scales,
            layout.pattern(),
        );
    }
    if let Some(pattern) = structure.filter(|pattern| {
        pattern.rows() as usize == jacobian.nrows()
            && pattern.columns() as usize == jacobian.ncols()
    }) {
        return sparse_jacobian_row_scales(jacobian, variable_scales, fallback_scales, pattern);
    }
    Ok((0..jacobian.nrows())
        .map(|row| {
            let derivative_scale = (0..jacobian.ncols()).fold(0.0_f64, |scale, column| {
                let contribution = jacobian.as_slice()[column * jacobian.nrows() + row].abs()
                    * valid_variable_scale(variable_scales[column]);
                if contribution.is_finite() {
                    scale.max(contribution)
                } else {
                    scale
                }
            });
            if derivative_scale > 0.0 {
                derivative_scale
            } else {
                fallback_scales.get(row).copied().unwrap_or(1.0)
            }
        })
        .collect())
}

fn sparse_jacobian_row_scales(
    jacobian: &dyn JacobianMatrix,
    variable_scales: &[f64],
    fallback_scales: &[f64],
    pattern: &solve::StructuralPattern,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut scales = vec![0.0_f64; jacobian.nrows()];
    for (row, scale) in scales.iter_mut().enumerate() {
        let derivative = if let Some(layout) = jacobian.value_layout() {
            compact_row_scale(jacobian.as_slice(), layout, row, variable_scales)?
        } else {
            dense_sparse_row_scale(jacobian, pattern, row, variable_scales)
        };
        *scale = if derivative == 0.0 {
            fallback_scales.get(row).copied().unwrap_or(1.0)
        } else {
            derivative
        };
    }
    Ok(scales)
}

fn finite_scale_max(scale: f64, contribution: f64) -> f64 {
    if contribution.is_finite() {
        scale.max(contribution)
    } else {
        scale
    }
}

fn compact_row_scale(
    values: &[f64],
    layout: &solve::JacobianValueLayout,
    row: usize,
    variable_scales: &[f64],
) -> Result<f64, RuntimeSolveError> {
    let slots = layout
        .row_slots(row)
        .ok_or_else(|| RuntimeSolveError::solve_ir("compact Jacobian row outside owner"))?;
    let mut scale = 0.0_f64;
    for slot in slots {
        let (_, column) = layout
            .coordinate(slot)
            .ok_or_else(|| RuntimeSolveError::solve_ir("compact Jacobian slot outside owner"))?;
        let value = values
            .get(slot)
            .ok_or_else(|| RuntimeSolveError::solve_ir("compact Jacobian value missing"))?;
        let variable = variable_scales
            .get(column)
            .ok_or_else(|| RuntimeSolveError::solve_ir("compact Jacobian scale missing"))?;
        scale = finite_scale_max(scale, value.abs() * valid_variable_scale(*variable));
    }
    Ok(scale)
}

fn dense_sparse_row_scale(
    jacobian: &dyn JacobianMatrix,
    pattern: &solve::StructuralPattern,
    row: usize,
    variable_scales: &[f64],
) -> f64 {
    let mut scale = 0.0_f64;
    pattern.visit_row_columns(row, |column| {
        let contribution = jacobian.as_slice()[column * jacobian.nrows() + row].abs()
            * valid_variable_scale(variable_scales[column]);
        scale = finite_scale_max(scale, contribution);
    });
    scale
}

pub(super) fn algebraic_plan_row_scales<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut scales = Vec::new();
    for (block_index, block) in plan.blocks.iter().enumerate() {
        let structure = model.algebraic_projection_block_structure(block_index);
        let jacobian = algebraic_block_jacobian_storage(
            model,
            (y, p, t),
            (&block.rows, &block.y_indices),
            structure,
        )?;
        scales.extend(
            algebraic_block_scales(
                model,
                y,
                block,
                &jacobian,
                structure.map(solve::JacobianStructure::pattern),
            )?
            .0,
        );
    }
    Ok(scales)
}

pub(super) fn initial_residual_scales<M: AlgebraicProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let residual_len = model.initial_residual_len();
    let mut scales = (0..residual_len)
        .map(|row| {
            model
                .initial_target(row)
                .and_then(y_index_for_slot)
                .map_or(1.0, |index| model_variable_scale(model, index, y[index]))
        })
        .collect::<Vec<_>>();
    let mut full_residual = vec![0.0; residual_len];
    model.eval_initial_residual(y, p, t, &mut full_residual)?;
    for (block_index, block) in plan.blocks.iter().enumerate() {
        let jacobian = initial_block_jacobian(
            model,
            y,
            p,
            t,
            &block.rows,
            &block.y_indices,
            &full_residual,
        )?;
        let structure = model
            .initial_projection_block_structure(block_index)
            .map(solve::JacobianStructure::pattern);
        let block_scales = initial_block_scales(model, y, block, &jacobian, structure)?.0;
        for (&row, scale) in block.rows.iter().zip(block_scales) {
            let Some(slot) = scales.get_mut(row) else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "initial projection scale references row {row}, but the model has only \
                     {residual_len} residual rows"
                )));
            };
            *slot = scale;
        }
    }
    Ok(scales)
}

/// The scaled Newton system of one projection block.
///
/// The Newton step is taken in scaled coordinates: `row_scales` conditions the
/// residual rows and `variable_scales` the unknowns, `structure` is the
/// compiler-derived sparsity that decides which linear kernel solves it, and
/// `tolerance` is the rank tolerance of the dense fallback. The six travel
/// together because a delta scaled by anything other than the scales its
/// Jacobian was formed with is not a Newton step for this block.
#[derive(Clone, Copy)]
pub(crate) struct ScaledNewtonSystem<'a> {
    pub(crate) jacobian: &'a dyn JacobianMatrix,
    pub(crate) residual: &'a [f64],
    pub(crate) row_scales: &'a [f64],
    pub(crate) variable_scales: &'a [f64],
    pub(crate) structure: Option<&'a solve::StructuralPattern>,
    pub(crate) tolerance: f64,
}

pub(crate) fn scaled_newton_delta(system: ScaledNewtonSystem<'_>) -> Option<DVector<f64>> {
    scaled_newton_delta_impl(system, None, true)
}

pub(crate) fn scaled_newton_delta_with_cache(
    system: ScaledNewtonSystem<'_>,
    cache: &mut SparseNewtonCache,
) -> Option<DVector<f64>> {
    scaled_newton_delta_impl(system, Some(cache), true)
}

pub(crate) fn scaled_newton_delta_with_tearing(
    system: ScaledNewtonSystem<'_>,
    cache: &mut SparseNewtonCache,
    candidate: solve::TearingCandidate,
    layout: &solve::AffineEliminationLayout,
) -> Option<DVector<f64>> {
    if system.structure != Some(layout.pattern())
        || system.jacobian.nrows() != system.residual.len()
        || system.jacobian.nrows() != system.row_scales.len()
        || system.jacobian.ncols() != system.variable_scales.len()
        || !matches!(
            select_linear_solve_kernel(layout.tears().len(), layout.reduced_pattern()).ok(),
            Some(LinearSolveKernel::SmallDense)
        )
    {
        return None;
    }
    let rhs = scaled_newton_rhs(system.residual, system.row_scales);
    let delta = cache.solve_torn_scaled(
        system.jacobian,
        &rhs,
        system.row_scales,
        system.variable_scales,
        candidate,
        layout,
    )?;
    Some(unscale_newton_delta(&delta, system.variable_scales))
}

fn scaled_newton_rhs(residual: &[f64], row_scales: &[f64]) -> DVector<f64> {
    DVector::from_iterator(
        residual.len(),
        residual
            .iter()
            .zip(row_scales)
            .map(|(&value, &scale)| -value / valid_variable_scale(scale)),
    )
}

fn scaled_newton_delta_impl(
    system: ScaledNewtonSystem<'_>,
    cache: Option<&mut SparseNewtonCache>,
    allow_rank_deficient_fallback: bool,
) -> Option<DVector<f64>> {
    let ScaledNewtonSystem {
        jacobian,
        residual,
        row_scales,
        variable_scales,
        structure,
        tolerance,
    } = system;
    if jacobian.nrows() != residual.len()
        || jacobian.nrows() != row_scales.len()
        || jacobian.ncols() != variable_scales.len()
        || jacobian
            .value_layout()
            .is_some_and(|layout| structure != Some(layout.pattern()))
    {
        return None;
    }
    let rhs = scaled_newton_rhs(residual, row_scales);
    let sparse = structure.and_then(|pattern| {
        matches!(
            select_linear_solve_kernel(jacobian.nrows(), pattern).ok(),
            Some(LinearSolveKernel::SparseCandidate)
        )
        .then(|| {
            sparse_scaled_newton_delta(jacobian, &rhs, row_scales, variable_scales, pattern, cache)
        })
        .flatten()
    });
    if let Some(scaled_delta) = sparse {
        return Some(unscale_newton_delta(&scaled_delta, variable_scales));
    }
    let matrix = scaled_jacobian(jacobian, row_scales, variable_scales)?;
    let direct = solve_square_newton_system(matrix, &rhs);
    let scaled_delta = if allow_rank_deficient_fallback {
        direct.or_else(|| {
            // LU consumes its local matrix. Rebuild the identical SVD input
            // only when the existing direct solve declined or failed.
            scaled_jacobian(jacobian, row_scales, variable_scales)?
                .svd(true, true)
                .solve(&rhs, tolerance)
                .ok()
        })?
    } else {
        direct?
    };
    Some(unscale_newton_delta(&scaled_delta, variable_scales))
}

fn solve_square_newton_system(matrix: DMatrix<f64>, rhs: &DVector<f64>) -> Option<DVector<f64>> {
    if matrix.nrows() != matrix.ncols() {
        return None;
    }
    #[cfg(test)]
    let input_storage = matrix.as_ptr();
    let factor = matrix.lu();
    #[cfg(test)]
    dense_ownership_tests::observe_lu_storage(input_storage, factor.lu_internal().as_ptr());
    factor.solve(rhs)
}

fn scaled_jacobian(
    jacobian: &dyn JacobianMatrix,
    row_scales: &[f64],
    variable_scales: &[f64],
) -> Option<DMatrix<f64>> {
    let (rows, columns) = jacobian.shape();
    if row_scales.len() != rows || variable_scales.len() != columns {
        return None;
    }
    if jacobian.value_layout().is_some_and(|layout| {
        layout.shape() != (rows, columns) || layout.len() != jacobian.as_slice().len()
    }) {
        return None;
    }
    #[cfg(test)]
    SCALED_DENSE_ALLOCATIONS.set(SCALED_DENSE_ALLOCATIONS.get() + 1);
    let mut scaled = DMatrix::zeros(rows, columns);
    if let Some(layout) = jacobian.value_layout() {
        fill_scaled_compact(
            &mut scaled,
            jacobian.as_slice(),
            layout,
            row_scales,
            variable_scales,
        )?;
    } else {
        for column in 0..columns {
            for row in 0..rows {
                let value = *jacobian
                    .as_slice()
                    .get(column.checked_mul(rows)?.checked_add(row)?)?;
                scaled[(row, column)] = value * valid_variable_scale(variable_scales[column])
                    / valid_variable_scale(row_scales[row]);
            }
        }
    }
    Some(scaled)
}

fn fill_scaled_compact(
    scaled: &mut DMatrix<f64>,
    values: &[f64],
    layout: &solve::JacobianValueLayout,
    row_scales: &[f64],
    variable_scales: &[f64],
) -> Option<()> {
    for row in 0..scaled.nrows() {
        let mut slots = layout.row_slots(row)?;
        let mut next = slots.next();
        for column in 0..scaled.ncols() {
            let value = compact_cell(values, layout, row, column, &mut slots, &mut next)?;
            scaled[(row, column)] = value * valid_variable_scale(variable_scales[column])
                / valid_variable_scale(row_scales[row]);
        }
        if next.is_some() {
            return None;
        }
    }
    Some(())
}

fn compact_cell(
    values: &[f64],
    layout: &solve::JacobianValueLayout,
    row: usize,
    column: usize,
    slots: &mut std::ops::Range<usize>,
    next: &mut Option<usize>,
) -> Option<f64> {
    let Some(slot) = *next else {
        return Some(0.0);
    };
    let (slot_row, slot_column) = layout.coordinate(slot)?;
    if slot_row != row || slot_column < column {
        return None;
    }
    if slot_column != column {
        return Some(0.0);
    }
    *next = slots.next();
    values.get(slot).copied()
}

fn unscale_newton_delta(scaled_delta: &DVector<f64>, variable_scales: &[f64]) -> DVector<f64> {
    DVector::from_iterator(
        scaled_delta.len(),
        scaled_delta
            .iter()
            .copied()
            .zip(variable_scales.iter().copied())
            .map(|(value, scale)| value * valid_variable_scale(scale)),
    )
}

fn sparse_scaled_newton_delta(
    matrix: &dyn JacobianMatrix,
    rhs: &DVector<f64>,
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: &solve::StructuralPattern,
    cache: Option<&mut SparseNewtonCache>,
) -> Option<DVector<f64>> {
    let dimension = matrix.nrows();
    if structure.rows() as usize != dimension || structure.columns() as usize != dimension {
        return None;
    }
    if let Some(cache) = cache {
        return cache.solve_scaled(matrix, rhs, row_scales, variable_scales, structure);
    }
    if matrix
        .value_layout()
        .is_some_and(|layout| layout.pattern() != structure)
    {
        return None;
    }
    let triplets = structure
        .nonzero_coordinates()
        .into_iter()
        .enumerate()
        .map(|(slot, (row, column))| {
            let raw = if let Some(layout) = matrix.value_layout() {
                if layout.coordinate(slot)? != (row, column) {
                    return None;
                }
                *matrix.as_slice().get(slot)?
            } else {
                *matrix
                    .as_slice()
                    .get(column.checked_mul(dimension)?.checked_add(row)?)?
            };
            let value = raw * valid_variable_scale(variable_scales[column])
                / valid_variable_scale(row_scales[row]);
            Some(Triplet::new(row, column, value))
        })
        .collect::<Option<Vec<_>>>()?;
    solve_sparse_triplets(dimension, rhs, &triplets)
}

#[cfg(test)]
pub(super) fn sparse_newton_delta(
    matrix: &DMatrix<f64>,
    rhs: &DVector<f64>,
    structure: &solve::StructuralPattern,
) -> Option<DVector<f64>> {
    let dimension = matrix.nrows();
    if structure.rows() as usize != dimension || structure.columns() as usize != dimension {
        return None;
    }
    let triplets = sparse_triplets(matrix, structure);
    solve_sparse_triplets(dimension, rhs, &triplets)
}

fn solve_sparse_triplets(
    dimension: usize,
    rhs: &DVector<f64>,
    triplets: &[Triplet<usize, usize, f64>],
) -> Option<DVector<f64>> {
    let sparse =
        SparseColMat::<usize, f64>::try_new_from_triplets(dimension, dimension, triplets).ok()?;
    let factorization = sparse.sp_lu().ok()?;
    solve_with_sparse_factor(&factorization, rhs)
}

fn solve_with_sparse_factor(
    factorization: &Lu<usize, f64>,
    rhs: &DVector<f64>,
) -> Option<DVector<f64>> {
    let dimension = rhs.len();
    let sparse_rhs = Col::from_fn(dimension, |row| rhs[row]);
    let solution = factorization.solve(&sparse_rhs);
    solution
        .iter()
        .all(|value| value.is_finite())
        .then(|| DVector::from_iterator(dimension, solution.iter().copied()))
}

#[cfg(test)]
fn sparse_triplets(
    matrix: &DMatrix<f64>,
    structure: &solve::StructuralPattern,
) -> Vec<Triplet<usize, usize, f64>> {
    structure
        .nonzero_coordinates()
        .into_iter()
        .map(|(row, column)| Triplet::new(row, column, matrix[(row, column)]))
        .collect()
}
