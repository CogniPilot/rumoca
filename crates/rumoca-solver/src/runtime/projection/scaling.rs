use faer::{
    Col,
    prelude::Solve,
    sparse::{
        SparseColMat, Triplet,
        linalg::solvers::{Lu, SymbolicLu},
    },
};
use nalgebra::{DMatrix, DVector};
use rumoca_eval_solve::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};
use rumoca_ir_solve as solve;

use super::{
    AlgebraicProjectionModel, ImplicitProjectionModel, RuntimeSolveError, algebraic_block_jacobian,
    initial_block_jacobian, y_index_for_slot,
};

#[derive(Clone, Debug, Default)]
pub(crate) struct SparseNewtonCache {
    symbolic_lu: Option<SymbolicLu<usize>>,
    matrix: Option<SparseColMat<usize, f64>>,
    coordinates: Box<[(usize, usize)]>,
    factor_values: Box<[u64]>,
    factorization: Option<Lu<usize, f64>>,
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

fn valid_variable_scale(scale: f64) -> f64 {
    if scale.is_finite() && scale > 0.0 {
        scale
    } else {
        1.0
    }
}

pub(super) fn model_variable_scale<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    index: usize,
) -> f64 {
    valid_variable_scale(model.variable_scale_for_y_index(index))
}

pub(super) fn algebraic_block_scales<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index))
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
                    |index| model_variable_scale(model, index),
                )
        })
        .collect::<Vec<_>>();
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure);
    (row_scales, variable_scales)
}

fn initial_block_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index))
        .collect::<Vec<_>>();
    let fallback_scales = initial_block_fallback_scales(model, block, &variable_scales);
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure);
    (row_scales, variable_scales)
}

pub(super) fn initial_block_fallback_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
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
                    |index| model_variable_scale(model, index),
                )
        })
        .collect()
}

pub(super) fn jacobian_row_scales(
    jacobian: &DMatrix<f64>,
    variable_scales: &[f64],
    fallback_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
) -> Vec<f64> {
    if let Some(pattern) = structure.filter(|pattern| {
        pattern.rows() as usize == jacobian.nrows()
            && pattern.columns() as usize == jacobian.ncols()
    }) {
        return sparse_jacobian_row_scales(jacobian, variable_scales, fallback_scales, pattern);
    }
    (0..jacobian.nrows())
        .map(|row| {
            let derivative_scale = (0..jacobian.ncols()).fold(0.0_f64, |scale, column| {
                let contribution =
                    jacobian[(row, column)].abs() * valid_variable_scale(variable_scales[column]);
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
        .collect()
}

fn sparse_jacobian_row_scales(
    jacobian: &DMatrix<f64>,
    variable_scales: &[f64],
    fallback_scales: &[f64],
    pattern: &solve::StructuralPattern,
) -> Vec<f64> {
    let mut scales = vec![0.0_f64; jacobian.nrows()];
    for (row, column) in pattern.nonzero_coordinates() {
        let contribution =
            jacobian[(row, column)].abs() * valid_variable_scale(variable_scales[column]);
        if contribution.is_finite() {
            scales[row] = scales[row].max(contribution);
        }
    }
    for (row, scale) in scales.iter_mut().enumerate() {
        if *scale == 0.0 {
            *scale = fallback_scales.get(row).copied().unwrap_or(1.0);
        }
    }
    scales
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
        let jacobian =
            algebraic_block_jacobian(model, y, p, t, &block.rows, &block.y_indices, structure)?;
        scales.extend(
            algebraic_block_scales(
                model,
                block,
                &jacobian,
                structure.map(solve::JacobianStructure::pattern),
            )
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
                .map_or(1.0, |index| model_variable_scale(model, index))
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
        let block_scales = initial_block_scales(model, block, &jacobian, structure).0;
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

pub(crate) fn scaled_newton_delta(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
    tolerance: f64,
) -> Option<DVector<f64>> {
    scaled_newton_delta_impl(
        jacobian,
        residual,
        row_scales,
        variable_scales,
        structure,
        tolerance,
        None,
        true,
    )
}

pub(crate) fn scaled_newton_delta_with_cache(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
    tolerance: f64,
    cache: &mut SparseNewtonCache,
) -> Option<DVector<f64>> {
    scaled_newton_delta_impl(
        jacobian,
        residual,
        row_scales,
        variable_scales,
        structure,
        tolerance,
        Some(cache),
        true,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn scaled_unique_delta(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
    tolerance: f64,
    cache: Option<&mut SparseNewtonCache>,
) -> Option<DVector<f64>> {
    scaled_newton_delta_impl(
        jacobian,
        residual,
        row_scales,
        variable_scales,
        structure,
        tolerance,
        cache,
        false,
    )
}

#[allow(clippy::too_many_arguments)]
fn scaled_newton_delta_impl(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
    tolerance: f64,
    cache: Option<&mut SparseNewtonCache>,
    allow_rank_deficient_fallback: bool,
) -> Option<DVector<f64>> {
    if jacobian.nrows() != residual.len()
        || jacobian.nrows() != row_scales.len()
        || jacobian.ncols() != variable_scales.len()
    {
        return None;
    }
    let rhs = DVector::from_iterator(
        residual.len(),
        residual
            .iter()
            .copied()
            .zip(row_scales.iter().copied())
            .map(|(value, scale)| -value / valid_variable_scale(scale)),
    );
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
    let scaled_jacobian = scaled_jacobian(jacobian, row_scales, variable_scales);
    let direct = solve_square_newton_system(&scaled_jacobian, &rhs);
    let scaled_delta = if allow_rank_deficient_fallback {
        direct.or_else(|| scaled_jacobian.svd(true, true).solve(&rhs, tolerance).ok())?
    } else {
        direct?
    };
    Some(unscale_newton_delta(&scaled_delta, variable_scales))
}

fn solve_square_newton_system(matrix: &DMatrix<f64>, rhs: &DVector<f64>) -> Option<DVector<f64>> {
    if matrix.nrows() != matrix.ncols() {
        return None;
    }
    matrix.clone().lu().solve(rhs)
}

fn scaled_jacobian(
    jacobian: &DMatrix<f64>,
    row_scales: &[f64],
    variable_scales: &[f64],
) -> DMatrix<f64> {
    DMatrix::from_fn(jacobian.nrows(), jacobian.ncols(), |row, column| {
        jacobian[(row, column)] * valid_variable_scale(variable_scales[column])
            / valid_variable_scale(row_scales[row])
    })
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
    matrix: &DMatrix<f64>,
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
        return solve_cached_sparse_matrix(
            matrix,
            rhs,
            row_scales,
            variable_scales,
            structure,
            cache,
        );
    }
    let triplets = structure
        .nonzero_coordinates()
        .into_iter()
        .map(|(row, column)| {
            let value = matrix[(row, column)] * valid_variable_scale(variable_scales[column])
                / valid_variable_scale(row_scales[row]);
            Triplet::new(row, column, value)
        })
        .collect::<Vec<_>>();
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

fn solve_cached_sparse_matrix(
    source: &DMatrix<f64>,
    rhs: &DVector<f64>,
    row_scales: &[f64],
    variable_scales: &[f64],
    structure: &solve::StructuralPattern,
    cache: &mut SparseNewtonCache,
) -> Option<DVector<f64>> {
    prepare_sparse_cache(source.nrows(), structure, cache)?;
    let coordinates = &cache.coordinates;
    let sparse = cache.matrix.as_mut()?;
    for (value, &(row, column)) in sparse.val_mut().iter_mut().zip(coordinates) {
        *value = source[(row, column)] * valid_variable_scale(variable_scales[column])
            / valid_variable_scale(row_scales[row]);
    }
    let values_changed = sparse.val().len() != cache.factor_values.len()
        || sparse
            .val()
            .iter()
            .zip(cache.factor_values.iter())
            .any(|(value, cached)| value.to_bits() != *cached);
    if values_changed {
        let symbolic = cache.symbolic_lu.as_ref()?.clone();
        cache.factorization = Lu::try_new_with_symbolic(symbolic, sparse.as_ref()).ok();
        cache.factor_values = sparse
            .val()
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>()
            .into_boxed_slice();
    }
    solve_with_sparse_factor(cache.factorization.as_ref()?, rhs)
}

fn prepare_sparse_cache(
    dimension: usize,
    structure: &solve::StructuralPattern,
    cache: &mut SparseNewtonCache,
) -> Option<()> {
    if cache.matrix.is_some() {
        return Some(());
    }
    let triplets = structure
        .nonzero_coordinates()
        .into_iter()
        .map(|(row, column)| Triplet::new(row, column, 0.0))
        .collect::<Vec<_>>();
    let sparse =
        SparseColMat::<usize, f64>::try_new_from_triplets(dimension, dimension, &triplets).ok()?;
    let coordinates = sparse
        .as_ref()
        .triplet_iter()
        .map(|entry| (entry.row, entry.col))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    cache.symbolic_lu = SymbolicLu::try_new(sparse.symbolic()).ok();
    cache.coordinates = coordinates;
    cache.matrix = Some(sparse);
    Some(())
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
