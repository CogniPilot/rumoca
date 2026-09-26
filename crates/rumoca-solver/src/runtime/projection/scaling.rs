//! Row scaling and the scaled Newton system for the algebraic projection.
//!
//! Scaling rows by their own magnitude before measuring convergence, so that a
//! residual test is a statement about the unknowns rather than about the units
//! the equations happen to be written in, is J. E. Dennis Jr. and R. B.
//! Schnabel, "Numerical Methods for Unconstrained Optimization and Nonlinear
//! Equations", SIAM 1996, section 7.2 and the scaled stopping criteria of
//! section 7.2.1.

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
    algebraic_block_jacobian, initial_block_jacobian, y_index_for_slot,
};

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

/// The solver-Y coordinate whose scale is each row's fallback: its implicit
/// target, if it has one in solver Y.
pub(super) fn fallback_targets<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
) -> Vec<Option<usize>> {
    let mut targets = Vec::with_capacity(block.rows.len());
    for &row in &block.rows {
        targets.push(match model.implicit_target(row) {
            Some(slot) => y_index_for_slot(slot),
            None => None,
        });
    }
    targets
}

pub(super) fn algebraic_block_scales<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();
    let fallback_scales = fallback_targets(model, block)
        .into_iter()
        .enumerate()
        .map(|(offset, target)| {
            target.map_or_else(
                || variable_scales.get(offset).copied().unwrap_or(1.0),
                |index| model_variable_scale(model, index, y[index]),
            )
        })
        .collect::<Vec<_>>();
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure);
    (row_scales, variable_scales)
}

fn initial_block_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();
    let fallback_scales = initial_block_fallback_scales(model, y, block, &variable_scales);
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales, structure);
    (row_scales, variable_scales)
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
    for (row, scale) in scales.iter_mut().enumerate() {
        pattern.visit_row_columns(row, |column| {
            let contribution =
                jacobian[(row, column)].abs() * valid_variable_scale(variable_scales[column]);
            if contribution.is_finite() {
                *scale = scale.max(contribution);
            }
        });
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
                y,
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
        let block_scales = initial_block_scales(model, y, block, &jacobian, structure).0;
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
    pub(crate) jacobian: &'a DMatrix<f64>,
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
    layout: &solve::AffineEliminationLayout,
) -> Option<DVector<f64>> {
    if system.structure != Some(layout.pattern())
        || system.jacobian.nrows() != system.residual.len()
        || system.jacobian.nrows() != system.row_scales.len()
        || system.jacobian.ncols() != system.variable_scales.len()
        || rumoca_eval_solve::projection_policy::affine_elimination_capacity(layout).is_none()
    {
        return None;
    }
    let rhs = scaled_newton_rhs(system.residual, system.row_scales);
    let delta = cache.solve_torn_scaled(
        system.jacobian,
        &rhs,
        system.row_scales,
        system.variable_scales,
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
    mut cache: Option<&mut SparseNewtonCache>,
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
            sparse_scaled_newton_delta(
                jacobian,
                &rhs,
                row_scales,
                variable_scales,
                pattern,
                cache.as_deref_mut(),
            )
        })
        .flatten()
    });
    if let Some(scaled_delta) = sparse {
        return Some(unscale_newton_delta(&scaled_delta, variable_scales));
    }
    if let Some(cache) = cache {
        // The block cache keys its dense factorization on the exact Jacobian
        // and scale bits, so a fixed system (the affine solve and its
        // refinement) factors once and solves every residual bit-identically.
        let scaled_delta = cache.solve_dense_scaled(
            jacobian,
            &rhs,
            (row_scales, variable_scales),
            tolerance,
            allow_rank_deficient_fallback,
        )?;
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

pub(super) fn scaled_jacobian(
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
        return cache.solve_scaled(matrix, rhs, row_scales, variable_scales, structure);
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

/// Largest finite magnitude of each row of `jacobian` over its structural
/// entries (every column without a pattern).
/// Whether each row's scale at `variable_scales` comes from a nonzero finite
/// contribution rather than its fallback.
pub(super) fn jacobian_row_derived(
    jacobian: &DMatrix<f64>,
    variable_scales: &[f64],
    structure: Option<&solve::StructuralPattern>,
) -> Vec<bool> {
    (0..jacobian.nrows())
        .map(|row| jacobian_row_scale(jacobian, row, variable_scales, 0.0, structure) > 0.0)
        .collect()
}

pub(super) fn jacobian_row_magnitudes(
    jacobian: &DMatrix<f64>,
    structure: Option<&solve::StructuralPattern>,
) -> Vec<f64> {
    let pattern = structure.filter(|pattern| {
        pattern.rows() as usize == jacobian.nrows()
            && pattern.columns() as usize == jacobian.ncols()
    });
    (0..jacobian.nrows())
        .map(|row| {
            let mut magnitude = 0.0_f64;
            let mut visit = |column: usize| {
                let value = jacobian[(row, column)].abs();
                if value.is_finite() {
                    magnitude = magnitude.max(value);
                }
            };
            match pattern {
                Some(pattern) => pattern.visit_row_columns(row, &mut visit),
                None => (0..jacobian.ncols()).for_each(visit),
            }
            magnitude
        })
        .collect()
}

/// One row's scale exactly as [`jacobian_row_scales`] forms it.
fn jacobian_row_scale(
    jacobian: &DMatrix<f64>,
    row: usize,
    variable_scales: &[f64],
    fallback: f64,
    structure: Option<&solve::StructuralPattern>,
) -> f64 {
    let pattern = structure.filter(|pattern| {
        pattern.rows() as usize == jacobian.nrows()
            && pattern.columns() as usize == jacobian.ncols()
    });
    let mut scale = 0.0_f64;
    let mut visit = |column: usize| {
        let contribution =
            jacobian[(row, column)].abs() * valid_variable_scale(variable_scales[column]);
        if contribution.is_finite() {
            scale = scale.max(contribution);
        }
    };
    match pattern {
        Some(pattern) => pattern.visit_row_columns(row, &mut visit),
        None => (0..jacobian.ncols()).for_each(visit),
    }
    if scale > 0.0 { scale } else { fallback }
}

/// A fixed block Jacobian with the row scales it had at the arithmetic
/// origin, where every block unknown is zero.
pub(super) struct OriginRowScales<'a> {
    pub(super) jacobian: &'a DMatrix<f64>,
    pub(super) structure: Option<&'a solve::StructuralPattern>,
    /// [`algebraic_block_scales`] at the origin.
    pub(super) scales: &'a [f64],
    /// [`jacobian_row_magnitudes`] of the same Jacobian.
    pub(super) magnitudes: &'a [f64],
    /// [`jacobian_row_derived`] at the origin variable scales.
    pub(super) derived: &'a [bool],
}

/// The model data the origin-bounded certificate reads, resolved once per
/// affine projection: each block unknown's y index and declared scale, and
/// each row's fallback coordinate (its implicit target, or the unknown at the
/// same offset) with that coordinate's declared scale.
pub(super) struct CertificateScales {
    pub(super) unknowns: Vec<(usize, f64)>,
    pub(super) fallbacks: Vec<Option<(usize, f64)>>,
}

/// [`model_variable_scale`] of a coordinate with declared scale `declared`.
fn declared_variable_scale(declared: f64, current_value: f64) -> f64 {
    let current_magnitude = if current_value.is_finite() {
        current_value.abs()
    } else {
        0.0
    };
    valid_variable_scale(declared).max(current_magnitude)
}

/// Whether `residual` converges under the row scales [`algebraic_block_scales`]
/// gives at `y`, decided without forming every row scale.
///
/// A block unknown's scale `max(nominal, |y|)` never falls below its origin
/// value, and a row target outside the block keeps its value, so every
/// finite contribution `|J| * scale` is at least its origin value while no
/// contribution overflows (bounded by the row's largest magnitude times the
/// largest unknown scale). A row whose origin scale came from a nonzero
/// contribution, or which has no nonzero entry, and which is within
/// tolerance of that origin scale, therefore converges under its scale at
/// `y`; any other row forms that scale exactly. The decision equals
/// [`scaled_residual_converged`] over the full scales.
pub(super) fn origin_bounded_residual_converged(
    y: &[f64],
    scales: &CertificateScales,
    origin: &OriginRowScales<'_>,
    residual: &[f64],
    tol: f64,
) -> bool {
    if residual.len() != origin.scales.len() {
        return false;
    }
    let mut variable_scales = Vec::with_capacity(scales.unknowns.len());
    for &(index, declared) in &scales.unknowns {
        variable_scales.push(declared_variable_scale(declared, y[index]));
    }
    let mut largest = 0.0_f64;
    for &scale in &variable_scales {
        largest = largest.max(valid_variable_scale(scale));
    }
    for (row, &value) in residual.iter().enumerate() {
        // A contribution only grows from the origin, so an origin scale formed
        // from a nonzero contribution bounds the scale at `y` from below. A row
        // on its fallback at the origin with a nonzero entry may form a
        // smaller scale from contributions at `y` (an underflowed product), so
        // only a row with no nonzero entry keeps its growing fallback.
        let bounded = (origin.magnitudes[row] * largest).is_finite()
            && (origin.derived[row] || origin.magnitudes[row] == 0.0);
        if bounded && value.abs() <= scaled_tolerance(tol, origin.scales[row]) {
            continue;
        }
        let fallback = match scales.fallbacks[row] {
            Some((index, declared)) => declared_variable_scale(declared, y[index]),
            None => variable_scales.get(row).copied().unwrap_or(1.0),
        };
        let scale = jacobian_row_scale(
            origin.jacobian,
            row,
            &variable_scales,
            fallback,
            origin.structure,
        );
        if !(value.is_finite() && value.abs() <= scaled_tolerance(tol, scale)) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod origin_bounded_tests;
