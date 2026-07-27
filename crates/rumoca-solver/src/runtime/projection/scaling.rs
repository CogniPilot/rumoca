use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve as solve;

use super::{
    AlgebraicProjectionModel, ImplicitProjectionModel, RuntimeSolveError, algebraic_block_jacobian,
    initial_block_jacobian, y_index_for_slot,
};

pub(super) fn scaled_residual_converged(residual: &[f64], scales: &[f64], tol: f64) -> bool {
    residual.len() == scales.len()
        && residual
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
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales);
    (row_scales, variable_scales)
}

fn initial_block_scales<M: AlgebraicProjectionModel + ?Sized>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
    jacobian: &DMatrix<f64>,
) -> (Vec<f64>, Vec<f64>) {
    let variable_scales = block
        .y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index))
        .collect::<Vec<_>>();
    let fallback_scales = initial_block_fallback_scales(model, block, &variable_scales);
    let row_scales = jacobian_row_scales(jacobian, &variable_scales, &fallback_scales);
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
) -> Vec<f64> {
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

pub(super) fn algebraic_plan_row_scales<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut scales = Vec::new();
    for block in &plan.blocks {
        let jacobian = algebraic_block_jacobian(model, y, p, t, &block.rows, &block.y_indices)?;
        scales.extend(algebraic_block_scales(model, block, &jacobian).0);
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
    for block in &plan.blocks {
        let jacobian = initial_block_jacobian(
            model,
            y,
            p,
            t,
            &block.rows,
            &block.y_indices,
            &full_residual,
        )?;
        let block_scales = initial_block_scales(model, block, &jacobian).0;
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

pub(super) fn scaled_newton_delta(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    tolerance: f64,
) -> Option<DVector<f64>> {
    if jacobian.nrows() != residual.len()
        || jacobian.nrows() != row_scales.len()
        || jacobian.ncols() != variable_scales.len()
    {
        return None;
    }
    let mut scaled_jacobian = jacobian.clone();
    for row in 0..scaled_jacobian.nrows() {
        let row_scale = valid_variable_scale(row_scales[row]);
        for column in 0..scaled_jacobian.ncols() {
            scaled_jacobian[(row, column)] *=
                valid_variable_scale(variable_scales[column]) / row_scale;
        }
    }
    let rhs = DVector::from_iterator(
        residual.len(),
        residual
            .iter()
            .copied()
            .zip(row_scales.iter().copied())
            .map(|(value, scale)| -value / valid_variable_scale(scale)),
    );
    let scaled_delta = scaled_jacobian
        .clone()
        .lu()
        .solve(&rhs)
        .or_else(|| scaled_jacobian.svd(true, true).solve(&rhs, tolerance).ok())?;
    Some(DVector::from_iterator(
        scaled_delta.len(),
        scaled_delta
            .iter()
            .copied()
            .zip(variable_scales.iter().copied())
            .map(|(value, scale)| value * valid_variable_scale(scale)),
    ))
}
