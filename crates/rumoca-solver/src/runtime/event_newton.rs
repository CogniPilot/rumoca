use nalgebra::{DMatrix, DVector};

use super::solve_ops::RuntimeSolveError;

const FINITE_DIFFERENCE_RELATIVE_STEP: f64 = 1.490_116_119_384_765_6e-8;
const NEWTON_LINE_SEARCH_STEPS: usize = 16;

/// Backend-neutral residual interface for a discrete-frozen event solve.
///
/// The adapter owns the mapping between this dense unknown vector and its
/// runtime `y`/`p` slots. Discrete-valued modes are intentionally absent from
/// the vector, so each Newton solve operates on one fixed event mode.
pub trait CoupledEventNewtonModel {
    fn eval_residual(
        &self,
        unknowns: &[f64],
        residual: &mut [f64],
    ) -> Result<(), RuntimeSolveError>;

    fn variable_scale(&self, _index: usize) -> f64 {
        1.0
    }

    fn residual_scale(&self, _index: usize) -> f64 {
        1.0
    }
}

/// Solve a square event residual system by scaled Newton iteration.
///
/// This is a recovery path after ordinary event fixed-point iteration stalls,
/// so a numerical Jacobian keeps the interface independent of evaluator/AD
/// implementation details. The incoming unknown vector is restored on failure.
pub fn solve_coupled_event_newton<M: CoupledEventNewtonModel>(
    model: &M,
    unknowns: &mut [f64],
    tolerance: f64,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    if unknowns.is_empty() {
        return Ok(());
    }
    validate_newton_options(tolerance, max_iters)?;
    let incoming = unknowns.to_vec();
    let result = solve_coupled_event_newton_inner(model, unknowns, tolerance, max_iters);
    if result.is_err() {
        unknowns.copy_from_slice(&incoming);
    }
    result
}

fn solve_coupled_event_newton_inner<M: CoupledEventNewtonModel>(
    model: &M,
    unknowns: &mut [f64],
    tolerance: f64,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    let count = unknowns.len();
    let variable_scales = (0..count)
        .map(|index| valid_scale(model.variable_scale(index)))
        .collect::<Vec<_>>();
    let fallback_scales = (0..count)
        .map(|index| valid_scale(model.residual_scale(index)))
        .collect::<Vec<_>>();
    let mut residual = vec![0.0; count];
    for iteration in 0..max_iters {
        eval_finite_residual(model, unknowns, &mut residual)?;
        let jacobian = finite_difference_jacobian(model, unknowns, &residual, &variable_scales)?;
        let row_scales = jacobian_row_scales(&jacobian, &variable_scales, &fallback_scales);
        if scaled_residual_norm(&residual, &row_scales) <= tolerance {
            tracing::debug!(
                target: "rumoca_solver::event_newton",
                iteration,
                "coupled event Newton solve converged"
            );
            return Ok(());
        }
        let delta = scaled_newton_delta(
            &jacobian,
            &residual,
            &row_scales,
            &variable_scales,
            tolerance,
        )
        .ok_or_else(|| coupled_event_error("Jacobian is singular"))?;
        if !accept_newton_delta(model, unknowns, delta.as_slice(), &row_scales, tolerance)? {
            return Err(coupled_event_error(
                "line search could not reduce the residual",
            ));
        }
    }
    eval_finite_residual(model, unknowns, &mut residual)?;
    let jacobian = finite_difference_jacobian(model, unknowns, &residual, &variable_scales)?;
    let row_scales = jacobian_row_scales(&jacobian, &variable_scales, &fallback_scales);
    if scaled_residual_norm(&residual, &row_scales) <= tolerance {
        return Ok(());
    }
    Err(coupled_event_error(&format!(
        "iteration limit {max_iters} was reached with residual norm {}",
        residual
            .iter()
            .map(|value| value.abs())
            .fold(0.0_f64, f64::max)
    )))
}

fn validate_newton_options(tolerance: f64, max_iters: usize) -> Result<(), RuntimeSolveError> {
    if tolerance.is_finite() && tolerance > 0.0 && max_iters > 0 {
        return Ok(());
    }
    Err(coupled_event_error(
        "tolerance must be finite and positive and max_iters must be nonzero",
    ))
}

fn eval_finite_residual<M: CoupledEventNewtonModel>(
    model: &M,
    unknowns: &[f64],
    residual: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    model.eval_residual(unknowns, residual)?;
    if residual.iter().all(|value| value.is_finite()) {
        return Ok(());
    }
    Err(coupled_event_error(
        "residual evaluation produced a non-finite value",
    ))
}

fn finite_difference_jacobian<M: CoupledEventNewtonModel>(
    model: &M,
    unknowns: &[f64],
    residual: &[f64],
    variable_scales: &[f64],
) -> Result<DMatrix<f64>, RuntimeSolveError> {
    let count = unknowns.len();
    let mut jacobian = DMatrix::zeros(count, count);
    let mut probe = unknowns.to_vec();
    let mut probe_residual = vec![0.0; count];
    for column in 0..count {
        let step = finite_difference_step(unknowns[column], variable_scales[column]);
        probe[column] = unknowns[column] + step;
        let forward = eval_finite_residual(model, &probe, &mut probe_residual);
        let direction = if let Err(forward_error) = forward {
            probe[column] = unknowns[column] - step;
            if eval_finite_residual(model, &probe, &mut probe_residual).is_err() {
                return Err(forward_error);
            }
            -1.0
        } else {
            1.0
        };
        write_finite_difference_column(
            &mut jacobian,
            column,
            &probe_residual,
            residual,
            direction * step,
        );
        probe[column] = unknowns[column];
    }
    Ok(jacobian)
}

fn write_finite_difference_column(
    jacobian: &mut DMatrix<f64>,
    column: usize,
    probe_residual: &[f64],
    residual: &[f64],
    step: f64,
) {
    for row in 0..jacobian.nrows() {
        jacobian[(row, column)] = (probe_residual[row] - residual[row]) / step;
    }
}

fn finite_difference_step(value: f64, scale: f64) -> f64 {
    let magnitude = value.abs().max(valid_scale(scale));
    FINITE_DIFFERENCE_RELATIVE_STEP * magnitude
}

fn jacobian_row_scales(
    jacobian: &DMatrix<f64>,
    variable_scales: &[f64],
    fallback_scales: &[f64],
) -> Vec<f64> {
    (0..jacobian.nrows())
        .map(|row| {
            let derivative_scale = (0..jacobian.ncols()).fold(0.0_f64, |scale, column| {
                let contribution = jacobian[(row, column)].abs() * variable_scales[column];
                if contribution.is_finite() {
                    scale.max(contribution)
                } else {
                    scale
                }
            });
            if derivative_scale > 0.0 {
                derivative_scale
            } else {
                fallback_scales[row]
            }
        })
        .collect()
}

fn scaled_newton_delta(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    row_scales: &[f64],
    variable_scales: &[f64],
    tolerance: f64,
) -> Option<DVector<f64>> {
    let mut scaled_jacobian = jacobian.clone();
    for row in 0..scaled_jacobian.nrows() {
        for column in 0..scaled_jacobian.ncols() {
            scaled_jacobian[(row, column)] *= variable_scales[column] / row_scales[row];
        }
    }
    let rhs = DVector::from_iterator(
        residual.len(),
        residual
            .iter()
            .zip(row_scales)
            .map(|(value, scale)| -value / scale),
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
            .zip(variable_scales)
            .map(|(value, scale)| value * scale),
    ))
}

fn accept_newton_delta<M: CoupledEventNewtonModel>(
    model: &M,
    unknowns: &mut [f64],
    delta: &[f64],
    row_scales: &[f64],
    tolerance: f64,
) -> Result<bool, RuntimeSolveError> {
    let incoming = unknowns.to_vec();
    let mut before_residual = vec![0.0; unknowns.len()];
    eval_finite_residual(model, unknowns, &mut before_residual)?;
    let before_norm = scaled_residual_norm(&before_residual, row_scales);
    let mut trial_residual = vec![0.0; unknowns.len()];
    let mut fraction = 1.0;
    for _ in 0..NEWTON_LINE_SEARCH_STEPS {
        for ((slot, base), update) in unknowns.iter_mut().zip(&incoming).zip(delta) {
            *slot = base + fraction * update;
        }
        if eval_finite_residual(model, unknowns, &mut trial_residual).is_err() {
            fraction *= 0.5;
            continue;
        }
        let after_norm = scaled_residual_norm(&trial_residual, row_scales);
        if after_norm <= tolerance || after_norm < before_norm {
            return Ok(true);
        }
        fraction *= 0.5;
    }
    unknowns.copy_from_slice(&incoming);
    Ok(false)
}

fn scaled_residual_norm(residual: &[f64], row_scales: &[f64]) -> f64 {
    residual
        .iter()
        .zip(row_scales)
        .map(|(value, scale)| value.abs() / valid_scale(*scale))
        .fold(0.0_f64, f64::max)
}

fn valid_scale(value: f64) -> f64 {
    if value.is_finite() && value > 0.0 {
        value
    } else {
        1.0
    }
}

fn coupled_event_error(reason: &str) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!("coupled event Newton solve failed: {reason}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    struct OscillatingEventModel;

    impl CoupledEventNewtonModel for OscillatingEventModel {
        fn eval_residual(
            &self,
            unknowns: &[f64],
            residual: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            let z = unknowns[0];
            let d = unknowns[1];
            residual[0] = z - d;
            residual[1] = d - (2.0 - z);
            Ok(())
        }
    }

    #[test]
    fn coupled_event_newton_recovers_oscillating_fixed_point() {
        let mut unknowns = [0.0, 0.0];

        solve_coupled_event_newton(&OscillatingEventModel, &mut unknowns, 1.0e-12, 1)
            .expect("the square coupled system should converge");

        assert!((unknowns[0] - 1.0).abs() < 1.0e-10);
        assert!((unknowns[1] - 1.0).abs() < 1.0e-10);
    }

    struct SingularEventModel;

    impl CoupledEventNewtonModel for SingularEventModel {
        fn eval_residual(
            &self,
            _unknowns: &[f64],
            residual: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            residual.fill(1.0);
            Ok(())
        }
    }

    #[test]
    fn coupled_event_newton_restores_unknowns_on_failure() {
        let mut unknowns = [3.0, 4.0];

        let error = solve_coupled_event_newton(&SingularEventModel, &mut unknowns, 1.0e-12, 4)
            .expect_err("a singular residual system must fail loudly");

        assert!(
            error
                .to_string()
                .contains("coupled event Newton solve failed")
        );
        assert_eq!(unknowns, [3.0, 4.0]);
    }
}
