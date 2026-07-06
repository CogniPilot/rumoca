use crate::{DifferentiableModel, OptError, TrainableSet};
use std::collections::HashMap;

/// Gradient calculation strategy for an objective.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GradientMode {
    /// Prefer reverse-mode VJP when exact for the model, otherwise use forward AD.
    Auto,
    /// Use reverse-mode VJP of the derivative RHS.
    Reverse,
    /// Use forward-mode parameter Jacobian columns.
    Forward,
}

/// Concrete strategy used for a gradient report.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GradientStrategy {
    /// Reverse-mode VJP over the derivative RHS.
    ReverseVjp,
    /// Forward-mode parameter Jacobian.
    ForwardJacobian,
}

/// Mean-squared-error target for a model RHS value.
#[derive(Debug, Clone)]
pub struct RhsMseObjective {
    /// Evaluation time.
    pub t: f64,
    /// Target derivative values, aligned with model state order.
    pub target_derivatives: Vec<f64>,
}

impl RhsMseObjective {
    /// Create an RHS MSE objective evaluated at time `t`.
    pub fn new(t: f64, target_derivatives: Vec<f64>) -> Self {
        Self {
            t,
            target_derivatives,
        }
    }
}

/// Objective value and gradient for a selected trainable set.
#[derive(Debug, Clone)]
pub struct GradientReport {
    /// Scalar objective value.
    pub loss: f64,
    /// Strategy used to assemble the gradient.
    pub strategy: GradientStrategy,
    /// Trainable names aligned with `gradients`.
    pub trainable_names: Vec<String>,
    /// Parameter slots aligned with `gradients`.
    pub trainable_slots: Vec<usize>,
    /// `d(loss)/d(trainable_i)`.
    pub gradients: Vec<f64>,
    /// Primal RHS value used to evaluate the loss.
    pub rhs: Vec<f64>,
}

/// Evaluate RHS MSE and `d(loss)/d(trainables)` for the current model point.
pub fn rhs_mse_value_and_gradient(
    model: &DifferentiableModel,
    objective: &RhsMseObjective,
    trainables: &TrainableSet,
    mode: GradientMode,
) -> Result<GradientReport, OptError> {
    let rhs = model.eval_rhs(objective.t)?;
    validate_target_len(&rhs, objective)?;
    let residual = residual(&rhs, &objective.target_derivatives);
    let loss = mse_loss(&residual)?;
    let cotangents = loss_cotangents(&residual);
    let strategy = select_strategy(model, mode)?;
    let gradients = match strategy {
        GradientStrategy::ReverseVjp => {
            reverse_gradients(model, objective.t, trainables, &cotangents)?
        }
        GradientStrategy::ForwardJacobian => {
            forward_gradients(model, objective.t, trainables, &cotangents)?
        }
    };
    Ok(report(loss, strategy, trainables, gradients, rhs))
}

fn validate_target_len(rhs: &[f64], objective: &RhsMseObjective) -> Result<(), OptError> {
    if rhs.len() != objective.target_derivatives.len() {
        return Err(OptError::LengthMismatch {
            what: "target derivative",
            got: objective.target_derivatives.len(),
            expected: rhs.len(),
        });
    }
    Ok(())
}

fn residual(rhs: &[f64], target: &[f64]) -> Vec<f64> {
    rhs.iter()
        .zip(target)
        .map(|(value, target)| value - target)
        .collect()
}

fn mse_loss(residual: &[f64]) -> Result<f64, OptError> {
    let n = residual.len().max(1) as f64;
    let loss = residual.iter().map(|value| value * value).sum::<f64>() / (2.0 * n);
    finite("loss", loss)?;
    Ok(loss)
}

fn loss_cotangents(residual: &[f64]) -> Vec<f64> {
    let n = residual.len().max(1) as f64;
    residual.iter().map(|value| value / n).collect()
}

fn select_strategy(
    model: &DifferentiableModel,
    mode: GradientMode,
) -> Result<GradientStrategy, OptError> {
    match mode {
        GradientMode::Auto if model.supports_rhs_reverse_vjp() => Ok(GradientStrategy::ReverseVjp),
        GradientMode::Auto => Ok(GradientStrategy::ForwardJacobian),
        GradientMode::Reverse if model.supports_rhs_reverse_vjp() => {
            Ok(GradientStrategy::ReverseVjp)
        }
        GradientMode::Reverse => Err(OptError::UnsupportedGradientMode {
            mode: "reverse",
            reason: "derivative RHS reverse VJP is exact only for pure ODE models today"
                .to_string(),
        }),
        GradientMode::Forward => Ok(GradientStrategy::ForwardJacobian),
    }
}

fn reverse_gradients(
    model: &DifferentiableModel,
    t: f64,
    trainables: &TrainableSet,
    cotangents: &[f64],
) -> Result<Vec<f64>, OptError> {
    let p_scalars = model.runtime().model.problem.layout.p_scalars();
    let mut vjp = vec![0.0; model.runtime().solver_count + p_scalars];
    model.runtime().reverse_state_derivative_vjp(
        model.linearization(t),
        model.state(),
        cotangents,
        &mut vjp,
    )?;
    trainables
        .entries()
        .iter()
        .map(|trainable| checked_gradient(vjp[model.runtime().solver_count + trainable.slot]))
        .collect()
}

fn forward_gradients(
    model: &DifferentiableModel,
    t: f64,
    trainables: &TrainableSet,
    cotangents: &[f64],
) -> Result<Vec<f64>, OptError> {
    let report = model.runtime().eval_parameter_jacobian(
        t,
        model.state(),
        model.parameters(),
        model.linearization(t).settle,
    );
    if let Some(error) = report.error {
        return Err(OptError::Runtime(error));
    }
    let columns = parameter_columns_by_slot(&report);
    trainables
        .entries()
        .iter()
        .map(|trainable| gradient_for_slot(&report.matrix, cotangents, &columns, trainable.slot))
        .collect()
}

fn parameter_columns_by_slot(
    report: &rumoca_eval_solve::ParameterJacobianReport,
) -> HashMap<usize, usize> {
    report
        .param_slots
        .iter()
        .copied()
        .enumerate()
        .map(|(column, slot)| (slot, column))
        .collect()
}

fn gradient_for_slot(
    matrix: &[Vec<f64>],
    cotangents: &[f64],
    columns: &HashMap<usize, usize>,
    slot: usize,
) -> Result<f64, OptError> {
    let Some(&column) = columns.get(&slot) else {
        return Ok(0.0);
    };
    let value = cotangents
        .iter()
        .zip(matrix)
        .map(|(cotangent, row)| cotangent * row[column])
        .sum::<f64>();
    checked_gradient(value)
}

fn checked_gradient(value: f64) -> Result<f64, OptError> {
    finite("gradient", value)?;
    Ok(value)
}

fn report(
    loss: f64,
    strategy: GradientStrategy,
    trainables: &TrainableSet,
    gradients: Vec<f64>,
    rhs: Vec<f64>,
) -> GradientReport {
    GradientReport {
        loss,
        strategy,
        trainable_names: trainables
            .entries()
            .iter()
            .map(|trainable| trainable.name.clone())
            .collect(),
        trainable_slots: trainables
            .entries()
            .iter()
            .map(|trainable| trainable.slot)
            .collect(),
        gradients,
        rhs,
    }
}

fn finite(what: &'static str, value: f64) -> Result<(), OptError> {
    if value.is_finite() {
        Ok(())
    } else {
        Err(OptError::NonFinite { what, value })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gradient_validation_rejects_non_finite_values() {
        let error =
            checked_gradient(f64::INFINITY).expect_err("non-finite gradient should be rejected");

        assert!(matches!(
            error,
            OptError::NonFinite {
                what: "gradient",
                ..
            }
        ));
    }
}
