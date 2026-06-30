//! Optimization and training APIs over Rumoca's differentiable Solve runtime.
//!
//! This crate does not extend Modelica semantics. Modelica still describes the
//! model; optimization code describes trainables, objectives, and update rules
//! over a compiled differentiable model.

mod error;
mod model;
mod objective;
mod optimizer;

pub use error::OptError;
pub use model::{DifferentiableModel, OptOptions, TrainableParameter, TrainableSet};
pub use objective::{
    GradientMode, GradientReport, GradientStrategy, RhsMseObjective, rhs_mse_value_and_gradient,
};
pub use optimizer::{GradientDescent, OptimizationReport, OptimizationStep};

#[cfg(test)]
mod tests;
