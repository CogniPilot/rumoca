use crate::{DifferentiableModel, GradientDescent, OptError, TrainableSet};
use rumoca_ir_dae as dae;
use rumoca_sim::{DataSeries, RunningKind, RunningTerm, SimOptions, TrajectoryObjective};

/// Least-squares fit of model variables to measured series over a run.
///
/// The loss is `∫ Σ_k w_k (v_k(t) - d_k(t))² dt` over the simulation window of
/// `sim_options`, with each measured series interpolated linearly. Its gradient
/// with respect to the trainable parameters is the adjoint sensitivity gradient,
/// whose cost does not grow with the number of trainables.
#[derive(Debug, Clone)]
pub struct TrajectoryFit {
    /// Window, tolerances, and any fixed overrides of the simulated run.
    pub sim_options: SimOptions,
    /// Measured series by solver-variable name (a state or solver algebraic).
    pub measurements: Vec<(String, DataSeries)>,
}

impl TrajectoryFit {
    /// Create a fit of `measurements` over the run `sim_options` describes.
    pub fn new(sim_options: SimOptions, measurements: Vec<(String, DataSeries)>) -> Self {
        Self {
            sim_options,
            measurements,
        }
    }

    fn objective(&self) -> TrajectoryObjective {
        TrajectoryObjective {
            running: self
                .measurements
                .iter()
                .map(|(variable, series)| RunningTerm {
                    variable: variable.clone(),
                    weight: 1.0,
                    kind: RunningKind::SquaredError(series.clone()),
                })
                .collect(),
            terminal: Vec::new(),
        }
    }
}

/// One iteration of a trajectory fit.
#[derive(Debug, Clone)]
pub struct TrajectoryFitStep {
    /// Zero-based iteration index. Step 0 is the initial point.
    pub index: usize,
    /// Loss at this step's parameter values.
    pub loss: f64,
    /// Trainable values at this step, aligned with the report's names.
    pub parameters: Vec<f64>,
    /// `d(loss)/d(trainable_i)` at this step.
    pub gradients: Vec<f64>,
}

/// Complete history of a trajectory fit.
#[derive(Debug, Clone)]
pub struct TrajectoryFitReport {
    /// Trainable names aligned with each step's vectors.
    pub trainable_names: Vec<String>,
    /// Initial step plus every post-update step.
    pub steps: Vec<TrajectoryFitStep>,
}

impl TrajectoryFitReport {
    /// Initial loss.
    pub fn initial_loss(&self) -> Option<f64> {
        self.steps.first().map(|step| step.loss)
    }

    /// Final loss.
    pub fn final_loss(&self) -> Option<f64> {
        self.steps.last().map(|step| step.loss)
    }

    /// Final trainable values.
    pub fn final_parameters(&self) -> Option<&[f64]> {
        self.steps.last().map(|step| step.parameters.as_slice())
    }
}

impl GradientDescent {
    /// Fit `trainables` of `model` to measured trajectories by gradient descent
    /// on the adjoint gradient of the least-squares loss.
    ///
    /// `model` carries the parameter values and receives the fitted ones; `dae`
    /// is the compiled model the runs are simulated from. The learning rate and
    /// step count are this optimizer's own.
    pub fn fit_trajectory(
        self,
        dae: &dae::Dae,
        model: &mut DifferentiableModel,
        fit: &TrajectoryFit,
        trainables: &TrainableSet,
    ) -> Result<TrajectoryFitReport, OptError> {
        crate::optimizer::validate_learning_rate(self.learning_rate)?;
        let names: Vec<String> = trainables
            .entries()
            .iter()
            .map(|entry| entry.name.clone())
            .collect();
        let objective = fit.objective();
        let trajectory_error =
            |error: rumoca_sim::SimulationDiagnosticError| OptError::Trajectory(error.to_string());
        // Lowered and proved once; each iteration re-settles the initial point of
        // the same runtime at the current parameter values.
        let session = rumoca_sim::TrajectorySession::new(dae, &fit.sim_options, &names)
            .map_err(trajectory_error)?;
        let mut steps = Vec::with_capacity(self.steps.saturating_add(1));
        for index in 0..=self.steps {
            let values: Vec<f64> = trainables
                .entries()
                .iter()
                .map(|entry| model.parameters()[entry.slot])
                .collect();
            let gradient = session
                .with_parameter_values(&values)
                .and_then(|session| session.gradient(&objective, true))
                .map_err(trajectory_error)?;
            if !gradient.value.is_finite() {
                return Err(OptError::NonFinite {
                    what: "trajectory loss",
                    value: gradient.value,
                });
            }
            let gradients = gradient.gradient;
            if index < self.steps {
                descend(model, trainables, &gradients, self.learning_rate);
            }
            steps.push(TrajectoryFitStep {
                index,
                loss: gradient.value,
                parameters: values,
                gradients,
            });
        }
        Ok(TrajectoryFitReport {
            trainable_names: names,
            steps,
        })
    }
}

/// One gradient-descent update of the trainable slots.
fn descend(
    model: &mut DifferentiableModel,
    trainables: &TrainableSet,
    gradients: &[f64],
    learning_rate: f64,
) {
    for (entry, gradient) in trainables.entries().iter().zip(gradients) {
        model.params_mut()[entry.slot] -= learning_rate * gradient;
    }
}
