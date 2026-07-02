use crate::{
    DifferentiableModel, GradientMode, GradientReport, OptError, RhsMseObjective, TrainableSet,
    rhs_mse_value_and_gradient,
};

/// Basic gradient-descent optimizer for model fitting and training smoke tests.
#[derive(Debug, Clone, Copy)]
pub struct GradientDescent {
    /// Positive parameter update scale.
    pub learning_rate: f64,
    /// Number of optimization steps to run.
    pub steps: usize,
    /// Gradient implementation to use.
    pub gradient_mode: GradientMode,
}

impl GradientDescent {
    /// Create gradient descent with an explicit step count.
    pub fn new(learning_rate: f64, steps: usize) -> Self {
        Self {
            learning_rate,
            steps,
            gradient_mode: GradientMode::Auto,
        }
    }

    /// Set the gradient implementation.
    pub fn with_gradient_mode(mut self, mode: GradientMode) -> Self {
        self.gradient_mode = mode;
        self
    }

    /// Fit trainable parameters against an RHS MSE objective.
    pub fn optimize_rhs_mse(
        self,
        model: &mut DifferentiableModel,
        objective: &RhsMseObjective,
        trainables: &TrainableSet,
    ) -> Result<OptimizationReport, OptError> {
        validate_learning_rate(self.learning_rate)?;
        let mut steps = Vec::with_capacity(self.steps.saturating_add(1));
        steps.push(step_report(
            0,
            model,
            objective,
            trainables,
            self.gradient_mode,
        )?);
        for index in 1..=self.steps {
            let previous = steps
                .last()
                .expect("initial step report was pushed before optimization");
            apply_gradient_step(model, trainables, &previous.gradient, self.learning_rate)?;
            steps.push(step_report(
                index,
                model,
                objective,
                trainables,
                self.gradient_mode,
            )?);
        }
        Ok(OptimizationReport { steps })
    }
}

/// Per-iteration optimizer state.
#[derive(Debug, Clone)]
pub struct OptimizationStep {
    /// Zero-based iteration index. Step 0 is the initial point.
    pub index: usize,
    /// Objective and gradient report at this step.
    pub gradient: GradientReport,
}

/// Complete optimizer history.
#[derive(Debug, Clone)]
pub struct OptimizationReport {
    /// Initial step plus every post-update step.
    pub steps: Vec<OptimizationStep>,
}

impl OptimizationReport {
    /// Initial objective value.
    pub fn initial_loss(&self) -> Option<f64> {
        self.steps.first().map(|step| step.gradient.loss)
    }

    /// Final objective value.
    pub fn final_loss(&self) -> Option<f64> {
        self.steps.last().map(|step| step.gradient.loss)
    }
}

fn validate_learning_rate(learning_rate: f64) -> Result<(), OptError> {
    if learning_rate.is_finite() && learning_rate > 0.0 {
        Ok(())
    } else {
        Err(OptError::NonFinite {
            what: "learning_rate",
            value: learning_rate,
        })
    }
}

fn step_report(
    index: usize,
    model: &DifferentiableModel,
    objective: &RhsMseObjective,
    trainables: &TrainableSet,
    gradient_mode: GradientMode,
) -> Result<OptimizationStep, OptError> {
    Ok(OptimizationStep {
        index,
        gradient: rhs_mse_value_and_gradient(model, objective, trainables, gradient_mode)?,
    })
}

fn apply_gradient_step(
    model: &mut DifferentiableModel,
    trainables: &TrainableSet,
    report: &GradientReport,
    learning_rate: f64,
) -> Result<(), OptError> {
    if report.gradients.len() != trainables.len() {
        return Err(OptError::LengthMismatch {
            what: "gradient",
            got: report.gradients.len(),
            expected: trainables.len(),
        });
    }
    for (trainable, gradient) in trainables.entries().iter().zip(&report.gradients) {
        let slot = trainable.slot;
        model.params_mut()[slot] -= learning_rate * gradient;
    }
    Ok(())
}
