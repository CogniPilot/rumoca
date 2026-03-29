//! Real-time stepper API for interactive simulation with external control inputs.
//!
//! The [`SimStepper`] allows stepping a simulation forward incrementally,
//! injecting input values between steps, and reading outputs — suitable for
//! controller-in-the-loop and real-time use cases.

use std::collections::HashMap;

use rumoca_ir_dae as dae;

use super::problem::SharedInputOverrides;
use super::{Dae, SimError};
use crate::TimeoutBudget;
use crate::runtime::layout::SimulationContext;

/// Options for creating a [`SimStepper`].
#[derive(Debug, Clone)]
pub struct StepperOptions {
    pub rtol: f64,
    pub atol: f64,
    pub scalarize: bool,
    pub max_wall_seconds_per_step: Option<f64>,
}

impl Default for StepperOptions {
    fn default() -> Self {
        Self {
            rtol: 1e-6,
            atol: 1e-6,
            scalarize: true,
            max_wall_seconds_per_step: None,
        }
    }
}

/// Snapshot of the stepper's current state.
#[derive(Debug, Clone)]
pub struct StepperState {
    pub time: f64,
    pub values: HashMap<String, f64>,
}

/// Trait for type-erasing the diffsol solver internals.
pub(crate) trait StepperInner {
    fn step(&mut self, dt: f64, dae: &Dae, budget: &TimeoutBudget) -> Result<(), SimError>;
    fn time(&self) -> f64;
    fn solver_state_y(&self) -> Vec<f64>;
}

/// A real-time simulation stepper that supports external input injection.
///
/// Created from a compiled DAE model, the stepper allows:
/// - Setting input values by name between steps
/// - Stepping forward by a time increment
/// - Reading state/output values by name
pub struct SimStepper {
    pub(crate) inner: Box<dyn StepperInner>,
    pub(crate) dae: Dae,
    pub(crate) sim_context: SimulationContext,
    #[allow(dead_code)]
    pub(crate) param_values: Vec<f64>,
    pub(crate) input_overrides: SharedInputOverrides,
    #[allow(dead_code)]
    pub(crate) n_x: usize,
    #[allow(dead_code)]
    pub(crate) n_total: usize,
    pub(crate) solver_names: Vec<String>,
    pub(crate) max_wall_seconds_per_step: Option<f64>,
}

impl SimStepper {
    /// Create a new stepper from a DAE model.
    ///
    /// This runs the full preparation pipeline (structural analysis, initial
    /// condition solving, kernel compilation) and creates a BDF solver ready
    /// for interactive stepping.
    pub fn new(dae: &dae::Dae, opts: StepperOptions) -> Result<Self, SimError> {
        super::build_stepper(dae, opts)
    }

    /// Set an input value by name. Takes effect on the next `step()` call.
    ///
    /// The name should match the flattened scalar name of an input variable
    /// (e.g., `"u"` for a scalar input, `"u[1]"` for an array element).
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let valid_names = self.sim_context.input_scalar_names();
        if !valid_names.iter().any(|n| n == name) {
            return Err(SimError::SolverError(format!(
                "unknown input '{}', available inputs: {:?}",
                name, valid_names
            )));
        }
        self.input_overrides
            .borrow_mut()
            .insert(name.to_string(), value);
        Ok(())
    }

    /// Set multiple inputs at once.
    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for &(name, value) in inputs {
            self.set_input(name, value)?;
        }
        Ok(())
    }

    /// Step the simulation forward by `dt` seconds.
    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        let budget = TimeoutBudget::new(self.max_wall_seconds_per_step);
        self.inner.step(dt, &self.dae, &budget)
    }

    /// Get the current simulation time.
    pub fn time(&self) -> f64 {
        self.inner.time()
    }

    /// Read a single variable value by name.
    ///
    /// Works for states, algebraics, outputs, and inputs.
    pub fn get(&self, name: &str) -> Option<f64> {
        let y = self.inner.solver_state_y();
        if let Some(idx) = self.sim_context.solver_idx_for_target(name) {
            return y.get(idx).copied();
        }
        if let Some(&val) = self.input_overrides.borrow().get(name) {
            return Some(val);
        }
        None
    }

    /// Get a snapshot of all current variable values.
    pub fn state(&self) -> StepperState {
        let y = self.inner.solver_state_y();
        let mut values = HashMap::new();
        for (idx, name) in self.solver_names.iter().enumerate() {
            if let Some(&val) = y.get(idx) {
                values.insert(name.clone(), val);
            }
        }
        for (name, &val) in self.input_overrides.borrow().iter() {
            values.insert(name.clone(), val);
        }
        StepperState {
            time: self.inner.time(),
            values,
        }
    }

    /// List available input names.
    pub fn input_names(&self) -> &[String] {
        self.sim_context.input_scalar_names()
    }

    /// List all solver variable names (states, algebraics, outputs).
    pub fn variable_names(&self) -> &[String] {
        &self.solver_names
    }
}
