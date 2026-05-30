use std::collections::HashMap;

use rumoca_ir_dae as dae;

use crate::InteractiveStepper;
use crate::solve_lowering::{SimulationDiagnosticError, lower_dae_for_simulation};

pub use rumoca_solver_rk45::SimError;
pub use rumoca_solver_rk45::StepperState;

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
    rumoca_solver_rk45::simulate(&solve_model, opts)
}

pub use simulate as simulate_dae;

pub fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    let solve_model = lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    rumoca_solver_rk45::simulate(&solve_model, opts)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

pub use simulate_with_diagnostics as simulate_dae_with_diagnostics;

pub struct SimStepper {
    inner: rumoca_solver_rk45::SimStepper,
}

impl SimStepper {
    pub fn new(dae_model: &dae::Dae, opts: rumoca_solver::SimOptions) -> Result<Self, SimError> {
        let solve_model = lower_dae_for_simulation(dae_model, &opts)
            .map_err(|err| SimError::SolveIr(err.to_string()))?;
        let inner = rumoca_solver_rk45::SimStepper::new(&solve_model, opts)?;
        Ok(Self { inner })
    }

    pub fn new_with_diagnostics(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let solve_model = lower_dae_for_simulation(dae_model, &opts)
            .map_err(SimulationDiagnosticError::SolveLowering)?;
        let inner = rumoca_solver_rk45::SimStepper::new(&solve_model, opts)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        Ok(Self { inner })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.inner.set_input(name, value)
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        self.inner.set_inputs(inputs)
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        self.inner.step(dt)
    }

    pub fn time(&self) -> f64 {
        self.inner.time()
    }

    pub fn get(&self, name: &str) -> Option<f64> {
        self.inner.get(name)
    }

    pub fn state(&self) -> StepperState {
        self.inner.state()
    }

    pub fn values_for(&self, names: &[String]) -> HashMap<String, f64> {
        self.inner.values_for(names)
    }

    pub fn input_names(&self) -> &[String] {
        self.inner.input_names()
    }

    pub fn variable_names(&self) -> &[String] {
        self.inner.variable_names()
    }
}

impl InteractiveStepper for SimStepper {
    type Error = SimError;

    fn new_from_dae(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, Self::Error> {
        Self::new(dae_model, opts)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn step(&mut self, dt: f64) -> Result<(), Self::Error> {
        Self::step(self, dt)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn get(&self, name: &str) -> Option<f64> {
        Self::get(self, name)
    }

    fn input_names(&self) -> &[String] {
        Self::input_names(self)
    }
}
