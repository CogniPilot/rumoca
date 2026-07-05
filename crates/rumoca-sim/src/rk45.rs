use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[cfg(feature = "runner")]
use crate::SimulationSessionApi;
use crate::solve_lowering::{SimulationDiagnosticError, lower_dae_for_simulation};

pub use rumoca_solver_rk45::SessionState;
pub use rumoca_solver_rk45::SimError;

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
    rumoca_solver_rk45::simulate(&solve_model, opts)
}

pub use simulate as simulate_dae;

pub fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)?;
    rumoca_solver_rk45::simulate(&solve_model, opts)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

pub use simulate_with_diagnostics as simulate_dae_with_diagnostics;

pub struct SimulationSession {
    inner: rumoca_solver_rk45::SimulationSession,
}

impl SimulationSession {
    pub fn new(dae_model: &dae::Dae, opts: rumoca_solver::SimOptions) -> Result<Self, SimError> {
        let mut solve_model = lower_dae_for_simulation(dae_model, &opts)
            .map_err(|err| SimError::SolveIr(err.to_string()))?;
        crate::solve_lowering::apply_simulation_overrides(&mut solve_model, dae_model, &opts, true)
            .map_err(|err| SimError::SolveIr(err.to_string()))?;
        let inner = rumoca_solver_rk45::SimulationSession::new(&solve_model, opts)?;
        Ok(Self { inner })
    }

    pub fn new_with_diagnostics(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let mut solve_model = lower_dae_for_simulation(dae_model, &opts)
            .map_err(SimulationDiagnosticError::SolveLowering)?;
        crate::solve_lowering::apply_simulation_overrides(
            &mut solve_model,
            dae_model,
            &opts,
            true,
        )?;
        Self::from_solve_model(solve_model, opts)
    }

    /// Build directly from an already-lowered, override-applied solve model, so
    /// callers that lowered once (e.g. auto solver dispatch that first
    /// probes for a pure-discrete model) do not lower the model a second time.
    pub(crate) fn from_solve_model(
        solve_model: solve::SolveModel,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let inner = rumoca_solver_rk45::SimulationSession::new(&solve_model, opts)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        Ok(Self { inner })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.inner.set_input(name, value)
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        self.inner.set_inputs(inputs)
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.inner.advance_to(target_time)
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.inner.reset(t_start)
    }

    pub fn time(&self) -> f64 {
        self.inner.time()
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        self.inner.get(name)
    }

    pub fn state(&self) -> Result<SessionState, SimError> {
        self.inner.state()
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        self.inner.values_for(names)
    }

    pub fn input_names(&self) -> &[String] {
        self.inner.input_names()
    }

    pub fn variable_names(&self) -> &[String] {
        self.inner.variable_names()
    }
}

#[cfg(feature = "runner")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimError;

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::reset(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error> {
        Self::advance_to(self, target_time)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn get(&self, name: &str) -> Result<Option<f64>, Self::Error> {
        Self::get(self, name)
    }
}
