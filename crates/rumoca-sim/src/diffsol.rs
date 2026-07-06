use std::time::Instant;

#[cfg(feature = "runner")]
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::BuildSimulationTimings;
#[cfg(feature = "runner")]
use crate::SimulationSessionApi;
use crate::solve_lowering::{
    SimulationDiagnosticError, lower_dae_for_simulation_with_stage_timing_and_param_overrides,
    tunable_param_overrides,
};

pub use rumoca_solver_diffsol::SimError;
pub(crate) use rumoca_solver_diffsol::session::SessionState;

pub struct PreparedSimulation {
    inner: rumoca_solver_diffsol::PreparedSimulation,
}

impl PreparedSimulation {
    pub fn backend(&self) -> rumoca_solver::SimBackend {
        self.inner.backend()
    }

    pub fn run(&self) -> Result<rumoca_solver::SimResult, SimError> {
        self.inner.run()
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        self.inner.check_initialization()
    }

    pub fn model(&self) -> &solve::SolveModel {
        self.inner.model()
    }

    pub fn set_parameter_value(&mut self, _name: &str, _value: f64) -> Result<(), SimError> {
        Err(SimError::SolverError(
            "parameter overrides are not yet supported after Solve IR lowering".to_string(),
        ))
    }

    pub fn set_parameter_values(&mut self, _name: &str, _values: &[f64]) -> Result<(), SimError> {
        Err(SimError::SolverError(
            "parameter overrides are not yet supported after Solve IR lowering".to_string(),
        ))
    }

    pub fn clear_parameter_overrides(&mut self) {}
}

pub fn build_simulation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_with_stage_timing(dae_model, opts, |_| {}).map(|(prepared, _)| prepared)
}

pub fn build_simulation_with_stage_timing(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    begin_stage: impl FnMut(&'static str),
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    build_simulation_with_stage_timing_and_solve_model(dae_model, opts, begin_stage, |_| {})
}

pub fn build_simulation_with_stage_timing_and_solve_model(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    mut begin_stage: impl FnMut(&'static str),
    mut observe_solve_model: impl FnMut(&solve::SolveModel),
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    let param_overrides = tunable_param_overrides(dae_model, opts);
    let (mut solve_model, solve_timings) =
        lower_dae_for_simulation_with_stage_timing_and_param_overrides(
            dae_model,
            opts,
            &param_overrides,
            &mut begin_stage,
        )
        .map_err(solve_lowering_sim_error)?;
    begin_stage("sim_overrides");
    let override_apply_start = Instant::now();
    crate::solve_lowering::apply_simulation_overrides(&mut solve_model, dae_model, opts)
        .map_err(|err| SimError::SolverError(err.to_string()))?;
    let override_apply_seconds = override_apply_start.elapsed().as_secs_f64();
    observe_solve_model(&solve_model);
    begin_stage("sim_build");
    let backend_build_start = Instant::now();
    let inner = rumoca_solver_diffsol::build_simulation(&solve_model, opts)?;
    let backend_build_seconds = backend_build_start.elapsed().as_secs_f64();
    Ok((
        PreparedSimulation { inner },
        BuildSimulationTimings {
            ir_solve_structural_dae_seconds: solve_timings.structural_dae_seconds,
            ir_solve_lower_seconds: solve_timings.solve_ir_seconds,
            ir_solve_seconds: solve_timings.total_seconds(),
            override_apply_seconds,
            backend_build_seconds,
        },
    ))
}

pub fn run_prepared_simulation(
    prepared: &PreparedSimulation,
) -> Result<rumoca_solver::SimResult, SimError> {
    prepared.run()
}

pub fn check_prepared_initialization(prepared: &PreparedSimulation) -> Result<(), SimError> {
    prepared.check_initialization()
}

pub fn check_initialization(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<(), SimError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
        .map_err(|err| SimError::SolverError(err.to_string()))?;
    rumoca_solver_diffsol::check_initialization(&solve_model, opts)
}

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
        .map_err(|err| SimError::SolverError(err.to_string()))?;
    rumoca_solver_diffsol::simulate(&solve_model, opts)
}

pub use simulate as simulate_dae;

pub(crate) fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)?;
    rumoca_solver_diffsol::simulate(&solve_model, opts)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

pub(crate) struct SimulationSession {
    inner: rumoca_solver_diffsol::session::SimulationSession,
}

impl SimulationSession {
    /// Build directly from an already-lowered, override-applied solve model, so
    /// callers that lowered once (e.g. auto solver dispatch that first
    /// probes for a pure-discrete model) do not lower the model a second time.
    pub(crate) fn from_solve_model(
        solve_model: solve::SolveModel,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let inner = rumoca_solver_diffsol::session::SimulationSession::new(&solve_model, opts)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        Ok(Self { inner })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.inner.set_input(name, value)
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.inner.advance_to(target_time)
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.time() + dt)
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.inner.reset(t_start)
    }

    pub(crate) fn time(&self) -> f64 {
        self.inner.time()
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        self.inner.get(name)
    }

    pub(crate) fn state(&self) -> Result<SessionState, SimError> {
        self.inner.state()
    }

    #[cfg(feature = "runner")]
    pub(crate) fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        self.inner.values_for(names)
    }

    pub(crate) fn input_names(&self) -> &[String] {
        self.inner.input_names()
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        self.inner.variable_names()
    }

    #[cfg(feature = "runner")]
    pub(crate) fn max_runner_advance_dt(&self) -> Option<f64> {
        self.inner.max_runner_advance_dt()
    }
}

fn solve_lowering_sim_error(err: rumoca_phase_solve::SolveModelLowerError) -> SimError {
    SimError::SolveIr(err.to_string())
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

    fn max_runner_advance_dt(&self) -> Option<f64> {
        Self::max_runner_advance_dt(self)
    }
}
