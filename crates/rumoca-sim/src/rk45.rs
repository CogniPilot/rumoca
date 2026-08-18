use std::time::Instant;

use indexmap::IndexMap;
use rumoca_ir_dae as dae;

use crate::BuildSimulationTimings;
#[cfg(feature = "scheduled-sim")]
use crate::SimulationSessionApi;
use crate::solve_lowering::{
    SimulationDiagnosticError, apply_correlated_simulation_overrides, finish_runtime_fmi_artifact,
    lower_correlated_for_simulation_with_stage_timing_and_param_overrides, tunable_param_overrides,
};

pub use rumoca_solver_rk45::SessionState;
pub use rumoca_solver_rk45::SimError;

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    let (artifact, execution_backend) =
        lower_runtime_artifact(dae_model, opts).map_err(diagnostic_sim_error)?;
    simulate_artifact(artifact, opts, execution_backend)
}

pub use simulate as simulate_dae;

pub fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    let (artifact, execution_backend) = lower_runtime_artifact(dae_model, opts)?;
    simulate_artifact(artifact, opts, execution_backend)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

pub(crate) fn simulate_artifact(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<rumoca_solver::SimResult, SimError> {
    rumoca_solver_rk45::simulate_with_execution_backend(artifact, opts, execution_backend)
}

fn lower_runtime_artifact(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<
    (
        rumoca_solver::fmi_me::MeModelArtifact,
        Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ),
    SimulationDiagnosticError,
> {
    crate::solve_lowering::lower_runtime_fmi_artifact(dae_model, opts)
}

pub use simulate_with_diagnostics as simulate_dae_with_diagnostics;

/// Preserve the originating diagnostic code when adapting to the backend's
/// string-carrying error.
/// diagnostic, which also carries the runtime `EX0xx` codes (notably `EX003`
/// for a rejected parameter/start override).
fn diagnostic_sim_error(err: SimulationDiagnosticError) -> SimError {
    SimError::SolveIr(format!("[{}] {err}", err.diagnostic_code()))
}

pub struct SimulationSession {
    inner: rumoca_solver_rk45::SimulationSession,
}

impl SimulationSession {
    pub fn new(dae_model: &dae::Dae, opts: rumoca_solver::SimOptions) -> Result<Self, SimError> {
        Self::new_with_stage_timing(dae_model, opts, |_| {}).map(|(stepper, _)| stepper)
    }

    pub fn new_with_stage_timing(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
        mut begin_stage: impl FnMut(&'static str),
    ) -> Result<(Self, BuildSimulationTimings), SimError> {
        let param_overrides =
            tunable_param_overrides(dae_model, &opts).map_err(diagnostic_sim_error)?;
        let (mut lowered, solve_timings) =
            lower_correlated_for_simulation_with_stage_timing_and_param_overrides(
                dae_model,
                &opts,
                &param_overrides,
                &mut begin_stage,
            )
            .map_err(diagnostic_sim_error)?;
        begin_stage("sim_overrides");
        let override_apply_start = Instant::now();
        apply_correlated_simulation_overrides(&mut lowered, dae_model, &opts)
            .map_err(diagnostic_sim_error)?;
        let override_apply_seconds = override_apply_start.elapsed().as_secs_f64();
        begin_stage("sim_build");
        let backend_build_start = Instant::now();
        let (artifact, execution_backend) =
            finish_runtime_fmi_artifact(lowered, &opts).map_err(diagnostic_sim_error)?;
        let inner = rumoca_solver_rk45::SimulationSession::new_with_execution_backend(
            artifact,
            opts,
            execution_backend,
        )?;
        let backend_build_seconds = backend_build_start.elapsed().as_secs_f64();
        Ok((
            Self { inner },
            BuildSimulationTimings {
                ir_solve_structural_dae_seconds: solve_timings.ir_solve_structural_dae_seconds,
                ir_solve_lower_seconds: solve_timings.ir_solve_lower_seconds,
                ir_solve_seconds: solve_timings.ir_solve_seconds,
                override_apply_seconds,
                backend_build_seconds,
            },
        ))
    }

    pub fn new_with_diagnostics(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let (artifact, execution_backend) = lower_runtime_artifact(dae_model, &opts)?;
        Self::from_artifact(artifact, opts, execution_backend)
    }

    /// Build from one checked correlated FMI artifact.
    pub(crate) fn from_artifact(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: rumoca_solver::SimOptions,
        execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ) -> Result<Self, SimulationDiagnosticError> {
        let inner = rumoca_solver_rk45::SimulationSession::new_with_execution_backend(
            artifact,
            opts,
            execution_backend,
        )
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

    pub fn trace_eval_snapshot(&self, label: &str) {
        self.inner.trace_eval_snapshot(label);
    }

    pub fn ensure_end_time(&mut self, target_time: f64) {
        self.inner.ensure_end_time(target_time);
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        self.inner.step(dt)
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

#[cfg(feature = "scheduled-sim")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimError;

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::reset(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn ensure_end_time(&mut self, target_time: f64) {
        Self::ensure_end_time(self, target_time);
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Both backends must tag solve-lowering failures with the SPEC_0008 code
    /// of the phase that raised them. Without the prefix, `rumoca-worker`'s
    /// `sim_error_diagnostic_code` can only re-derive the generic `EX002`
    /// fallback, so `--backend rk45` reported a different (and coarser) code
    /// than `--backend diffsol` for the identical defect.
    #[test]
    fn solve_lowering_failures_carry_their_phase_code() {
        let err =
            SimulationDiagnosticError::SolveLowering(rumoca_phase_solve::LowerError::Structural {
                reason: "empty checked system".to_string(),
                span: None,
            });
        let code = err.diagnostic_code();
        let SimError::SolveIr(message) = diagnostic_sim_error(err) else {
            panic!("solve-lowering failures must surface as SimError::SolveIr");
        };
        assert_eq!(code, "EL005");
        assert!(
            message.starts_with("[EL005] "),
            "rk45 must tag the lowering code like diffsol does, got {message:?}"
        );
    }

    /// `EX003` (rejected parameter/start override) exists only on the
    /// structured diagnostic; re-deriving a code from the stringified
    /// `SimError` can never produce it. Tagging is what keeps it reachable.
    #[test]
    fn rejected_override_carries_its_runtime_code() {
        let err = SimulationDiagnosticError::InvalidOverride {
            message: "unknown parameter 'nope'".to_string(),
        };
        assert_eq!(err.diagnostic_code(), "EX003");
        let SimError::SolveIr(message) = diagnostic_sim_error(err) else {
            panic!("override rejection must surface as SimError::SolveIr");
        };
        assert!(
            message.starts_with("[EX003] "),
            "expected the override code to survive into the message, got {message:?}"
        );
    }
}
