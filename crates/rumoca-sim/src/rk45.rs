use std::time::Instant;

use indexmap::IndexMap;
use rumoca_ir_dae as dae;

scheduled_executor_items! {
    use crate::SimulationSessionApi;
}
use crate::me_backend::BackendSimulationSession;
use crate::simulation_session::SessionState;
use crate::solve_lowering::{
    SimulationDiagnosticError, apply_correlated_simulation_overrides, finish_runtime_fmi_artifact,
    lower_correlated_for_simulation_with_stage_timing_and_param_overrides, tunable_param_overrides,
};
use crate::{BuildSimulationTimings, SimError};

const INSTANCE_NAME: &str = "rk-like";

const RK45_INTEGRATOR: crate::me_backend::IntegratorFactory =
    crate::me_backend::IntegratorFactory {
        method: "rk45",
        backend: rumoca_solver::NumericalBackend::Rk45,
        model_jacobian_request: rumoca_solver::ModelStateJacobianRequest::NotRequested,
        build: rk45_integrator,
    };

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    crate::backend_options::validate(opts)?;
    let (artifact, execution_backend) =
        lower_runtime_artifact(dae_model, opts).map_err(diagnostic_sim_error)?;
    simulate_artifact(artifact, opts, execution_backend)
}

fn rk45_integrator(
    setup: rumoca_solver::fmi_me::MeNumericalSetup,
    options: crate::backend_options::ValidatedOptions,
    observation: Option<rumoca_solver::fmi_me::numerical_observation::AdapterWriter>,
) -> Result<Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend + 'static>, SimError> {
    if !matches!(
        options,
        crate::backend_options::ValidatedOptions::Defaults
            | crate::backend_options::ValidatedOptions::Rk45(_)
    ) {
        return Err(rumoca_solver::BackendOptionsError::ConcreteType {
            backend: rumoca_solver::NumericalBackend::Rk45,
        }
        .into());
    }
    Ok(rumoca_solver_rk45::model_exchange_integrator_with_observation(setup, observation))
}

#[cfg(feature = "solver-velimplicit")]
const VELIMPLICIT_INSTANCE_NAME: &str = "velimplicit";

#[cfg(feature = "solver-velimplicit")]
const VELIMPLICIT_INTEGRATOR: crate::me_backend::IntegratorFactory =
    crate::me_backend::IntegratorFactory {
        method: "velimplicit",
        backend: rumoca_solver::NumericalBackend::VelImplicit,
        model_jacobian_request: rumoca_solver::ModelStateJacobianRequest::FiniteAssembly,
        build: velimplicit_integrator,
    };

/// The opt-in MuJoCo-style solver at its default configuration (1 ms fixed
/// step, Jacobian reused for 200 steps).
#[cfg(feature = "solver-velimplicit")]
fn velimplicit_integrator(
    setup: rumoca_solver::fmi_me::MeNumericalSetup,
    options: crate::backend_options::ValidatedOptions,
    observation: Option<rumoca_solver::fmi_me::numerical_observation::AdapterWriter>,
) -> Result<Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend + 'static>, SimError> {
    let config = match options {
        crate::backend_options::ValidatedOptions::Defaults => {
            rumoca_solver_velimplicit::VelImplicitConfig::default()
        }
        crate::backend_options::ValidatedOptions::VelImplicit(config) => config,
        _ => {
            return Err(rumoca_solver::BackendOptionsError::ConcreteType {
                backend: rumoca_solver::NumericalBackend::VelImplicit,
            }
            .into());
        }
    };
    if let Some(observation) = observation {
        observation.unavailable();
    }
    Ok(rumoca_solver_velimplicit::model_exchange_integrator(
        setup, config,
    ))
}

const FIXED_STEP_INSTANCE_NAME: &str = "fixed-step";

const FIXED_STEP_INTEGRATOR: crate::me_backend::IntegratorFactory =
    crate::me_backend::IntegratorFactory {
        method: "fixed-step",
        backend: rumoca_solver::NumericalBackend::FixedStep,
        model_jacobian_request: rumoca_solver::ModelStateJacobianRequest::NotRequested,
        build: fixed_step_integrator,
    };

/// The opt-in fixed-step explicit Runge-Kutta solver: the validated request,
/// or its default configuration (1 ms step, classical fourth order).
fn fixed_step_integrator(
    setup: rumoca_solver::fmi_me::MeNumericalSetup,
    options: crate::backend_options::ValidatedOptions,
    observation: Option<rumoca_solver::fmi_me::numerical_observation::AdapterWriter>,
) -> Result<Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend + 'static>, SimError> {
    let config = match options {
        crate::backend_options::ValidatedOptions::Defaults => {
            rumoca_solver_rk45::FixedStepConfig::default()
        }
        crate::backend_options::ValidatedOptions::FixedStep(config) => config,
        _ => {
            return Err(rumoca_solver::BackendOptionsError::ConcreteType {
                backend: rumoca_solver::NumericalBackend::FixedStep,
            }
            .into());
        }
    };
    if let Some(observation) = observation {
        observation.unavailable();
    }
    Ok(rumoca_solver_rk45::fixed_step_integrator(setup, config)?)
}

pub use simulate as simulate_dae;

pub fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    simulate_with_observation(dae_model, opts, None)
}

pub(crate) fn simulate_with_observation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    observation: Option<&rumoca_solver::fmi_me::numerical_observation::SettingsCollector>,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    crate::backend_options::validate(opts)?;
    let (artifact, execution_backend) = lower_runtime_artifact(dae_model, opts)?;
    simulate_artifact_with_observation(artifact, opts, execution_backend, observation)
        .map_err(SimulationDiagnosticError::from)
}

pub(crate) fn simulate_artifact(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<rumoca_solver::SimResult, SimError> {
    simulate_artifact_with_observation(artifact, opts, execution_backend, None)
}

pub(crate) fn simulate_artifact_with_observation(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    observation: Option<&rumoca_solver::fmi_me::numerical_observation::SettingsCollector>,
) -> Result<rumoca_solver::SimResult, SimError> {
    let (instance_name, integrator) = integrator_for(opts.solver_mode)?;
    crate::me_backend::simulate_artifact(
        artifact,
        opts,
        execution_backend,
        instance_name,
        integrator,
        observation,
    )
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
    let (_, factory) = integrator_for(opts.solver_mode).map_err(SimulationDiagnosticError::from)?;
    crate::backend_options::validate_for_backend(opts, factory.backend)?;
    crate::solve_lowering::lower_runtime_fmi_artifact(dae_model, opts)
}

pub use simulate_with_diagnostics as simulate_dae_with_diagnostics;

/// Preserve the originating diagnostic code when adapting to the backend's
/// string-carrying error.
/// diagnostic, which also carries the runtime `EX0xx` codes (notably `EX003`
/// for a rejected parameter/start override).
fn diagnostic_sim_error(err: SimulationDiagnosticError) -> SimError {
    match err {
        SimulationDiagnosticError::Simulation(error) => error,
        SimulationDiagnosticError::BackendOptions(error) => error.into(),
        error => SimError::SolveIr(format!("[{}] {error}", error.diagnostic_code())),
    }
}

pub struct SimulationSession {
    inner: BackendSimulationSession,
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
        let (instance_name, integrator) = integrator_for(opts.solver_mode)?;
        crate::backend_options::validate_for_backend(&opts, integrator.backend)?;
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
        let inner = BackendSimulationSession::new(
            artifact,
            &opts,
            execution_backend,
            instance_name,
            integrator,
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
        crate::backend_options::validate(&opts)?;
        let (artifact, execution_backend) = lower_runtime_artifact(dae_model, &opts)?;
        Self::from_artifact(artifact, opts, execution_backend)
    }

    /// Build from one checked correlated FMI artifact.
    pub(crate) fn from_artifact(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: rumoca_solver::SimOptions,
        execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ) -> Result<Self, SimulationDiagnosticError> {
        let (instance_name, integrator) =
            integrator_for(opts.solver_mode).map_err(SimulationDiagnosticError::from)?;
        let inner = BackendSimulationSession::new(
            artifact,
            &opts,
            execution_backend,
            instance_name,
            integrator,
        )
        .map_err(SimulationDiagnosticError::from)?;
        Ok(Self { inner })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.set_inputs(&[(name, value)])
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        self.inner.set_inputs(inputs)
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.inner.advance_to(target_time)
    }

    pub fn trace_eval_snapshot(&self, label: &str) {
        tracing::debug!(label, "RK45 uses the common Model Exchange evaluation path");
    }

    pub fn ensure_end_time(&mut self, _target_time: f64) {}

    pub fn execution_receipt(&self) -> rumoca_solver::SimExecutionReceipt {
        self.inner.execution_receipt()
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt > 0.0 {
            self.advance_to(self.time() + dt)?;
        }
        Ok(())
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
        Ok(SessionState {
            time: self.time(),
            values: self.inner.visible_values()?,
        })
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        self.inner.values_for(names)
    }

    /// The requested values in request order; `None` for an unknown name.
    pub fn selected_values(&self, names: &[String]) -> Result<Vec<Option<f64>>, SimError> {
        self.inner.selected_values(names)
    }

    pub fn input_names(&self) -> &[String] {
        self.inner.input_names()
    }

    pub fn variable_names(&self) -> &[String] {
        self.inner.variable_names()
    }
}

/// The plugins this session hosts: rk45 for `rk-like` (and `auto` routed
/// here), the opt-in `fixed-step` explicit solver and, when compiled in, the
/// opt-in `velimplicit` solver.
fn integrator_for(
    requested: rumoca_solver::SimSolverMode,
) -> Result<(&'static str, crate::me_backend::IntegratorFactory), SimError> {
    match requested {
        rumoca_solver::SimSolverMode::Auto | rumoca_solver::SimSolverMode::RkLike => {
            Ok((INSTANCE_NAME, RK45_INTEGRATOR))
        }
        #[cfg(feature = "solver-velimplicit")]
        rumoca_solver::SimSolverMode::VelImplicit => {
            Ok((VELIMPLICIT_INSTANCE_NAME, VELIMPLICIT_INTEGRATOR))
        }
        rumoca_solver::SimSolverMode::FixedStep => {
            Ok((FIXED_STEP_INSTANCE_NAME, FIXED_STEP_INTEGRATOR))
        }
        requested => Err(SimError::UnsupportedSolverMode {
            backend: INSTANCE_NAME,
            requested,
        }),
    }
}

scheduled_executor_items! {
    impl SimulationSessionApi for SimulationSession {
        type Error = SimError;

        fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
            Self::reset(self, t_start)
        }

        fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), Self::Error> {
            Self::set_inputs(self, inputs)
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
