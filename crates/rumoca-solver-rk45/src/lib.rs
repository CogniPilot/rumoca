//! Dormand-Prince 5(4) behind Rumoca's sole FMI Model Exchange host.
//!
//! This crate owns only the numerical method and its native continuous
//! extension. Initialization, FMI lifecycle, events, root scanning, tracing,
//! output scheduling, live inputs, reset, and zero-state execution are all
//! owned by `rumoca-solver`'s common host.

use indexmap::IndexMap;
use rumoca_solver::{
    SimOptions, SimResult, SimSolverMode,
    fmi_me::{
        MeError, MeExecutionBackend, MeInstanceConfig, MeModelArtifact, MeSessionError,
        driver::{
            advance_live_session, batch_output_cursor, batch_session_options, live_session_options,
        },
        session::{MeRetainedComponent, MeSimulationSession},
    },
};

mod dense_output;
mod me_integrator;

pub use me_integrator::model_exchange_integrator;

const INSTANCE_NAME: &str = "rk-like";

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("rk45 backend does not support solver mode {requested:?}")]
    UnsupportedSolverMode { requested: SimSolverMode },

    #[error("rk45 backend only supports a narrow explicit ODE subset: {reason}")]
    UnsupportedModel { reason: String },

    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    #[error("solve-IR evaluation failed: {0}")]
    SolveIr(String),

    #[error("directional derivative is unavailable: {reason}")]
    DirectionalDerivativeUnavailable { reason: String },

    #[error("rk45 runtime contract violation: {reason}")]
    RuntimeContract { reason: String },

    #[error("{context} allocation failed for {entries} entries")]
    Allocation {
        context: &'static str,
        entries: usize,
    },

    #[error("Modelica assert failed at t={time:.9}: {message}")]
    AssertionFailed { time: f64, message: String },

    #[error(transparent)]
    ExecutionPolicyContradiction(#[from] rumoca_solver::fmi_me::MeExecutionPolicyContradiction),

    #[error(transparent)]
    ModelExchangeSession(#[from] MeSessionError),
}

impl From<MeError> for SimError {
    fn from(value: MeError) -> Self {
        match value.into_kind() {
            MeError::NoContinuousStates => Self::RuntimeContract {
                reason: "a stateful plugin was requested for a zero-state component".to_owned(),
            },
            MeError::UnsupportedModel { reason } => Self::UnsupportedModel { reason },
            MeError::Evaluation { message } => Self::SolveIr(message),
            MeError::NonFiniteDerivative { state_name } => Self::NonFiniteDerivative { state_name },
            MeError::DirectionalDerivativeUnavailable { reason } => {
                Self::DirectionalDerivativeUnavailable { reason }
            }
            MeError::Contract { reason } => Self::RuntimeContract { reason },
            MeError::Assertion { time, message } => Self::AssertionFailed { time, message },
            MeError::Allocation { context, entries } => Self::Allocation { context, entries },
            staged @ MeError::Staged { .. } => Self::RuntimeContract {
                reason: format!("stage annotation survived peeling: {staged}"),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

/// One open-ended RK45 live session over the common ME master algorithm.
pub struct SimulationSession {
    session: MeSimulationSession<'static, 'static>,
    input_names: Vec<String>,
    variable_names: Vec<String>,
}

impl SimulationSession {
    pub fn new(model: impl Into<MeModelArtifact>, opts: SimOptions) -> Result<Self, SimError> {
        Self::new_with_execution_backend(model, opts, None)
    }

    pub fn new_with_execution_backend(
        model: impl Into<MeModelArtifact>,
        opts: SimOptions,
        execution_backend: Option<MeExecutionBackend>,
    ) -> Result<Self, SimError> {
        require_rk_mode(opts.solver_mode)?;
        let execution_backend = rumoca_solver::fmi_me::admit_execution_backend(
            opts.execution_policy,
            execution_backend,
        )?;
        let model = model.into();
        let retained = MeRetainedComponent::instantiate(
            model.source(),
            &instance_config(&opts)?,
            execution_backend,
        )?;
        let options = live_session_options(
            opts.t_start,
            opts.rtol,
            opts.atol,
            live_scan_scale(&opts),
            opts.max_wall_seconds,
        )?;
        let host = retained.into_lease(options)?;
        let variable_names = host.output_names();
        let plugin = if host.state_count() == 0 {
            None
        } else {
            let setup = host.numerical_setup(Some(default_step_size(&opts)))?;
            Some(model_exchange_integrator(setup))
        };
        let session = host.into_session(plugin)?;
        let input_names = session.input_names();
        Ok(Self {
            session,
            input_names,
            variable_names,
        })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.session.set_input(name, value).map_err(Into::into)
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.session.set_input(name, *value)?;
        }
        Ok(())
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        advance_live_session(&mut self.session, target_time)?;
        Ok(())
    }

    pub fn ensure_end_time(&mut self, _target_time: f64) {}

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt > 0.0 {
            self.advance_to(self.time() + dt)?;
        }
        Ok(())
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.session.reset(t_start).map_err(Into::into)
    }

    /// Callback counts now belong to the common host; retain the benchmark API
    /// without creating an RK-private semantic recorder.
    pub fn trace_eval_snapshot(&self, label: &str) {
        tracing::debug!(label, "RK45 uses the common Model Exchange evaluation path");
    }

    #[must_use]
    pub fn time(&self) -> f64 {
        self.session.time()
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        Ok(self.session.visible_values()?.get(name).copied())
    }

    pub fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session.visible_values()?,
        })
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible = self.session.visible_values()?;
        Ok(names
            .iter()
            .filter_map(|name| {
                visible
                    .get(name)
                    .copied()
                    .map(|value| (name.clone(), value))
            })
            .collect())
    }

    #[must_use]
    pub fn input_names(&self) -> &[String] {
        &self.input_names
    }

    #[must_use]
    pub fn variable_names(&self) -> &[String] {
        &self.variable_names
    }
}

pub fn simulate(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    simulate_with_execution_backend(model, opts, None)
}

pub fn simulate_with_execution_backend(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SimResult, SimError> {
    require_rk_mode(opts.solver_mode)?;
    let execution_backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, execution_backend)?;
    let model = model.into();
    let options = batch_session_options(
        opts.t_start,
        opts.t_end,
        opts.rtol,
        opts.atol,
        default_output_dt(opts),
        opts.max_wall_seconds,
    )?;
    let mut cursor = batch_output_cursor(&options)?;
    let retained = MeRetainedComponent::instantiate(
        model.source(),
        &instance_config(opts)?,
        execution_backend,
    )?;
    let host = retained.into_lease(options)?;
    if host.is_terminated() {
        return Ok(host.finish());
    }
    let plugin = if host.state_count() == 0 {
        None
    } else {
        let setup = host.numerical_setup(Some(default_step_size(opts)))?;
        Some(model_exchange_integrator(setup))
    };
    let mut session = host.into_session(plugin)?;
    session.run_to_stop(&mut cursor)?;
    Ok(session.finish())
}

fn require_rk_mode(requested: SimSolverMode) -> Result<(), SimError> {
    match requested {
        SimSolverMode::Auto | SimSolverMode::RkLike => Ok(()),
        requested => Err(SimError::UnsupportedSolverMode { requested }),
    }
}

fn instance_config(opts: &SimOptions) -> Result<MeInstanceConfig, SimError> {
    MeInstanceConfig::new(INSTANCE_NAME, opts.rtol, opts.t_start, opts.t_end).map_err(Into::into)
}

fn default_output_dt(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .unwrap_or_else(|| ((opts.t_end - opts.t_start).abs() / 500.0).max(1.0e-3))
}

fn default_step_size(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .map(|dt| dt.min(0.01))
        .unwrap_or(1.0e-3)
}

fn live_scan_scale(opts: &SimOptions) -> f64 {
    let requested = (opts.t_end - opts.t_start).abs();
    if requested.is_finite() && requested > 0.0 {
        requested
    } else {
        1.0
    }
}

fn checked_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, SimError> {
    let mut values = Vec::new();
    values
        .try_reserve(capacity)
        .map_err(|_| SimError::Allocation {
            context,
            entries: capacity,
        })?;
    Ok(values)
}

fn ensure_len(actual: usize, expected: usize, label: &str) -> Result<(), SimError> {
    if actual == expected {
        return Ok(());
    }
    Err(runtime_contract_violation(format!(
        "{label} {actual} does not match expected length {expected}"
    )))
}

fn runtime_contract_violation(reason: impl Into<String>) -> SimError {
    SimError::RuntimeContract {
        reason: reason.into(),
    }
}

#[cfg(test)]
mod tests;
