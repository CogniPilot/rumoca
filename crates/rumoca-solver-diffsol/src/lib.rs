//! Diffsol wiring for solver-facing IR.
//!
//! This crate intentionally does not depend on DAE-IR or compiler phases.
//! DAE-to-Solve lowering must happen before a `SolveModel`
//! reaches this backend.

// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
#![allow(clippy::arc_with_non_send_sync)]

mod error;
mod me_integrator;
mod prepared;
pub mod session;

use diffsol::{FaerSparseLU, FaerSparseMat, MatrixCommon};
use rumoca_solver::{
    SimOptions, SimResult,
    fmi_me::{
        MeExecutionBackend, MeInstanceConfig, MeModelArtifact,
        driver::{batch_output_cursor, batch_session_options},
        session::MeRetainedComponent,
    },
};
type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;
pub use error::{SimError, SimFailureStage};
pub use me_integrator::model_exchange_integrator;
pub use prepared::PreparedSimulation;

/// Whether the initialized FMI ME component can supply the exact local
/// directional derivatives required by the BDF importer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BdfCapability {
    Eligible,
    InitialLinearizationUnavailable { reason: String },
}

/// Probe BDF eligibility before integration by evaluating every basis direction
/// through the same FMI ME directional-derivative operation used by BDF.
pub fn assess_bdf_capability(
    model: &MeModelArtifact,
    opts: &SimOptions,
) -> Result<BdfCapability, SimError> {
    if model.continuous_state_count() == 0 {
        return Ok(BdfCapability::Eligible);
    }
    match check_initialization_artifact(model, opts, None) {
        Ok(()) => Ok(BdfCapability::Eligible),
        Err(SimError::ModelExchangeSession(
            rumoca_solver::fmi_me::session::MeSessionError::Component(error),
        )) => match error.into_kind() {
            rumoca_solver::fmi_me::MeError::DirectionalDerivativeUnavailable { reason } => {
                Ok(BdfCapability::InitialLinearizationUnavailable { reason })
            }
            other => Err(SimError::from(other)),
        },
        Err(error) => Err(error),
    }
}

/// Admit a host-supplied compiled execution backend against the request's
/// execution policy.
///
/// `SimExecutionPolicy::Interpreter` plus a supplied handle is a direct
/// contradiction. Executing natively would falsify the interpreter side of the
/// backend differential oracle; silently withholding the handle would let the
/// caller believe it was honored. The rule itself is owned once, by
/// [`rumoca_solver::fmi_me::admit_execution_backend`] at the ME contract
/// boundary, so every concrete backend rejects the identical contradictory
/// input identically; this crate only lifts the typed rejection into its own
/// [`SimError`] without rewording it.
fn admit_execution_backend(
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<Option<MeExecutionBackend>, SimError> {
    rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, execution_backend)
        .map_err(|contradiction| SimError::ExecutionPolicyContradiction {
            policy: contradiction.policy,
        })
}

pub fn build_simulation(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_with_execution_backend(model, opts, None)
}

/// [`build_simulation`] with a host-supplied compiled execution backend.
///
/// The handle stays opaque: it is admitted against `opts.execution_policy`
/// and handed to the generic ME runtime, which unwraps it inside the
/// `rumoca-solver` contract boundary. This crate composes no execution
/// backend of its own.
pub fn build_simulation_with_execution_backend(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_inner(model.into(), opts, execution_backend)
        .map_err(|error| error.at_stage(SimFailureStage::BackendBuild))
}

/// Backend problem construction. Failures are annotated as
/// [`SimFailureStage::BackendBuild`] by the wrapper above: nothing has been
/// integrated yet, so these are never numeric-oder failures.
fn build_simulation_inner(
    model: MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<PreparedSimulation, SimError> {
    let execution_backend = admit_execution_backend(opts, execution_backend)?;
    let retained = MeRetainedComponent::instantiate(
        model.source(),
        &instance_config(opts)?,
        execution_backend,
    )?;
    let prepared = PreparedSimulation::new(opts.clone(), retained);
    // Warm component-owned compiled evaluators during the measured build
    // stage. Initialization failures remain run-time initialization failures,
    // as before; the next lease restores the pristine component before they
    // are reported with their owning stage.
    drop(check_prepared_component(&prepared));
    Ok(prepared)
}

pub fn run_prepared_simulation(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    simulate_prepared(prepared)
}

pub fn check_prepared_initialization(prepared: &PreparedSimulation) -> Result<(), SimError> {
    prepared.check_initialization()
}

pub fn check_initialization(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
) -> Result<(), SimError> {
    check_initialization_with_execution_backend(model, opts, None)
}

/// [`check_initialization`] with a host-supplied compiled execution backend,
/// admitted against `opts.execution_policy` exactly like
/// [`build_simulation_with_execution_backend`].
pub fn check_initialization_with_execution_backend(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<(), SimError> {
    check_initialization_inner(model.into(), opts, execution_backend)
        .map_err(|error| error.at_stage(SimFailureStage::Initialization))
}

/// Settle initial conditions without integrating. Failures are annotated as
/// [`SimFailureStage::Initialization`] by the wrapper above; paths that already
/// recorded a more precise stage keep it.
fn check_initialization_inner(
    model: MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<(), SimError> {
    check_initialization_artifact(&model, opts, execution_backend)
}

fn check_initialization_artifact(
    model: &MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<(), SimError> {
    let execution_backend = admit_execution_backend(opts, execution_backend)?;
    let retained = MeRetainedComponent::instantiate(
        model.source(),
        &instance_config(opts)?,
        execution_backend,
    )?;
    check_prepared_component(&PreparedSimulation::new(opts.clone(), retained))
}

pub fn simulate(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    simulate_with_execution_backend(model, opts, None)
}

/// [`simulate`] with a host-supplied compiled execution backend, admitted
/// against `opts.execution_policy` exactly like
/// [`build_simulation_with_execution_backend`].
pub fn simulate_with_execution_backend(
    model: impl Into<MeModelArtifact>,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SimResult, SimError> {
    let prepared = build_simulation_with_execution_backend(model, opts, execution_backend)?;
    run_prepared_simulation(&prepared)
}

fn simulate_prepared(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    let options = common_session_options(&prepared.opts)?;
    let mut cursor = batch_output_cursor(&options)?;
    let mut retained =
        prepared
            .retained
            .try_borrow_mut()
            .map_err(|_| SimError::RuntimeContract {
                reason: "a prepared FMI component already has an active simulation lease"
                    .to_owned(),
            })?;
    let host = retained.lease(options)?;
    if host.is_terminated() {
        return Ok(host.finish());
    }
    let plugin = plugin_for_host(&host, &prepared.opts)?;
    let mut session = host.into_session(plugin)?;
    session.run_to_stop(&mut cursor)?;
    Ok(session.finish())
}

pub(crate) fn check_prepared_component(prepared: &PreparedSimulation) -> Result<(), SimError> {
    let options = common_session_options(&prepared.opts)?;
    let mut retained =
        prepared
            .retained
            .try_borrow_mut()
            .map_err(|_| SimError::RuntimeContract {
                reason: "a prepared FMI component already has an active initialization lease"
                    .to_owned(),
            })?;
    let host = retained.lease(options)?;
    if host.is_terminated() {
        return Ok(());
    }
    let plugin = plugin_for_host(&host, &prepared.opts)?;
    drop(host.into_session(plugin)?);
    Ok(())
}

fn plugin_for_host(
    host: &rumoca_solver::fmi_me::session::MeComponentHost<'_>,
    opts: &SimOptions,
) -> Result<Option<Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend>>, SimError> {
    if host.state_count() == 0 {
        return Ok(None);
    }
    let setup = host.numerical_setup(Some(default_step_size(opts)))?;
    Ok(Some(model_exchange_integrator(setup)))
}

fn common_session_options(
    opts: &SimOptions,
) -> Result<rumoca_solver::fmi_me::session::MeSessionOptions, SimError> {
    Ok(batch_session_options(
        opts.t_start,
        opts.t_end,
        opts.rtol,
        opts.atol,
        default_output_dt(opts),
        opts.max_wall_seconds,
    )?)
}

fn instance_config(opts: &SimOptions) -> Result<MeInstanceConfig, SimError> {
    MeInstanceConfig::new("bdf", opts.rtol, opts.t_start, opts.t_end).map_err(Into::into)
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

#[cfg(test)]
mod tests;
