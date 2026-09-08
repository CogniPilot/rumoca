//! Canonical one-shot batch preparation.
//!
//! Solver selection, FMI batch admission, initialization, numerical-plugin
//! construction, and output-grid ownership meet here exactly once. The public
//! product is already initialized and can only be consumed into its one run.

use std::time::Instant;

#[cfg(all(test, feature = "solver-diffsol", feature = "solver-rk45"))]
use std::cell::Cell;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::fmi_me::{
    MeExecutionBackend, MeModelArtifact,
    session::{MeAdmittedBatch, MeBatchSession, MeRetainedComponent},
};

use crate::me_backend::{batch_options, instance_config, plugin_for_host};
use crate::solve_lowering::{
    SimulationDiagnosticError, construction_overrides, finish_runtime_fmi_artifact,
    lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides,
};
use crate::{BuildSimulationTimings, SimError, SimFailureStage, SimSolverMode};

/// One admitted and initialized batch, paired with its selected integrator and
/// checked output cursor.
///
/// The fields and constructors are private, the type is non-cloneable, and
/// [`Self::run`] consumes the product. A caller therefore cannot re-admit or
/// reinitialize the retained FMI component through this surface.
#[must_use]
pub struct PreparedSimulation {
    solver: SelectedSolver,
    state: PreparedState,
}

enum PreparedState {
    Terminated(MeAdmittedBatch<'static>),
    Active(MeBatchSession<'static, 'static>),
}

#[derive(Clone, Copy)]
enum SelectedSolver {
    #[cfg(feature = "solver-diffsol")]
    Bdf,
    #[cfg(feature = "solver-rk45")]
    RkLike,
}

pub(crate) enum SelectedRetainedComponent {
    #[cfg(feature = "solver-diffsol")]
    Bdf(MeRetainedComponent),
    #[cfg(feature = "solver-rk45")]
    RkLike(MeRetainedComponent),
}

impl PreparedSimulation {
    /// Report the solver selected before initialization and integration.
    #[must_use]
    pub fn backend(&self) -> rumoca_solver::SimBackend {
        match self.solver {
            #[cfg(feature = "solver-diffsol")]
            SelectedSolver::Bdf => rumoca_solver::SimBackend::Diffsol,
            #[cfg(feature = "solver-rk45")]
            SelectedSolver::RkLike => rumoca_solver::SimBackend::Rk45,
        }
    }

    /// Consume the one prepared batch and execute its remaining integration.
    ///
    /// ```compile_fail,E0382
    /// use rumoca_sim::PreparedSimulation;
    ///
    /// fn cannot_run_twice(prepared: PreparedSimulation) {
    ///     let _first = prepared.run();
    ///     let _second = prepared.run();
    /// }
    /// ```
    pub fn run(self) -> Result<rumoca_solver::SimResult, SimError> {
        match self.state {
            PreparedState::Terminated(batch) => Ok(batch.finish()),
            PreparedState::Active(mut batch) => {
                batch.run_to_stop()?;
                Ok(batch.finish())
            }
        }
    }
}

/// Construct the sole public prepared-simulation product.
///
/// `begin_stage` observes phase boundaries and `observe_solve_model` borrows
/// the exact Solve root retained by the FMI construction. Neither callback can
/// construct, replace, or validate the returned product.
pub fn prepare_simulation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    mut begin_stage: impl FnMut(&'static str),
    mut observe_solve_model: impl FnMut(&solve::SolveModel),
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    begin_stage("sim_overrides");
    let override_apply_start = Instant::now();
    let overrides = construction_overrides(dae_model, opts).map_err(diagnostic_sim_error)?;
    let override_apply_seconds = override_apply_start.elapsed().as_secs_f64();
    let (lowered, solve_timings) =
        lower_correlated_for_simulation_with_stage_timing_and_runtime_overrides(
            dae_model,
            opts,
            &overrides,
            &mut begin_stage,
        )
        .map_err(diagnostic_sim_error)?;
    observe_solve_model(lowered.model());

    begin_stage("sim_build");
    let backend_build_start = Instant::now();
    let (artifact, execution_backend) =
        finish_runtime_fmi_artifact(lowered, opts).map_err(diagnostic_sim_error)?;
    let selected = select_retained_component(artifact, opts, execution_backend)
        .map_err(|error| error.at_stage(SimFailureStage::BackendBuild))?;
    let backend_build_seconds = backend_build_start.elapsed().as_secs_f64();

    begin_stage("sim_initialization");
    let initialization_start = Instant::now();
    let prepared = prepare_selected(selected, opts)?;
    let initialization_seconds = initialization_start.elapsed().as_secs_f64();
    Ok((
        prepared,
        BuildSimulationTimings {
            ir_solve_structural_dae_seconds: solve_timings.ir_solve_structural_dae_seconds,
            ir_solve_lower_seconds: solve_timings.ir_solve_lower_seconds,
            ir_solve_seconds: solve_timings.ir_solve_seconds,
            override_apply_seconds,
            backend_build_seconds,
            initialization_seconds,
        },
    ))
}

pub(crate) fn prepare_fmi_component(
    component: rumoca_ir_solve::fmi::FmiComponent,
    opts: &rumoca_solver::SimOptions,
) -> Result<PreparedSimulation, SimError> {
    let execution_backend = crate::native_execution::admitted_native_execution_backend(
        opts,
        component.runtime_model(),
    )?;
    prepare_artifact(MeModelArtifact::new(component), opts, execution_backend)
}

pub(crate) fn prepare_artifact(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<PreparedSimulation, SimError> {
    let selected = select_retained_component(artifact, opts, execution_backend)
        .map_err(|error| error.at_stage(SimFailureStage::BackendBuild))?;
    prepare_selected(selected, opts)
}

pub(crate) fn select_retained_component(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    match opts.solver_mode {
        SimSolverMode::Auto => select_auto(artifact, opts, execution_backend),
        SimSolverMode::Bdf => select_bdf(artifact, opts, execution_backend),
        SimSolverMode::RkLike => select_rk_like(artifact, opts, execution_backend),
    }
}

fn instantiate(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
    instance_name: &'static str,
) -> Result<MeRetainedComponent, SimError> {
    let execution =
        rumoca_solver::fmi_me::select_execution(opts.execution_policy, execution_backend)?;
    MeRetainedComponent::instantiate(
        artifact.into_source(),
        &instance_config(instance_name, opts)?,
        execution,
    )
    .map_err(Into::into)
}

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
fn select_auto(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    let mut retained = instantiate(artifact, opts, execution_backend, "auto")?;
    match assess_bdf_capability(&mut retained, opts)? {
        BdfCapability::Eligible => Ok(SelectedRetainedComponent::Bdf(retained)),
        BdfCapability::InitialLinearizationUnavailable { reason } => {
            tracing::debug!(
                target: "rumoca_sim::solver_selection",
                %reason,
                "auto selected rk-like because the initial BDF linearization is unavailable"
            );
            Ok(SelectedRetainedComponent::RkLike(retained))
        }
    }
}

#[cfg(all(feature = "solver-diffsol", not(feature = "solver-rk45")))]
fn select_auto(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    select_bdf(artifact, opts, execution_backend)
}

#[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
fn select_auto(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    select_rk_like(artifact, opts, execution_backend)
}

#[cfg(feature = "solver-diffsol")]
fn select_bdf(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    instantiate(artifact, opts, execution_backend, "bdf").map(SelectedRetainedComponent::Bdf)
}

#[cfg(not(feature = "solver-diffsol"))]
fn select_bdf(
    _artifact: MeModelArtifact,
    _opts: &rumoca_solver::SimOptions,
    _execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    Err(SimError::UnsupportedSolverMode {
        backend: "rumoca-sim",
        requested: SimSolverMode::Bdf,
    })
}

#[cfg(feature = "solver-rk45")]
fn select_rk_like(
    artifact: MeModelArtifact,
    opts: &rumoca_solver::SimOptions,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    instantiate(artifact, opts, execution_backend, "rk-like").map(SelectedRetainedComponent::RkLike)
}

#[cfg(not(feature = "solver-rk45"))]
fn select_rk_like(
    _artifact: MeModelArtifact,
    _opts: &rumoca_solver::SimOptions,
    _execution_backend: Option<MeExecutionBackend>,
) -> Result<SelectedRetainedComponent, SimError> {
    Err(SimError::UnsupportedSolverMode {
        backend: "rumoca-sim",
        requested: SimSolverMode::RkLike,
    })
}

fn prepare_selected(
    selected: SelectedRetainedComponent,
    opts: &rumoca_solver::SimOptions,
) -> Result<PreparedSimulation, SimError> {
    let (retained, solver) = match selected {
        #[cfg(feature = "solver-diffsol")]
        SelectedRetainedComponent::Bdf(retained) => (retained, SelectedSolver::Bdf),
        #[cfg(feature = "solver-rk45")]
        SelectedRetainedComponent::RkLike(retained) => (retained, SelectedSolver::RkLike),
    };
    let admission = retained.into_batch_admission(batch_options(opts)?)?;
    let batch = admission.into_batch()?;
    if batch.is_terminated() {
        return Ok(PreparedSimulation {
            solver,
            state: PreparedState::Terminated(batch),
        });
    }
    let plugin = match solver {
        #[cfg(feature = "solver-diffsol")]
        SelectedSolver::Bdf => plugin_for_host(
            batch.component_host(),
            opts,
            rumoca_solver_diffsol::model_exchange_integrator,
        )?,
        #[cfg(feature = "solver-rk45")]
        SelectedSolver::RkLike => plugin_for_host(
            batch.component_host(),
            opts,
            rumoca_solver_rk45::model_exchange_integrator,
        )?,
    };
    let batch = batch.into_session(plugin)?;
    Ok(PreparedSimulation {
        solver,
        state: PreparedState::Active(batch),
    })
}

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
#[derive(Debug, PartialEq, Eq)]
enum BdfCapability {
    Eligible,
    InitialLinearizationUnavailable { reason: String },
}

#[cfg(all(test, feature = "solver-diffsol", feature = "solver-rk45"))]
thread_local! {
    static BDF_CAPABILITY_PROBES: Cell<usize> = const { Cell::new(0) };
}

#[cfg(all(test, feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) fn verification_reset_bdf_capability_probes() {
    BDF_CAPABILITY_PROBES.set(0);
}

#[cfg(all(test, feature = "solver-diffsol", feature = "solver-rk45"))]
pub(crate) fn verification_bdf_capability_probe_count() -> usize {
    BDF_CAPABILITY_PROBES.get()
}

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
fn assess_bdf_capability(
    retained: &mut MeRetainedComponent,
    opts: &rumoca_solver::SimOptions,
) -> Result<BdfCapability, SimError> {
    #[cfg(test)]
    BDF_CAPABILITY_PROBES.set(BDF_CAPABILITY_PROBES.get() + 1);
    if retained.state_count() == 0 {
        return Ok(BdfCapability::Eligible);
    }
    let admission = retained.admit_batch(batch_options(opts)?)?;
    let batch = admission.into_batch()?;
    if batch.is_terminated() {
        return Ok(BdfCapability::Eligible);
    }
    let plugin = plugin_for_host(
        batch.component_host(),
        opts,
        rumoca_solver_diffsol::model_exchange_integrator,
    )?;
    let probe = match batch.into_session(plugin) {
        Ok(session) => {
            drop(session);
            Ok(())
        }
        Err(error) => Err(SimError::from(error)),
    };
    classify_bdf_capability(probe)
}

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
fn classify_bdf_capability(probe: Result<(), SimError>) -> Result<BdfCapability, SimError> {
    match probe {
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

fn diagnostic_sim_error(err: SimulationDiagnosticError) -> SimError {
    let code = err.diagnostic_code();
    match err {
        SimulationDiagnosticError::NativeExecution {
            stage: execution_stage,
            owner,
            reason,
        } => SimError::NativeExecution {
            execution_stage,
            owner,
            reason,
        },
        other => SimError::SolveIr(format!("[{code}] {other}")),
    }
}

#[cfg(all(test, feature = "solver-diffsol", feature = "solver-rk45"))]
mod tests {
    use super::{BdfCapability, classify_bdf_capability};
    use crate::SimError;

    #[test]
    fn only_directional_derivative_unavailability_is_a_capability_result() {
        let ordinary_failure = classify_bdf_capability(Err(SimError::EmptySystem));
        assert!(matches!(ordinary_failure, Err(SimError::EmptySystem)));

        let unavailable = classify_bdf_capability(Err(SimError::ModelExchangeSession(
            rumoca_solver::fmi_me::session::MeSessionError::Component(
                rumoca_solver::fmi_me::MeError::DirectionalDerivativeUnavailable {
                    reason: "undefined local sensitivity".to_owned(),
                },
            ),
        )))
        .expect("the one typed capability result selects the explicit host");
        assert_eq!(
            unavailable,
            BdfCapability::InitialLinearizationUnavailable {
                reason: "undefined local sensitivity".to_owned(),
            }
        );
    }
}
