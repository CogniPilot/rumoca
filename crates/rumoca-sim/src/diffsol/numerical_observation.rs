//! Optional numerical observation around the existing BDF construction and lifecycle.

use std::time::Instant;

use rumoca_ir_dae as dae;
use rumoca_solver::fmi_me::numerical_observation::SettingsCollector;

use super::{
    BDF_INTEGRATOR, BuildSimulationTimings, PreparedSimulation, SimError, SimFailureStage,
    build_simulation_artifact, diagnostic_sim_error,
};
use crate::me_backend::{batch_options, plugin_for_host_with_observation};
use crate::solve_lowering::{
    apply_correlated_simulation_overrides, finish_runtime_fmi_artifact,
    lower_correlated_for_simulation_with_stage_timing_and_param_overrides, tunable_param_overrides,
};

pub(super) fn build_simulation_with_observation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    mut begin_stage: impl FnMut(&'static str),
    mut observe_lowered_model: impl FnMut(&rumoca_phase_solve::LoweredSolveModel<'_>),
    observation: Option<&SettingsCollector>,
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    let param_overrides = tunable_param_overrides(dae_model, opts).map_err(diagnostic_sim_error)?;
    let (mut lowered, solve_timings) =
        lower_correlated_for_simulation_with_stage_timing_and_param_overrides(
            dae_model,
            opts,
            &param_overrides,
            &mut begin_stage,
        )
        .map_err(diagnostic_sim_error)?;
    begin_stage("sim_overrides");
    let override_apply_start = Instant::now();
    apply_correlated_simulation_overrides(&mut lowered, dae_model, opts)
        .map_err(diagnostic_sim_error)?;
    let override_apply_seconds = override_apply_start.elapsed().as_secs_f64();
    observe_lowered_model(&lowered);
    begin_stage("sim_build");
    let backend_build_start = Instant::now();
    let (artifact, execution_backend) =
        finish_runtime_fmi_artifact(lowered, opts).map_err(diagnostic_sim_error)?;
    let prepared = build_simulation_artifact(artifact, opts, execution_backend, observation)
        .map_err(|error| error.at_stage(SimFailureStage::BackendBuild))?;
    let backend_build_seconds = backend_build_start.elapsed().as_secs_f64();
    Ok((
        prepared,
        BuildSimulationTimings {
            ir_solve_structural_dae_seconds: solve_timings.ir_solve_structural_dae_seconds,
            ir_solve_lower_seconds: solve_timings.ir_solve_lower_seconds,
            ir_solve_seconds: solve_timings.ir_solve_seconds,
            override_apply_seconds,
            backend_build_seconds,
        },
    ))
}

pub(super) fn simulate_prepared_with_observation(
    prepared: &PreparedSimulation,
    observation: Option<&SettingsCollector>,
) -> Result<rumoca_solver::SimResult, SimError> {
    let options = batch_options(&prepared.root_location, &prepared.opts)?;
    let mut cursor = rumoca_solver::fmi_me::driver::batch_output_cursor(&options)?;
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
    let (plugin, host_writer) =
        plugin_for_host_with_observation(&host, &prepared.opts, BDF_INTEGRATOR, observation)?;
    let mut session = host.into_session_with_numerical_observation(plugin, host_writer)?;
    session.run_to_stop(&mut cursor)?;
    Ok(session.finish())
}

pub(super) fn check_prepared_component_with_observation(
    prepared: &PreparedSimulation,
    observation: Option<&SettingsCollector>,
) -> Result<(), SimError> {
    let options = batch_options(&prepared.root_location, &prepared.opts)?;
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
    let (plugin, host_writer) =
        plugin_for_host_with_observation(&host, &prepared.opts, BDF_INTEGRATOR, observation)?;
    drop(host.into_session_with_numerical_observation(plugin, host_writer)?);
    Ok(())
}

/// Observe BDF batch construction and execution through the same numerical path.
///
/// The existing warm-up and actual run receive distinct construction associations.
/// The caller owns the collector and must finalize it after this function returns,
/// including on an error. This does not certify coordinates or source provenance,
/// and does not imply observation support for trajectory, live or FMI-LS routes.
///
/// The high-level facade exposes this explicit, opt-in batch entry:
/// ```no_run
/// use rumoca_solver::fmi_me::numerical_observation::SettingsCollector;
/// let _entry: fn(
///     &rumoca_ir_dae::Dae,
///     &rumoca_sim::SimOptions,
///     &SettingsCollector,
/// ) -> Result<rumoca_sim::SimResult, rumoca_sim::SimError> =
///     rumoca_sim::simulate_with_numerical_observation;
/// ```
pub fn simulate_with_numerical_observation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    observation: &SettingsCollector,
) -> Result<rumoca_solver::SimResult, SimError> {
    let (prepared, _) =
        build_simulation_with_observation(dae_model, opts, |_| {}, |_| {}, Some(observation))?;
    simulate_prepared_with_observation(&prepared, Some(observation))
}
