//! Solver-neutral composition of a numerical ME plugin with the common host.

use indexmap::IndexMap;
use rumoca_solver::{
    SimOptions,
    fmi_me::{
        MeExecutionBackend, MeInstanceConfig, MeIntegratorBackend, MeModelArtifact,
        MeNumericalSetup,
        driver::{advance_live_session, batch_session_options, live_session_options},
        session::{MeRetainedComponent, MeSimulationSession},
    },
};

#[cfg(feature = "solver-rk45")]
use rumoca_solver::{SimResult, fmi_me::driver::batch_output_cursor};

use crate::SimError;

pub(crate) type IntegratorFactory = fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend + 'static>;

pub(crate) struct BackendSimulationSession {
    session: MeSimulationSession<'static, 'static>,
    input_names: Vec<String>,
    variable_names: Vec<String>,
}

impl BackendSimulationSession {
    pub(crate) fn new(
        artifact: MeModelArtifact,
        opts: &SimOptions,
        execution_backend: Option<MeExecutionBackend>,
        instance_name: &'static str,
        integrator: IntegratorFactory,
    ) -> Result<Self, SimError> {
        let execution_backend = rumoca_solver::fmi_me::admit_execution_backend(
            opts.execution_policy,
            execution_backend,
        )?;
        let retained = MeRetainedComponent::instantiate(
            artifact.source(),
            &instance_config(instance_name, opts)?,
            execution_backend,
        )?;
        let options = live_session_options(
            opts.t_start,
            opts.rtol,
            opts.atol,
            live_scan_scale(opts),
            opts.max_wall_seconds,
        )?;
        let host = retained.into_lease(options)?;
        let variable_names = host.output_names();
        let plugin = plugin_for_host(&host, opts, integrator)?;
        let session = host.into_session(plugin)?;
        let input_names = session.input_names();
        Ok(Self {
            session,
            input_names,
            variable_names,
        })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.session.set_input(name, value).map_err(Into::into)
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        advance_live_session(&mut self.session, target_time)?;
        Ok(())
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.session.reset(t_start).map_err(Into::into)
    }

    pub(crate) fn time(&self) -> f64 {
        self.session.time()
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        Ok(self.session.visible_values()?.get(name).copied())
    }

    pub(crate) fn visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        self.session.visible_values().map_err(Into::into)
    }

    #[cfg(feature = "solver-rk45")]
    pub(crate) fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible = self.visible_values()?;
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

    pub(crate) fn input_names(&self) -> &[String] {
        &self.input_names
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        &self.variable_names
    }
}

#[cfg(feature = "solver-rk45")]
pub(crate) fn simulate_artifact(
    artifact: MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<MeExecutionBackend>,
    instance_name: &'static str,
    integrator: IntegratorFactory,
) -> Result<SimResult, SimError> {
    let execution_backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, execution_backend)?;
    let options = batch_options(opts)?;
    let mut cursor = batch_output_cursor(&options)?;
    let retained = MeRetainedComponent::instantiate(
        artifact.source(),
        &instance_config(instance_name, opts)?,
        execution_backend,
    )?;
    let host = retained.into_lease(options)?;
    if host.is_terminated() {
        return Ok(host.finish());
    }
    let plugin = plugin_for_host(&host, opts, integrator)?;
    let mut session = host.into_session(plugin)?;
    session.run_to_stop(&mut cursor)?;
    Ok(session.finish())
}

pub(crate) fn default_step_size(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .map(|dt| dt.min(0.01))
        .unwrap_or(1.0e-3)
}

pub(crate) fn plugin_for_host(
    host: &rumoca_solver::fmi_me::session::MeComponentHost<'_>,
    opts: &SimOptions,
    integrator: IntegratorFactory,
) -> Result<Option<Box<dyn MeIntegratorBackend>>, SimError> {
    if host.state_count() == 0 {
        return Ok(None);
    }
    let setup = host.numerical_setup(Some(default_step_size(opts)))?;
    Ok(Some(integrator(setup)))
}

pub(crate) fn instance_config(
    instance_name: &'static str,
    opts: &SimOptions,
) -> Result<MeInstanceConfig, SimError> {
    MeInstanceConfig::new(instance_name, opts.rtol, opts.t_start, opts.t_end).map_err(Into::into)
}

fn default_output_dt(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .unwrap_or_else(|| ((opts.t_end - opts.t_start).abs() / 500.0).max(1.0e-3))
}

pub(crate) fn batch_options(
    opts: &SimOptions,
) -> Result<rumoca_solver::fmi_me::session::MeSessionOptions, SimError> {
    batch_session_options(
        opts.t_start,
        opts.t_end,
        opts.rtol,
        opts.atol,
        default_output_dt(opts),
        opts.max_wall_seconds,
    )
    .map_err(Into::into)
}

fn live_scan_scale(opts: &SimOptions) -> f64 {
    let requested = (opts.t_end - opts.t_start).abs();
    if requested.is_finite() && requested > 0.0 {
        requested
    } else {
        1.0
    }
}
