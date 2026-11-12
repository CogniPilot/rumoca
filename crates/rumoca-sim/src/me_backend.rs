//! Solver-neutral composition of a numerical ME plugin with the common host.

use indexmap::IndexMap;
use rumoca_solver::{
    SimExecutionEngine, SimExecutionPolicy, SimExecutionReceipt, SimOptions,
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
use rumoca_solver::fmi_me::numerical_observation::{
    AdapterWriter, ConstructionPurpose, HostWriter, ObservationRoute, SettingsCollector,
    SettingsStage,
};

/// A validated numerical setup composed with optional passive observation.
pub(crate) type IntegratorBuilder = fn(
    MeNumericalSetup,
    crate::backend_options::ValidatedOptions,
    Option<AdapterWriter>,
) -> Result<Box<dyn MeIntegratorBackend + 'static>, SimError>;

/// An integrator plugin factory with the method name a run records.
#[derive(Clone, Copy)]
pub(crate) struct IntegratorFactory {
    pub(crate) method: &'static str,
    pub(crate) backend: rumoca_solver::NumericalBackend,
    pub(crate) model_jacobian_request: rumoca_solver::ModelStateJacobianRequest,
    pub(crate) build: IntegratorBuilder,
}

pub(crate) struct BackendSimulationSession {
    session: MeSimulationSession<'static, 'static>,
    input_names: Vec<String>,
    variable_names: Vec<String>,
    execution: SimExecutionReceipt,
    execution_backend: Option<MeExecutionBackend>,
}

impl BackendSimulationSession {
    pub(crate) fn new(
        artifact: MeModelArtifact,
        opts: &SimOptions,
        execution_backend: Option<MeExecutionBackend>,
        instance_name: &'static str,
        integrator: IntegratorFactory,
    ) -> Result<Self, SimError> {
        crate::backend_options::validate_for_backend(opts, integrator.backend)?;
        let execution_backend = rumoca_solver::fmi_me::admit_execution_backend(
            opts.execution_policy,
            execution_backend,
        )?;
        let execution = artifact
            .execution_receipt()
            .unwrap_or(SimExecutionReceipt::admission(
                SimExecutionPolicy::Interpreter,
                0,
                0,
                SimExecutionEngine::Interpreter,
            ));
        let handle = execution_backend.clone();
        let retained = MeRetainedComponent::instantiate(
            artifact.source(),
            &MeInstanceConfig::open_ended(instance_name, opts.rtol, opts.t_start)?
                .with_model_state_jacobian_request(integrator.model_jacobian_request),
            execution_backend,
        )?;
        let options = live_session_options(
            &artifact.root_location(),
            opts.t_start,
            opts.rtol,
            opts.atol,
            live_scan_scale(opts),
            opts.max_wall_seconds,
        )?;
        let host = retained.into_lease(options)?;
        let variable_names = host.output_names()?;
        let plugin = plugin_for_host(&host, opts, integrator)?;
        let session = host.into_session(plugin)?;
        let input_names = session.input_names();
        Ok(Self {
            session,
            input_names,
            variable_names,
            execution,
            execution_backend: handle,
        })
    }

    /// The engine selection with the compile requests the backend has declined
    /// so far; compilation is lazy, so the list can grow as the session runs.
    pub(crate) fn execution_receipt(&self) -> SimExecutionReceipt {
        let Some(backend) = self.execution_backend.as_ref() else {
            return self.execution.clone();
        };
        self.execution
            .clone()
            .with_declined(backend.declined())
            .with_programs(backend.programs())
    }

    pub(crate) fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        self.session.set_inputs(inputs).map_err(Into::into)
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
        Ok(self.selected_values(&[name])?.into_iter().next().flatten())
    }

    /// The requested values in request order; `None` for an unknown name.
    pub(crate) fn selected_values<N: AsRef<str>>(
        &self,
        names: &[N],
    ) -> Result<Vec<Option<f64>>, SimError> {
        self.session.selected_values(names).map_err(Into::into)
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
    observation: Option<&SettingsCollector>,
) -> Result<SimResult, SimError> {
    crate::backend_options::validate_for_backend(opts, integrator.backend)?;
    let execution_backend =
        rumoca_solver::fmi_me::admit_execution_backend(opts.execution_policy, execution_backend)?;
    let options = batch_options(&artifact.root_location(), opts)?;
    let mut cursor = batch_output_cursor(&options)?;
    let retained = MeRetainedComponent::instantiate(
        artifact.source(),
        &instance_config(instance_name, opts)?
            .with_model_state_jacobian_request(integrator.model_jacobian_request),
        execution_backend,
    )?;
    let host = retained.into_lease(options)?;
    if host.is_terminated() {
        if host.state_count() == 0
            && let Some(collector) = observation
        {
            collector.no_numerical_backend(ObservationRoute::ModelExchangeSession);
        }
        return Ok(host.finish());
    }
    let (plugin, writer) = plugin_for_host_with_observation(
        &host,
        opts,
        integrator,
        observation,
        ConstructionPurpose::SelectedRun,
    )?;
    let mut session = host.into_session_with_numerical_observation(plugin, writer)?;
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
    plugin_for_host_with_observation(
        host,
        opts,
        integrator,
        None,
        ConstructionPurpose::SelectedRun,
    )
    .map(|(plugin, _)| plugin)
}

/// The constructed plugin and its paired host observation endpoint.
/// Naming the existing tuple changes no construction, ownership or error ordering.
type ObservedPlugin = (Option<Box<dyn MeIntegratorBackend>>, Option<HostWriter>);

/// Associate only the actual construction, after the existing setup succeeds.
pub(crate) fn plugin_for_host_with_observation(
    host: &rumoca_solver::fmi_me::session::MeComponentHost<'_>,
    opts: &SimOptions,
    integrator: IntegratorFactory,
    observation: Option<&SettingsCollector>,
    purpose: ConstructionPurpose,
) -> Result<ObservedPlugin, SimError> {
    let configured = crate::backend_options::validate_for_backend(opts, integrator.backend)?;
    if host.state_count() == 0 {
        rumoca_solver::note_integrator("none (no continuous states)");
        if let Some(collector) = observation {
            collector.no_numerical_backend(ObservationRoute::ModelExchangeSession);
        }
        return Ok((None, None));
    }
    let setup = host.numerical_setup(Some(default_step_size(opts)))?;
    let endpoints = observation
        .and_then(|collector| collector.associate(ObservationRoute::ModelExchangeSession));
    let (host_writer, adapter_writer) = match endpoints {
        Some((host, adapter)) => {
            host.issued_setup(&setup);
            host.construction(
                integrator.backend,
                purpose,
                SettingsStage::ConstructionStarted,
            );
            (Some(host), Some(adapter))
        }
        None => (None, None),
    };
    rumoca_solver::note_integrator(integrator.method);
    let plugin = (integrator.build)(setup, configured, adapter_writer)?;
    if let Some(writer) = &host_writer {
        writer.construction(integrator.backend, purpose, SettingsStage::Constructed);
    }
    Ok((Some(plugin), host_writer))
}

pub(crate) fn instance_config(
    instance_name: &'static str,
    opts: &SimOptions,
) -> Result<MeInstanceConfig, SimError> {
    MeInstanceConfig::new(instance_name, opts.rtol, opts.t_start, opts.t_end).map_err(Into::into)
}

pub(crate) fn default_output_dt(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .unwrap_or_else(|| ((opts.t_end - opts.t_start).abs() / 500.0).max(1.0e-3))
}

pub(crate) fn batch_options(
    plan: &rumoca_ir_solve::fmi::RootLocationPlan,
    opts: &SimOptions,
) -> Result<rumoca_solver::fmi_me::session::MeSessionOptions, SimError> {
    batch_session_options(
        plan,
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
