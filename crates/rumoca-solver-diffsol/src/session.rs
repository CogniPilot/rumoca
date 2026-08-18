//! Diffsol live sessions over the sole common FMI Model Exchange host.
//!
//! This module owns no Modelica lifecycle, event loop, trace recorder, root
//! policy, or zero-state path. Diffsol contributes only its numerical plugin;
//! [`MeSimulationSession`] owns every semantic transition for both stateful and
//! time-only components.

use indexmap::IndexMap;
use rumoca_solver::{
    SimOptions,
    fmi_me::{
        MeExecutionBackend, MeModelArtifact,
        driver::{advance_live_session, live_session_options},
        session::{MeRetainedComponent, MeSimulationSession},
    },
};

use crate::{SimError, default_step_size, instance_config, model_exchange_integrator};

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

/// One open-ended live session using Diffsol only as its numerical method.
pub struct SimulationSession {
    session: MeSimulationSession<'static, 'static>,
    input_names: Vec<String>,
    variable_names: Vec<String>,
}

impl SimulationSession {
    pub fn new(model: impl Into<MeModelArtifact>, opts: SimOptions) -> Result<Self, SimError> {
        Self::new_with_execution_backend(model, opts, None)
    }

    /// Construct the same common live host with an optional opaque evaluator.
    pub fn new_with_execution_backend(
        model: impl Into<MeModelArtifact>,
        opts: SimOptions,
        execution_backend: Option<MeExecutionBackend>,
    ) -> Result<Self, SimError> {
        let execution_backend = crate::admit_execution_backend(&opts, execution_backend)?;
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

    /// Live sessions have undefined FMI stop metadata and therefore need no
    /// mutable horizon. The next `advance_to` yield is the exact public bound.
    pub fn ensure_end_time(&mut self, _target_time: f64) {}

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.session.reset(t_start).map_err(Into::into)
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

    /// Scheduling cadence is host policy, not a BDF characteristic.
    #[must_use]
    pub const fn max_schedule_advance_dt(&self) -> Option<f64> {
        None
    }
}

fn live_scan_scale(opts: &SimOptions) -> f64 {
    let requested = (opts.t_end - opts.t_start).abs();
    if requested.is_finite() && requested > 0.0 {
        requested
    } else {
        1.0
    }
}
