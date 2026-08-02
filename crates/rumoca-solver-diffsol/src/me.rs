//! Diffsol's FMI 3 Model Exchange host adapter.
//!
//! Diffsol requires cloneable `Fn` callbacks while the ME component is mutable.
//! SPEC_0038 phase 2 therefore uses one `Rc<RefCell<SolveMeKernel>>`: every
//! callback takes exactly one mutable borrow for its ordered set-then-get FMI
//! sequence. `MeRootProfile::DiffsolFrozen` is a temporary migration shim; it
//! preserves the old root-search inventory and initial-event trigger until
//! those divergences are discharged independently.

use std::{cell::RefCell, rc::Rc};

use rumoca_solver::{
    SimOptions, SimTermination,
    fmi_me::{
        MeError, MeEventCause, MeEventEntry, MeIndicatorCrossing, MeInstanceConfig, MeModelSource,
        MeNumericsProfile, MeObservation, MeRootProfile, MeStage, MeStepCompletion, MeTime,
        ModelExchangeKernel, SolveMeKernel,
    },
    time_match_with_tol,
    timeline::sample_time_match_with_tol,
};

use crate::SimError;

pub(crate) const INSTANCE_NAME: &str = "bdf";

pub(crate) type SharedMeKernel = Rc<RefCell<SolveMeKernel>>;
type SharedCallbackError = Rc<RefCell<Option<MeError>>>;

/// Settled state handed from the ME initialization lifecycle to Diffsol.
pub(crate) struct MeInitialState {
    pub(crate) time: f64,
    pub(crate) states: Vec<f64>,
    pub(crate) observations: Vec<MeObservation>,
    pub(crate) termination: Option<SimTermination>,
}

pub(crate) struct MePostEventState {
    pub(crate) time: f64,
    pub(crate) states: Vec<f64>,
    pub(crate) entry: MeEventEntry,
    pub(crate) termination: Option<SimTermination>,
}

/// State-only Diffsol host over the one checked ME component.
#[derive(Clone)]
pub(crate) struct DiffsolMeHost {
    kernel: SharedMeKernel,
    callback_error: SharedCallbackError,
}

impl DiffsolMeHost {
    pub(crate) fn instantiate<'a>(
        source: impl Into<MeModelSource<'a>>,
        opts: &SimOptions,
    ) -> Result<Self, SimError> {
        let config = MeInstanceConfig {
            instance_name: INSTANCE_NAME,
            tolerance: opts.atol.max(1.0e-10),
            start_time: opts.t_start,
            stop_time: opts.t_end,
            root_profile: MeRootProfile::DiffsolFrozen,
            numerics_profile: MeNumericsProfile::DiffsolFrozen,
        };
        let kernel = SolveMeKernel::instantiate(source.into(), &config)?;
        Ok(Self {
            kernel: Rc::new(RefCell::new(kernel)),
            callback_error: Rc::new(RefCell::new(None)),
        })
    }

    pub(crate) fn initialize(
        &self,
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
    ) -> Result<MeInitialState, SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.enter_initialization_mode()?;
        kernel.exit_initialization_mode()?;
        let mut discrete = kernel.update_discrete_states()?;
        while discrete.discrete_states_need_update {
            discrete = kernel.update_discrete_states()?;
        }
        kernel.enter_continuous_time_mode()?;
        let state_count = kernel.model_description().continuous_state_count;
        let mut states = vec![0.0; state_count];
        kernel.get_continuous_states(&mut states)?;
        kernel.verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            MeStage::Initialization,
        )?;
        Ok(MeInitialState {
            time: discrete.time,
            states,
            observations: kernel.initial_observations().to_vec(),
            termination: discrete.terminate_simulation,
        })
    }

    pub(crate) fn state_count(&self) -> usize {
        self.kernel
            .borrow()
            .model_description()
            .continuous_state_count
    }

    pub(crate) fn prepare_bdf_initial_seed(
        &self,
        frozen_solver_y: &[f64],
        stage: MeStage,
    ) -> Result<(), SimError> {
        self.synchronize_frozen_callback_seed(frozen_solver_y)
            .map_err(|error| error.at_stage(stage))?;
        Ok(())
    }

    pub(crate) fn synchronize_frozen_callback_seed(
        &self,
        frozen_solver_y: &[f64],
    ) -> Result<(), MeError> {
        self.kernel
            .borrow_mut()
            .prepare_frozen_bdf_initial_seed(frozen_solver_y)
    }

    pub(crate) fn event_indicator_count(&self) -> usize {
        self.kernel
            .borrow()
            .model_description()
            .event_indicator_count
    }

    pub(crate) fn take_callback_error(&self) -> Option<SimError> {
        self.callback_error.borrow_mut().take().map(SimError::from)
    }

    pub(crate) fn sync_continuous_point(&self, time: f64, states: &[f64]) -> Result<(), SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        Ok(())
    }

    pub(crate) fn accept_continuous_step(
        &self,
        time: f64,
        states: &[f64],
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
    ) -> Result<Vec<f64>, SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel
            .prepare_frozen_bdf_initial_seed(frozen_solver_y)
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        // The frozen driver has already projected the accepted state and its
        // opaque algebraic warm seed. Re-projecting either in the component
        // would perform an additional numerical solve at this migration seam.
        let projected = states.to_vec();
        kernel.set_continuous_states(&projected)?;
        kernel.completed_integrator_step(MeStepCompletion::Continuous {
            accepted_derivatives: None,
        })?;
        kernel.verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            MeStage::Integration,
        )?;
        Ok(projected)
    }

    pub(crate) fn verify_frozen_compatibility_state(
        &self,
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
        stage: MeStage,
    ) -> Result<(), SimError> {
        self.kernel.borrow().verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            stage,
        )?;
        Ok(())
    }

    pub(crate) fn arm_time_event(
        &self,
        current_time: f64,
        current_states: &[f64],
        event_time: f64,
        horizon: f64,
    ) -> Result<(), SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(current_time))?;
        kernel.set_continuous_states(current_states)?;
        let stop = kernel.next_event_stop(horizon)?;
        if !stop.is_event || !time_match_with_tol(stop.time, event_time) {
            return Err(SimError::RuntimeContract {
                reason: format!(
                    "frozen driver selected time event {event_time}, but the ME component returned \
                     time={} is_event={}",
                    stop.time, stop.is_event
                ),
            });
        }
        Ok(())
    }

    pub(crate) fn process_time_event(
        &self,
        event_time: f64,
        states: &[f64],
        horizon: f64,
    ) -> Result<MePostEventState, SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(event_time))?;
        kernel.set_continuous_states(states)?;
        kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::TimeEvent,
            event_time,
            horizon,
        })?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        kernel.enter_continuous_time_mode()?;
        let mut states = vec![0.0; kernel.model_description().continuous_state_count];
        kernel.get_continuous_states(&mut states)?;
        Ok(MePostEventState {
            time: discrete.time,
            states,
            entry: MeEventEntry {
                cause: MeEventCause::TimeEvent,
                event_time,
                horizon,
            },
            termination: discrete.terminate_simulation,
        })
    }

    pub(crate) fn process_state_event(
        &self,
        root_time: f64,
        root_index: usize,
        root_states: &[f64],
        right_time: f64,
        horizon: f64,
    ) -> Result<MePostEventState, SimError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(root_time))?;
        kernel.set_continuous_states(root_states)?;
        let derivatives = kernel.frozen_event_state_derivatives(root_time, root_states)?;
        let dt = right_time - root_time;
        let mut pre_states = root_states.to_vec();
        let mut right_states = root_states.to_vec();
        if dt > 0.0 && !sample_time_match_with_tol(root_time, right_time) {
            for ((pre, right), derivative) in pre_states
                .iter_mut()
                .zip(&mut right_states)
                .zip(derivatives)
            {
                *pre -= dt * derivative;
                *right += dt * derivative;
            }
        }
        kernel.capture_frozen_located_event_pre(&pre_states)?;
        kernel.set_continuous_states(&pre_states)?;
        let mut before = Vec::new();
        kernel.get_event_indicators(&mut before)?;

        kernel.set_time(MeTime::at(right_time))?;
        kernel.set_continuous_states(&right_states)?;
        let mut after = Vec::new();
        kernel.get_event_indicators(&mut after)?;
        let mut crossings = Vec::new();
        kernel.event_indicator_crossings(&before, &after, &mut crossings)?;
        if !crossings
            .iter()
            .any(|crossing| crossing.index == root_index)
        {
            let post_indicator_value = after
                .get(root_index)
                .copied()
                .map_or(1.0, |value| if value >= 0.0 { 1.0 } else { 0.0 });
            crossings.push(MeIndicatorCrossing {
                index: root_index,
                post_indicator_value,
            });
        }
        kernel.arm_state_event(&crossings)?;
        kernel.completed_integrator_step(MeStepCompletion::AtStateEvent)?;
        if kernel.has_scheduled_event_at(root_time) {
            kernel.set_time(MeTime::at(root_time))?;
            kernel.set_continuous_states(root_states)?;
        }
        kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: root_time,
            horizon,
        })?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        kernel.enter_continuous_time_mode()?;
        let mut states = vec![0.0; kernel.model_description().continuous_state_count];
        kernel.get_continuous_states(&mut states)?;
        Ok(MePostEventState {
            time: discrete.time,
            states,
            entry: MeEventEntry {
                cause: MeEventCause::StateEvent,
                event_time: root_time,
                horizon,
            },
            termination: discrete.terminate_simulation,
        })
    }

    /// Diffsol RHS closure: `setTime`, `setContinuousStates`, derivatives.
    pub(crate) fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) {
        let result = (|| {
            let mut kernel = self.kernel.borrow_mut();
            kernel.set_time(MeTime::at(time))?;
            kernel.set_continuous_states(states)?;
            let mut values = Vec::new();
            kernel.get_continuous_state_derivatives(&mut values)?;
            copy_callback_values("state derivative", &values, out)
        })();
        self.finish_callback(result, out);
    }

    /// Diffsol JVP closure: the FMI directional derivative operation.
    pub(crate) fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) {
        let result = (|| {
            let mut kernel = self.kernel.borrow_mut();
            kernel.set_time(MeTime::at(time))?;
            kernel.set_continuous_states(states)?;
            kernel.get_directional_derivative(seed, out)
        })();
        self.finish_callback(result, out);
    }

    /// Diffsol root closure using the explicit frozen root profile.
    pub(crate) fn event_indicators_into(&self, time: f64, states: &[f64], out: &mut [f64]) {
        let result = (|| {
            let mut kernel = self.kernel.borrow_mut();
            kernel.set_time(MeTime::at(time))?;
            kernel.set_continuous_states(states)?;
            let mut values = Vec::new();
            kernel.get_event_indicators(&mut values)?;
            if values.is_empty() {
                out.fill(1.0);
                return Ok(());
            }
            copy_callback_values("event indicator", &values, out)
        })();
        self.finish_callback(result, out);
    }

    fn finish_callback(&self, result: Result<(), MeError>, out: &mut [f64]) {
        if let Err(error) = result {
            out.fill(f64::NAN);
            let mut pending = self.callback_error.borrow_mut();
            if pending.is_none() {
                *pending = Some(error);
            }
        }
    }
}

fn update_discrete_states_to_completion(
    kernel: &mut impl ModelExchangeKernel,
) -> Result<rumoca_solver::fmi_me::MeDiscreteStates, MeError> {
    let mut discrete = kernel.update_discrete_states()?;
    while discrete.discrete_states_need_update {
        discrete = kernel.update_discrete_states()?;
    }
    Ok(discrete)
}

fn copy_callback_values(label: &str, values: &[f64], out: &mut [f64]) -> Result<(), MeError> {
    if values.len() != out.len() {
        return Err(MeError::Contract {
            reason: format!(
                "{label} callback produced {} values for {} Diffsol slots",
                values.len(),
                out.len()
            ),
        });
    }
    out.copy_from_slice(values);
    Ok(())
}
