//! Solver-neutral linked host for the FMI 3 Model Exchange runtime.
//!
//! Numerical integrator plugins share this host. It owns the ME lifecycle,
//! discrete iteration, event crossing classification, and callback failure
//! state; a plugin only supplies continuous integration and calls the ME
//! evaluation operations exposed here.

use std::{cell::RefCell, rc::Rc};

use super::{
    MeDiscreteStates, MeError, MeEventCause, MeEventEntry, MeEventStop, MeIndicatorCrossing,
    MeInstanceConfig, MeModelSource, MeObservation, MeStage, MeTime, ModelExchangeKernel,
    SolveMeKernel,
};
use crate::{SimTermination, time_match_with_tol, timeline::sample_time_match_with_tol};

type SharedMeKernel = Rc<RefCell<SolveMeKernel>>;
type SharedCallbackError = Rc<RefCell<Option<MeError>>>;

/// Settled state handed from the ME initialization lifecycle to an integrator.
pub struct MeRuntimeInitialState {
    pub time: f64,
    pub states: Vec<f64>,
    pub observations: Vec<MeObservation>,
    pub termination: Option<SimTermination>,
}

/// State returned after the shared ME runtime completes an event boundary.
pub struct MeRuntimePostEventState {
    pub time: f64,
    pub states: Vec<f64>,
    pub entry: MeEventEntry,
    pub observation: Option<MeRuntimeOutput>,
    pub termination: Option<SimTermination>,
}

/// One backend-neutral output observation returned by the ME runtime.
pub struct MeRuntimeOutput {
    pub time: f64,
    pub values: Vec<f64>,
}

/// Linked FMI 3 ME host shared by every numerical integrator plugin.
#[derive(Clone)]
pub struct MeRuntimeHost {
    kernel: SharedMeKernel,
    callback_error: SharedCallbackError,
    start_time: f64,
    next_event_time: Rc<RefCell<Option<f64>>>,
}

impl MeRuntimeHost {
    pub fn instantiate(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        let kernel = SolveMeKernel::instantiate(source, config)?;
        Ok(Self {
            kernel: Rc::new(RefCell::new(kernel)),
            callback_error: Rc::new(RefCell::new(None)),
            start_time: config.start_time,
            next_event_time: Rc::new(RefCell::new(None)),
        })
    }

    pub fn initialize(
        &self,
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
    ) -> Result<MeRuntimeInitialState, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.enter_initialization_mode()?;
        kernel.exit_initialization_mode()?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        kernel.enter_continuous_time_mode()?;
        let state_count = kernel.model_description().continuous_state_count;
        let mut states = vec![0.0; state_count];
        kernel.get_continuous_states(&mut states)?;
        kernel.verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            MeStage::Initialization,
        )?;
        let initial = MeRuntimeInitialState {
            time: self.start_time,
            states,
            observations: kernel.initial_observations().to_vec(),
            termination: discrete.terminate_simulation,
        };
        *self.next_event_time.borrow_mut() = discrete.next_event_time;
        drop(kernel);
        Ok(initial)
    }

    /// Initialize through the ordinary ME lifecycle without a migration
    /// compatibility oracle.
    pub fn initialize_component(&self) -> Result<MeRuntimeInitialState, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.enter_initialization_mode()?;
        kernel.exit_initialization_mode()?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        kernel.enter_continuous_time_mode()?;
        let mut states = vec![0.0; kernel.model_description().continuous_state_count];
        kernel.get_continuous_states(&mut states)?;
        let initial = MeRuntimeInitialState {
            time: self.start_time,
            states,
            observations: kernel.initial_observations().to_vec(),
            termination: discrete.terminate_simulation,
        };
        *self.next_event_time.borrow_mut() = discrete.next_event_time;
        drop(kernel);
        Ok(initial)
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.kernel
            .borrow()
            .model_description()
            .continuous_state_count
    }

    pub fn prepare_integrator_initial_seed(
        &self,
        frozen_solver_y: &[f64],
        stage: MeStage,
    ) -> Result<(), MeError> {
        self.synchronize_frozen_callback_seed(frozen_solver_y)
            .map_err(|error| error.at_stage(stage))
    }

    pub fn synchronize_frozen_callback_seed(&self, frozen_solver_y: &[f64]) -> Result<(), MeError> {
        self.kernel
            .borrow_mut()
            .prepare_frozen_bdf_initial_seed(frozen_solver_y)
    }

    #[must_use]
    pub fn event_indicator_count(&self) -> usize {
        self.kernel
            .borrow()
            .model_description()
            .event_indicator_count
    }

    pub fn continuous_state_nominals(&self) -> Result<Vec<f64>, MeError> {
        let mut nominals = vec![0.0; self.state_count()];
        self.kernel
            .borrow()
            .get_nominals_of_continuous_states(&mut nominals)?;
        Ok(nominals)
    }

    pub fn next_event_stop(&self, horizon: f64) -> Result<MeEventStop, MeError> {
        if !horizon.is_finite() {
            return Err(MeError::Contract {
                reason: "next event-stop horizon must be finite".to_owned(),
            });
        }
        let event_time = *self.next_event_time.borrow();
        Ok(match event_time {
            Some(time) if time < horizon || time_match_with_tol(time, horizon) => MeEventStop {
                time,
                is_event: true,
            },
            _ => MeEventStop {
                time: horizon,
                is_event: false,
            },
        })
    }

    #[must_use]
    pub fn max_step_size(&self) -> Option<f64> {
        self.kernel.borrow().max_step_size()
    }

    pub fn observe(&self) -> Result<MeRuntimeOutput, MeError> {
        let kernel = self.kernel.borrow_mut();
        let observation = kernel.observe()?;
        let mut values = vec![0.0; kernel.model_description().output_names.len()];
        kernel.get_outputs(&observation, observation.time(), &mut values)?;
        Ok(MeRuntimeOutput {
            time: observation.time(),
            values,
        })
    }

    /// Read outputs at an interpolated continuous point without accepting it.
    pub fn observe_continuous_point(
        &self,
        time: f64,
        states: &[f64],
    ) -> Result<MeRuntimeOutput, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        runtime_output(&mut *kernel)
    }

    pub fn outputs_for_observation(
        &self,
        observation: &MeObservation,
    ) -> Result<MeRuntimeOutput, MeError> {
        let kernel = self.kernel.borrow();
        let mut values = vec![0.0; kernel.model_description().output_names.len()];
        kernel.get_outputs(observation, observation.time(), &mut values)?;
        Ok(MeRuntimeOutput {
            time: observation.time(),
            values,
        })
    }

    pub fn output_names(&self) -> Vec<String> {
        self.kernel
            .borrow()
            .model_description()
            .output_names
            .to_vec()
    }

    pub fn output_meta(&self) -> Vec<crate::SimVariableMeta> {
        self.kernel
            .borrow()
            .model_description()
            .output_meta
            .to_vec()
    }

    pub fn derivatives(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        let mut values = Vec::new();
        kernel.get_continuous_state_derivatives(&mut values)?;
        Ok(values)
    }

    pub fn accept_integrator_step(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        let mut projected = states.to_vec();
        kernel.project_continuous_states(&mut projected)?;
        kernel.set_continuous_states(&projected)?;
        complete_integrator_step(&mut *kernel)?;
        drop(kernel);
        Ok(projected)
    }

    pub fn take_callback_error(&self) -> Option<MeError> {
        self.callback_error.borrow_mut().take()
    }

    pub fn sync_continuous_point(&self, time: f64, states: &[f64]) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        drop(kernel);
        Ok(())
    }

    pub fn accept_continuous_step(
        &self,
        time: f64,
        states: &[f64],
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel
            .prepare_frozen_bdf_initial_seed(frozen_solver_y)
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        let projected = states.to_vec();
        kernel.set_continuous_states(&projected)?;
        complete_integrator_step(&mut *kernel)?;
        kernel.verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            MeStage::Integration,
        )?;
        drop(kernel);
        Ok(projected)
    }

    pub fn verify_frozen_compatibility_state(
        &self,
        frozen_solver_y: &[f64],
        frozen_parameters: &[f64],
        stage: MeStage,
    ) -> Result<(), MeError> {
        self.kernel.borrow().verify_frozen_compatibility_state(
            frozen_solver_y,
            frozen_parameters,
            stage,
        )
    }

    pub fn arm_time_event(&self, event_time: f64, horizon: f64) -> Result<(), MeError> {
        let stop = self.next_event_stop(horizon)?;
        if !stop.is_event || !time_match_with_tol(stop.time, event_time) {
            return Err(MeError::Contract {
                reason: format!(
                    "integrator selected time event {event_time}, but the ME runtime returned \
                     time={} is_event={}",
                    stop.time, stop.is_event
                ),
            });
        }
        Ok(())
    }

    pub fn process_time_event(
        &self,
        event_time: f64,
        states: &[f64],
        horizon: f64,
    ) -> Result<MeRuntimePostEventState, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(event_time))?;
        kernel.set_continuous_states(states)?;
        let entry = MeEventEntry {
            cause: MeEventCause::TimeEvent,
            event_time,
            horizon,
        };
        kernel.enter_event_mode(entry)?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        let next_event_time = discrete.next_event_time;
        kernel.enter_continuous_time_mode()?;
        let post = post_event_state(&mut *kernel, entry, event_time, discrete, true)?;
        drop(kernel);
        *self.next_event_time.borrow_mut() = next_event_time;
        Ok(post)
    }

    pub fn process_state_event(
        &self,
        root_time: f64,
        root_index: usize,
        root_states: &[f64],
        right_time: f64,
        horizon: f64,
    ) -> Result<MeRuntimePostEventState, MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(root_time))?;
        kernel.set_continuous_states(root_states)?;
        let derivatives = kernel.frozen_event_state_derivatives(root_time, root_states)?;
        let (pre_states, right_states) =
            bracket_root_states(root_states, &derivatives, root_time, right_time);
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
        retain_reported_root_crossing(root_index, &after, &mut crossings);
        kernel.arm_state_event(&crossings)?;
        complete_integrator_step(&mut *kernel)?;
        let includes_scheduled_event = kernel.has_scheduled_event_at(root_time);
        if includes_scheduled_event {
            kernel.set_time(MeTime::at(root_time))?;
            kernel.set_continuous_states(root_states)?;
        }
        let entry = MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: root_time,
            horizon,
        };
        kernel.enter_event_mode(entry)?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        let next_event_time = discrete.next_event_time;
        kernel.enter_continuous_time_mode()?;
        let post = post_event_state(
            &mut *kernel,
            entry,
            right_time,
            discrete,
            includes_scheduled_event,
        )?;
        drop(kernel);
        *self.next_event_time.borrow_mut() = next_event_time;
        Ok(post)
    }

    /// Integrator RHS callback: set time/states, then read derivatives.
    pub fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) {
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

    /// Integrator JVP callback through the FMI directional derivative.
    pub fn directional_derivative_into(
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

    /// Integrator root callback through FMI event indicators.
    pub fn event_indicators_into(&self, time: f64, states: &[f64], out: &mut [f64]) {
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

fn bracket_root_states(
    root_states: &[f64],
    derivatives: &[f64],
    root_time: f64,
    right_time: f64,
) -> (Vec<f64>, Vec<f64>) {
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
    (pre_states, right_states)
}

fn retain_reported_root_crossing(
    root_index: usize,
    after: &[f64],
    crossings: &mut Vec<MeIndicatorCrossing>,
) {
    if crossings
        .iter()
        .any(|crossing| crossing.index == root_index)
    {
        return;
    }
    let post_indicator_value = after
        .get(root_index)
        .copied()
        .map_or(1.0, crate::relation_memory_value_from_root);
    crossings.push(MeIndicatorCrossing {
        index: root_index,
        post_indicator_value,
    });
}

fn post_event_state(
    kernel: &mut impl ModelExchangeKernel,
    entry: MeEventEntry,
    continuation_time: f64,
    discrete: MeDiscreteStates,
    record_observation: bool,
) -> Result<MeRuntimePostEventState, MeError> {
    let mut states = vec![0.0; kernel.model_description().continuous_state_count];
    kernel.get_continuous_states(&mut states)?;
    let observation = record_observation
        .then(|| runtime_output(kernel))
        .transpose()?;
    Ok(MeRuntimePostEventState {
        time: continuation_time,
        states,
        entry,
        observation,
        termination: discrete.terminate_simulation,
    })
}

fn runtime_output(kernel: &mut impl ModelExchangeKernel) -> Result<MeRuntimeOutput, MeError> {
    let observation = kernel.observe()?;
    let mut values = vec![0.0; kernel.model_description().output_names.len()];
    kernel.get_outputs(&observation, observation.time(), &mut values)?;
    Ok(MeRuntimeOutput {
        time: observation.time(),
        values,
    })
}

fn update_discrete_states_to_completion(
    kernel: &mut impl ModelExchangeKernel,
) -> Result<MeDiscreteStates, MeError> {
    let mut discrete = kernel.update_discrete_states()?;
    while discrete.discrete_states_need_update {
        discrete = kernel.update_discrete_states()?;
    }
    Ok(discrete)
}

fn complete_integrator_step(kernel: &mut impl ModelExchangeKernel) -> Result<(), MeError> {
    if !kernel.model_description().needs_completed_integrator_step {
        return Ok(());
    }
    let completed = kernel.completed_integrator_step(true)?;
    if completed.enter_event_mode || completed.terminate_simulation {
        return Err(MeError::Contract {
            reason: format!(
                "ME host cannot yet consume completed-integrator-step outputs: \
                 enter_event_mode={} terminate_simulation={}",
                completed.enter_event_mode, completed.terminate_simulation
            ),
        });
    }
    Ok(())
}

fn copy_callback_values(label: &str, values: &[f64], out: &mut [f64]) -> Result<(), MeError> {
    if values.len() != out.len() {
        return Err(MeError::Contract {
            reason: format!(
                "{label} callback produced {} values for {} integrator slots",
                values.len(),
                out.len()
            ),
        });
    }
    out.copy_from_slice(values);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reported_root_fallback_uses_canonical_relation_memory_sign() {
        let mut crossings = Vec::new();
        retain_reported_root_crossing(0, &[-2.0], &mut crossings);
        retain_reported_root_crossing(1, &[-2.0, 3.0], &mut crossings);

        assert_eq!(
            crossings,
            [
                MeIndicatorCrossing {
                    index: 0,
                    post_indicator_value: 1.0,
                },
                MeIndicatorCrossing {
                    index: 1,
                    post_indicator_value: 0.0,
                },
            ]
        );
    }
}
