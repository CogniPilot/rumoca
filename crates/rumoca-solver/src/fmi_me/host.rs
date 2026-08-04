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
use crate::{
    SimTermination, time_match_with_tol,
    timeline::{event_right_probe_time, sample_time_match_with_tol},
};

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
    /// FMI `valuesOfContinuousStatesChanged` from the settled event iteration.
    pub values_of_continuous_states_changed: bool,
    pub observation: Option<MeRuntimeOutput>,
    /// Left-limit observation for scheduled events. FMI/OMC traces preserve
    /// both superdense values at a time-event boundary.
    pub pre_observation: Option<MeRuntimeOutput>,
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
    event_probe_tolerance: f64,
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
            event_probe_tolerance: config.tolerance,
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
        let mut projected = states.to_vec();
        kernel.project_continuous_states(&mut projected)?;
        kernel.set_continuous_states(&projected)?;
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
        let pre_observation = runtime_output(&mut *kernel)?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        let next_event_time = discrete.next_event_time;
        kernel.enter_continuous_time_mode()?;
        let mut post = post_event_state(&mut *kernel, entry, event_time, discrete, true)?;
        post.pre_observation = Some(pre_observation);
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
        let probe_time = event_right_probe_time(root_time, self.event_probe_tolerance);
        let (pre_states, right_states) =
            bracket_root_states(root_states, &derivatives, root_time, probe_time);
        kernel.capture_frozen_located_event_pre(&pre_states)?;
        kernel.set_continuous_states(&pre_states)?;
        let mut before = Vec::new();
        kernel.get_event_indicators(&mut before)?;

        kernel.set_time(MeTime::at(probe_time))?;
        kernel.set_continuous_states(&right_states)?;
        let mut after = Vec::new();
        kernel.get_event_indicators(&mut after)?;
        let mut crossings = Vec::new();
        kernel.event_indicator_crossings(&before, &after, &mut crossings)?;
        retain_reported_root_crossing(root_index, &after, &mut crossings);
        kernel.arm_state_event(&crossings)?;
        complete_integrator_step(&mut *kernel)?;
        let includes_scheduled_event = kernel.has_scheduled_event_at(root_time);
        // A forward right-limit belongs to the located root. A root that the
        // numerical host snapped backward onto its horizon instead belongs to
        // that host-owned time. In both cases typed relation-memory crossings
        // carry strict post-side truth without changing the exact located
        // state used by reinitialization.
        // Commit relation updates at the semantic right-limit owned by the
        // numerical host.  Keeping the update at the located root would make
        // discontinuous relations visible one integration probe too early.
        let event_mode_time = right_time;
        kernel.set_time(MeTime::at(event_mode_time))?;
        kernel.set_continuous_states(root_states)?;
        let entry = MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: root_time,
            horizon,
        };
        kernel.enter_event_mode(entry)?;
        let pre_update_observation = runtime_output(&mut *kernel)?;
        let discrete = update_discrete_states_to_completion(&mut *kernel)?;
        let next_event_time = discrete.next_event_time;
        kernel.enter_continuous_time_mode()?;
        // Observe at the semantic event instant, after discrete updates but
        // before the numerical driver's right-limit probe advances continuous
        // states.  This keeps discontinuous contact outputs at their
        // zero-crossing value while still exposing event-updated discrete
        // outputs (for example clocked assignments).
        let event_entry_observation =
            if !includes_scheduled_event && !discrete.values_of_continuous_states_changed {
                MeRuntimeOutput {
                    time: pre_update_observation.time,
                    values: pre_update_observation.values.clone(),
                }
            } else {
                runtime_output(&mut *kernel)?
            };
        if !includes_scheduled_event {
            advance_post_event_state(&mut *kernel, root_time, right_time)?;
        }
        // A located state event is an observable superdense instant even when
        // it does not coincide with a scheduled event. The numerical plugin
        // owns trace storage, while the ME host owns the settled right-limit
        // values handed to that storage.
        let mut post = post_event_state(&mut *kernel, entry, right_time, discrete, false)?;
        post.observation = Some(event_entry_observation);
        // Located roots are also FMI superdense event instants. Preserve the
        // left-limit observation so the numerical trace can retain both sides
        // of a discontinuity at the same timestamp (as OMC does).
        post.pre_observation = Some(pre_update_observation);
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
            kernel.continuous_state_derivatives_into(out)
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
            if kernel.model_description().event_indicator_count == 0 {
                out.fill(1.0);
                return Ok(());
            }
            kernel.event_indicators_into(out)
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

fn advance_post_event_state(
    kernel: &mut impl ModelExchangeKernel,
    event_time: f64,
    continuation_time: f64,
) -> Result<(), MeError> {
    if continuation_time <= event_time {
        return Ok(());
    }
    if sample_time_match_with_tol(event_time, continuation_time) {
        kernel.set_time(MeTime::at(continuation_time))?;
        return Ok(());
    }
    let state_count = kernel.model_description().continuous_state_count;
    let mut states = vec![0.0; state_count];
    let mut derivatives = vec![0.0; state_count];
    kernel.get_continuous_states(&mut states)?;
    kernel.get_continuous_state_derivatives(&mut derivatives)?;
    let dt = continuation_time - event_time;
    for (state, derivative) in states.iter_mut().zip(derivatives) {
        *state += dt * derivative;
    }
    kernel.set_time(MeTime::at(continuation_time))?;
    kernel.set_continuous_states(&states)
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
    let values_of_continuous_states_changed = discrete.values_of_continuous_states_changed;
    let mut states = vec![0.0; kernel.model_description().continuous_state_count];
    kernel.get_continuous_states(&mut states)?;
    let observation = record_observation
        .then(|| runtime_output(kernel))
        .transpose()?;
    Ok(MeRuntimePostEventState {
        time: continuation_time,
        states,
        entry,
        values_of_continuous_states_changed,
        observation,
        pre_observation: None,
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
