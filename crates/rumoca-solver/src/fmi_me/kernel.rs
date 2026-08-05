use std::{cell::RefCell, rc::Rc};

use super::lifecycle::{MeLifecycle, MeLifecycleCommand, MeLifecycleViolation, MeState};
use super::{
    MeCompletedIntegratorStep, MeDiscreteStates, MeError, MeEventCause, MeEventEntry, MeEventStop,
    MeFmuState, MeIndicatorCrossing, MeInstanceConfig, MeModelDescription, MeModelSource,
    MeNumericsProfile, MeObservation, MeOutputSeries, MeRootProfile, MeStage, MeTime, MeValueRef,
    ModelExchangeKernel, advance_states_to_event_probe,
};
use crate::runtime::event::{
    RuntimeEventBoundary, RuntimeEventBoundaryHandler, process_runtime_event_boundary,
    runtime_event_horizon, runtime_root_event_application_time,
};
use crate::runtime::pre_params::{
    clear_scheduled_root_relation_memory, commit_pre_params_after_event_at,
};
use crate::runtime::schedule::{RuntimeEventStop, ScheduledEventConsumption, SolveStopSchedule};
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RootCrossing, convert_variable_meta,
    filter_scheduled_root_crossings, root_crossings_with_relation_memory, runtime_values_changed,
};
use crate::runtime::solve_runtime::{
    AlgebraicLinearization, AlgebraicSettle, EventUpdateRowFilter, InitialEventObservation,
    ProjectedEventUpdateInput, ProjectedInitialEventInput, SolveRuntime, SolveRuntimeSnapshot,
};
use crate::runtime::time::time_match_with_tol;
use crate::solver::{SimTermination, SimVariableMeta};
use crate::timeline;

/// Residual tolerance for the component's internal algebraic refresh.
const ALGEBRAIC_REFRESH_TOL: f64 = 1.0e-10;
/// Iteration ceiling for the component's internal algebraic/event fixed points.
const UPDATE_MAX_ITERS: usize = 32;

#[derive(Clone)]
struct CachedDerivative {
    time: f64,
    state: Vec<f64>,
    derivative: Vec<f64>,
}

#[derive(Clone)]
struct CachedRootConditions {
    time: f64,
    state: Vec<f64>,
    values: Vec<f64>,
}

#[derive(Clone, Copy)]
struct MeAlgebraicProjectionPolicy {
    state_count: usize,
    tolerance: f64,
    profile: MeNumericsProfile,
    settle: AlgebraicSettle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StateTimeCoincidence {
    None,
    Unconsumed,
    Consumed,
}

impl StateTimeCoincidence {
    fn is_some(self) -> bool {
        !matches!(self, Self::None)
    }

    fn is_consumed(self) -> bool {
        matches!(self, Self::Consumed)
    }
}

/// The one projection of a checked `SolveModel` into an FMI 3 ME component.
///
/// Every field below used to live in a backend. They are private here because
/// SPEC_0038 forbids integrators from reaching Solve rows, layouts, opcodes,
/// events, or runtime objects: the only way in is [`ModelExchangeKernel`].
pub struct SolveMeKernel {
    runtime: Rc<SolveRuntime>,
    instance_brand: Rc<()>,
    instance_name: &'static str,
    lifecycle: MeLifecycle,

    /// FMI `tolerance`.
    tolerance: f64,
    /// FMI `stopTime`.
    stop_time: f64,
    /// Temporary phase-2 compatibility profile selected by the owning host.
    root_profile: MeRootProfile,
    /// Temporary phase-2 callback-numerics profile selected by the host.
    numerics_profile: MeNumericsProfile,

    /// The time `fmi3SetTime` last set.
    time: f64,
    /// The event instant the integrator is stepping toward, if any.
    event_boundary: Option<f64>,
    /// The evaluation time that represents the right limit of the last event.
    post_event_eval_time: Option<f64>,
    /// The component time the right limit above belongs to.
    event_anchor_time: f64,

    states: Vec<f64>,
    params: Vec<f64>,
    state_count: usize,

    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<MeEventEntry>,
    last_event_entry: Option<MeEventEntry>,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    skip_next_enter_continuous_delay_commit: bool,

    pending_root_crossings: Vec<RootCrossing>,
    frozen_event_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    frozen_event_accepted_seed: Option<Vec<f64>>,

    solver_y_guess: RefCell<Vec<f64>>,
    delay_params_scratch: RefCell<Vec<f64>>,
    delay_solver_y_scratch: RefCell<Vec<f64>>,
    derivative_cache: RefCell<Option<CachedDerivative>>,
    root_cache: RefCell<Option<CachedRootConditions>>,

    initial_observations: Vec<MeObservation>,
    delay_step_limit: Option<f64>,
    last_projection_changed: bool,
    termination: Option<SimTermination>,
    output_meta: Vec<SimVariableMeta>,
    /// The settled full solver vector `exit_initialization_mode` produced, so
    /// the initial `update_discrete_states` continues from the same vector
    /// instead of rebuilding one.
    settled_initialization_y: Option<Vec<f64>>,
}

/// Complete continuation state captured by `fmi3GetFMUState`.
///
/// This stays opaque outside the component implementation so a host cannot
/// synthesize a state that bypasses lifecycle or buffer invariants.
#[derive(Clone)]
pub(crate) struct MeKernelSnapshot {
    lifecycle: MeState,
    stop_time: f64,
    time: f64,
    event_boundary: Option<f64>,
    post_event_eval_time: Option<f64>,
    event_anchor_time: f64,
    states: Vec<f64>,
    params: Vec<f64>,
    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<MeEventEntry>,
    last_event_entry: Option<MeEventEntry>,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    skip_next_enter_continuous_delay_commit: bool,
    pending_root_crossings: Vec<RootCrossing>,
    frozen_event_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    frozen_event_accepted_seed: Option<Vec<f64>>,
    solver_y_guess: Vec<f64>,
    delay_params_scratch: Vec<f64>,
    delay_solver_y_scratch: Vec<f64>,
    derivative_cache: Option<CachedDerivative>,
    root_cache: Option<CachedRootConditions>,
    initial_observations: Vec<MeObservation>,
    delay_step_limit: Option<f64>,
    last_projection_changed: bool,
    termination: Option<SimTermination>,
    settled_initialization_y: Option<Vec<f64>>,
    runtime: SolveRuntimeSnapshot,
}

mod component;
impl RuntimeEventBoundaryHandler for SolveMeKernel {
    type Error = MeError;

    fn on_event_time(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        if self.advance_state_to_event_right_limit {
            self.time = event_time;
        }
        if event.terminal
            && let Some(index) = self
                .runtime
                .model
                .problem
                .solve_layout
                .terminal_event_parameter_index
            && let Some(slot) = self.params.get_mut(index)
        {
            *slot = 1.0;
        }
        let (event_pre_y, event_pre_p) = self.event_pre_for_update(event_time, event)?;
        self.boundary_event_pre_y = Some(event_pre_y.clone());
        self.boundary_event_pre_p = Some(event_pre_p.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        self.seed_scheduled_root_relation_overrides(event_time, event);
        let application_time = event_update_application_time(
            event_time,
            self.time,
            self.state_time_coincidence.is_some(),
        );
        let row_filter = if self.state_time_coincidence.is_consumed() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(application_time, event, row_filter)?;
        if self.advance_state_to_event_right_limit {
            self.refresh_frozen_event_observation(event_time)?;
        }
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        if self.advance_state_to_event_right_limit || self.state_time_coincidence.is_some() {
            let event_time = self.time;
            let settle = self.numerics_settle();
            let derivatives = event_right_limit_state_derivatives(
                &self.runtime,
                &self.solver_y_guess.borrow(),
                event_time,
                &self.states,
                &self.params,
                settle,
            )?;
            advance_states_to_event_probe(&mut self.states, &derivatives, event_time, right_time);
        }
        self.time = right_time;
        let event_pre_y = if let Some(event_pre_y) = self.boundary_event_pre_y.clone() {
            event_pre_y
        } else {
            self.current_solver_y()?
        };
        let event_pre_p = self
            .boundary_event_pre_p
            .clone()
            .unwrap_or_else(|| self.params.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        let row_filter = if self.state_time_coincidence.is_some() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(right_time, event, row_filter)?;
        if self.advance_state_to_event_right_limit {
            self.refresh_frozen_event_observation(right_time)?;
        }
        self.set_post_event_eval_time(Some(right_time));
        Ok(())
    }
}

pub(super) fn event_right_limit_state_derivatives(
    runtime: &SolveRuntime,
    retained_solver_y: &[f64],
    time: f64,
    states: &[f64],
    params: &[f64],
    settle: AlgebraicSettle,
) -> Result<Vec<f64>, crate::runtime::solve_ops::RuntimeSolveError> {
    let mut solver_y_guess = retained_solver_y.to_vec();
    runtime.eval_state_derivatives_with_guess(
        time,
        states,
        params,
        &mut solver_y_guess,
        settle.tol,
        settle.max_iters,
    )
}

impl ModelExchangeKernel for SolveMeKernel {
    fn model_description(&self) -> MeModelDescription<'_> {
        MeModelDescription {
            continuous_state_count: self.state_count,
            event_indicator_count: self.runtime.root_condition_count(),
            // The linked kernel commits accepted-point history and invalidates
            // component caches here. Discrete-delay models need this even when
            // they have no continuous delay channel, so the current component
            // profile honestly requires the call for every model.
            needs_completed_integrator_step: true,
            output_names: &self.runtime.model.visible_names,
            input_names: self.runtime.model.problem.solve_layout.input_scalar_names(),
            output_meta: &self.output_meta,
        }
    }

    fn get_nominals_of_continuous_states(&self, nominals: &mut [f64]) -> Result<(), MeError> {
        if nominals.len() != self.state_count {
            return Err(contract(format!(
                "nominal buffer has {} entries for {} continuous states",
                nominals.len(),
                self.state_count
            )));
        }
        for (index, slot) in nominals.iter_mut().enumerate() {
            *slot = self.runtime.model.solver_variable_scale(index);
        }
        Ok(())
    }

    fn value_reference(&self, name: &str) -> Option<MeValueRef> {
        self.runtime
            .model
            .problem
            .solve_layout
            .input_parameter_index(name)
            .map(|index| MeValueRef {
                index,
                instance_brand: Rc::clone(&self.instance_brand),
            })
    }

    fn enter_initialization_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterInitializationMode)
            .and_then(|()| self.enter_initialization_mode_inner())
            .and_then(|()| {
                self.commit_lifecycle_transition(MeLifecycleCommand::EnterInitializationMode)
            })
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn exit_initialization_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::ExitInitializationMode)
            .and_then(|()| self.exit_initialization_mode_inner())
            .and_then(|()| {
                self.commit_lifecycle_transition(MeLifecycleCommand::ExitInitializationMode)
            })
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn enter_event_mode(&mut self, entry: MeEventEntry) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterEventMode)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        validate_event_entry(entry, self.tolerance)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            self.frozen_event_accepted_seed = Some(self.solver_y_guess.borrow().clone());
        }
        // Entering Event Mode is the standard signal that any continuous-
        // mode accepted-point cache is no longer authoritative. This replaces
        // the retired Rumoca-only `AtStateEvent` completed-step variant.
        self.clear_runtime_caches();
        self.last_event_entry = Some(entry);
        self.pending_event_entry = Some(entry);
        self.commit_lifecycle_transition(MeLifecycleCommand::EnterEventMode)
            .map_err(|error| error.at_stage(MeStage::EventIteration))
    }

    fn update_discrete_states(&mut self) -> Result<MeDiscreteStates, MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::UpdateDiscreteStates)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        let result = if self.initial_event_pending {
            self.run_initial_event_boundary()
                .map_err(|error| error.at_stage(MeStage::Initialization))
        } else {
            let entry = self
                .pending_event_entry
                .take()
                .ok_or_else(|| contract("event mode has no pending event entry"))
                .map_err(|error| error.at_stage(MeStage::EventIteration))?;
            self.run_runtime_event_boundary(entry)
                .map_err(|error| error.at_stage(MeStage::EventIteration))
        }?;
        self.commit_lifecycle_transition(MeLifecycleCommand::UpdateDiscreteStates)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        Ok(result)
    }

    fn enter_continuous_time_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterContinuousTimeMode)?;
        if self.initial_event_pending || self.pending_event_entry.is_some() {
            return Err(contract(
                "enter_continuous_time_mode requires the pending event update to complete",
            ));
        }
        if self.skip_next_enter_continuous_delay_commit {
            self.skip_next_enter_continuous_delay_commit = false;
        } else {
            self.commit_delay_point()?;
        }
        self.clear_all_scheduled_root_relation_memory()?;
        self.clear_runtime_caches();
        self.commit_lifecycle_transition(MeLifecycleCommand::EnterContinuousTimeMode)
    }

    fn terminate(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::Terminate)?;
        rumoca_eval_solve::trace_solve_row_eval_snapshot(self.instance_name);
        self.commit_lifecycle_transition(MeLifecycleCommand::Terminate)
    }

    fn set_time(&mut self, time: MeTime) -> Result<(), MeError> {
        self.require_active_lifecycle("set_time")?;
        if !time.time.is_finite() {
            return Err(contract("set_time requires a finite time"));
        }
        if let Some(boundary) = time.event_boundary
            && (!boundary.is_finite() || boundary < time.time)
        {
            return Err(contract(
                "set_time event boundary must be finite and not earlier than time",
            ));
        }
        self.time = time.time;
        self.event_boundary = time.event_boundary;
        Ok(())
    }

    fn set_continuous_states(&mut self, states: &[f64]) -> Result<(), MeError> {
        self.require_active_lifecycle("set_continuous_states")?;
        if states.len() != self.state_count {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.state_count
            )));
        }
        if states.iter().any(|value| !value.is_finite()) {
            return Err(contract("continuous state values must all be finite"));
        }
        self.states.copy_from_slice(states);
        Ok(())
    }

    fn get_continuous_states(&self, states: &mut [f64]) -> Result<(), MeError> {
        self.require_active_lifecycle("get_continuous_states")?;
        if states.len() != self.state_count {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.state_count
            )));
        }
        states.copy_from_slice(&self.states);
        Ok(())
    }

    fn get_continuous_state_derivatives(&self, derivatives: &mut Vec<f64>) -> Result<(), MeError> {
        derivatives.resize(self.state_count, 0.0);
        self.continuous_state_derivatives_into(derivatives)
    }

    fn get_directional_derivative(
        &self,
        seed: &[f64],
        sensitivity: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_directional_derivative")?;
        if seed.len() != self.state_count {
            return Err(contract(format!(
                "directional-derivative seed has {} entries for {} continuous states",
                seed.len(),
                self.state_count
            ))
            .at_stage(MeStage::Integration));
        }
        if sensitivity.len() != self.state_count {
            return Err(contract(format!(
                "directional-derivative sensitivity buffer has {} entries for {} state derivatives",
                sensitivity.len(),
                self.state_count
            ))
            .at_stage(MeStage::Integration));
        }
        // The same evaluation time and the same algebraic settle
        // `get_continuous_state_derivatives` uses, so the returned sensitivity
        // is the derivative of exactly the vector that operation reports rather
        // than of a differently-settled one.
        let time = self.continuous_eval_time();
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.with_callback_solver_y(|guess| {
                self.runtime
                    .eval_state_jacobian_v_ad_with_guess_into(
                        AlgebraicLinearization {
                            t: time,
                            params,
                            settle,
                        },
                        &self.states,
                        seed,
                        guess,
                        sensitivity,
                    )
                    .map_err(MeError::from)
            })
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn get_event_indicators(&self, indicators: &mut Vec<f64>) -> Result<(), MeError> {
        indicators.resize(self.runtime.root_condition_count(), 0.0);
        self.event_indicators_into(indicators)
    }

    fn project_continuous_states(&mut self, states: &mut [f64]) -> Result<bool, MeError> {
        self.require_active_lifecycle("project_continuous_states")?;
        self.project_continuous_states_inner(states)
            .map_err(|error| error.at_stage(MeStage::ManifoldProjection))
    }

    fn completed_integrator_step(
        &mut self,
        _no_set_fmu_state_prior_to_current_point: bool,
    ) -> Result<MeCompletedIntegratorStep, MeError> {
        self.require_active_lifecycle("completed_integrator_step")?;
        self.post_event_eval_time = None;
        if !self.pending_root_crossings.is_empty() {
            self.clear_runtime_caches();
        } else if self.last_projection_changed {
            self.clear_derivative_cache();
        } else if matches!(self.numerics_profile, MeNumericsProfile::Component) {
            // FMI does not let an importer hand an FSAL stage into the FMU.
            // Keep the ordinary accepted-point cache private by evaluating it
            // through the same standard derivative operation the importer
            // could call here. A located event stays cache-free until Event
            // Mode consumes it. The frozen migration profile deliberately has
            // no derivative cache, so evaluating here would immediately
            // discard the result.
            let mut accepted_derivatives = Vec::new();
            self.get_continuous_state_derivatives(&mut accepted_derivatives)?;
        }
        self.commit_delay_point()
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        Ok(MeCompletedIntegratorStep::default())
    }

    fn max_step_size(&self) -> Option<f64> {
        self.delay_step_limit
    }

    fn next_event_stop(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        self.require_active_lifecycle("next_event_stop")?;
        self.next_event_stop_inner(horizon)
            .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn event_indicator_crossings(
        &self,
        before: &[f64],
        after: &[f64],
        crossings: &mut Vec<MeIndicatorCrossing>,
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("event_indicator_crossings")?;
        let expected = self.runtime.root_condition_count();
        if before.len() != expected || after.len() != expected {
            return Err(contract(format!(
                "event-indicator crossing buffers have {}/{} entries for {expected} indicators",
                before.len(),
                after.len(),
            )));
        }
        if before.iter().chain(after).any(|value| !value.is_finite()) {
            return Err(contract(
                "event-indicator crossing buffers must contain finite values",
            ));
        }
        let mut located = root_crossings_with_relation_memory(
            before,
            after,
            self.tolerance,
            &self
                .runtime
                .model
                .problem
                .events
                .root_relation_memory_targets,
            &self.params,
        );
        filter_scheduled_root_crossings(
            &mut located,
            &self.runtime.model.problem.events.scheduled_root_conditions,
        );
        crossings.clear();
        crossings.extend(located.into_iter().map(|crossing| MeIndicatorCrossing {
            index: crossing.index,
            post_indicator_value: crossing.post_relation_memory_value,
        }));
        Ok(())
    }

    fn capture_pre_event_state(&mut self) -> Result<(), MeError> {
        self.require_active_lifecycle("capture_pre_event_state")?;
        let pre_y = match self.numerics_profile {
            MeNumericsProfile::Component => self.runtime.full_solver_y(
                self.time,
                &self.states,
                &self.params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )?,
            MeNumericsProfile::DiffsolFrozen => self.solver_y_at_time(self.time)?,
        };
        self.pending_event_pre_y = Some(pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    fn arm_state_event(&mut self, crossings: &[MeIndicatorCrossing]) -> Result<(), MeError> {
        self.require_active_lifecycle("arm_state_event")?;
        let indicator_count = self.runtime.root_condition_count();
        if let Some(crossing) = crossings.iter().find(|crossing| {
            crossing.index >= indicator_count
                || !crossing.post_indicator_value.is_finite()
                || !matches!(crossing.post_indicator_value, 0.0 | 1.0)
        }) {
            return Err(contract(format!(
                "state-event crossing index {} value {} is invalid for {indicator_count} indicators",
                crossing.index, crossing.post_indicator_value,
            )));
        }
        self.pending_root_crossings.clear();
        self.pending_root_crossings
            .extend(crossings.iter().map(|crossing| RootCrossing {
                index: crossing.index,
                post_relation_memory_value: crossing.post_indicator_value,
            }));
        Ok(())
    }

    fn observe(&self) -> Result<MeObservation, MeError> {
        self.require_active_lifecycle("observe")?;
        Ok(MeObservation {
            time: self.time,
            solver_y: self.current_solver_y()?,
            parameters: self.params.clone(),
            instance_brand: Rc::clone(&self.instance_brand),
        })
    }

    fn record_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        series: &mut MeOutputSeries,
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("record_outputs")?;
        self.require_observation_brand(observation)?;
        if series.columns_mut().len() != self.runtime.model.visible_names.len() {
            return Err(contract(format!(
                "output series has {} columns for {} visible outputs",
                series.columns_mut().len(),
                self.runtime.model.visible_names.len(),
            )));
        }
        self.runtime
            .record_visible_sample(
                series.columns_mut(),
                &observation.solver_y,
                &observation.parameters,
                sample_time,
            )
            .map_err(MeError::from)
    }

    fn get_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        values: &mut Vec<f64>,
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_outputs")?;
        self.require_observation_brand(observation)?;
        *values = self.runtime.visible_values(
            &observation.solver_y,
            &observation.parameters,
            sample_time,
        )?;
        Ok(())
    }

    fn initial_observations(&self) -> &[MeObservation] {
        &self.initial_observations
    }

    fn set_float64(&mut self, refs: &[MeValueRef], values: &[f64]) -> Result<(), MeError> {
        self.require_active_lifecycle("set_float64")?;
        if refs.len() != values.len() {
            return Err(contract(format!(
                "{} value references do not match {} values",
                refs.len(),
                values.len()
            )));
        }
        if let Some((reference, value)) =
            refs.iter()
                .zip(values.iter().copied())
                .find(|(reference, value)| {
                    !Rc::ptr_eq(&reference.instance_brand, &self.instance_brand)
                        || reference.index >= self.params.len()
                        || !value.is_finite()
                })
        {
            return Err(contract(format!(
                "Float64 value reference {} with value {value} is invalid for {} parameters",
                reference.index,
                self.params.len(),
            )));
        }
        let checkpoint = self.fmu_state();
        for (reference, value) in refs.iter().zip(values.iter().copied()) {
            self.params[reference.index] = value;
        }
        self.clear_runtime_caches();
        if let Err(error) = self.commit_delay_point() {
            self.reset_to_fmu_state(&checkpoint)
                .expect("a same-instance internal checkpoint is always restorable");
            return Err(error);
        }
        Ok(())
    }

    fn fmu_state(&self) -> MeFmuState {
        MeFmuState {
            component: MeKernelSnapshot {
                lifecycle: self.lifecycle.state(),
                stop_time: self.stop_time,
                time: self.time,
                event_boundary: self.event_boundary,
                post_event_eval_time: self.post_event_eval_time,
                event_anchor_time: self.event_anchor_time,
                states: self.states.clone(),
                params: self.params.clone(),
                stop_schedule: self.stop_schedule.clone(),
                pending_event_entry: self.pending_event_entry,
                last_event_entry: self.last_event_entry,
                pending_event_stop: self.pending_event_stop,
                advance_state_to_event_right_limit: self.advance_state_to_event_right_limit,
                state_time_coincidence: self.state_time_coincidence,
                initial_event_pending: self.initial_event_pending,
                skip_next_enter_continuous_delay_commit: self
                    .skip_next_enter_continuous_delay_commit,
                pending_root_crossings: self.pending_root_crossings.clone(),
                frozen_event_root_crossings: self.frozen_event_root_crossings.clone(),
                pending_event_pre_y: self.pending_event_pre_y.clone(),
                pending_event_pre_p: self.pending_event_pre_p.clone(),
                boundary_event_pre_y: self.boundary_event_pre_y.clone(),
                boundary_event_pre_p: self.boundary_event_pre_p.clone(),
                frozen_event_accepted_seed: self.frozen_event_accepted_seed.clone(),
                solver_y_guess: self.solver_y_guess.borrow().clone(),
                delay_params_scratch: self.delay_params_scratch.borrow().clone(),
                delay_solver_y_scratch: self.delay_solver_y_scratch.borrow().clone(),
                derivative_cache: self.derivative_cache.borrow().clone(),
                root_cache: self.root_cache.borrow().clone(),
                initial_observations: self.initial_observations.clone(),
                delay_step_limit: self.delay_step_limit,
                last_projection_changed: self.last_projection_changed,
                termination: self.termination.clone(),
                settled_initialization_y: self.settled_initialization_y.clone(),
                runtime: self.runtime.snapshot(),
            },
            instance_brand: Rc::clone(&self.instance_brand),
        }
    }

    fn reset_to_fmu_state(&mut self, saved: &MeFmuState) -> Result<(), MeError> {
        if !Rc::ptr_eq(&saved.instance_brand, &self.instance_brand) {
            return Err(contract(
                "component snapshot belongs to a different ME instance",
            ));
        }
        let state = &saved.component;
        self.stop_time = state.stop_time;
        self.time = state.time;
        self.event_boundary = state.event_boundary;
        self.post_event_eval_time = state.post_event_eval_time;
        self.event_anchor_time = state.event_anchor_time;
        self.states.clone_from(&state.states);
        self.params.clone_from(&state.params);
        self.stop_schedule.clone_from(&state.stop_schedule);
        self.pending_event_entry = state.pending_event_entry;
        self.last_event_entry = state.last_event_entry;
        self.pending_event_stop = state.pending_event_stop;
        self.advance_state_to_event_right_limit = state.advance_state_to_event_right_limit;
        self.state_time_coincidence = state.state_time_coincidence;
        self.initial_event_pending = state.initial_event_pending;
        self.skip_next_enter_continuous_delay_commit =
            state.skip_next_enter_continuous_delay_commit;
        self.pending_root_crossings
            .clone_from(&state.pending_root_crossings);
        self.frozen_event_root_crossings
            .clone_from(&state.frozen_event_root_crossings);
        self.pending_event_pre_y
            .clone_from(&state.pending_event_pre_y);
        self.pending_event_pre_p
            .clone_from(&state.pending_event_pre_p);
        self.boundary_event_pre_y
            .clone_from(&state.boundary_event_pre_y);
        self.boundary_event_pre_p
            .clone_from(&state.boundary_event_pre_p);
        self.frozen_event_accepted_seed
            .clone_from(&state.frozen_event_accepted_seed);
        self.solver_y_guess
            .borrow_mut()
            .clone_from(&state.solver_y_guess);
        self.delay_params_scratch
            .borrow_mut()
            .clone_from(&state.delay_params_scratch);
        self.delay_solver_y_scratch
            .borrow_mut()
            .clone_from(&state.delay_solver_y_scratch);
        self.derivative_cache
            .borrow_mut()
            .clone_from(&state.derivative_cache);
        self.root_cache.borrow_mut().clone_from(&state.root_cache);
        self.initial_observations
            .clone_from(&state.initial_observations);
        self.delay_step_limit = state.delay_step_limit;
        self.last_projection_changed = state.last_projection_changed;
        self.termination.clone_from(&state.termination);
        self.settled_initialization_y
            .clone_from(&state.settled_initialization_y);
        self.runtime.restore(&state.runtime);
        self.lifecycle.restore(state.lifecycle);
        Ok(())
    }

    fn restart_from_fmu_state(
        &mut self,
        saved: &MeFmuState,
        start_time: f64,
    ) -> Result<(), MeError> {
        if !start_time.is_finite() {
            return Err(contract("component restart requires a finite start time"));
        }
        self.reset_to_fmu_state(saved)?;
        self.time = start_time;
        self.event_boundary = None;
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, start_time, self.stop_time);
        self.termination = None;
        self.pending_root_crossings.clear();
        self.pending_event_pre_y = None;
        self.pending_event_pre_p = None;
        self.boundary_event_pre_y = None;
        self.boundary_event_pre_p = None;
        self.frozen_event_accepted_seed = None;
        self.post_event_eval_time = None;
        self.event_anchor_time = start_time;
        self.pending_event_entry = None;
        self.last_event_entry = None;
        self.pending_event_stop = None;
        self.advance_state_to_event_right_limit = false;
        self.state_time_coincidence = StateTimeCoincidence::None;
        self.skip_next_enter_continuous_delay_commit = false;
        self.initial_observations.clear();
        self.clear_runtime_caches();
        self.runtime.reset_delay_history();
        self.commit_delay_point()
    }

    fn extend_stop_time(&mut self, from_time: f64, stop_time: f64) -> Result<(), MeError> {
        self.require_active_lifecycle("extend_stop_time")?;
        if !from_time.is_finite() || !stop_time.is_finite() || stop_time < from_time {
            return Err(contract(
                "extended stop time requires finite values with stop_time >= from_time",
            ));
        }
        self.stop_time = stop_time;
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, from_time, stop_time);
        Ok(())
    }
}

fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

pub(super) fn continuous_state_values_changed(before: &[f64], after: &[f64]) -> bool {
    !float_slice_bit_eq(before, after)
}

#[cfg(any(test, kani))]
fn option_float_bit_eq(left: Option<f64>, right: Option<f64>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => left.to_bits() == right.to_bits(),
        (None, None) => true,
        _ => false,
    }
}

/// Select the time owned by the first event-update pass.
///
/// A coincident scheduled clock owns its exact semantic tick. An ordinary
/// located state event is applied where the host positioned the component —
/// normally the numerical right limit, or a target/horizon it snapped to.
pub(super) fn event_update_application_time(
    semantic_event_time: f64,
    component_time: f64,
    coincident_state_time_event: bool,
) -> f64 {
    if coincident_state_time_event {
        semantic_event_time
    } else {
        component_time
    }
}

#[cfg(any(test, kani))]
fn option_float_vec_bit_eq(left: &Option<Vec<f64>>, right: &Option<Vec<f64>>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => float_slice_bit_eq(left, right),
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn option_event_entry_bit_eq(left: Option<MeEventEntry>, right: Option<MeEventEntry>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.cause == right.cause
                && left.event_time.to_bits() == right.event_time.to_bits()
                && left.horizon.to_bits() == right.horizon.to_bits()
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn option_event_stop_bit_eq(
    left: Option<(f64, RuntimeEventStop)>,
    right: Option<(f64, RuntimeEventStop)>,
) -> bool {
    match (left, right) {
        (Some((left_time, left)), Some((right_time, right))) => {
            left_time.to_bits() == right_time.to_bits() && left == right
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn root_crossings_bit_eq(left: &[RootCrossing], right: &[RootCrossing]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.index == right.index
                && left.post_relation_memory_value.to_bits()
                    == right.post_relation_memory_value.to_bits()
        })
}

#[cfg(any(test, kani))]
fn derivative_cache_bit_eq(
    left: Option<&CachedDerivative>,
    right: Option<&CachedDerivative>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.state, &right.state)
                && float_slice_bit_eq(&left.derivative, &right.derivative)
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn root_cache_bit_eq(
    left: Option<&CachedRootConditions>,
    right: Option<&CachedRootConditions>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.state, &right.state)
                && float_slice_bit_eq(&left.values, &right.values)
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn observations_bit_eq(left: &[MeObservation], right: &[MeObservation]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.solver_y, &right.solver_y)
                && float_slice_bit_eq(&left.parameters, &right.parameters)
                && Rc::ptr_eq(&left.instance_brand, &right.instance_brand)
        })
}

#[cfg(any(test, kani))]
fn termination_bit_eq(left: Option<&SimTermination>, right: Option<&SimTermination>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits() && left.message == right.message
        }
        (None, None) => true,
        _ => false,
    }
}

fn observation_from_initial_event(
    observation: &InitialEventObservation,
    instance_brand: &Rc<()>,
) -> MeObservation {
    MeObservation {
        time: observation.t,
        solver_y: observation.y.clone(),
        parameters: observation.p.clone(),
        instance_brand: Rc::clone(instance_brand),
    }
}

fn contract(reason: impl Into<String>) -> MeError {
    MeError::Contract {
        reason: reason.into(),
    }
}

fn lifecycle_contract(violation: MeLifecycleViolation) -> MeError {
    contract(format!(
        "{} is invalid in ME lifecycle state {}",
        violation.command.name(),
        violation.state.name(),
    ))
}

fn validate_instance_config(config: &MeInstanceConfig) -> Result<(), MeError> {
    if config.instance_name.is_empty() {
        return Err(contract("ME instance name must not be empty"));
    }
    if !config.tolerance.is_finite() || config.tolerance <= 0.0 {
        return Err(contract("ME tolerance must be finite and positive"));
    }
    if !config.start_time.is_finite()
        || !config.stop_time.is_finite()
        || config.stop_time < config.start_time
    {
        return Err(contract(
            "ME time horizon requires finite values with stop_time >= start_time",
        ));
    }
    Ok(())
}

fn validate_event_entry(entry: MeEventEntry, tolerance: f64) -> Result<(), MeError> {
    let order_tolerance =
        tolerance.max(1.0e-12 * (1.0 + entry.event_time.abs().max(entry.horizon.abs())));
    if !entry.event_time.is_finite()
        || !entry.horizon.is_finite()
        || (entry.horizon < entry.event_time && entry.event_time - entry.horizon > order_tolerance)
    {
        return Err(contract(format!(
            "event entry requires finite values with horizon >= event_time; cause={:?} event_time={} horizon={}",
            entry.cause, entry.event_time, entry.horizon
        )));
    }
    Ok(())
}

fn state_values_match(a: &[f64], b: &[f64]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits())
}

fn first_bit_mismatch_except(
    a: &[f64],
    b: &[f64],
    excluded: &[usize],
) -> Option<(usize, Option<f64>, Option<f64>)> {
    let len = a.len().max(b.len());
    (0..len).find_map(|index| {
        if excluded.contains(&index) {
            return None;
        }
        let lhs = a.get(index).copied();
        let rhs = b.get(index).copied();
        (lhs.map(f64::to_bits) != rhs.map(f64::to_bits)).then_some((index, lhs, rhs))
    })
}

fn project_algebraics(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    policy: MeAlgebraicProjectionPolicy,
) -> Result<bool, crate::runtime::solve_ops::RuntimeSolveError> {
    let MeAlgebraicProjectionPolicy {
        state_count,
        tolerance: tol,
        profile,
        settle,
    } = policy;
    match profile {
        MeNumericsProfile::Component => {
            let before = y.to_vec();
            runtime.project_state_manifold(y, p, t, tol)?;
            let state = y[..state_count.min(y.len())].to_vec();
            let refreshed =
                runtime.full_solver_y(t, &state, p, ALGEBRAIC_REFRESH_TOL, UPDATE_MAX_ITERS)?;
            y.copy_from_slice(&refreshed);
            Ok(runtime_values_changed(&before, y, tol))
        }
        MeNumericsProfile::DiffsolFrozen => {
            runtime.refresh_delay_values(t, y, p)?;
            let manifold_changed = runtime.project_state_manifold(y, p, t, tol)?;
            // Mirror the frozen driver's callback exactly: state-manifold
            // projection reports every bit-level state change, while the
            // scaled comparison applies only to the subsequent runtime-lane
            // refresh.  Dropping `manifold_changed` can stop an event fixed
            // point one pass early for large-magnitude states.
            let before_runtime_refresh = y.to_vec();
            runtime.refresh_algebraic_and_output_slots_certified(
                t,
                y,
                p,
                settle.tol,
                settle.max_iters,
            )?;
            Ok(frozen_projection_changed(
                manifold_changed,
                &before_runtime_refresh,
                y,
                tol,
            ))
        }
    }
}

pub(super) fn frozen_projection_changed(
    manifold_changed: bool,
    before_runtime_refresh: &[f64],
    after_runtime_refresh: &[f64],
    tolerance: f64,
) -> bool {
    manifold_changed
        || runtime_values_changed(before_runtime_refresh, after_runtime_refresh, tolerance)
}

/// SPEC_0038 "Unsupported lifecycle capability fails before execution".
fn validate_explicit_solve_model(model: &rumoca_ir_solve::SolveModel) -> Result<(), MeError> {
    super::validation::validate_explicit_solve_model(model)
}
