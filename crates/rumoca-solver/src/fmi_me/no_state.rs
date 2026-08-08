//! The zero-continuous-state execution path of the ME component.
//!
//! A model with no continuous states has no integrator: FMI 3.0 ME reduces to
//! Event Mode plus time advance. SPEC_0038 therefore keeps the whole path
//! inside the component rather than duplicating it in every host — a host
//! drives it through [`MeNoStateSession`] and never names Solve rows,
//! layouts, opcodes, or events.
//!
//! Relocated verbatim from the rk-like backend in SPEC_0038 phase 1; the
//! behaviour is unchanged.

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

use super::{MeError, MeModelSource};
use crate::runtime::event::{
    RuntimeEventBoundary, RuntimeEventBoundaryHandler, process_runtime_event_boundary,
    runtime_event_horizon,
};
use crate::runtime::no_state::{
    NO_STATE_EVENT_TIME_TOLERANCE, NoStateEventStep, NoStateOrchestrationBackend,
    NoStateRootSearchScratch, NoStateScheduledStop, first_no_state_root_crossing,
    no_state_root_scan_step_ceiling, run_no_state_output_schedule,
};
use crate::runtime::pre_params::commit_pre_params_after_event_at;
use crate::runtime::schedule::{RuntimeEventStop, SolveStopSchedule};
use crate::runtime::solve_ops::build_sim_result_from_solve_model;
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RootCrossing, RuntimeSolveError,
    root_crossings_with_relation_memory, runtime_values_changed,
};
use crate::runtime::solve_runtime::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, ProjectedInitialEventInput, SolveRuntime,
};
use crate::solver::{SimOptions, SimResult, SimTermination};
use crate::timeline::{event_left_limit_time, event_left_probe_time, sample_time_match_with_tol};

const NO_STATE_EVENT_UPDATE_MAX_ITERS: usize = 256;

/// An ME component instance for a model with zero continuous states.
pub struct MeNoStateSession {
    model: solve::SolveModel,
    opts: SimOptions,
    runtime: NoStateRuntime,
}

struct NoStateRuntime {
    runtime: SolveRuntime,
    params: Vec<f64>,
    current_y: Vec<f64>,
    current_t: f64,
    last_event_t: Option<f64>,
    last_event_was_root: bool,
    termination: Option<SimTermination>,
    stop_schedule: SolveStopSchedule,
    root_params_scratch: Vec<f64>,
    root_search_scratch: NoStateRootSearchScratch,
    root_before_scratch: Vec<f64>,
    root_after_scratch: Vec<f64>,
    pending_root_crossings: Vec<RootCrossing>,
    recorded_times: Vec<f64>,
    data: Vec<Vec<f64>>,
}

impl MeNoStateSession {
    /// `fmi3InstantiateModelExchange` for a zero-state model.
    pub fn instantiate(source: MeModelSource<'_>, opts: SimOptions) -> Result<Self, MeError> {
        Self::instantiate_with_initial_event(source, opts, false)
    }

    fn instantiate_with_initial_event(
        source: MeModelSource<'_>,
        opts: SimOptions,
        apply_without_initial_event: bool,
    ) -> Result<Self, MeError> {
        let model = source.model();
        if model.state_scalar_count() != 0 {
            return Err(MeError::UnsupportedModel {
                reason: "no-state session requires a model with zero continuous states".to_string(),
            });
        }
        let model = model.clone();
        let runtime = initialize_no_state_runtime(&model, &opts, apply_without_initial_event)?;
        Ok(Self {
            model,
            opts,
            runtime,
        })
    }

    /// Run the zero-state ME component over the configured output schedule.
    ///
    /// No numerical solver participates because there are no continuous
    /// states. The shared ME executor owns initialization, events, clocks, and
    /// observations and returns the same backend-neutral trace a solver host
    /// would consume.
    pub fn simulate(source: MeModelSource<'_>, opts: SimOptions) -> Result<SimResult, MeError> {
        let mut session = Self::instantiate_with_initial_event(source, opts, true)?;
        let dt = session
            .opts
            .dt
            .unwrap_or((session.opts.t_end - session.opts.t_start).abs() / 500.0);
        let times =
            crate::timeline::try_build_output_times(session.opts.t_start, session.opts.t_end, dt)
                .map_err(|error| MeError::Contract {
                reason: error.to_string(),
            })?;
        let tol = session.tol();
        run_no_state_output_schedule(
            &mut NoStateOrchestration {
                model: &session.model,
                opts: &session.opts,
                runtime: &mut session.runtime,
            },
            times,
            tol,
        )?;
        Ok(build_sim_result_from_solve_model(
            &session.model,
            session.runtime.recorded_times,
            session.runtime.data,
            session.runtime.termination,
        ))
    }

    /// Validate and settle the zero-state ME component without advancing it.
    pub fn check_initialization(
        source: MeModelSource<'_>,
        opts: SimOptions,
    ) -> Result<(), MeError> {
        Self::instantiate_with_initial_event(source, opts, true).map(|_| ())
    }

    /// Batched `fmi3SetFloat64` by input name.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<bool, MeError> {
        let Some(param_idx) = self.model.problem.solve_layout.input_parameter_index(name) else {
            return Ok(false);
        };
        if let Some(slot) = self.runtime.params.get_mut(param_idx) {
            *slot = value;
        }
        let tol = self.tol();
        refresh_observation_rows_and_relation_memory(
            &self.runtime.runtime,
            &mut self.runtime.current_y,
            &mut self.runtime.params,
            self.runtime.current_t,
            tol,
        )?;
        self.runtime.runtime.commit_delay_history(
            self.runtime.current_t,
            &self.runtime.current_y,
            &self.runtime.params,
        )?;
        Ok(true)
    }

    /// Advance the component to `target_time`, processing every event on the
    /// way (FMI 3.0 ME with zero continuous states: Event Mode only).
    pub fn advance_to(&mut self, target_time: f64) -> Result<(), MeError> {
        let target_time = target_time.min(self.opts.t_end);
        if target_time <= self.runtime.current_t || self.runtime.termination.is_some() {
            return Ok(());
        }
        let tol = self.tol();
        run_no_state_output_schedule(
            &mut NoStateOrchestration {
                model: &self.model,
                opts: &self.opts,
                runtime: &mut self.runtime,
            },
            [target_time],
            tol,
        )?;
        let can_tick_at_target = self.runtime.current_t < target_time
            || sample_time_match_with_tol(self.runtime.current_t, target_time);
        let event_at_target = self
            .runtime
            .last_event_t
            .is_some_and(|event_t| sample_time_match_with_tol(event_t, target_time));
        if can_tick_at_target && !event_at_target && self.runtime.termination.is_none() {
            apply_no_state_deadline_tick(&self.model, &mut self.runtime, target_time, tol)?;
        }
        Ok(())
    }

    /// See [`super::ModelExchangeKernel::extend_stop_time`].
    pub fn extend_stop_time(&mut self, target_time: f64) {
        if !target_time.is_finite() || target_time <= self.opts.t_end {
            return;
        }
        let t_end = target_time + (target_time - self.runtime.current_t).max(1.0);
        self.opts.t_end = t_end;
        self.runtime.stop_schedule =
            SolveStopSchedule::new(&self.model.problem, self.runtime.current_t, t_end);
    }

    /// `fmi3Reset`.
    pub fn reset(&mut self, t_start: f64) -> Result<(), MeError> {
        let mut opts = self.opts.clone();
        opts.t_start = t_start;
        self.runtime = initialize_no_state_runtime(&self.model, &opts, false)?;
        self.opts = opts;
        Ok(())
    }

    #[must_use]
    pub fn time(&self) -> f64 {
        self.runtime.current_t
    }

    #[must_use]
    pub fn output_names(&self) -> &[String] {
        &self.model.visible_names
    }

    #[must_use]
    pub fn input_names(&self) -> &[String] {
        self.model.problem.solve_layout.input_scalar_names()
    }

    /// Batched `fmi3GetFloat64` over every output, keyed by name.
    pub fn output_values(&self) -> Result<IndexMap<String, f64>, MeError> {
        let visible_values = self.runtime.runtime.visible_values(
            &self.runtime.current_y,
            &self.runtime.params,
            self.runtime.current_t,
        )?;
        collect_visible_values(&self.model.visible_names, visible_values)
    }

    fn tol(&self) -> f64 {
        self.opts.atol.max(1.0e-10)
    }
}

struct NoStateOrchestration<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a mut NoStateRuntime,
}

impl NoStateOrchestrationBackend for NoStateOrchestration<'_> {
    type Error = MeError;

    fn current_time(&self) -> f64 {
        self.runtime.current_t
    }

    fn set_current_time(&mut self, time: f64) {
        self.runtime.current_t = time;
    }

    fn max_accepted_step_size(&self) -> Option<f64> {
        no_state_root_scan_step_ceiling(self.opts.dt, self.runtime.runtime.delay_step_limit())
    }

    fn next_scheduled_stop(&mut self, target: f64) -> Result<NoStateScheduledStop, Self::Error> {
        let (stop_time, event_stop) = self.runtime.runtime.next_runtime_event_stop(
            &self.runtime.current_y,
            &self.runtime.params,
            &mut self.runtime.stop_schedule,
            self.runtime.current_t,
            target,
        )?;
        Ok(NoStateScheduledStop {
            stop_time,
            event_stop,
        })
    }

    fn next_root_event_time(&mut self, target: f64, tol: f64) -> Result<Option<f64>, Self::Error> {
        let located = next_no_state_root_event(
            NoStateRootEvaluation {
                runtime: &self.runtime.runtime,
                y: &self.runtime.current_y,
                p: &self.runtime.params,
                params_scratch: &mut self.runtime.root_params_scratch,
                root_scratch: &mut self.runtime.root_search_scratch,
                before_scratch: &mut self.runtime.root_before_scratch,
                after_scratch: &mut self.runtime.root_after_scratch,
            },
            self.runtime.current_t,
            target,
            tol,
        )?;
        self.runtime.pending_root_crossings.clear();
        self.runtime.pending_root_crossings.extend(
            located
                .as_ref()
                .into_iter()
                .flat_map(|root| root.crossings.iter().copied()),
        );
        Ok(located.map(|root| root.time))
    }

    fn handle_event_step(&mut self, step: NoStateEventStep) -> Result<(), Self::Error> {
        apply_no_state_event_step(self.model, self.opts, self.runtime, step)
    }

    fn settle_accepted_step(&mut self) -> Result<(), Self::Error> {
        refresh_observation_rows_and_relation_memory(
            &self.runtime.runtime,
            &mut self.runtime.current_y,
            &mut self.runtime.params,
            self.runtime.current_t,
            self.opts.atol.max(1.0e-10),
        )?;
        self.runtime.runtime.commit_delay_history(
            self.runtime.current_t,
            &self.runtime.current_y,
            &self.runtime.params,
        )?;
        Ok(())
    }

    fn record_output(&mut self) -> Result<(), Self::Error> {
        // Event Mode already records the settled post-event value at its
        // semantic timestamp. Re-evaluating an output at the same instant can
        // select the pre-side of a strict relation and overwrite that accepted
        // observation, even though the relation-memory override correctly
        // selected the post-root branch during event iteration.
        if self.runtime.last_event_was_root
            && self
                .runtime
                .last_event_t
                .is_some_and(|event_t| sample_time_match_with_tol(event_t, self.runtime.current_t))
        {
            if let Some(recorded_t) = self.runtime.recorded_times.last_mut()
                && sample_time_match_with_tol(*recorded_t, self.runtime.current_t)
            {
                *recorded_t = self.runtime.current_t;
            }
            return Ok(());
        }
        record_no_state_observation(self.runtime, self.runtime.current_t, false)
    }
}

fn initialize_no_state_runtime(
    model: &solve::SolveModel,
    opts: &SimOptions,
    apply_without_initial_event: bool,
) -> Result<NoStateRuntime, MeError> {
    let runtime = SolveRuntime::new(model)?;
    let mut params = model.parameters.clone();
    let mut current_y = model.initial_y.clone();
    let mut current_t = opts.t_start;
    let tol = opts.atol.max(1.0e-10);
    runtime.initialize_delay_history(current_t, &current_y, &mut params)?;
    runtime.set_initial_event_flag(&mut params, true);
    runtime.settle_initialization_system(
        &mut current_y,
        &mut params,
        current_t,
        tol,
        NO_STATE_EVENT_UPDATE_MAX_ITERS,
    )?;
    settle_algebraics_and_relation_memory(&runtime, &mut current_y, &mut params, current_t, tol)?;
    // MLS 3.6 §8.6: "Before the start of the integration, it must be guaranteed
    // that for all variables `v`, `v = pre(v)`. If this is not the case for
    // some variables `vi`, `pre(vi) := vi` must be set and an event iteration
    // at the initial time must follow". The `pre()` the initial event iteration
    // reads is therefore the settled initialization value, not the declared
    // `start` the settle replaced.
    let event_pre_y = current_y.clone();
    let event_pre_p = params.clone();
    let dynamic_event = runtime.current_dynamic_time_event_stop(&current_y, &params, current_t)?;
    let outcome = runtime.apply_projected_initial_event_boundary(
        ProjectedInitialEventInput {
            y: &mut current_y,
            p: &mut params,
            t_start: current_t,
            t_end: opts.t_end,
            tol,
            event_pre_y: &event_pre_y,
            event_pre_p: &event_pre_p,
            max_iters: NO_STATE_EVENT_UPDATE_MAX_ITERS,
            dynamic_event,
            apply_without_initial_event,
        },
        |y, p, t| refresh_algebraics_and_detect_changes(&runtime, y, p, t, tol),
    )?;
    current_t = outcome.final_t;
    let mut termination = None;
    apply_event_action_outcome(&mut termination, outcome.action, current_t)?;
    if apply_without_initial_event || !outcome.observations.is_empty() {
        refresh_observation_rows_and_relation_memory(
            &runtime,
            &mut current_y,
            &mut params,
            current_t,
            tol,
        )?;
    }
    runtime.commit_delay_history(current_t, &current_y, &params)?;
    let root_count = runtime.root_condition_count();
    let mut recorded_times = Vec::new();
    let mut data = vec![Vec::new(); model.visible_names.len()];
    for observation in &outcome.observations {
        let mut y = observation.y.clone();
        let mut p = observation.p.clone();
        refresh_observation_rows_and_relation_memory(&runtime, &mut y, &mut p, observation.t, tol)?;
        runtime.record_visible_sample_if_new(
            &mut recorded_times,
            &mut data,
            &y,
            &p,
            observation.t,
        )?;
    }
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);
    // Initialization owns every event at the start instant, including a
    // phase-zero periodic tick. The continuation schedule begins strictly
    // after that completed superdense instant so the first advance cannot
    // execute the same clock owner again.
    stop_schedule.advance_past(current_t);
    Ok(NoStateRuntime {
        runtime,
        params,
        current_y,
        current_t,
        last_event_t: None,
        last_event_was_root: false,
        termination,
        stop_schedule,
        root_params_scratch: vec![0.0; model.parameters.len()],
        root_search_scratch: NoStateRootSearchScratch::new(root_count),
        root_before_scratch: vec![0.0; root_count],
        root_after_scratch: vec![0.0; root_count],
        pending_root_crossings: Vec::new(),
        recorded_times,
        data,
    })
}

fn apply_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
) -> Result<(), MeError> {
    let event_t = step.event_time();
    let root_boundary = step.root_boundary();
    let root_relation_overrides = if step.root_event {
        runtime
            .pending_root_crossings
            .drain(..)
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect::<Vec<_>>()
    } else {
        runtime.pending_root_crossings.clear();
        Vec::new()
    };
    runtime.last_event_t = Some(event_t);
    runtime.last_event_was_root = step.root_event;
    let event = if step.root_event {
        RuntimeEventStop::static_event(EventPreMode::EventEntry)
    } else {
        step.event_stop.unwrap_or(RuntimeEventStop {
            pre_mode: step.pre_mode(),
            observe_right_limit: false,
            terminal: false,
        })
    };
    let outcome = {
        let mut handler = NoStateEventBoundary {
            runtime: &runtime.runtime,
            y: &mut runtime.current_y,
            p: &mut runtime.params,
            tol: step.tol,
            event_pre_y: Vec::new(),
            event_pre_p: Vec::new(),
            termination: &mut runtime.termination,
            root_event: step.root_event,
            root_relation_overrides: &root_relation_overrides,
            terminal_p_index: model.problem.solve_layout.terminal_event_parameter_index,
            recorded_times: &mut runtime.recorded_times,
            data: &mut runtime.data,
        };
        if let Some(boundary) = root_boundary {
            handler.on_event_time(boundary.evaluation_time(), event)?;
            crate::runtime::event::RuntimeEventBoundaryOutcome {
                final_t: boundary.continuation_time(),
                right_limit_t: None,
            }
        } else {
            process_runtime_event_boundary(
                RuntimeEventBoundary {
                    event_t,
                    horizon_t: runtime_event_horizon(event, step.target, opts.t_end),
                    tolerance: NO_STATE_EVENT_TIME_TOLERANCE,
                    event,
                },
                &mut handler,
            )?
        }
    };
    if let Some(boundary) = root_boundary {
        record_no_state_observation(runtime, boundary.observation_time(), false)?;
    }
    runtime.current_t =
        root_boundary.map_or(outcome.final_t, |boundary| boundary.continuation_time());
    commit_pre_params_after_event_at(
        model,
        &runtime.current_y,
        &mut runtime.params,
        Some(event_t),
        step.tol,
    );
    runtime
        .runtime
        .commit_delay_history(event_t, &runtime.current_y, &runtime.params)?;
    runtime.stop_schedule.advance_past(runtime.current_t);
    Ok(())
}

struct NoStateEventBoundary<'a> {
    runtime: &'a SolveRuntime,
    y: &'a mut [f64],
    p: &'a mut [f64],
    tol: f64,
    event_pre_y: Vec<f64>,
    event_pre_p: Vec<f64>,
    termination: &'a mut Option<SimTermination>,
    root_event: bool,
    root_relation_overrides: &'a [(usize, f64)],
    terminal_p_index: Option<usize>,
    recorded_times: &'a mut Vec<f64>,
    data: &'a mut [Vec<f64>],
}

impl RuntimeEventBoundaryHandler for NoStateEventBoundary<'_> {
    type Error = MeError;

    fn on_event_time(&mut self, event_t: f64, event: RuntimeEventStop) -> Result<(), Self::Error> {
        if event.terminal
            && let Some(index) = self.terminal_p_index
            && let Some(slot) = self.p.get_mut(index)
        {
            *slot = 1.0;
        }
        let prepared_left_limit = !self.root_event
            && matches!(
                event.pre_mode,
                EventPreMode::EventEntry | EventPreMode::Fixed
            );
        if prepared_left_limit {
            let probe_t = event_left_probe_time(event_t, self.tol);
            refresh_observation_rows_and_relation_memory(
                self.runtime,
                self.y,
                self.p,
                probe_t,
                self.tol,
            )?;
            let left_t = event_left_limit_time(event_t);
            // The widened probe establishes the inactive pre-clock discrete
            // side. Re-project at the adjacent representable time so a
            // continuous expression such as `sample(time)` retains the exact
            // event-left value rather than a finite 2*atol displacement.
            settle_algebraics_and_relation_memory(self.runtime, self.y, self.p, left_t, self.tol)?;
        } else {
            refresh_observation_rows_and_relation_memory(
                self.runtime,
                self.y,
                self.p,
                event_t,
                self.tol,
            )?;
        }
        // Commit the accepted left limit before applying event updates. The
        // post-event commit in apply_no_state_event_step records the matching
        // right limit at the same source time.
        if prepared_left_limit {
            self.runtime.commit_delay_history_evaluated_at(
                event_t,
                event_left_limit_time(event_t),
                self.y,
                self.p,
            )?;
        } else {
            self.runtime.commit_delay_history(event_t, self.y, self.p)?;
        }
        self.event_pre_y = self.y.to_vec();
        self.event_pre_p = self.p.to_vec();
        self.apply_event_updates(event_t)?;
        refresh_observation_rows_and_relation_memory(
            self.runtime,
            self.y,
            self.p,
            event_t,
            self.tol,
        )?;
        if self.root_event {
            // Observation refresh may reevaluate a numerically rounded strict
            // relation at the boundary. The typed crossing side accepted by
            // root localization remains authoritative for that superdense
            // instant.
            self.runtime.apply_root_relation_memory_overrides(
                self.root_relation_overrides,
                self.y,
                self.p,
                self.tol,
            )?;
        }
        if !self.root_event {
            self.runtime.record_visible_sample_if_new(
                self.recorded_times,
                self.data,
                self.y,
                self.p,
                event_t,
            )?;
        }
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_t: f64,
        _event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        self.apply_event_updates(right_t)?;
        refresh_observation_rows_and_relation_memory(
            self.runtime,
            self.y,
            self.p,
            right_t,
            self.tol,
        )?;
        self.recorded_times.push(right_t);
        self.runtime
            .record_visible_sample(self.data, self.y, self.p, right_t)?;
        Ok(())
    }
}

fn record_no_state_observation(
    runtime: &mut NoStateRuntime,
    observation_t: f64,
    force_distinct: bool,
) -> Result<(), MeError> {
    if force_distinct {
        runtime.recorded_times.push(observation_t);
        runtime.runtime.record_visible_sample(
            &mut runtime.data,
            &runtime.current_y,
            &runtime.params,
            observation_t,
        )?;
    } else {
        runtime.runtime.record_visible_sample_if_new(
            &mut runtime.recorded_times,
            &mut runtime.data,
            &runtime.current_y,
            &runtime.params,
            observation_t,
        )?;
    }
    Ok(())
}

impl NoStateEventBoundary<'_> {
    fn apply_event_updates(&mut self, t: f64) -> Result<(), MeError> {
        let outcome = self.runtime.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: self.y,
                p: self.p,
                t,
                tol: self.tol,
                event_pre_y: &self.event_pre_y,
                event_pre_p: &self.event_pre_p,
                max_iters: NO_STATE_EVENT_UPDATE_MAX_ITERS,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: self.root_relation_overrides,
            },
            |y, p| refresh_algebraics_and_detect_changes(self.runtime, y, p, t, self.tol),
        )?;
        apply_event_action_outcome(self.termination, outcome, t)
    }
}

fn apply_no_state_deadline_tick(
    model: &solve::SolveModel,
    runtime: &mut NoStateRuntime,
    target: f64,
    tol: f64,
) -> Result<(), MeError> {
    runtime.current_t = target;
    runtime.runtime.apply_unfiltered_discrete_rows_once(
        &mut runtime.current_y,
        &mut runtime.params,
        target,
        tol,
    )?;
    runtime.runtime.apply_runtime_assignments_once(
        &mut runtime.current_y,
        &mut runtime.params,
        target,
    )?;
    settle_algebraics_and_relation_memory(
        &runtime.runtime,
        &mut runtime.current_y,
        &mut runtime.params,
        target,
        tol,
    )?;
    commit_pre_params_after_event_at(
        model,
        &runtime.current_y,
        &mut runtime.params,
        Some(target),
        tol,
    );
    runtime
        .runtime
        .commit_delay_history(target, &runtime.current_y, &runtime.params)?;
    Ok(())
}

fn refresh_observation_rows_and_relation_memory(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), MeError> {
    settle_algebraics_and_relation_memory(runtime, y, p, t, tol)?;
    if runtime.refresh_observation_discrete_rows(y, p, t, tol, NO_STATE_EVENT_UPDATE_MAX_ITERS)? {
        settle_algebraics_and_relation_memory(runtime, y, p, t, tol)?;
    }
    Ok(())
}

fn settle_algebraics_and_relation_memory(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), MeError> {
    runtime.settle_projected_runtime_and_relation_memory(
        y,
        p,
        t,
        tol,
        NO_STATE_EVENT_UPDATE_MAX_ITERS,
        |y, p| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol),
    )?;
    Ok(())
}

fn refresh_algebraics_and_detect_changes(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let before = y.to_vec();
    runtime.refresh_delay_values(t, y, p)?;
    runtime.refresh_algebraic_and_output_slots(t, y, p, tol, NO_STATE_EVENT_UPDATE_MAX_ITERS)?;
    Ok(runtime_values_changed(&before, y, tol))
}

fn apply_event_action_outcome(
    termination: &mut Option<SimTermination>,
    outcome: EventActionOutcome,
    event_t: f64,
) -> Result<(), MeError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { time, message } => Err(MeError::Assertion {
            time: if time.is_finite() { time } else { event_t },
            message,
        }),
        EventActionOutcome::Terminated { time, message } => {
            *termination = Some(SimTermination {
                time: if time.is_finite() { time } else { event_t },
                message,
            });
            Ok(())
        }
    }
}

struct NoStateRootEvaluation<'a> {
    runtime: &'a SolveRuntime,
    y: &'a [f64],
    p: &'a [f64],
    params_scratch: &'a mut Vec<f64>,
    root_scratch: &'a mut NoStateRootSearchScratch,
    before_scratch: &'a mut Vec<f64>,
    after_scratch: &'a mut Vec<f64>,
}

struct LocatedNoStateRoot {
    time: f64,
    crossings: Vec<RootCrossing>,
}

fn next_no_state_root_event(
    input: NoStateRootEvaluation<'_>,
    current_t: f64,
    target: f64,
    tol: f64,
) -> Result<Option<LocatedNoStateRoot>, MeError> {
    let NoStateRootEvaluation {
        runtime,
        y,
        p,
        params_scratch,
        root_scratch,
        before_scratch,
        after_scratch,
    } = input;
    let planned_root = runtime.next_planned_time_root(p, current_t, target, tol)?;
    let search_target = planned_root.unwrap_or(target);
    let root_count = runtime.root_condition_count();
    let Some(root_time) = first_no_state_root_crossing(
        root_scratch,
        root_count,
        current_t,
        search_target,
        tol,
        |t, out| eval_refreshed_roots(runtime, y, p, params_scratch, t, tol, out),
    )?
    else {
        return locate_no_state_root_post_side(
            runtime,
            y,
            p,
            params_scratch,
            before_scratch,
            after_scratch,
            current_t,
            planned_root,
            tol,
        );
    };
    let root_time = if root_time > current_t + tol
        && (root_time < target || sample_time_match_with_tol(root_time, target))
    {
        Some(root_time)
    } else {
        planned_root
    };
    locate_no_state_root_post_side(
        runtime,
        y,
        p,
        params_scratch,
        before_scratch,
        after_scratch,
        current_t,
        root_time,
        tol,
    )
}

#[allow(clippy::too_many_arguments)]
fn locate_no_state_root_post_side(
    runtime: &SolveRuntime,
    y: &[f64],
    p: &[f64],
    params_scratch: &mut Vec<f64>,
    before_scratch: &mut Vec<f64>,
    after_scratch: &mut Vec<f64>,
    current_t: f64,
    root_time: Option<f64>,
    tol: f64,
) -> Result<Option<LocatedNoStateRoot>, MeError> {
    let Some(root_time) = root_time else {
        return Ok(None);
    };
    let root_count = runtime.root_condition_count();
    before_scratch.resize(root_count, 0.0);
    after_scratch.resize(root_count, 0.0);
    let before_t = event_left_probe_time(root_time, tol).max(current_t);
    eval_refreshed_roots(runtime, y, p, params_scratch, before_t, tol, before_scratch)?;
    eval_refreshed_roots(
        runtime,
        y,
        p,
        params_scratch,
        root_time.next_up(),
        tol,
        after_scratch,
    )?;
    let crossings = root_crossings_with_relation_memory(
        before_scratch,
        after_scratch,
        tol,
        &runtime.model.problem.events.root_relation_memory_targets,
        p,
    );
    Ok(Some(LocatedNoStateRoot {
        time: root_time,
        crossings,
    }))
}

fn eval_refreshed_roots(
    runtime: &SolveRuntime,
    y: &[f64],
    p: &[f64],
    params: &mut Vec<f64>,
    t: f64,
    tol: f64,
    out: &mut [f64],
) -> Result<(), MeError> {
    params.clear();
    params.extend_from_slice(p);
    runtime.refresh_delay_values(t, y, params)?;
    runtime.eval_root_search_conditions_into(
        t,
        y,
        params,
        tol,
        NO_STATE_EVENT_UPDATE_MAX_ITERS,
        out,
    )?;
    Ok(())
}

fn collect_visible_values(
    names: &[String],
    values: Vec<f64>,
) -> Result<IndexMap<String, f64>, MeError> {
    if names.len() != values.len() {
        return Err(MeError::Contract {
            reason: format!(
                "runtime returned {} visible values for {} visible names",
                values.len(),
                names.len()
            ),
        });
    }
    Ok(names.iter().cloned().zip(values).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_root_crossing_side_overrides_a_strict_relation_at_the_exact_boundary() {
        let model = strict_no_state_relation_model();
        let runtime = SolveRuntime::new(&model).expect("strict relation fixture prepares");
        let mut y = Vec::new();
        let mut p = vec![0.0];
        let mut termination = None;
        let mut recorded_times = Vec::new();
        let mut data = Vec::new();
        let mut boundary = NoStateEventBoundary {
            runtime: &runtime,
            y: &mut y,
            p: &mut p,
            tol: 1.0e-10,
            event_pre_y: Vec::new(),
            event_pre_p: vec![0.0],
            termination: &mut termination,
            root_event: true,
            root_relation_overrides: &[(0, 1.0)],
            terminal_p_index: None,
            recorded_times: &mut recorded_times,
            data: &mut data,
        };

        boundary
            .apply_event_updates(0.5)
            .expect("typed crossing side settles at the exact root");
        drop(boundary);

        assert_eq!(p, vec![1.0]);
    }

    #[test]
    fn no_state_root_search_retains_the_typed_post_side() {
        let model = strict_no_state_relation_model();
        let runtime = SolveRuntime::new(&model).expect("strict relation fixture prepares");
        let mut params_scratch = vec![0.0];
        let mut root_scratch = NoStateRootSearchScratch::new(1);
        let mut before_scratch = vec![0.0];
        let mut after_scratch = vec![0.0];

        let located = next_no_state_root_event(
            NoStateRootEvaluation {
                runtime: &runtime,
                y: &[],
                p: &model.parameters,
                params_scratch: &mut params_scratch,
                root_scratch: &mut root_scratch,
                before_scratch: &mut before_scratch,
                after_scratch: &mut after_scratch,
            },
            0.0,
            1.0,
            1.0e-10,
        )
        .expect("root search succeeds")
        .expect("strict root is located");

        assert!((located.time - 0.5).abs() <= 1.0e-10);
        assert_eq!(
            located.crossings,
            [RootCrossing {
                index: 0,
                post_relation_memory_value: 1.0,
            }]
        );
    }

    fn strict_no_state_relation_model() -> solve::SolveModel {
        let relation = scalar_block(
            vec![
                solve::LinearOp::LoadTime { dst: 0 },
                solve::LinearOp::Const { dst: 1, value: 0.5 },
                solve::LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
            "no_state_strict_relation.mo",
        );
        let root = scalar_block(
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.5 },
                solve::LinearOp::LoadTime { dst: 1 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
            "no_state_strict_root.mo",
        );
        solve::SolveModel {
            problem: solve::SolveProblem {
                discrete: solve::DiscreteSolveSystem {
                    rhs: relation,
                    update_targets: vec![solve::scalar_slot_p(0)],
                    row_roles: vec![solve::DiscreteRowRole::Equation],
                    pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                    observation_refresh: vec![false],
                    clock_owners: vec![None],
                    ..Default::default()
                },
                events: solve::SolveEventPartition {
                    root_conditions: root,
                    root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
                    root_zero_domains: vec![solve::RootZeroDomain::Positive],
                    ..Default::default()
                },
                solve_layout: solve::SolveLayout {
                    compiled_parameter_len: 1,
                    ..Default::default()
                },
                ..Default::default()
            },
            parameters: vec![0.0],
            ..Default::default()
        }
    }

    fn scalar_block(
        program: Vec<solve::LinearOp>,
        name: &'static str,
    ) -> solve::ScalarProgramBlock {
        let span =
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
        solve::ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("no-state ME fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }
}
