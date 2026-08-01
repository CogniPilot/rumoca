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
    NoStateEventStep, NoStateOrchestrationBackend, NoStateRootSearchScratch, NoStateScheduledStop,
    first_no_state_root_crossing, no_state_root_scan_step_ceiling, run_no_state_output_schedule,
};
use crate::runtime::pre_params::commit_pre_params_after_event_at;
use crate::runtime::schedule::{RuntimeEventStop, SolveStopSchedule};
use crate::runtime::solve_events::apply_discrete_slot_values;
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RuntimeSolveError, runtime_values_changed,
};
use crate::runtime::solve_runtime::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, ProjectedInitialEventInput, SolveRuntime,
};
use crate::solver::{SimOptions, SimTermination};
use crate::timeline::{event_left_probe_time, sample_time_match_with_tol};

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
    termination: Option<SimTermination>,
    stop_schedule: SolveStopSchedule,
    root_params_scratch: Vec<f64>,
    root_search_scratch: NoStateRootSearchScratch,
}

impl MeNoStateSession {
    /// `fmi3InstantiateModelExchange` for a zero-state model.
    pub fn instantiate(source: MeModelSource<'_>, opts: SimOptions) -> Result<Self, MeError> {
        let model = source.model();
        if model.state_scalar_count() != 0 {
            return Err(MeError::UnsupportedModel {
                reason: "no-state session requires a model with zero continuous states".to_string(),
            });
        }
        let model = model.clone();
        let runtime = initialize_no_state_runtime(&model, &opts)?;
        Ok(Self {
            model,
            opts,
            runtime,
        })
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
        self.runtime = initialize_no_state_runtime(&self.model, &opts)?;
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
        next_no_state_root_event_time(
            NoStateRootEvaluation {
                runtime: &self.runtime.runtime,
                y: &self.runtime.current_y,
                p: &self.runtime.params,
                params_scratch: &mut self.runtime.root_params_scratch,
                root_scratch: &mut self.runtime.root_search_scratch,
            },
            self.runtime.current_t,
            target,
            tol,
        )
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
        Ok(())
    }
}

fn initialize_no_state_runtime(
    model: &solve::SolveModel,
    opts: &SimOptions,
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
            apply_without_initial_event: false,
        },
        |y, p, t| refresh_algebraics_and_detect_changes(&runtime, y, p, t, tol),
    )?;
    current_t = outcome.final_t;
    let mut termination = None;
    apply_event_action_outcome(&mut termination, outcome.action, current_t)?;
    if !outcome.observations.is_empty() {
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
    Ok(NoStateRuntime {
        runtime,
        params,
        current_y,
        current_t,
        last_event_t: None,
        termination,
        stop_schedule: SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end),
        root_params_scratch: vec![0.0; model.parameters.len()],
        root_search_scratch: NoStateRootSearchScratch::new(root_count),
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
    runtime.last_event_t = Some(event_t);
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
            terminal_p_index: model.problem.solve_layout.terminal_event_parameter_index,
        };
        process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t,
                horizon_t: root_boundary.map_or_else(
                    || runtime_event_horizon(event, step.target, opts.t_end),
                    |boundary| boundary.evaluation_time(),
                ),
                tolerance: step.tol,
                event,
            },
            &mut handler,
        )?
    };
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
    terminal_p_index: Option<usize>,
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
            let left_t = event_left_probe_time(event_t, self.tol);
            refresh_observation_rows_and_relation_memory(
                self.runtime,
                self.y,
                self.p,
                left_t,
                self.tol,
            )?;
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
                event_left_probe_time(event_t, self.tol),
                self.y,
                self.p,
            )?;
        } else {
            self.runtime.commit_delay_history(event_t, self.y, self.p)?;
        }
        self.event_pre_y = self.y.to_vec();
        self.event_pre_p = self.p.to_vec();
        if self.root_event {
            return Ok(());
        }
        self.apply_event_updates(event_t)?;
        refresh_observation_rows_and_relation_memory(
            self.runtime,
            self.y,
            self.p,
            event_t,
            self.tol,
        )
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
        )
    }
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
                root_relation_overrides: &[],
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
    let values = runtime.runtime.eval_scalar_program_block(
        &model.problem.discrete.rhs,
        &runtime.current_y,
        &runtime.params,
        target,
    )?;
    apply_discrete_slot_values(
        &model.problem.discrete.update_targets,
        &values,
        &mut runtime.current_y,
        &mut runtime.params,
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
}

fn next_no_state_root_event_time(
    input: NoStateRootEvaluation<'_>,
    current_t: f64,
    target: f64,
    tol: f64,
) -> Result<Option<f64>, MeError> {
    let NoStateRootEvaluation {
        runtime,
        y,
        p,
        params_scratch,
        root_scratch,
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
        return Ok(planned_root);
    };
    if root_time > current_t + tol
        && (root_time < target || sample_time_match_with_tol(root_time, target))
    {
        Ok(Some(root_time))
    } else {
        Ok(planned_root)
    }
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
