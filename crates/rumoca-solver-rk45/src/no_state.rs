use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventActionOutcome, EventPreMode, EventUpdateRowFilter, NoStateEventStep,
    NoStateOrchestrationBackend, NoStateRootSearchScratch, NoStateScheduledStop,
    ProjectedEventUpdateInput, ProjectedInitialEventInput, RuntimeEventBoundary,
    RuntimeEventBoundaryHandler, RuntimeEventStop, RuntimeSolveError, SimOptions, SimTermination,
    SolveRuntime, SolveStopSchedule, apply_discrete_slot_values, commit_pre_params_after_event_at,
    first_no_state_root_crossing, no_state_root_scan_step_ceiling, process_runtime_event_boundary,
    run_no_state_output_schedule, runtime_event_horizon, runtime_root_event_application_time,
    runtime_values_changed,
    timeline::{event_left_probe_time, sample_time_match_with_tol},
};

use crate::{SessionState, SimError};

const NO_STATE_EVENT_UPDATE_MAX_ITERS: usize = 256;
pub(crate) struct NoStateSession {
    model: solve::SolveModel,
    opts: SimOptions,
    runtime: NoStateRuntime,
    input_values: IndexMap<String, f64>,
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

impl NoStateSession {
    pub(crate) fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        if model.state_scalar_count() != 0 {
            return Err(SimError::UnsupportedModel {
                reason: "no-state session requires a model with zero continuous states".to_string(),
            });
        }
        let model = model.clone();
        let runtime = initialize_no_state_runtime(&model, &opts)?;
        Ok(Self {
            model,
            opts,
            runtime,
            input_values: IndexMap::new(),
        })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let Some(param_idx) = self.model.problem.solve_layout.input_parameter_index(name) else {
            return Err(SimError::SolveIr(format!("unknown input '{name}'")));
        };
        self.input_values.insert(name.to_string(), value);
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
        Ok(())
    }

    pub(crate) fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        let target_time = target_time.min(self.opts.t_end);
        if target_time <= self.runtime.current_t || self.runtime.termination.is_some() {
            return Ok(());
        }
        let tol = self.tol();
        run_no_state_output_schedule(
            &mut Rk45NoStateOrchestration {
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

    pub(crate) fn ensure_end_time(&mut self, target_time: f64) {
        if !target_time.is_finite() || target_time <= self.opts.t_end {
            return;
        }
        let t_end = target_time + (target_time - self.runtime.current_t).max(1.0);
        self.opts.t_end = t_end;
        self.runtime.stop_schedule =
            SolveStopSchedule::new(&self.model.problem, self.runtime.current_t, t_end);
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.runtime.current_t + dt)
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        let mut opts = self.opts.clone();
        opts.t_start = t_start;
        self.runtime = initialize_no_state_runtime(&self.model, &opts)?;
        self.opts = opts;
        Ok(())
    }

    pub(crate) fn time(&self) -> f64 {
        self.runtime.current_t
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let values = self.session_visible_values()?;
        Ok(values.get(name).copied())
    }

    pub(crate) fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    pub(crate) fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    pub(crate) fn input_names(&self) -> &[String] {
        self.model.problem.solve_layout.input_scalar_names()
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        &self.model.visible_names
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.runtime.runtime.visible_values(
            &self.runtime.current_y,
            &self.runtime.params,
            self.runtime.current_t,
        )?;
        let mut values = collect_visible_values(&self.model.visible_names, visible_values)?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }

    fn tol(&self) -> f64 {
        self.opts.atol.max(1.0e-10)
    }
}

struct Rk45NoStateOrchestration<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a mut NoStateRuntime,
}

impl NoStateOrchestrationBackend for Rk45NoStateOrchestration<'_> {
    type Error = SimError;

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
) -> Result<NoStateRuntime, SimError> {
    let runtime = SolveRuntime::new(model)?;
    let mut params = model.parameters.clone();
    let mut current_y = model.initial_y.clone();
    let mut current_t = opts.t_start;
    let tol = opts.atol.max(1.0e-10);
    runtime.initialize_delay_history(current_t, &current_y, &mut params)?;
    runtime.set_initial_event_flag(&mut params, true);
    // Preserve declared/start values for `pre()` throughout initial event
    // iteration. Initialization projection is allowed to change current
    // values, but not the frozen initial-event snapshot.
    let event_pre_y = current_y.clone();
    let event_pre_p = params.clone();
    runtime.settle_initialization_system(
        &mut current_y,
        &mut params,
        current_t,
        tol,
        NO_STATE_EVENT_UPDATE_MAX_ITERS,
    )?;
    settle_algebraics_and_relation_memory(&runtime, &mut current_y, &mut params, current_t, tol)?;
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
) -> Result<(), SimError> {
    let event_t = step.event_time();
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
                horizon_t: if step.root_event {
                    event_t.min(step.target)
                } else {
                    runtime_event_horizon(event, step.target, opts.t_end)
                },
                tolerance: step.tol,
                event,
            },
            &mut handler,
        )?
    };
    runtime.current_t = if step.root_event {
        runtime_root_event_application_time(outcome.final_t, step.target, step.tol)
    } else {
        outcome.final_t
    };
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
    type Error = SimError;

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
    fn apply_event_updates(&mut self, t: f64) -> Result<(), SimError> {
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
) -> Result<(), SimError> {
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
) -> Result<(), SimError> {
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
) -> Result<(), SimError> {
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
) -> Result<(), SimError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { time, message } => Err(SimError::AssertionFailed {
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
) -> Result<Option<f64>, SimError> {
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
) -> Result<(), SimError> {
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
) -> Result<IndexMap<String, f64>, SimError> {
    if names.len() != values.len() {
        return Err(SimError::RuntimeContract {
            reason: format!(
                "runtime returned {} visible values for {} visible names",
                values.len(),
                names.len()
            ),
        });
    }
    Ok(names.iter().cloned().zip(values).collect())
}
