use crate::{
    Dae, LS, MassMatrix, OutputBuffers, REGULARIZATION_LEVELS, SimError, SimOptions,
    SolverStartupProfile, TimeoutBudget, build_output_names, build_parameter_values,
    configure_solver_problem_with_profile, interp_err, problem, sim_trace_enabled,
    trace_timer_start_if,
};
use diffsol::{OdeEquations, OdeSolverMethod, OdeSolverStopReason, VectorHost};
use rumoca_eval_flat::eval::{self, build_env};
use rumoca_sim_core::{
    build_initialization_capture_env, check_budget_or_trace_timeout, event_restart_step_hint,
    event_restart_time, initialize_output_capture, integration_direction,
    is_interpolation_outside_step_error, is_interpolation_outside_step_sim_error, map_solver_panic,
    record_outputs_until, refresh_pre_values_from_state, reset_stop_time_error,
    runtime_capture_target_names, runtime_snapshot, sample_state_at_stop,
    settle_runtime_discrete_capture_env, should_recover_interpolation_window_error,
    should_recover_nonlinear_failure_near_active_stop, should_recover_stop_time_error,
    startup_profile_label, stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol,
};
use std::{collections::HashMap, time::Instant};

fn trace_step_failure_diagnostics(dae: &Dae, y: &[f64], t: f64, param_values: &[f64]) {
    if !sim_trace_enabled() {
        return;
    }
    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    rumoca_sim_core::trace_step_failure_diagnostics(dae, y, t, param_values, |v, out| {
        crate::problem::eval_jacobian_vector_ad(dae, y, param_values, t, v, out, n_x);
    });
}

pub(crate) struct IntegrationOutput {
    t_out_list: Vec<f64>,
    pub(super) out_len: usize,
    pub(super) t_out_idx: usize,
    pub(super) buf: OutputBuffers,
}

impl IntegrationOutput {
    pub(super) fn new(opts: &SimOptions, n_total: usize, y0: &[f64]) -> Self {
        let (t_out_list, out_len, buf, t_out_idx) = initialize_output_capture(opts, n_total, y0);
        Self {
            t_out_list,
            out_len,
            t_out_idx,
            buf,
        }
    }

    pub(super) fn snapshot(&self, steps: usize, root_hits: usize, t: f64) -> BdfProgressSnapshot {
        bdf_snapshot(steps, root_hits, t, self.t_out_idx, self.out_len)
    }

    pub(super) fn record_until<'a, Eqn, S>(
        &mut self,
        solver: &S,
        t_limit: f64,
        budget: &TimeoutBudget,
    ) -> Result<(), SimError>
    where
        Eqn: OdeEquations<T = f64> + 'a,
        Eqn::V: VectorHost<T = f64>,
        S: OdeSolverMethod<'a, Eqn>,
    {
        record_outputs_until(
            &self.t_out_list,
            &mut self.t_out_idx,
            t_limit,
            &mut self.buf,
            |t_interp, out| {
                let y = interpolate_output_state::<Eqn, S>(solver, t_interp, budget)?;
                out.record(t_interp, &y);
                Ok(())
            },
        )
    }
}

struct RuntimeChannelCapture {
    names: Vec<String>,
    solver_name_to_idx: HashMap<String, usize>,
}

#[derive(Clone, Copy)]
enum RuntimeSampleMode {
    Initialization,
    Regular,
}

pub(super) type BdfTraceCtx = rumoca_sim_core::RuntimeTraceContext;
pub(super) type BdfProgressSnapshot = rumoca_sim_core::RuntimeProgressSnapshot;

pub(super) fn bdf_trace_ctx(enabled: bool, eps: f64, profile: SolverStartupProfile) -> BdfTraceCtx {
    BdfTraceCtx::new(enabled, "BDF", eps, startup_profile_label(profile))
}

pub(super) fn bdf_snapshot(
    steps: usize,
    root_hits: usize,
    t: f64,
    output_idx: usize,
    output_len: usize,
) -> BdfProgressSnapshot {
    runtime_snapshot(steps, root_hits, t, output_idx, output_len)
}

pub(super) fn trace_bdf_start(ctx: BdfTraceCtx, h0: f64, max_wall_seconds: Option<f64>) {
    rumoca_sim_core::trace_runtime_start(ctx, h0, max_wall_seconds);
}

pub(super) fn trace_bdf_step_fail(
    ctx: BdfTraceCtx,
    snap: BdfProgressSnapshot,
    err: impl std::fmt::Display,
) {
    rumoca_sim_core::trace_runtime_step_fail(ctx, snap, err);
}

pub(super) fn trace_bdf_progress(
    ctx: BdfTraceCtx,
    snap: BdfProgressSnapshot,
    t_limit: f64,
    last_log: &mut Option<Instant>,
) {
    rumoca_sim_core::trace_runtime_progress(ctx, snap, t_limit, last_log);
}

pub(super) fn trace_bdf_done(ctx: BdfTraceCtx, steps: usize, root_hits: usize, final_t: f64) {
    rumoca_sim_core::trace_runtime_done(ctx, steps, root_hits, final_t);
}

fn maybe_trace_unrecoverable_step(
    maybe_trace_ctx: Option<BdfTraceCtx>,
    output_snapshot: BdfProgressSnapshot,
    active_stop_at_step: f64,
    current_t: f64,
    msg: &str,
) {
    let Some(trace_ctx) = maybe_trace_ctx else {
        return;
    };
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] unrecoverable step: current_t={} active_stop={} msg={}",
            current_t, active_stop_at_step, msg
        );
    }
    trace_bdf_step_fail(trace_ctx, output_snapshot, msg);
}

pub(super) fn solver_t_limit(reason: &OdeSolverStopReason<f64>, current_t: f64) -> f64 {
    match reason {
        OdeSolverStopReason::InternalTimestep | OdeSolverStopReason::TstopReached => current_t,
        OdeSolverStopReason::RootFound(t_root) => *t_root,
    }
}

pub(super) fn set_solver_stop_time<'a, Eqn, S>(
    solver: &mut S,
    stop_time: f64,
    budget: &TimeoutBudget,
    context: &str,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        solver.set_stop_time(stop_time)
    })) {
        Ok(Ok(())) => Ok(()),
        Ok(Err(err)) => Err(SimError::SolverError(format!("{context}: {err}"))),
        Err(panic_info) => Err(map_solver_panic(budget, context, panic_info)),
    }
}

pub(super) fn solver_step_reason<'a, Eqn, S>(
    solver: &mut S,
    budget: &TimeoutBudget,
) -> Result<OdeSolverStopReason<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| solver.step())) {
        Ok(Ok(reason)) => Ok(reason),
        Ok(Err(err)) => Err(SimError::SolverError(err.to_string())),
        Err(panic_info) => Err(map_solver_panic(budget, "solver step", panic_info)),
    }
}

pub(super) struct SolverLoopContext<'a> {
    dae: &'a Dae,
    opts: &'a SimOptions,
    startup_profile: SolverStartupProfile,
    n_x: usize,
    param_values: Vec<f64>,
    discrete_event_ctx: Option<CompiledDiscreteEventContext>,
    budget: &'a TimeoutBudget,
}

#[derive(Debug)]
pub(super) enum StepAdvance {
    Advanced(OdeSolverStopReason<f64>),
    Recovered,
    Finished,
}

pub(super) fn recover_to_active_stop<'a, Eqn, S>(
    solver: &mut S,
    active_stop: f64,
    ctx: &SolverLoopContext<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let stop_t = active_stop;
    let current_t = solver.state().t;
    let y_current = solver_state_to_vec::<Eqn, S>(solver);
    let y_at_stop = sample_state_at_stop(current_t, stop_t, &y_current, |t_sample| {
        solver_interpolate_to_vec::<Eqn, S>(
            solver,
            t_sample,
            ctx.budget,
            "interpolate(active stop recovery)",
        )
    })?;
    let projected = maybe_project_scheduled_event_state(
        ctx.dae,
        &y_at_stop,
        ctx.n_x,
        stop_t,
        ctx.opts.atol,
        ctx.budget,
    )?;
    overwrite_solver_state::<Eqn, S>(
        solver,
        SolverStateOverwriteInput {
            opts: ctx.opts,
            startup_profile: ctx.startup_profile,
            dae: ctx.dae,
            param_values: ctx.param_values.as_slice(),
            n_x: ctx.n_x,
            t: stop_t,
            y: &projected,
        },
    )?;
    refresh_pre_values_from_state(
        ctx.dae,
        solver.state().y.as_slice(),
        ctx.param_values.as_slice(),
        stop_t,
    );
    set_solver_stop_time::<Eqn, S>(
        solver,
        ctx.opts.t_end,
        ctx.budget,
        "Reset stop time after active stop recovery",
    )
    .map_err(reset_stop_time_error)
}

pub(super) fn step_with_stop_recovery<'a, Eqn, S>(
    solver: &mut S,
    active_stop: f64,
    ctx: &SolverLoopContext<'_>,
    mut on_unrecoverable: impl FnMut(&str, f64, &[f64]),
) -> Result<StepAdvance, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let t_before = solver.state().t;
    match solver_step_reason::<Eqn, S>(solver, ctx.budget) {
        Ok(reason) => Ok(StepAdvance::Advanced(reason)),
        Err(SimError::SolverError(msg)) => {
            let current_t = solver.state().t;
            let recoverable_stop_time =
                should_recover_stop_time_error(&msg, current_t, active_stop);
            let recoverable_interp_window =
                should_recover_interpolation_window_error(&msg, current_t, active_stop);
            let recoverable_nonlinear_near_stop =
                should_recover_nonlinear_failure_near_active_stop(&msg, current_t, active_stop);
            if recoverable_nonlinear_near_stop && sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] step recovery: nonlinear-failure near active_stop current_t={} active_stop={} msg={}",
                    current_t, active_stop, msg
                );
            }
            let recoverable_interp_progress = is_interpolation_outside_step_error(&msg)
                && time_advanced_with_tol(t_before, current_t);

            if recoverable_interp_progress {
                if stop_time_reached_with_tol(current_t, ctx.opts.t_end) {
                    return Ok(StepAdvance::Finished);
                }
                if sim_trace_enabled() {
                    eprintln!(
                        "[sim-trace] step recovery: accepted-step interpolation miss t_before={} t_after={} active_stop={} msg={}",
                        t_before, current_t, active_stop, msg
                    );
                }
                return Ok(StepAdvance::Advanced(OdeSolverStopReason::InternalTimestep));
            }

            let recoverable = recoverable_stop_time
                || recoverable_interp_window
                || recoverable_nonlinear_near_stop;
            if !recoverable {
                let y = solver.state().y.as_slice().to_vec();
                on_unrecoverable(&msg, current_t, &y);
                return Err(SimError::SolverError(format!("Step failed: {msg}")));
            }
            if stop_time_reached_with_tol(current_t, ctx.opts.t_end) {
                return Ok(StepAdvance::Finished);
            }
            recover_to_active_stop::<Eqn, S>(solver, active_stop, ctx)?;
            Ok(StepAdvance::Recovered)
        }
        Err(err) => Err(err),
    }
}

pub(super) fn apply_event_updates_at_time<'a, Eqn, S>(
    solver: &mut S,
    t_event: f64,
    ctx: &SolverLoopContext<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let mut y_at_event = solver_interpolate_to_vec::<Eqn, S>(
        solver,
        t_event,
        ctx.budget,
        "interpolate(event update)",
    )?;
    refresh_pre_values_from_state(
        ctx.dae,
        y_at_event.as_slice(),
        ctx.param_values.as_slice(),
        t_event,
    );
    let restart_t = event_restart_time(ctx.opts.t_start, ctx.opts.t_end, t_event);
    let event_env = settle_runtime_event_updates(
        ctx.dae,
        y_at_event.as_mut_slice(),
        ctx.param_values.as_slice(),
        ctx.n_x,
        t_event,
        ctx.discrete_event_ctx.as_ref(),
    );
    eval::seed_pre_values_from_env(&event_env);
    y_at_event = maybe_project_scheduled_event_state(
        ctx.dae,
        y_at_event.as_slice(),
        ctx.n_x,
        restart_t,
        ctx.opts.atol,
        ctx.budget,
    )?;
    overwrite_solver_state::<Eqn, S>(
        solver,
        SolverStateOverwriteInput {
            opts: ctx.opts,
            startup_profile: ctx.startup_profile,
            dae: ctx.dae,
            param_values: ctx.param_values.as_slice(),
            n_x: ctx.n_x,
            t: restart_t,
            y: y_at_event.as_slice(),
        },
    )?;
    let final_env = build_env(
        ctx.dae,
        y_at_event.as_slice(),
        ctx.param_values.as_slice(),
        restart_t,
    );
    eval::seed_pre_values_from_env(&final_env);
    Ok(())
}

struct DiffsolBackend<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver: S,
    output: IntegrationOutput,
    ctx: SolverLoopContext<'a>,
    bdf_trace: Option<BdfTraceCtx>,
    bdf_last_log: Option<Instant>,
    steps: usize,
    root_hits: usize,
    stalled_output_steps: usize,
    last_output_idx: usize,
    runtime_capture: Option<RuntimeChannelCapture>,
    _phantom: std::marker::PhantomData<Eqn>,
}

impl<'a, Eqn, S> DiffsolBackend<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    const MAX_STEPS_WITHOUT_OUTPUT_PROGRESS: usize = 8_000;

    fn new(
        solver: S,
        mut output: IntegrationOutput,
        ctx: SolverLoopContext<'a>,
        bdf_trace: Option<BdfTraceCtx>,
        solver_names: Vec<String>,
    ) -> Self {
        let runtime_names = runtime_capture_target_names(ctx.dae);
        if !runtime_names.is_empty() {
            output
                .buf
                .set_runtime_channels(runtime_names.clone(), output.out_len);
        }
        let runtime_capture = if runtime_names.is_empty() {
            None
        } else {
            let solver_name_to_idx: HashMap<String, usize> = solver_names
                .iter()
                .enumerate()
                .map(|(idx, name)| (name.clone(), idx))
                .collect();
            Some(RuntimeChannelCapture {
                names: runtime_names,
                solver_name_to_idx,
            })
        };
        let last_output_idx = output.t_out_idx;
        let mut backend = Self {
            solver,
            output,
            ctx,
            bdf_trace,
            bdf_last_log: trace_timer_start_if(bdf_trace.is_some()),
            steps: 0,
            root_hits: 0,
            stalled_output_steps: 0,
            last_output_idx,
            runtime_capture,
            _phantom: std::marker::PhantomData,
        };
        let t0 = backend.solver.state().t;
        let y0 = backend.solver.state().y.as_slice().to_vec();
        backend.record_runtime_sample(t0, y0.as_slice(), RuntimeSampleMode::Initialization);
        backend
    }

    fn into_parts(self) -> (S, IntegrationOutput, usize, usize) {
        (self.solver, self.output, self.steps, self.root_hits)
    }

    fn record_output_for_step(
        &mut self,
        reason: &OdeSolverStopReason<f64>,
    ) -> Result<(), SimError> {
        let t_limit = solver_t_limit(reason, self.solver.state().t);
        if let Some(trace_ctx) = self.bdf_trace {
            trace_bdf_progress(
                trace_ctx,
                self.output
                    .snapshot(self.steps, self.root_hits, self.solver.state().t),
                t_limit,
                &mut self.bdf_last_log,
            );
        }
        let output_idx_before = self.output.t_out_idx;
        self.output
            .record_until::<Eqn, S>(&self.solver, t_limit, self.ctx.budget)?;
        let output_idx_after = self.output.t_out_idx;
        let new_samples = output_idx_after.saturating_sub(output_idx_before);
        if new_samples > 0 {
            let start_row = self.output.buf.times.len().saturating_sub(new_samples);
            for row in start_row..self.output.buf.times.len() {
                let t_sample = self.output.buf.times[row];
                let sample_state: Vec<f64> = self
                    .output
                    .buf
                    .data
                    .iter()
                    .map(|series| series.get(row).copied().unwrap_or(0.0))
                    .collect();
                self.record_runtime_sample(
                    t_sample,
                    sample_state.as_slice(),
                    RuntimeSampleMode::Regular,
                );
            }
        }
        if self.output.t_out_idx == self.last_output_idx {
            self.stalled_output_steps += 1;
            if self.stalled_output_steps >= Self::MAX_STEPS_WITHOUT_OUTPUT_PROGRESS {
                return Err(SimError::SolverError(format!(
                    "solver stalled near t={} ({} steps without output progress at sample index {})",
                    self.solver.state().t,
                    self.stalled_output_steps,
                    self.output.t_out_idx
                )));
            }
        } else {
            self.stalled_output_steps = 0;
            self.last_output_idx = self.output.t_out_idx;
        }
        project_internal_step_state_if_needed::<Eqn, S>(&mut self.solver, reason, &self.ctx)
    }

    fn evaluate_runtime_sample_values(
        &self,
        t_sample: f64,
        sample_state: &[f64],
        mode: RuntimeSampleMode,
    ) -> Option<Vec<f64>> {
        let capture = self.runtime_capture.as_ref()?;
        let mut y = sample_state.to_vec();
        let env = match mode {
            RuntimeSampleMode::Initialization => {
                // Keep startup channels consistent with initialized solver state.
                build_initialization_capture_env(
                    self.ctx.dae,
                    y.as_mut_slice(),
                    self.ctx.param_values.as_slice(),
                    t_sample,
                )
            }
            RuntimeSampleMode::Regular => settle_runtime_discrete_capture_env(
                self.ctx.dae,
                y.as_mut_slice(),
                self.ctx.param_values.as_slice(),
                self.ctx.n_x,
                t_sample,
            ),
        };
        let values: Vec<f64> = capture
            .names
            .iter()
            .map(|name| {
                env.vars
                    .get(name.as_str())
                    .copied()
                    .or_else(|| {
                        capture
                            .solver_name_to_idx
                            .get(name)
                            .and_then(|idx| y.get(*idx).copied())
                    })
                    .unwrap_or(0.0)
            })
            .collect();
        Some(values)
    }

    fn record_runtime_sample(
        &mut self,
        t_sample: f64,
        sample_state: &[f64],
        mode: RuntimeSampleMode,
    ) {
        let Some(values) = self.evaluate_runtime_sample_values(t_sample, sample_state, mode) else {
            return;
        };
        self.output.buf.record_runtime_values(values.as_slice());
    }

    fn overwrite_runtime_sample_at_time(
        &mut self,
        t_sample: f64,
        sample_state: &[f64],
        mode: RuntimeSampleMode,
    ) {
        let Some(values) = self.evaluate_runtime_sample_values(t_sample, sample_state, mode) else {
            return;
        };
        self.output
            .buf
            .overwrite_runtime_values_at_time(t_sample, values.as_slice());
    }
}

impl<'a, Eqn, S> rumoca_sim_core::SimulationBackend for DiffsolBackend<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    type Error = SimError;

    fn init(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn step_until(
        &mut self,
        stop_time: f64,
    ) -> Result<rumoca_sim_core::StepUntilOutcome, Self::Error> {
        if stop_time_reached_with_tol(self.solver.state().t, self.ctx.opts.t_end) {
            return Ok(rumoca_sim_core::StepUntilOutcome::Finished);
        }
        if let Some(trace_ctx) = self.bdf_trace {
            check_budget_or_trace_timeout(
                self.ctx.budget,
                trace_ctx,
                self.steps,
                self.root_hits,
                self.solver.state().t,
                self.output.t_out_idx,
                self.output.out_len,
            )?;
        } else {
            self.ctx.budget.check()?;
        }
        set_solver_stop_time::<Eqn, S>(
            &mut self.solver,
            stop_time,
            self.ctx.budget,
            "Reset stop time",
        )
        .map_err(reset_stop_time_error)?;

        let active_stop_at_step = stop_time;
        let reason = loop {
            let maybe_trace_ctx = self.bdf_trace;
            let steps = self.steps;
            let root_hits = self.root_hits;
            let output_snapshot = self
                .output
                .snapshot(steps, root_hits, self.solver.state().t);
            match step_with_stop_recovery::<Eqn, S>(
                &mut self.solver,
                stop_time,
                &self.ctx,
                |msg, current_t, y| {
                    maybe_trace_unrecoverable_step(
                        maybe_trace_ctx,
                        output_snapshot,
                        active_stop_at_step,
                        current_t,
                        msg,
                    );
                    trace_step_failure_diagnostics(
                        self.ctx.dae,
                        y,
                        current_t,
                        self.ctx.param_values.as_slice(),
                    );
                },
            )? {
                StepAdvance::Advanced(reason) => break reason,
                StepAdvance::Recovered => continue,
                StepAdvance::Finished => return Ok(rumoca_sim_core::StepUntilOutcome::Finished),
            }
        };

        self.steps += 1;
        self.record_output_for_step(&reason)?;

        match reason {
            OdeSolverStopReason::InternalTimestep => {
                Ok(rumoca_sim_core::StepUntilOutcome::InternalStep)
            }
            OdeSolverStopReason::RootFound(t_root) => {
                self.root_hits += 1;
                Ok(rumoca_sim_core::StepUntilOutcome::RootFound { t_root })
            }
            OdeSolverStopReason::TstopReached => Ok(rumoca_sim_core::StepUntilOutcome::StopReached),
        }
    }

    fn read_state(&self) -> rumoca_sim_core::BackendState {
        rumoca_sim_core::BackendState {
            t: self.solver.state().t,
        }
    }

    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error> {
        apply_event_updates_at_time::<Eqn, S>(&mut self.solver, event_time, &self.ctx)?;
        // Event updates happen after step output capture; rewrite the event-row
        // runtime channels with post-event settled values to match right-limit semantics.
        let post_event_state = self.solver.state().y.as_slice().to_vec();
        self.overwrite_runtime_sample_at_time(
            event_time,
            post_event_state.as_slice(),
            RuntimeSampleMode::Regular,
        );
        Ok(())
    }
}

fn project_internal_step_state_if_needed<'a, Eqn, S>(
    solver: &mut S,
    reason: &OdeSolverStopReason<f64>,
    ctx: &SolverLoopContext<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let _ = solver;
    let _ = reason;
    let _ = ctx;
    Ok(())
}
pub(super) fn try_integrate(
    dae: &Dae,
    opts: &SimOptions,
    eps: f64,
    n_total: usize,
    mass_matrix: &MassMatrix,
    startup_profile: SolverStartupProfile,
    budget: &TimeoutBudget,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let trace_ctx = bdf_trace_ctx(sim_trace_enabled(), eps, startup_profile);
    budget.check()?;
    let mut problem = problem::build_problem(dae, opts.rtol, opts.atol, eps, mass_matrix)?;
    configure_solver_problem_with_profile(&mut problem, opts, startup_profile);
    let mut solver = problem
        .bdf::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create BDF solver: {e}")))?;
    trace_bdf_start(trace_ctx, problem.h0, opts.max_wall_seconds);
    let n_x = problem::count_states(dae);
    let param_values = build_parameter_values(dae, budget)?;
    apply_initial_sections_and_sync_startup_state(
        &mut solver,
        dae,
        opts,
        startup_profile,
        &param_values,
        n_x,
        budget,
    )?;
    let mut solver_names = build_output_names(dae);
    solver_names.truncate(n_total);
    let output = IntegrationOutput::new(opts, n_total, solver.state().y.as_slice());
    let compiled_discrete_event_ctx = build_compiled_discrete_event_context(dae, n_total)?;

    let ctx = SolverLoopContext {
        dae,
        opts,
        startup_profile,
        n_x,
        param_values: param_values.clone(),
        discrete_event_ctx: compiled_discrete_event_ctx,
        budget,
    };
    let output = {
        let mut backend =
            DiffsolBackend::new(solver, output, ctx, Some(trace_ctx), solver_names.clone());
        let stats = rumoca_sim_core::run_with_runtime_schedule(
            &mut backend,
            dae,
            opts.t_start,
            opts.t_end,
            || budget.check().map_err(SimError::from),
        )?;
        let final_t = rumoca_sim_core::SimulationBackend::read_state(&backend).t;
        trace_bdf_done(trace_ctx, stats.steps, stats.root_hits, final_t);
        let (_solver, output, _steps, _roots) = backend.into_parts();
        output
    };

    Ok((output.buf, param_values))
}

pub(crate) use rumoca_sim_core::CompiledDiscreteEventContext;
pub(crate) use rumoca_sim_core::panic_on_expired_solver_deadline;

pub(crate) fn run_timeout_result<T, F>(budget: &TimeoutBudget, step: F) -> Result<T, SimError>
where
    F: FnOnce() -> Result<T, SimError>,
{
    rumoca_sim_core::run_timeout_result::<T, SimError, _>(budget, step)
}

pub(crate) fn solve_initial_conditions(
    dae: &mut Dae,
    ic_blocks: &[rumoca_phase_structural::IcBlock],
    n_x: usize,
    atol: f64,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    rumoca_sim_core::solve_initial_conditions(dae, ic_blocks, n_x, atol, budget)
}

pub(crate) fn integrate_with_fallbacks(
    dae: &Dae,
    opts: &SimOptions,
    n_total: usize,
    mass_matrix: &MassMatrix,
    budget: &TimeoutBudget,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    const AUTO_BDF_REGULARIZATION_LEVELS: &[f64] = &[1e-8, 1e-6, 1e-4, 1e-3];

    let try_bdf = |eps: f64, profile: SolverStartupProfile| {
        try_integrate(dae, opts, eps, n_total, mass_matrix, profile, budget)
    };
    let try_tr_bdf2 = |eps: f64, profile: SolverStartupProfile| {
        try_integrate(dae, opts, eps, n_total, mass_matrix, profile, budget)
    };
    let try_esdirk34 = |eps: f64, profile: SolverStartupProfile| {
        try_integrate(dae, opts, eps, n_total, mass_matrix, profile, budget)
    };

    let cfg = rumoca_sim_core::IntegrationFallbackConfig {
        regularization_levels: REGULARIZATION_LEVELS,
        auto_bdf_regularization_levels: AUTO_BDF_REGULARIZATION_LEVELS,
    };
    let attempts = rumoca_sim_core::IntegrationAttemptFns {
        bdf: &try_bdf,
        tr_bdf2: &try_tr_bdf2,
        esdirk34: &try_esdirk34,
    };
    rumoca_sim_core::integrate_with_runtime_fallbacks(opts, &cfg, &attempts)
}

pub(crate) fn solver_state_to_vec<'a, Eqn, S>(solver: &S) -> Vec<f64>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver.state().y.as_slice().to_vec()
}

pub(crate) fn solver_interpolate_to_vec<'a, Eqn, S>(
    solver: &S,
    t_sample: f64,
    budget: &TimeoutBudget,
    context: &str,
) -> Result<Vec<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        solver.interpolate(t_sample)
    })) {
        Ok(Ok(state)) => Ok(state.as_slice().to_vec()),
        Ok(Err(interp)) => Err(interp_err(t_sample, interp)),
        Err(panic_info) => Err(map_solver_panic(budget, context, panic_info)),
    }
}

pub(crate) fn interpolate_output_state<'a, Eqn, S>(
    solver: &S,
    t_interp: f64,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    match solver_interpolate_to_vec::<Eqn, S>(solver, t_interp, budget, "interpolate") {
        Ok(y) => Ok(y),
        Err(err) => {
            let current_t = solver.state().t;
            if is_interpolation_outside_step_sim_error(&err)
                && time_match_with_tol(t_interp, current_t)
            {
                if sim_trace_enabled() {
                    eprintln!(
                        "[sim-trace] output interpolation clamp: t_interp={} current_t={}",
                        t_interp, current_t
                    );
                }
                Ok(solver_state_to_vec::<Eqn, S>(solver))
            } else {
                Err(err)
            }
        }
    }
}

pub(crate) struct SolverStateOverwriteInput<'a> {
    pub(crate) opts: &'a SimOptions,
    pub(crate) startup_profile: SolverStartupProfile,
    pub(crate) dae: &'a Dae,
    pub(crate) param_values: &'a [f64],
    pub(crate) n_x: usize,
    pub(crate) t: f64,
    pub(crate) y: &'a [f64],
}

pub(crate) fn overwrite_solver_state<'a, Eqn, S>(
    solver: &mut S,
    input: SolverStateOverwriteInput<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let state_len = solver.state().y.as_slice().len();
    if input.y.len() != state_len {
        return Err(SimError::SolverError(format!(
            "state overwrite size mismatch: expected {state_len}, got {}",
            input.y.len()
        )));
    }

    let mut rhs = vec![0.0; state_len];
    problem::eval_rhs_equations(
        input.dae,
        input.y,
        input.param_values,
        input.t,
        &mut rhs,
        input.n_x,
    );

    let restart_h = event_restart_step_hint(input.opts, input.t, input.startup_profile);
    let state = solver.state_mut();
    *state.t = input.t;
    state.y.as_mut_slice().copy_from_slice(input.y);
    let dy = state.dy.as_mut_slice();
    if dy.len() == rhs.len() {
        dy.copy_from_slice(&rhs);
    } else {
        dy.fill(0.0);
        let copy_len = dy.len().min(rhs.len());
        dy[..copy_len].copy_from_slice(&rhs[..copy_len]);
    }
    state.dg.as_mut_slice().fill(0.0);
    for ds in state.ds.iter_mut() {
        ds.as_mut_slice().fill(0.0);
    }
    for dsg in state.dsg.iter_mut() {
        dsg.as_mut_slice().fill(0.0);
    }
    if let Some(target_h_abs) = restart_h {
        let direction = integration_direction(input.opts);
        let new_h = direction * target_h_abs;
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] event restart step-size reset: t={} h_old={} h_new={}",
                input.t, *state.h, new_h
            );
        }
        *state.h = new_h;
    }
    Ok(())
}

pub(super) fn maybe_project_scheduled_event_state(
    dae: &Dae,
    y_at_stop: &[f64],
    n_x: usize,
    t_stop: f64,
    atol: f64,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError> {
    if n_x == 0 || n_x >= y_at_stop.len() {
        return Ok(y_at_stop.to_vec());
    }
    match problem::project_algebraics_with_fixed_states_at_time(
        dae, y_at_stop, n_x, t_stop, atol, budget,
    )? {
        Some(projected) => {
            if sim_trace_enabled() {
                let changed = projected
                    .iter()
                    .zip(y_at_stop.iter())
                    .any(|(lhs, rhs)| (lhs - rhs).abs() > 1.0e-12);
                eprintln!(
                    "[sim-trace] runtime projection at t={} changed={}",
                    t_stop, changed
                );
            }
            Ok(projected)
        }
        None => {
            if sim_trace_enabled() {
                eprintln!(
                    "[sim-trace] runtime projection at t={} failed; continuing without projection",
                    t_stop
                );
            }
            Ok(y_at_stop.to_vec())
        }
    }
}

pub(super) fn apply_initial_sections_and_sync_startup_state<'a, Eqn, S>(
    solver: &mut S,
    dae: &Dae,
    opts: &SimOptions,
    startup_profile: SolverStartupProfile,
    param_values: &[f64],
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    eval::clear_pre_values();
    let mut startup_y = solver.state().y.as_slice().to_vec();
    let startup_updates = problem::apply_initial_section_assignments(
        dae,
        startup_y.as_mut_slice(),
        param_values,
        opts.t_start,
    );
    let projected = maybe_project_scheduled_event_state(
        dae,
        startup_y.as_slice(),
        n_x,
        opts.t_start,
        opts.atol,
        budget,
    )?;
    let projection_changed = projected
        .iter()
        .zip(startup_y.iter())
        .any(|(lhs, rhs)| (lhs - rhs).abs() > 1.0e-12);
    if projection_changed {
        startup_y = projected;
    }
    if startup_updates > 0 || projection_changed {
        overwrite_solver_state::<Eqn, S>(
            solver,
            SolverStateOverwriteInput {
                opts,
                startup_profile,
                dae,
                param_values,
                n_x,
                t: opts.t_start,
                y: startup_y.as_slice(),
            },
        )?;
    }
    rumoca_sim_core::refresh_pre_values_from_state_with_initial_assignments(
        dae,
        solver.state().y.as_slice(),
        param_values,
        opts.t_start,
    );
    Ok(())
}

pub(crate) fn build_compiled_discrete_event_context(
    dae: &Dae,
    solver_len: usize,
) -> Result<Option<CompiledDiscreteEventContext>, SimError> {
    rumoca_sim_core::build_compiled_discrete_event_context(dae, solver_len)
        .map_err(SimError::CompiledEval)
}

pub(crate) fn settle_runtime_event_updates(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    compiled_discrete: Option<&CompiledDiscreteEventContext>,
) -> eval::VarEnv<f64> {
    rumoca_sim_core::settle_runtime_event_updates_with_compiled_discrete(
        dae,
        y,
        p,
        n_x,
        t_eval,
        compiled_discrete,
    )
}

#[cfg(test)]
mod tests {
    use super::{
        SolverStateOverwriteInput, configure_solver_problem_with_profile, overwrite_solver_state,
    };
    use crate::{Dae, LS, SimError, SimOptions, SolverStartupProfile, problem, simulate};
    use diffsol::{OdeSolverMethod, VectorHost};
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;
    use rumoca_sim_core::test_support::{real, sub, var_ref};

    type BuiltinFunction = dae::BuiltinFunction;
    type Expression = dae::Expression;
    type OpBinary = dae::OpBinary;
    type VarName = dae::VarName;
    type Variable = dae::Variable;

    #[test]
    fn test_overwrite_solver_state_recomputes_dy_from_updated_event_state() {
        let mut dae = Dae::new();
        dae.states
            .insert(VarName::new("x"), Variable::new(VarName::new("x")));
        dae.algebraics
            .insert(VarName::new("z"), Variable::new(VarName::new("z")));
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                },
                var_ref("z"),
            ),
            span: Span::DUMMY,
            origin: "ode_x".to_string(),
            scalar_count: 1,
        });
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(
                var_ref("z"),
                Expression::If {
                    branches: vec![(
                        Expression::Binary {
                            op: OpBinary::Lt(Default::default()),
                            lhs: Box::new(var_ref("time")),
                            rhs: Box::new(real(0.5)),
                        },
                        real(1.0),
                    )],
                    else_branch: Box::new(real(2.0)),
                },
            ),
            span: Span::DUMMY,
            origin: "alg_z".to_string(),
            scalar_count: 1,
        });
        let budget = rumoca_sim_core::TimeoutBudget::new(None);
        let mass =
            rumoca_sim_core::compute_mass_matrix(&dae, 1, &[], &budget).expect("mass matrix");
        let mut problem =
            problem::build_problem(&dae, 1e-6, 1e-6, 1e-6, &mass).expect("build diffsol problem");
        configure_solver_problem_with_profile(
            &mut problem,
            &SimOptions::default(),
            SolverStartupProfile::Default,
        );
        let mut solver = problem.bdf::<LS>().expect("build BDF solver");
        {
            let state = solver.state_mut();
            state.dy.as_mut_slice().fill(123.0);
        }
        overwrite_solver_state::<_, _>(
            &mut solver,
            SolverStateOverwriteInput {
                opts: &SimOptions::default(),
                startup_profile: SolverStartupProfile::Default,
                dae: &dae,
                param_values: &[],
                n_x: 1,
                t: 0.75,
                y: &[0.0, 2.0],
            },
        )
        .expect("overwrite state should succeed");
        let state = solver.state();
        assert!((state.t - 0.75).abs() < 1e-12);
        assert!((state.y.as_slice()[0] - 0.0).abs() < 1e-12);
        assert!((state.y.as_slice()[1] - 2.0).abs() < 1e-12);
        assert!(
            (state.dy.as_slice()[0] - 2.0).abs() < 1e-9,
            "derivative should be recomputed at event state, got dy={:?}",
            state.dy.as_slice()
        );
        assert!(state.dy.as_slice()[1].abs() < 1e-9);
    }

    #[test]
    fn test_simulate_rejects_non_affine_derivative_mass_matrix_row() {
        let mut dae = Dae::new();
        dae.states
            .insert(VarName::new("x"), Variable::new(VarName::new("x")));

        let der_x = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var_ref("x")],
        };
        let nonlinear_derivative = Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(der_x.clone()),
            rhs: Box::new(der_x),
        };
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(nonlinear_derivative, real(1.0)),
            span: Span::DUMMY,
            origin: "non_affine_derivative_row".to_string(),
            scalar_count: 1,
        });
        let result = simulate(
            &dae,
            &SimOptions {
                t_start: 0.0,
                t_end: 0.1,
                dt: Some(0.05),
                max_wall_seconds: Some(1.0),
                ..SimOptions::default()
            },
        );
        match result {
            Err(SimError::SolverError(msg)) => {
                assert!(
                    msg.contains("mass-matrix form check failed")
                        && msg.contains("symbolic affine check failed"),
                    "expected mass-matrix form validation error, got: {msg}"
                );
            }
            other => panic!(
                "expected non-affine derivative row to be rejected before integration, got {other:?}"
            ),
        }
    }
}
