use crate::runtime::report::{
    RuntimeProgressSnapshot, RuntimeTraceContext, runtime_progress_snapshot, trace_runtime_timeout,
};
use crate::runtime::time::{stop_time_reached_with_tol, time_match_with_tol};
use crate::runtime::timeout::{TimeoutBudget, is_solver_timeout_panic};
use crate::sim_types::{OutputBuffers, SimError, SimOptions, SolverStartupProfile};
use crate::timeline::build_output_times;

pub fn startup_profile_label(profile: SolverStartupProfile) -> &'static str {
    match profile {
        SolverStartupProfile::Default => "Default",
        SolverStartupProfile::RobustTinyStep => "RobustTinyStep",
    }
}

pub fn is_stop_time_at_current_state_time_error(msg: &str) -> bool {
    msg.to_ascii_lowercase()
        .contains("stop time is at the current state time")
}

pub fn record_outputs_until(
    t_out_list: &[f64],
    t_out_idx: &mut usize,
    t_limit: f64,
    buf: &mut OutputBuffers,
    mut record_at: impl FnMut(f64, &mut OutputBuffers) -> Result<(), SimError>,
) -> Result<(), SimError> {
    while *t_out_idx < t_out_list.len() {
        let t_requested = t_out_list[*t_out_idx];
        if t_requested > t_limit && !time_match_with_tol(t_requested, t_limit) {
            break;
        }
        let t_interp = if t_requested > t_limit {
            t_limit
        } else {
            t_requested
        };
        record_at(t_interp, buf)?;
        *t_out_idx += 1;
    }
    Ok(())
}

pub fn initialize_output_capture(
    opts: &SimOptions,
    n_total: usize,
    y0: &[f64],
) -> (Vec<f64>, usize, OutputBuffers, usize) {
    let dt = opts.dt.unwrap_or(opts.t_end / 500.0);
    let t_out_list = build_output_times(opts.t_start, opts.t_end, dt);
    let out_len = t_out_list.len();
    let mut buf = OutputBuffers::new(n_total, out_len);
    buf.record(opts.t_start, y0);
    (t_out_list, out_len, buf, 1)
}

pub fn check_budget_or_trace_timeout(
    budget: &TimeoutBudget,
    trace_ctx: RuntimeTraceContext,
    steps: usize,
    root_hits: usize,
    t: f64,
    output_idx: usize,
    output_len: usize,
) -> Result<(), SimError> {
    if let Err(err) = budget.check() {
        trace_runtime_timeout(
            trace_ctx,
            runtime_progress_snapshot(steps, root_hits, t, output_idx, output_len),
        );
        return Err(err.into());
    }
    Ok(())
}

pub fn should_recover_stop_time_error(msg: &str, current_t: f64, active_stop: f64) -> bool {
    is_stop_time_at_current_state_time_error(msg)
        && stop_time_reached_with_tol(current_t, active_stop)
}

pub fn near_active_stop_for_recovery(current_t: f64, active_stop: f64) -> bool {
    let tol = 1e-6 * (1.0 + current_t.abs().max(active_stop.abs()));
    (current_t - active_stop).abs() <= tol
}

pub fn is_nonlinear_solver_failure(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("nonlinear solver failure")
        || lower.contains("nonlinear solver failures")
        || lower.contains("newton iteration failed")
}

pub fn should_recover_nonlinear_failure_near_active_stop(
    msg: &str,
    current_t: f64,
    active_stop: f64,
) -> bool {
    is_nonlinear_solver_failure(msg) && near_active_stop_for_recovery(current_t, active_stop)
}

pub fn is_interpolation_outside_step_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("interpolationtimeoutsidecurrentstep")
        || lower.contains("interpolation time outside current step")
}

pub fn is_interpolation_outside_step_sim_error(err: &SimError) -> bool {
    matches!(err, SimError::SolverError(msg) if is_interpolation_outside_step_error(msg))
}

pub fn should_recover_interpolation_window_error(
    msg: &str,
    current_t: f64,
    active_stop: f64,
) -> bool {
    is_interpolation_outside_step_error(msg) && stop_time_reached_with_tol(current_t, active_stop)
}

pub fn sample_state_at_stop<F>(
    current_t: f64,
    stop_t: f64,
    current_y: &[f64],
    interpolate: F,
) -> Result<Vec<f64>, SimError>
where
    F: FnOnce(f64) -> Result<Vec<f64>, SimError>,
{
    if time_match_with_tol(current_t, stop_t) {
        return Ok(current_y.to_vec());
    }
    interpolate(stop_t)
}

pub fn reset_stop_time_error<E: std::fmt::Display>(set_err: E) -> SimError {
    SimError::SolverError(format!("Reset stop time: {set_err}"))
}

pub fn panic_payload_message(panic_info: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = panic_info.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = panic_info.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}

pub fn map_solver_panic(
    budget: &TimeoutBudget,
    context: &str,
    panic_info: Box<dyn std::any::Any + Send>,
) -> SimError {
    if is_solver_timeout_panic(panic_info.as_ref()) {
        return budget.timeout_error().into();
    }
    let message = panic_payload_message(panic_info);
    if is_interpolation_outside_step_error(&message) {
        return SimError::SolverError(format!(
            "{context}: ODE solver error: InterpolationTimeOutsideCurrentStep"
        ));
    }
    SimError::SolverError(format!("{context}: panic: {message}"))
}

pub fn integration_direction(opts: &SimOptions) -> f64 {
    if opts.t_end >= opts.t_start {
        1.0
    } else {
        -1.0
    }
}

fn profile_startup_step_hint(opts: &SimOptions, profile: SolverStartupProfile) -> Option<f64> {
    let span = (opts.t_end - opts.t_start).abs();
    let mut hint = if span.is_finite() && span > 0.0 {
        (span / 500.0).max(1e-6)
    } else {
        1e-6
    };
    if let Some(dt) = opts.dt
        && dt.is_finite()
        && dt > 0.0
    {
        let cap = (dt.abs() * 20.0).max(1e-10);
        hint = hint.min(cap);
    }
    if profile == SolverStartupProfile::RobustTinyStep {
        hint = if span.is_finite() && span > 0.0 {
            (span / 5_000_000.0).max(1e-10)
        } else {
            1e-10
        };
    }
    if hint.is_finite() && hint > 0.0 {
        Some(hint)
    } else {
        None
    }
}

pub fn event_restart_step_hint(
    opts: &SimOptions,
    t: f64,
    profile: SolverStartupProfile,
) -> Option<f64> {
    let mut hint = if let Some(dt) = opts.dt.filter(|dt| dt.is_finite() && *dt > 0.0) {
        dt.abs()
    } else {
        let span = (opts.t_end - opts.t_start).abs();
        if !span.is_finite() || span <= 0.0 {
            return None;
        }
        (span / 500.0).max(1.0e-8)
    };

    if let Some(profile_hint) = profile_startup_step_hint(opts, profile) {
        hint = hint.min(profile_hint);
    }

    let remaining = (opts.t_end - t).abs();
    if remaining.is_finite() && remaining > 0.0 {
        hint = hint.min((remaining / 8.0).max(1.0e-8));
    }
    if !time_match_with_tol(t, opts.t_start) {
        hint = (hint * 0.1).max(1.0e-10);
    }
    if hint.is_finite() && hint > 0.0 {
        Some(hint)
    } else {
        None
    }
}

pub fn snapshot(
    steps: usize,
    root_hits: usize,
    t: f64,
    output_idx: usize,
    output_len: usize,
) -> RuntimeProgressSnapshot {
    runtime_progress_snapshot(steps, root_hits, t, output_idx, output_len)
}
