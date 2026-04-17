use super::*;
use rumoca_core::OptionalTimer;

fn trace_step_failure_diagnostics(
    dae: &Dae,
    compiled_runtime: &problem::CompiledRuntimeNewtonContext,
    y: &[f64],
    t: f64,
    param_values: &[f64],
) {
    if !sim_trace_enabled() {
        return;
    }

    let residuals = collect_residual_diagnostics(dae, compiled_runtime, y, param_values, t);
    trace_residual_diagnostics(dae, t, &residuals);
    if jacobian_failure_introspection_enabled()
        && residuals_need_function_eval_diagnostics(dae, &residuals)
    {
        // Function-level previews remain structural best-effort only.
        trace_function_eval_diagnostics(dae, &residuals);
    }
    let names = trace_state_value_diagnostics(dae, y);
    trace_jacobian_failure_diagnostics(compiled_runtime, dae, y, t, param_values, &names);
}

pub(crate) struct IntegrationOutput {
    t_out_list: Vec<f64>,
    pub(super) out_len: usize,
    pub(super) t_out_idx: usize,
    pub(super) buf: OutputBuffers,
}

impl IntegrationOutput {
    pub(super) fn new(dae: &Dae, opts: &SimOptions, n_total: usize, y0: &[f64]) -> Self {
        let (t_out_list, out_len, buf, t_out_idx) =
            initialize_output_capture(dae, opts, n_total, y0);
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

mod runtime_capture;
use runtime_capture::*;

fn startup_profile_label(profile: SolverStartupProfile) -> &'static str {
    match profile {
        SolverStartupProfile::Default => "Default",
        SolverStartupProfile::RobustTinyStep => "RobustTinyStep",
    }
}

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
    rumoca_sim::runtime_progress_snapshot(steps, root_hits, t, output_idx, output_len)
}

pub(super) fn trace_bdf_start(ctx: BdfTraceCtx, h0: f64, max_wall_seconds: Option<f64>) {
    rumoca_sim::trace_runtime_start(ctx, h0, max_wall_seconds);
}

pub(super) fn trace_bdf_timeout(ctx: BdfTraceCtx, snap: BdfProgressSnapshot) {
    rumoca_sim::trace_runtime_timeout(ctx, snap);
}

pub(super) fn trace_bdf_step_fail(
    ctx: BdfTraceCtx,
    snap: BdfProgressSnapshot,
    err: impl std::fmt::Display,
) {
    rumoca_sim::trace_runtime_step_fail(ctx, snap, err);
}

pub(super) fn trace_bdf_progress(
    ctx: BdfTraceCtx,
    snap: BdfProgressSnapshot,
    t_limit: f64,
    last_log: &mut OptionalTimer,
) {
    rumoca_sim::trace_runtime_progress(ctx, snap, t_limit, last_log);
}

pub(super) fn trace_bdf_done(ctx: BdfTraceCtx, steps: usize, root_hits: usize, final_t: f64) {
    rumoca_sim::trace_runtime_done(ctx, steps, root_hits, final_t);
}

pub(super) fn stop_time_reached_with_tol(t: f64, t_end: f64) -> bool {
    rumoca_sim::stop_time_reached_with_tol(t, t_end)
}

pub(super) fn time_match_with_tol(a: f64, b: f64) -> bool {
    rumoca_sim::time_match_with_tol(a, b)
}

pub(super) fn time_advanced_with_tol(previous_t: f64, current_t: f64) -> bool {
    rumoca_sim::time_advanced_with_tol(previous_t, current_t)
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

#[derive(Debug)]
pub(super) struct ResidualDiagnostic {
    eq_idx: usize,
    abs: f64,
    non_finite: bool,
    origin: String,
    rhs: String,
}

fn collect_residual_diagnostics(
    dae: &Dae,
    compiled_runtime: &problem::CompiledRuntimeNewtonContext,
    y: &[f64],
    param_values: &[f64],
    t: f64,
) -> Vec<ResidualDiagnostic> {
    let mut residual_values = vec![0.0_f64; dae.f_x.len()];
    problem::eval_compiled_runtime_residual(
        compiled_runtime,
        y,
        param_values,
        t,
        &mut residual_values,
    );
    let mut residuals = Vec::with_capacity(dae.f_x.len());
    for (idx, (eq, residual)) in dae
        .f_x
        .iter()
        .zip(residual_values.iter().copied())
        .enumerate()
    {
        residuals.push(ResidualDiagnostic {
            eq_idx: idx,
            abs: if residual.is_finite() {
                residual.abs()
            } else {
                f64::INFINITY
            },
            non_finite: !residual.is_finite(),
            origin: eq.origin.clone(),
            rhs: truncate_debug(&format!("{:?}", eq.rhs), 180),
        });
    }
    residuals.sort_by(|a, b| {
        b.abs
            .partial_cmp(&a.abs)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.eq_idx.cmp(&b.eq_idx))
    });
    residuals
}

struct JacobianFailureSummary {
    row_norms: Vec<f64>,
    col_norms: Vec<f64>,
    jac_preview: Vec<Vec<f64>>,
}

fn collect_jacobian_failure_summary(
    compiled_runtime: &problem::CompiledRuntimeNewtonContext,
    y: &[f64],
    param_values: &[f64],
    t: f64,
) -> JacobianFailureSummary {
    let n_total = y.len();
    let preview_n = n_total.min(8);
    let mut row_norms = vec![0.0_f64; n_total];
    let mut col_norms = vec![0.0_f64; n_total];
    let mut jac_preview = vec![vec![0.0_f64; preview_n]; preview_n];
    let mut v = vec![0.0_f64; n_total];
    let mut jv = vec![0.0_f64; n_total];

    for col in 0..n_total {
        v[col] = 1.0;
        problem::eval_compiled_runtime_jacobian(compiled_runtime, y, param_values, t, &v, &mut jv);
        v[col] = 0.0;
        col_norms[col] = jv.iter().fold(0.0_f64, |acc, value| acc.max(value.abs()));
        if col < preview_n {
            for row in 0..preview_n {
                jac_preview[row][col] = jv[row];
            }
        }
        for (row, val) in jv.iter().copied().enumerate() {
            row_norms[row] = row_norms[row].max(val.abs());
        }
    }

    JacobianFailureSummary {
        row_norms,
        col_norms,
        jac_preview,
    }
}

fn trace_residual_diagnostics(dae: &Dae, t: f64, residuals: &[ResidualDiagnostic]) {
    let worst = residuals.first().map(|entry| entry.abs).unwrap_or(0.0);
    let non_finite_rows = residuals.iter().filter(|entry| entry.non_finite).count();
    eprintln!(
        "[sim-trace] step-fail diagnostics: t={} n_eq={} non_finite_rows={} worst_abs_residual={}",
        t,
        dae.f_x.len(),
        non_finite_rows,
        worst
    );
    for (rank, row) in residuals.iter().take(8).enumerate() {
        eprintln!(
            "[sim-trace]   residual_rank={} eq=f_x[{}] abs={} non_finite={} origin='{}' rhs={}",
            rank, row.eq_idx, row.abs, row.non_finite, row.origin, row.rhs
        );
    }
}

pub(super) fn sorted_value_rows(y: &[f64]) -> Vec<(usize, f64, bool)> {
    let mut rows: Vec<(usize, f64, bool)> = y
        .iter()
        .copied()
        .enumerate()
        .map(|(idx, value)| {
            let abs = if value.is_finite() {
                value.abs()
            } else {
                f64::INFINITY
            };
            (idx, abs, !value.is_finite())
        })
        .collect();
    rows.sort_by(|a, b| {
        b.1.partial_cmp(&a.1)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.0.cmp(&b.0))
    });
    rows
}

fn trace_state_value_diagnostics(dae: &Dae, y: &[f64]) -> Vec<String> {
    let mut names = build_output_names(dae);
    names.truncate(y.len());
    for (rank, (idx, abs, non_finite)) in sorted_value_rows(y).iter().take(8).enumerate() {
        let name = names.get(*idx).map(String::as_str).unwrap_or("<unnamed>");
        let value = y.get(*idx).copied().unwrap_or(0.0);
        eprintln!(
            "[sim-trace]   value_rank={} y[{}] name={} value={} abs={} non_finite={}",
            rank, idx, name, value, abs, non_finite
        );
    }
    names
}

fn jacobian_failure_introspection_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT_JAC_FAILURE")
        .map(|v| {
            let s = v.trim().to_ascii_lowercase();
            !s.is_empty() && s != "0" && s != "false" && s != "no"
        })
        .unwrap_or(false)
}

fn expr_contains_function_eval_site(expr: &Expression) -> bool {
    match expr {
        // Only user-function calls justify another best-effort env build here.
        Expression::FunctionCall { .. } => true,
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_function_eval_site(lhs) || expr_contains_function_eval_site(rhs)
        }
        Expression::Unary { rhs, .. } => expr_contains_function_eval_site(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_function_eval_site(cond) || expr_contains_function_eval_site(value)
            }) || expr_contains_function_eval_site(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_function_eval_site)
        }
        Expression::Index { base, .. } => expr_contains_function_eval_site(base),
        _ => false,
    }
}

fn residuals_need_function_eval_diagnostics(dae: &Dae, residuals: &[ResidualDiagnostic]) -> bool {
    residuals
        .iter()
        .take(4)
        .filter_map(|row| dae.f_x.get(row.eq_idx))
        .any(|eq| expr_contains_function_eval_site(&eq.rhs))
}

fn trace_function_calls_in_expr(expr: &Expression, remaining: &mut usize) -> usize {
    if *remaining == 0 {
        return 0;
    }
    match expr {
        Expression::BuiltinCall { args, .. } => {
            let mut found = 0usize;
            for arg in args {
                found += trace_function_calls_in_expr(arg, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        Expression::FunctionCall { name, args, .. } => {
            let arg_values = args
                .iter()
                .take(4)
                .map(|arg| truncate_debug(&format!("{:?}", arg), 120))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!(
                "[sim-introspect]   function-preview call={} args=[{}] expr={}",
                name,
                arg_values,
                truncate_debug(&format!("{:?}", expr), 180)
            );
            *remaining -= 1;
            let mut found = 1usize;
            for arg in args {
                found += trace_function_calls_in_expr(arg, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        Expression::Binary { lhs, rhs, .. } => {
            trace_function_calls_in_expr(lhs, remaining)
                + trace_function_calls_in_expr(rhs, remaining)
        }
        Expression::Unary { rhs, .. } => trace_function_calls_in_expr(rhs, remaining),
        Expression::If {
            branches,
            else_branch,
        } => {
            let mut found = 0usize;
            for (cond, value) in branches {
                found += trace_function_calls_in_expr(cond, remaining);
                if *remaining == 0 {
                    return found;
                }
                found += trace_function_calls_in_expr(value, remaining);
                if *remaining == 0 {
                    return found;
                }
            }
            found + trace_function_calls_in_expr(else_branch, remaining)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            let mut found = 0usize;
            for elem in elements {
                found += trace_function_calls_in_expr(elem, remaining);
                if *remaining == 0 {
                    break;
                }
            }
            found
        }
        Expression::Index { base, .. } => trace_function_calls_in_expr(base, remaining),
        _ => 0,
    }
}

fn trace_function_eval_diagnostics(dae: &Dae, residuals: &[ResidualDiagnostic]) {
    if !jacobian_failure_introspection_enabled() {
        return;
    }
    let mut remaining = 12usize;
    for row in residuals.iter().take(4) {
        if remaining == 0 {
            break;
        }
        let Some(eq) = dae.f_x.get(row.eq_idx) else {
            continue;
        };
        eprintln!(
            "[sim-introspect] function-eval equation f_x[{}] origin='{}' abs_residual={}",
            row.eq_idx, row.origin, row.abs
        );
        let found = trace_function_calls_in_expr(&eq.rhs, &mut remaining);
        if found == 0 {
            eprintln!(
                "[sim-introspect]   no function calls found in f_x[{}]",
                row.eq_idx
            );
        }
    }
}

fn trace_jacobian_failure_diagnostics(
    compiled_runtime: &problem::CompiledRuntimeNewtonContext,
    dae: &Dae,
    y: &[f64],
    t: f64,
    param_values: &[f64],
    names: &[String],
) {
    if !jacobian_failure_introspection_enabled() || y.is_empty() {
        return;
    }

    let JacobianFailureSummary {
        row_norms,
        col_norms,
        jac_preview,
    } = collect_jacobian_failure_summary(compiled_runtime, y, param_values, t);
    let n_total = y.len();
    let preview_n = jac_preview.len();

    let near_zero_rows: Vec<usize> = row_norms
        .iter()
        .enumerate()
        .filter_map(|(idx, norm)| (*norm <= 1e-12).then_some(idx))
        .collect();
    let near_zero_cols: Vec<usize> = col_norms
        .iter()
        .enumerate()
        .filter_map(|(idx, norm)| (*norm <= 1e-12).then_some(idx))
        .collect();

    eprintln!(
        "[sim-introspect] step-fail Jacobian norms: n={} near_zero_rows={} near_zero_cols={}",
        n_total,
        near_zero_rows.len(),
        near_zero_cols.len()
    );
    for idx in near_zero_rows.iter().copied().take(8) {
        let origin = dae
            .f_x
            .get(idx)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        eprintln!(
            "[sim-introspect]   near-zero row f_x[{}] norm={} origin={}",
            idx, row_norms[idx], origin
        );
    }
    for idx in near_zero_cols.iter().copied().take(8) {
        let name = names.get(idx).map(String::as_str).unwrap_or("<unnamed>");
        eprintln!(
            "[sim-introspect]   near-zero col y[{}] {} norm={}",
            idx, name, col_norms[idx]
        );
    }

    if preview_n > 0 {
        eprintln!(
            "[sim-introspect] Jacobian preview (rows 0..{}, cols 0..{})",
            preview_n - 1,
            preview_n - 1
        );
        for (row_idx, row) in jac_preview.iter().enumerate() {
            let values = row
                .iter()
                .map(|value| format!("{value:.3e}"))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!(
                "[sim-introspect]   J[{}][0..{}] = [{}]",
                row_idx,
                preview_n - 1,
                values
            );
        }
    }
}

pub(super) fn is_stop_time_at_current_state_time_error(msg: &str) -> bool {
    msg.to_ascii_lowercase()
        .contains("stop time is at the current state time")
}

pub(super) fn record_outputs_until(
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

pub(super) fn initialize_output_capture(
    dae: &Dae,
    opts: &SimOptions,
    n_total: usize,
    y0: &[f64],
) -> (Vec<f64>, usize, OutputBuffers, usize) {
    let dt = opts.dt.unwrap_or(opts.t_end / 500.0);
    let coarse_times = timeline::build_output_times(opts.t_start, opts.t_end, dt);
    let event_times =
        rumoca_sim::timeline::collect_runtime_schedule_events(dae, opts.t_start, opts.t_end);
    // MLS §16.5.1 / Appendix B: stateful clocked and scheduled event updates
    // are observable at event instants, not only on the coarse output grid.
    let t_out_list = rumoca_sim::timeline::merge_output_times_with_event_observations(
        &coarse_times,
        &event_times,
        opts.t_end,
    );
    let out_len = t_out_list.len();
    let mut buf = OutputBuffers::new(n_total, out_len);
    buf.record(opts.t_start, y0);
    (t_out_list, out_len, buf, 1)
}

pub(super) fn refresh_pre_values_from_state(dae: &Dae, y: &[f64], p: &[f64], t: f64) {
    rumoca_sim::refresh_pre_values_from_state(dae, y, p, t);
}

pub(super) fn check_budget_or_trace_timeout(
    budget: &TimeoutBudget,
    trace_ctx: BdfTraceCtx,
    steps: usize,
    root_hits: usize,
    t: f64,
    output_idx: usize,
    output_len: usize,
) -> Result<(), SimError> {
    if let Err(err) = budget.check() {
        trace_bdf_timeout(
            trace_ctx,
            bdf_snapshot(steps, root_hits, t, output_idx, output_len),
        );
        return Err(err.into());
    }
    Ok(())
}

pub(super) fn solver_t_limit(reason: &OdeSolverStopReason<f64>, current_t: f64) -> f64 {
    match reason {
        OdeSolverStopReason::InternalTimestep | OdeSolverStopReason::TstopReached => current_t,
        OdeSolverStopReason::RootFound(t_root) => *t_root,
    }
}

pub(super) fn should_recover_stop_time_error(msg: &str, current_t: f64, active_stop: f64) -> bool {
    is_stop_time_at_current_state_time_error(msg)
        && stop_time_reached_with_tol(current_t, active_stop)
}

pub(super) fn near_active_stop_for_recovery(current_t: f64, active_stop: f64) -> bool {
    let tol = 1e-6 * (1.0 + current_t.abs().max(active_stop.abs()));
    (current_t - active_stop).abs() <= tol
}

pub(super) fn is_nonlinear_solver_failure(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("nonlinear solver failure")
        || lower.contains("nonlinear solver failures")
        || lower.contains("newton iteration failed")
}

pub(super) fn should_recover_nonlinear_failure_near_active_stop(
    msg: &str,
    current_t: f64,
    active_stop: f64,
) -> bool {
    is_nonlinear_solver_failure(msg) && near_active_stop_for_recovery(current_t, active_stop)
}

pub(super) fn is_interpolation_outside_step_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("interpolationtimeoutsidecurrentstep")
        || lower.contains("interpolation time outside current step")
}

pub(super) fn is_interpolation_outside_step_sim_error(err: &SimError) -> bool {
    matches!(err, SimError::SolverError(msg) if is_interpolation_outside_step_error(msg))
}

pub(super) fn should_recover_interpolation_window_error(
    msg: &str,
    current_t: f64,
    active_stop: f64,
) -> bool {
    is_interpolation_outside_step_error(msg) && stop_time_reached_with_tol(current_t, active_stop)
}

pub(super) fn sample_state_at_stop<F>(
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

pub(super) fn reset_stop_time_error<E: std::fmt::Display>(set_err: E) -> SimError {
    SimError::SolverError(format!("Reset stop time: {set_err}"))
}

pub(super) fn panic_payload_message(panic_info: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = panic_info.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = panic_info.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}

pub(crate) fn map_solver_panic(
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
    pub(super) dae: &'a Dae,
    pub(super) elim: eliminate::EliminationResult,
    pub(super) opts: &'a SimOptions,
    pub(super) startup_profile: SolverStartupProfile,
    pub(super) n_x: usize,
    pub(super) param_values: Vec<f64>,
    pub(super) compiled_runtime: problem::CompiledRuntimeNewtonContext,
    pub(super) compiled_synthetic_root: problem::CompiledSyntheticRootContext,
    pub(super) discrete_event_ctx: Option<CompiledDiscreteEventContext>,
    pub(super) budget: &'a TimeoutBudget,
}

#[derive(Debug)]
pub(super) enum StepAdvance {
    Advanced(OdeSolverStopReason<f64>),
    Recovered,
    Finished,
}

pub(super) fn integration_direction(opts: &SimOptions) -> f64 {
    if opts.t_end >= opts.t_start {
        1.0
    } else {
        -1.0
    }
}

pub(crate) fn event_restart_time(opts: &SimOptions, t_event: f64) -> f64 {
    rumoca_sim::event_restart_time(opts.t_start, opts.t_end, t_event)
}

const SYNTHETIC_ROOT_RESTART_RECHECK_LIMIT: usize = 32;

fn synthetic_root_restart_clearance(dae: &Dae, opts: &SimOptions) -> f64 {
    let atol_clearance = opts.atol.abs().clamp(1.0e-6, 1.0e-4);
    let dt_clearance = opts
        .dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .map(|dt| (dt * 1.0e-2).clamp(1.0e-6, 1.0e-3))
        .unwrap_or(1.0e-6);
    let clock_interval_clearance = dae
        .clock_intervals
        .values()
        .copied()
        .filter(|interval| interval.is_finite() && *interval > 0.0)
        // Scheduled-clock models can keep the same synthetic root armed across
        // the immediate right limit, so use the same order of clearance as the
        // post-event restart floor to avoid dozens of rechecks on one surface.
        .map(|interval| (interval * 1.0e-2).clamp(1.0e-6, 1.0e-3))
        .fold(1.0e-6, f64::max);
    atol_clearance
        .max(dt_clearance)
        .max(clock_interval_clearance)
}

fn next_restart_time_if_synthetic_roots_still_armed(
    compiled_synthetic_root: &problem::CompiledSyntheticRootContext,
    dae: &Dae,
    y: &[f64],
    param_values: &[f64],
    opts: &SimOptions,
    restart_t: f64,
    atol: f64,
) -> Option<f64> {
    if dae.synthetic_root_conditions.is_empty() {
        return None;
    }
    let clearance =
        synthetic_root_restart_clearance(dae, opts).max(atol.abs().clamp(1.0e-6, 1.0e-4));
    let armed = problem::compiled_synthetic_roots_still_armed(
        compiled_synthetic_root,
        y,
        param_values,
        restart_t,
        clearance,
    );
    if !armed {
        return None;
    }
    // Synthetic roots are evaluated with a numeric clearance band, so when the
    // same surface is still armed after projection we need to move by at least
    // that clearance width instead of reusing the generic event epsilon. This
    // avoids repeated right-limit reprojections on the same root surface.
    let next_restart_t =
        event_restart_time(opts, restart_t).max((restart_t + clearance).min(opts.t_end));
    (!time_match_with_tol(next_restart_t, restart_t)).then_some(next_restart_t)
}

fn profile_startup_step_hint(opts: &SimOptions, profile: SolverStartupProfile) -> Option<f64> {
    let span = (opts.t_end - opts.t_start).abs();
    let mut hint = if span.is_finite() && span > 0.0 {
        (span / 500.0).max(1e-6)
    } else {
        1e-6
    };
    if let Some(cap) = startup_interval_cap(opts) {
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

fn event_restart_interval_floor(dae: &Dae, opts: &SimOptions) -> Option<f64> {
    let dt_interval = opts
        .dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .map(f64::abs);
    let clock_interval = dae
        .clock_intervals
        .values()
        .copied()
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .min_by(|lhs, rhs| lhs.total_cmp(rhs));
    let interval = match (dt_interval, clock_interval) {
        (Some(lhs), Some(rhs)) => Some(lhs.min(rhs)),
        (Some(lhs), None) => Some(lhs),
        (None, Some(rhs)) => Some(rhs),
        (None, None) => None,
    }?;
    Some((interval * 1.0e-2).clamp(1.0e-6, 1.0e-2))
}

pub(super) fn event_restart_step_hint(
    dae: &Dae,
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
        // MLS §16.5 synchronous clocks are scheduled events. Once we are on the
        // right limit, restarting with a small fraction of the known interval
        // keeps the solver on the post-event branch without collapsing every
        // exact-clock restart back to the startup-sized step.
        if let Some(interval_floor) = event_restart_interval_floor(dae, opts) {
            hint = hint.max(interval_floor);
        }
    }
    if hint.is_finite() && hint > 0.0 {
        Some(hint)
    } else {
        None
    }
}

pub(super) fn recover_to_active_stop<'a, Eqn, S>(
    solver: &mut S,
    active_stop: f64,
    ctx: &SolverLoopContext,
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
            dae: ctx.dae,
            opts: ctx.opts,
            startup_profile: ctx.startup_profile,
            compiled_runtime: &ctx.compiled_runtime,
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
    ctx: &SolverLoopContext,
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
    ctx: &SolverLoopContext,
) -> Result<EventObservationResult, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let mut y_at_event = sample_event_state::<Eqn, S>(solver, t_event, ctx)?;
    refresh_pre_values_from_state(
        ctx.dae,
        y_at_event.as_slice(),
        ctx.param_values.as_slice(),
        t_event,
    );
    let mut restart_t = event_restart_time(ctx.opts, t_event);
    let use_frozen_pre = runtime_event_uses_frozen_pre_values(
        ctx.dae,
        ctx.opts,
        y_at_event.as_slice(),
        ctx.param_values.as_slice(),
        t_event,
    );
    let mut event_env = settle_event_runtime_env(ctx, &mut y_at_event, t_event, use_frozen_pre);
    eval::seed_pre_values_from_env(&event_env);
    let event_observation_state =
        project_event_state_with_seed_env(ctx, &y_at_event, t_event, &event_env)?;
    let mut projected = project_event_state_with_seed_env(ctx, &y_at_event, restart_t, &event_env)?;
    // SPEC_0022 SIM-001/SIM-008 (MLS App B): event updates must settle before
    // continuous integration resumes, so keep nudging the right-limit restart
    // if a synthetic root is still numerically on the zero surface.
    for _ in 0..SYNTHETIC_ROOT_RESTART_RECHECK_LIMIT {
        let Some(next_restart_t) = next_restart_time_if_synthetic_roots_still_armed(
            &ctx.compiled_synthetic_root,
            ctx.dae,
            projected.as_slice(),
            ctx.param_values.as_slice(),
            ctx.opts,
            restart_t,
            ctx.opts.atol,
        ) else {
            break;
        };
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] event restart synthetic-root clearance: t={} -> {}",
                restart_t, next_restart_t
            );
        }
        restart_t = next_restart_t;
        projected = project_event_state_with_seed_env(ctx, &y_at_event, restart_t, &event_env)?;
    }
    let event_capture_env =
        build_event_capture_env(ctx, &event_observation_state, t_event, &event_env);
    y_at_event = projected;
    overwrite_solver_state::<Eqn, S>(
        solver,
        SolverStateOverwriteInput {
            dae: ctx.dae,
            opts: ctx.opts,
            startup_profile: ctx.startup_profile,
            compiled_runtime: &ctx.compiled_runtime,
            param_values: ctx.param_values.as_slice(),
            n_x: ctx.n_x,
            t: restart_t,
            y: y_at_event.as_slice(),
        },
    )?;
    eval::refresh_env_solver_and_parameter_values(
        &mut event_env,
        ctx.dae,
        solver.state().y.as_slice(),
        ctx.param_values.as_slice(),
        restart_t,
    );
    let _ = rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env(
        ctx.dae,
        y_at_event.as_mut_slice(),
        ctx.n_x,
        &mut event_env,
    );
    eval::seed_pre_values_from_env(&event_env);
    Ok(EventObservationResult {
        state: event_observation_state,
        runtime_env: event_capture_env,
    })
}

fn sample_event_state<'a, Eqn, S>(
    solver: &mut S,
    t_event: f64,
    ctx: &SolverLoopContext,
) -> Result<Vec<f64>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let current_t = solver.state().t;
    let current_y = solver.state().y.as_slice().to_vec();
    sample_state_at_stop(current_t, t_event, current_y.as_slice(), |t_sample| {
        solver_interpolate_to_vec::<Eqn, S>(
            solver,
            t_sample,
            ctx.budget,
            "interpolate(event update)",
        )
    })
}

fn settle_event_runtime_env(
    ctx: &SolverLoopContext,
    y_at_event: &mut [f64],
    t_event: f64,
    use_frozen_pre: bool,
) -> eval::VarEnv<f64> {
    if use_frozen_pre {
        // MLS §16.5.1: synchronous clock partitions use event-entry left-limit
        // values for the full settle round on a tick. Pure time-threshold
        // synthetic roots must do the same, or `pre(z)+...` counters will
        // spuriously re-fire on every settle pass at one event instant.
        settle_runtime_event_updates_frozen_pre(
            ctx.dae,
            y_at_event,
            ctx.param_values.as_slice(),
            ctx.n_x,
            t_event,
            ctx.discrete_event_ctx.as_ref(),
        )
    } else {
        settle_runtime_event_updates(
            ctx.dae,
            y_at_event,
            ctx.param_values.as_slice(),
            ctx.n_x,
            t_event,
            ctx.discrete_event_ctx.as_ref(),
        )
    }
}

fn project_event_state_with_seed_env(
    ctx: &SolverLoopContext,
    y_at_event: &[f64],
    t_eval: f64,
    seed_env: &eval::VarEnv<f64>,
) -> Result<Vec<f64>, SimError> {
    // MLS §16.5.1 / Appendix B: event-row observations must expose the
    // right-limit settled state at the event instant itself, not the later
    // restart state used to resume continuous integration.
    project_scheduled_event_state_with_seed_env(ScheduledEventProjectionInput {
        dae: ctx.dae,
        y_at_stop: y_at_event,
        p: ctx.param_values.as_slice(),
        n_x: ctx.n_x,
        t_stop: t_eval,
        atol: ctx.opts.atol,
        budget: ctx.budget,
        compiled_runtime: &ctx.compiled_runtime,
        seed_env,
    })
}

fn build_event_capture_env(
    ctx: &SolverLoopContext,
    event_observation_state: &[f64],
    t_event: f64,
    event_env: &eval::VarEnv<f64>,
) -> eval::VarEnv<f64> {
    let mut event_capture_env = event_env.clone();
    let mut event_capture_state = event_observation_state.to_vec();
    eval::refresh_env_solver_and_parameter_values(
        &mut event_capture_env,
        ctx.dae,
        event_capture_state.as_slice(),
        ctx.param_values.as_slice(),
        t_event,
    );
    let _ = rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env(
        ctx.dae,
        event_capture_state.as_mut_slice(),
        ctx.n_x,
        &mut event_capture_env,
    );
    event_capture_env
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
    bdf_last_log: OptionalTimer,
    steps: usize,
    root_hits: usize,
    stalled_output_steps: usize,
    last_output_idx: usize,
    runtime_capture: Option<RuntimeChannelCapture>,
    dynamic_stop_hints: Option<RuntimeDynamicStopHints>,
    _phantom: std::marker::PhantomData<&'a Eqn>,
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
    ) -> Result<Self, SimError> {
        let runtime_names = runtime_capture_target_names(ctx.dae, &solver_names);
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
            let settle_ctx = build_runtime_discrete_capture_context(
                ctx.dae,
                &ctx.elim,
                solver_names.len(),
                ctx.n_x,
                runtime_names.as_slice(),
            );
            Some(RuntimeChannelCapture {
                names: runtime_names,
                solver_name_to_idx,
                settle_ctx,
            })
        };
        let dynamic_stop_hints = RuntimeDynamicStopHints::from_dae(ctx.dae);
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
            dynamic_stop_hints,
            _phantom: std::marker::PhantomData,
        };
        let t0 = backend.solver.state().t;
        let y0 = backend.solver.state().y.as_slice().to_vec();
        backend.record_runtime_sample(t0, y0.as_slice(), RuntimeSampleMode::Initialization)?;
        Ok(backend)
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
                )?;
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
    ) -> Result<Option<Vec<f64>>, SimError> {
        let Some(capture) = self.runtime_capture.as_ref() else {
            return Ok(None);
        };
        let mut y = sample_state.to_vec();
        let env = match mode {
            RuntimeSampleMode::Initialization => {
                // Keep startup channels consistent with initialized solver state.
                rumoca_sim::runtime::startup::build_initial_section_env_strict(
                    self.ctx.dae,
                    y.as_mut_slice(),
                    self.ctx.param_values.as_slice(),
                    t_sample,
                )
                .map_err(SimError::CompiledEval)?
            }
            RuntimeSampleMode::Regular => {
                if runtime_event_matches_schedule(self.ctx.dae, self.ctx.opts, t_sample) {
                    let mut env = rumoca_sim::runtime::event::build_runtime_env(
                        self.ctx.dae,
                        y.as_mut_slice(),
                        self.ctx.param_values.as_slice(),
                        t_sample,
                    );
                    seed_capture_pre_values(&mut env, &capture.names);
                    env
                } else {
                    settle_runtime_discrete_capture_env_with_context(
                        self.ctx.dae,
                        &self.ctx.elim,
                        y.as_mut_slice(),
                        self.ctx.param_values.as_slice(),
                        self.ctx.n_x,
                        t_sample,
                        &capture.settle_ctx,
                    )
                }
            }
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
        Ok(Some(values))
    }

    fn record_runtime_sample(
        &mut self,
        t_sample: f64,
        sample_state: &[f64],
        mode: RuntimeSampleMode,
    ) -> Result<(), SimError> {
        let Some(values) = self.evaluate_runtime_sample_values(t_sample, sample_state, mode)?
        else {
            return Ok(());
        };
        self.output.buf.record_runtime_values(values.as_slice());
        Ok(())
    }
}

impl<'a, Eqn, S> rumoca_sim::SimulationBackend for DiffsolBackend<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    type Error = SimError;

    fn init(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn step_until(&mut self, stop_time: f64) -> Result<rumoca_sim::StepUntilOutcome, Self::Error> {
        if stop_time_reached_with_tol(self.solver.state().t, self.ctx.opts.t_end) {
            return Ok(rumoca_sim::StepUntilOutcome::Finished);
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
        let current_t = self.solver.state().t;
        let mut effective_stop = stop_time;
        if let Some(hints) = self.dynamic_stop_hints.as_ref()
            && let Some(dynamic_stop) = next_dynamic_runtime_stop_time(
                &RuntimeDynamicStopInput {
                    dae: self.ctx.dae,
                    elim: &self.ctx.elim,
                    p: self.ctx.param_values.as_slice(),
                    n_x: self.ctx.n_x,
                    hints,
                },
                self.solver.state().y.as_slice(),
                current_t,
                stop_time,
            )
        {
            effective_stop = dynamic_stop;
        }

        set_solver_stop_time::<Eqn, S>(
            &mut self.solver,
            effective_stop,
            self.ctx.budget,
            "Reset stop time",
        )
        .map_err(reset_stop_time_error)?;

        let active_stop_at_step = effective_stop;
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
                        &self.ctx.compiled_runtime,
                        y,
                        current_t,
                        self.ctx.param_values.as_slice(),
                    );
                },
            )? {
                StepAdvance::Advanced(reason) => break reason,
                StepAdvance::Recovered => continue,
                StepAdvance::Finished => return Ok(rumoca_sim::StepUntilOutcome::Finished),
            }
        };

        self.steps += 1;
        self.record_output_for_step(&reason)?;

        match reason {
            OdeSolverStopReason::InternalTimestep => Ok(rumoca_sim::StepUntilOutcome::InternalStep),
            OdeSolverStopReason::RootFound(t_root) => {
                self.root_hits += 1;
                Ok(rumoca_sim::StepUntilOutcome::RootFound { t_root })
            }
            OdeSolverStopReason::TstopReached => Ok(rumoca_sim::StepUntilOutcome::StopReached),
        }
    }

    fn read_state(&self) -> rumoca_sim::BackendState {
        rumoca_sim::BackendState {
            t: self.solver.state().t,
        }
    }

    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error> {
        let event_observation =
            apply_event_updates_at_time::<Eqn, S>(&mut self.solver, event_time, &self.ctx)?;
        if let Some(capture) = self.runtime_capture.as_ref() {
            let values: Vec<f64> = capture
                .names
                .iter()
                .map(|name| observed_runtime_sample_value(capture, &event_observation, name))
                .collect();
            self.output
                .buf
                .overwrite_runtime_values_at_time(event_time, values.as_slice());
        }
        Ok(())
    }
}

fn project_internal_step_state_if_needed<'a, Eqn, S>(
    solver: &mut S,
    reason: &OdeSolverStopReason<f64>,
    ctx: &SolverLoopContext,
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
    input: &IntegrationRunInput<'_>,
    eps: f64,
    startup_profile: SolverStartupProfile,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let trace_ctx = bdf_trace_ctx(sim_trace_enabled(), eps, startup_profile);
    input.budget.check()?;
    let mut problem = problem::build_problem_with_params(
        input.dae,
        input.opts.rtol,
        input.opts.atol,
        eps,
        input.mass_matrix,
        input.param_values,
    )?;
    configure_solver_problem_with_profile(&mut problem, input.opts, startup_profile);
    let mut solver = problem
        .bdf::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create BDF solver: {e}")))?;
    trace_bdf_start(trace_ctx, problem.h0, input.opts.max_wall_seconds);
    let n_x = problem::count_states(input.dae);
    let compiled_runtime =
        problem::build_compiled_runtime_newton_context(input.dae, input.n_total)?;
    let compiled_synthetic_root =
        problem::build_compiled_synthetic_root_context(input.dae, input.n_total)?;
    apply_initial_sections_and_sync_startup_state(
        &mut solver,
        StartupSyncInput {
            dae: input.dae,
            opts: input.opts,
            startup_profile,
            compiled_runtime: &compiled_runtime,
            param_values: input.param_values,
            n_x,
            budget: input.budget,
        },
    )?;
    let mut solver_names = build_output_names(input.dae);
    solver_names.truncate(input.n_total);
    let output = IntegrationOutput::new(
        input.dae,
        input.opts,
        input.n_total,
        solver.state().y.as_slice(),
    );
    let compiled_discrete_event_ctx =
        build_compiled_discrete_event_context(input.dae, input.n_total)?;

    let ctx = SolverLoopContext {
        dae: input.dae,
        elim: input.elim.clone(),
        opts: input.opts,
        startup_profile,
        n_x,
        param_values: input.param_values.to_vec(),
        compiled_runtime,
        compiled_synthetic_root,
        discrete_event_ctx: compiled_discrete_event_ctx,
        budget: input.budget,
    };
    let output = {
        let mut backend =
            DiffsolBackend::new(solver, output, ctx, Some(trace_ctx), solver_names.clone())?;
        let stats = rumoca_sim::run_with_runtime_schedule(
            &mut backend,
            input.dae,
            input.opts.t_start,
            input.opts.t_end,
            || input.budget.check().map_err(SimError::from),
        )?;
        let final_t = rumoca_sim::SimulationBackend::read_state(&backend).t;
        trace_bdf_done(trace_ctx, stats.steps, stats.root_hits, final_t);
        let (_solver, output, _steps, _roots) = backend.into_parts();
        output
    };
    Ok((output.buf, input.param_values.to_vec()))
}
pub(super) fn try_integrate_tr_bdf2(
    input: &IntegrationRunInput<'_>,
    eps: f64,
    startup_profile: SolverStartupProfile,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let trace_enabled = sim_trace_enabled();
    let start = trace_timer_start_if(trace_enabled);
    input.budget.check()?;
    let mut problem = problem::build_problem_with_params(
        input.dae,
        input.opts.rtol,
        input.opts.atol,
        eps,
        input.mass_matrix,
        input.param_values,
    )?;
    configure_solver_problem_with_profile(&mut problem, input.opts, startup_profile);
    if trace_enabled {
        eprintln!(
            "[sim-trace] TR-BDF2 start eps={} profile={:?} h0={} max_wall={:?}",
            eps, startup_profile, problem.h0, input.opts.max_wall_seconds
        );
    }
    let mut solver = problem
        .tr_bdf2::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create TR-BDF2 solver: {e}")))?;
    let PreparedIntegrationLoop {
        param_values,
        output,
        ctx,
        solver_names,
    } = prepare_integration_loop(&mut solver, input, startup_profile)?;
    let (output, stats, final_t) = {
        let mut backend = DiffsolBackend::new(solver, output, ctx, None, solver_names.clone())?;
        let stats = match rumoca_sim::run_with_runtime_schedule(
            &mut backend,
            input.dae,
            input.opts.t_start,
            input.opts.t_end,
            || input.budget.check().map_err(SimError::from),
        ) {
            Ok(stats) => stats,
            Err(err) => {
                let final_t = rumoca_sim::SimulationBackend::read_state(&backend).t;
                if trace_enabled {
                    eprintln!(
                        "[sim-trace] TR-BDF2 step-fail eps={} profile={:?} elapsed={:.3}s t={} err={}",
                        eps,
                        startup_profile,
                        trace_timer_elapsed_seconds(start),
                        final_t,
                        err
                    );
                }
                return Err(err);
            }
        };
        let final_t = rumoca_sim::SimulationBackend::read_state(&backend).t;
        let (_solver, output, _steps, _roots) = backend.into_parts();
        (output, stats, final_t)
    };
    if trace_enabled {
        eprintln!(
            "[sim-trace] TR-BDF2 done eps={} profile={:?} elapsed={:.3}s steps={} roots={} final_t={}",
            eps,
            startup_profile,
            trace_timer_elapsed_seconds(start),
            stats.steps,
            stats.root_hits,
            final_t
        );
    }
    Ok((output.buf, param_values))
}

pub(super) struct PreparedIntegrationLoop<'a> {
    pub(super) param_values: Vec<f64>,
    pub(super) output: IntegrationOutput,
    pub(super) ctx: SolverLoopContext<'a>,
    pub(super) solver_names: Vec<String>,
}

pub(super) fn prepare_integration_loop<'a, Eqn, S>(
    solver: &mut S,
    input: &IntegrationRunInput<'a>,
    startup_profile: SolverStartupProfile,
) -> Result<PreparedIntegrationLoop<'a>, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let n_x = problem::count_states(input.dae);
    let compiled_runtime =
        problem::build_compiled_runtime_newton_context(input.dae, input.n_total)?;
    let compiled_synthetic_root =
        problem::build_compiled_synthetic_root_context(input.dae, input.n_total)?;
    apply_initial_sections_and_sync_startup_state(
        solver,
        StartupSyncInput {
            dae: input.dae,
            opts: input.opts,
            startup_profile,
            compiled_runtime: &compiled_runtime,
            param_values: input.param_values,
            n_x,
            budget: input.budget,
        },
    )?;
    let solver_names = truncated_solver_names(input.dae, input.n_total);
    let output = IntegrationOutput::new(
        input.dae,
        input.opts,
        input.n_total,
        solver.state().y.as_slice(),
    );
    let discrete_event_ctx = build_compiled_discrete_event_context(input.dae, input.n_total)?;
    let ctx = SolverLoopContext {
        dae: input.dae,
        elim: input.elim.clone(),
        opts: input.opts,
        startup_profile,
        n_x,
        param_values: input.param_values.to_vec(),
        compiled_runtime,
        compiled_synthetic_root,
        discrete_event_ctx,
        budget: input.budget,
    };
    Ok(PreparedIntegrationLoop {
        param_values: input.param_values.to_vec(),
        output,
        ctx,
        solver_names,
    })
}

fn truncated_solver_names(dae: &Dae, n_total: usize) -> Vec<String> {
    let mut solver_names = build_output_names(dae);
    solver_names.truncate(n_total);
    solver_names
}

mod solver_state;
pub(super) use solver_state::{
    SolverStateOverwriteInput, interpolate_output_state, overwrite_solver_state,
    solver_interpolate_to_vec, solver_state_to_vec,
};

mod esdirk34;
pub(crate) use esdirk34::try_integrate_esdirk34;

mod event_settle;
pub(crate) use event_settle::settle_runtime_event_updates;
pub(super) use event_settle::{
    CompiledDiscreteEventContext, StartupSyncInput, apply_initial_sections_and_sync_startup_state,
    build_compiled_discrete_event_context, settle_runtime_event_updates_frozen_pre,
};
use event_settle::{
    ScheduledEventProjectionInput, maybe_project_scheduled_event_state,
    project_scheduled_event_state_with_seed_env,
};

mod fallback;
pub(crate) use fallback::{
    integrate_with_fallbacks, panic_on_expired_solver_deadline, run_timeout_result,
    solve_initial_conditions,
};

#[cfg(test)]
mod tests;
