use super::*;
pub(crate) use rumoca_sim_core::panic_on_expired_solver_deadline;

pub(crate) fn run_timeout_step<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce(),
{
    rumoca_sim_core::run_timeout_step::<SimError, _>(budget, step)
}

pub(crate) fn run_timeout_step_result<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce() -> Result<(), SimError>,
{
    rumoca_sim_core::run_timeout_step_result::<SimError, _>(budget, step)
}

pub(crate) fn run_timeout_result<T, F>(budget: &TimeoutBudget, step: F) -> Result<T, SimError>
where
    F: FnOnce() -> Result<T, SimError>,
{
    rumoca_sim_core::run_timeout_result::<T, SimError, _>(budget, step)
}

fn is_step_size_error(msg: &str) -> bool {
    msg.to_ascii_lowercase()
        .contains("step size is too small at time =")
}

fn parse_solver_error_time(msg: &str) -> Option<f64> {
    let lower = msg.to_ascii_lowercase();
    let marker = "time =";
    let idx = lower.find(marker)?;
    let tail = msg.get(idx + marker.len()..)?.trim_start();
    let token = tail
        .split_whitespace()
        .next()?
        .trim_end_matches([',', ';', ')']);
    token.parse::<f64>().ok()
}

fn solver_error_time_is_t0(msg: &str) -> bool {
    parse_solver_error_time(msg).is_some_and(|time| time.abs() <= 1.0e-12)
}

fn is_step_size_t0_error(msg: &str) -> bool {
    is_step_size_error(msg) && solver_error_time_is_t0(msg)
}

fn is_nonlinear_fail_t0_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("maximum number of nonlinear solver failures") && solver_error_time_is_t0(msg)
}

fn is_error_test_fail_t0_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("maximum number of error test failures") && solver_error_time_is_t0(msg)
}

fn is_startup_t0_error(msg: &str) -> bool {
    is_step_size_t0_error(msg)
        || is_nonlinear_fail_t0_error(msg)
        || is_error_test_fail_t0_error(msg)
}

fn trace_ic_newton_result(mode: &str, converged: bool) {
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC full-Newton {} converged={}",
            mode, converged
        );
    }
}

fn solve_ic_with_newton(
    dae: &mut Dae,
    n_x: usize,
    atol: f64,
    budget: &TimeoutBudget,
    mode: &str,
) -> Result<(), SimError> {
    let newton_ok = problem::solve_initial_algebraic(dae, n_x, atol, budget)?;
    trace_ic_newton_result(mode, newton_ok);
    Ok(())
}

fn ic_residual_requires_newton(
    dae: &Dae,
    n_x: usize,
    atol: f64,
    budget: &TimeoutBudget,
) -> Result<bool, SimError> {
    let residual_inf = problem::initial_free_residual_inf(dae, n_x, budget)?;
    let residual_limit = (atol * 10.0).max(1.0e-8);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] IC BLT residual quality residual_inf={} limit={}",
            residual_inf, residual_limit
        );
    }
    Ok(!residual_inf.is_finite() || residual_inf > residual_limit)
}

fn force_ic_newton_via_env() -> bool {
    std::env::var("RUMOCA_SIM_FORCE_IC_NEWTON")
        .map(|v| {
            let s = v.trim().to_ascii_lowercase();
            !s.is_empty() && s != "0" && s != "false" && s != "no"
        })
        .unwrap_or(false)
}

fn log_ic_quality_fallback() {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] IC BLT residual quality check failed; running full-Newton fallback");
    }
}

pub(crate) fn solve_initial_conditions(
    dae: &mut Dae,
    ic_blocks: &[rumoca_phase_structural::IcBlock],
    n_x: usize,
    atol: f64,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    run_timeout_result(budget, || {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] IC start blocks={} n_x={} atol={}",
                ic_blocks.len(),
                n_x,
                atol
            );
        }
        if !ic_blocks.is_empty() {
            let blt_ok = rumoca_sim_core::solve_initial_blt_with_deadline(
                dae,
                n_x,
                ic_blocks,
                atol,
                budget.deadline(),
            )
            .map_err(|e| match e {
                rumoca_sim_core::IcSolveError::Timeout => SimError::from(budget.timeout_error()),
            })?;
            if sim_trace_enabled() {
                eprintln!("[sim-trace] IC BLT result converged={}", blt_ok);
            }
            let force_newton = force_ic_newton_via_env();
            if force_newton && sim_trace_enabled() {
                eprintln!("[sim-trace] IC full-Newton forced by RUMOCA_SIM_FORCE_IC_NEWTON");
            }
            let mut fallback_newton = !blt_ok || force_newton;
            if blt_ok && !force_newton && ic_residual_requires_newton(dae, n_x, atol, budget)? {
                fallback_newton = true;
                log_ic_quality_fallback();
            }
            if fallback_newton {
                solve_ic_with_newton(dae, n_x, atol, budget, "fallback")?;
            }
        } else {
            solve_ic_with_newton(dae, n_x, atol, budget, "only")?;
        }
        Ok(())
    })
}

#[derive(Default)]
struct FallbackState {
    last_err: String,
    saw_startup_t0_error: bool,
    saw_step_size_error: bool,
    startup_t0_attempts: usize,
    startup_t0_step_size_attempts: usize,
    saw_bdf_startup_t0: bool,
    saw_tr_bdf2_startup_t0: bool,
    saw_esdirk34_startup_t0: bool,
    saw_non_startup_failure: bool,
    attempt_failures: Vec<String>,
    dropped_attempt_failures: usize,
}

const MAX_RECORDED_ATTEMPT_FAILURES: usize = 16;
const ATTEMPT_DETAIL_MAX_CHARS: usize = 220;

impl FallbackState {
    fn record_non_startup_failure(&mut self) {
        self.saw_non_startup_failure = true;
    }

    fn record_startup_t0_failure(
        &mut self,
        method: &'static str,
        msg: &str,
        track_startup_t0: bool,
    ) {
        if !track_startup_t0 {
            return;
        }
        self.saw_startup_t0_error = true;
        self.startup_t0_attempts += 1;
        if is_step_size_t0_error(msg) {
            self.startup_t0_step_size_attempts += 1;
        }
        match method {
            "BDF" => self.saw_bdf_startup_t0 = true,
            "TR-BDF2" => self.saw_tr_bdf2_startup_t0 = true,
            "ESDIRK34" => self.saw_esdirk34_startup_t0 = true,
            _ => {}
        }
    }

    fn record_attempt_failure(
        &mut self,
        method: &'static str,
        eps: f64,
        profile: SolverStartupProfile,
        class: &str,
        detail: &str,
    ) {
        self.last_err = detail.to_string();
        let detail = truncate_attempt_detail(detail);
        if self.attempt_failures.len() < MAX_RECORDED_ATTEMPT_FAILURES {
            self.attempt_failures.push(format!(
                "{method}(eps={eps:.1e},profile={profile:?},class={class}): {detail}"
            ));
        } else {
            self.dropped_attempt_failures += 1;
        }
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] fallback attempt failed: method={} eps={:.1e} profile={:?} class={} detail={}",
                method, eps, profile, class, detail
            );
        }
    }

    fn attempt_history_summary(&self) -> String {
        if self.attempt_failures.is_empty() {
            return String::new();
        }
        let mut summary = self.attempt_failures.join(" | ");
        if self.dropped_attempt_failures > 0 {
            summary.push_str(&format!(
                " | ... {} more attempt failures omitted",
                self.dropped_attempt_failures
            ));
        }
        summary
    }
}

fn truncate_attempt_detail(detail: &str) -> String {
    if detail.chars().count() <= ATTEMPT_DETAIL_MAX_CHARS {
        return detail.to_string();
    }
    let mut out = String::with_capacity(ATTEMPT_DETAIL_MAX_CHARS + 1);
    for (idx, ch) in detail.chars().enumerate() {
        if idx >= ATTEMPT_DETAIL_MAX_CHARS {
            break;
        }
        out.push(ch);
    }
    out.push('…');
    out
}

fn classify_solver_error(msg: &str) -> &'static str {
    let lower = msg.to_ascii_lowercase();
    if lower.contains("step size is too small") {
        "step_size_too_small"
    } else if lower.contains("interpolationtimeoutsidecurrentstep")
        || lower.contains("interpolation time outside current step")
    {
        "interpolation_outside_step"
    } else if lower.contains("maximum number of nonlinear solver failures") {
        "nonlinear_fail_limit"
    } else if lower.contains("maximum number of error test failures") {
        "error_test_fail_limit"
    } else if lower.contains("singular") {
        "singular_system"
    } else {
        "solver_error"
    }
}

struct IntegrationCtx<'a> {
    dae: &'a Dae,
    opts: &'a SimOptions,
    n_total: usize,
    mass_matrix: &'a MassMatrix,
    budget: &'a TimeoutBudget,
}

fn has_non_finite_output(buf: &OutputBuffers) -> bool {
    buf.data
        .iter()
        .any(|col| col.iter().any(|v| !v.is_finite()))
}

const MAX_ACCEPTABLE_OUTPUT_MAGNITUDE: f64 = 1.0e120;
const MAX_ACCEPTABLE_OUTPUT_REL_GROWTH: f64 = 1.0e8;
const MAX_ACCEPTABLE_OUTPUT_ABS_GROWTH_FLOOR: f64 = 1.0e12;

fn first_excessive_finite_output(buf: &OutputBuffers) -> Option<(usize, usize, f64)> {
    buf.data.iter().enumerate().find_map(|(col_idx, col)| {
        let baseline = col.first().copied().unwrap_or(0.0).abs().max(1.0);
        let growth_limit = (baseline * MAX_ACCEPTABLE_OUTPUT_REL_GROWTH)
            .max(MAX_ACCEPTABLE_OUTPUT_ABS_GROWTH_FLOOR);
        col.iter().enumerate().find_map(|(time_idx, value)| {
            (value.is_finite()
                && (value.abs() > MAX_ACCEPTABLE_OUTPUT_MAGNITUDE || value.abs() > growth_limit))
                .then_some((col_idx, time_idx, *value))
        })
    })
}

fn handle_attempt(
    method: &'static str,
    eps: f64,
    profile: SolverStartupProfile,
    attempt: Result<(OutputBuffers, Vec<f64>), SimError>,
    nan_msg: String,
    track_startup_t0: bool,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    match attempt {
        Ok((buf, _y0)) if has_non_finite_output(&buf) => {
            state.record_non_startup_failure();
            state.record_attempt_failure(method, eps, profile, "nan_inf_output", &nan_msg);
            Ok(None)
        }
        Ok((buf, y0)) => {
            if let Some((col_idx, time_idx, value)) = first_excessive_finite_output(&buf) {
                state.record_non_startup_failure();
                let detail = format!(
                    "Unstable output magnitude (>{MAX_ACCEPTABLE_OUTPUT_MAGNITUDE:.1e}) \
                     at col={col_idx} sample={time_idx} value={value}"
                );
                state.record_attempt_failure(method, eps, profile, "unstable_output", &detail);
                return Ok(None);
            }
            Ok(Some((buf, y0)))
        }
        Err(SimError::SolverError(msg)) => {
            if is_startup_t0_error(&msg) {
                state.record_startup_t0_failure(method, &msg, track_startup_t0);
            } else {
                state.record_non_startup_failure();
            }
            if is_step_size_error(&msg) {
                state.saw_step_size_error = true;
            }
            let class = classify_solver_error(&msg);
            state.record_attempt_failure(method, eps, profile, class, &msg);
            Ok(None)
        }
        Err(SimError::Timeout { seconds }) => {
            state.record_non_startup_failure();
            state.record_attempt_failure(
                method,
                eps,
                profile,
                "timeout",
                &format!("timeout after {seconds:.3}s"),
            );
            Err(SimError::Timeout { seconds })
        }
        Err(e) => {
            state.record_non_startup_failure();
            state.record_attempt_failure(method, eps, profile, "error", &e.to_string());
            Err(e)
        }
    }
}

fn try_regularized_bdf(
    ctx: &IntegrationCtx<'_>,
    regularization_levels: &[f64],
    profile: SolverStartupProfile,
    nan_suffix: &str,
    track_startup_t0: bool,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    for &eps in regularization_levels {
        let attempt = try_integrate(
            ctx.dae,
            ctx.opts,
            eps,
            ctx.n_total,
            ctx.mass_matrix,
            profile,
            ctx.budget,
        );
        let nan_msg = format!("NaN/Inf in output at eps={eps}{nan_suffix}");
        if let Some(result) = handle_attempt(
            "BDF",
            eps,
            profile,
            attempt,
            nan_msg,
            track_startup_t0,
            state,
        )? {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

fn try_regularized_tr_bdf2(
    ctx: &IntegrationCtx<'_>,
    profile: SolverStartupProfile,
    nan_suffix: &str,
    track_startup_t0: bool,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    for &eps in REGULARIZATION_LEVELS {
        let attempt = try_integrate_tr_bdf2(
            ctx.dae,
            ctx.opts,
            eps,
            ctx.n_total,
            ctx.mass_matrix,
            profile,
            ctx.budget,
        );
        let nan_msg = format!("NaN/Inf in output (TR-BDF2) at eps={eps}{nan_suffix}");
        if let Some(result) = handle_attempt(
            "TR-BDF2",
            eps,
            profile,
            attempt,
            nan_msg,
            track_startup_t0,
            state,
        )? {
            return Ok(Some(result));
        }
        if should_fail_fast_startup_auto(ctx.opts, state) {
            return Err(build_startup_fail_fast_error(state));
        }
    }
    Ok(None)
}

fn try_regularized_esdirk34(
    ctx: &IntegrationCtx<'_>,
    profile: SolverStartupProfile,
    nan_suffix: &str,
    track_startup_t0: bool,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    for &eps in REGULARIZATION_LEVELS {
        let attempt = try_integrate_esdirk34(
            ctx.dae,
            ctx.opts,
            eps,
            ctx.n_total,
            ctx.mass_matrix,
            profile,
            ctx.budget,
        );
        let nan_msg = format!("NaN/Inf in output (ESDIRK34) at eps={eps}{nan_suffix}");
        if let Some(result) = handle_attempt(
            "ESDIRK34",
            eps,
            profile,
            attempt,
            nan_msg,
            track_startup_t0,
            state,
        )? {
            return Ok(Some(result));
        }
        if should_fail_fast_startup_auto(ctx.opts, state) {
            return Err(build_startup_fail_fast_error(state));
        }
    }
    Ok(None)
}

fn saw_startup_or_step_issue(state: &FallbackState) -> bool {
    state.saw_startup_t0_error || state.saw_step_size_error
}

fn build_fallback_exhausted_error(state: &FallbackState) -> SimError {
    let history = state.attempt_history_summary();
    if history.is_empty() {
        return SimError::SolverError(format!(
            "Failed at all regularisation levels: {}",
            state.last_err
        ));
    }
    SimError::SolverError(format!(
        "Failed at all regularisation levels: {} | attempt_history: {}",
        state.last_err, history
    ))
}

fn should_fail_fast_startup_auto(opts: &SimOptions, state: &FallbackState) -> bool {
    matches!(opts.solver_mode, SimSolverMode::Auto)
        && !state.saw_non_startup_failure
        && state.saw_bdf_startup_t0
        && (state.saw_tr_bdf2_startup_t0 || state.saw_esdirk34_startup_t0)
        && state.startup_t0_step_size_attempts >= 5
}

fn build_startup_fail_fast_error(state: &FallbackState) -> SimError {
    let history = state.attempt_history_summary();
    let mut msg = format!(
        "deterministic startup failure at t=0 across fallback methods \
         (step size collapse observed in {} attempts, no non-startup failures observed)",
        state.startup_t0_step_size_attempts
    );
    if !state.last_err.is_empty() {
        msg.push_str(&format!(": {}", state.last_err));
    }
    if !history.is_empty() {
        msg.push_str(&format!(" | attempt_history: {}", history));
    }
    SimError::SolverError(msg)
}

fn try_bdf_stage(
    ctx: &IntegrationCtx<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    const AUTO_BDF_REGULARIZATION_LEVELS: &[f64] = &[1e-8, 1e-6, 1e-4, 1e-3];
    if !matches!(
        ctx.opts.solver_mode,
        SimSolverMode::Auto | SimSolverMode::Bdf
    ) {
        return Ok(None);
    }

    let bdf_levels = if matches!(ctx.opts.solver_mode, SimSolverMode::Auto) {
        AUTO_BDF_REGULARIZATION_LEVELS
    } else {
        REGULARIZATION_LEVELS
    };
    if let Some(result) = try_regularized_bdf(
        ctx,
        bdf_levels,
        SolverStartupProfile::Default,
        "",
        true,
        state,
    )? {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state) {
        let (run_robust, levels, label) = match ctx.opts.solver_mode {
            SimSolverMode::Auto => (
                true,
                AUTO_BDF_REGULARIZATION_LEVELS,
                " (auto robust tiny-step retry)",
            ),
            SimSolverMode::Bdf => (true, REGULARIZATION_LEVELS, " (robust tiny-step retry)"),
            SimSolverMode::RkLike => (false, &[][..], ""),
        };
        if run_robust
            && let Some(result) = try_regularized_bdf(
                ctx,
                levels,
                SolverStartupProfile::RobustTinyStep,
                label,
                false,
                state,
            )?
        {
            return Ok(Some(result));
        }
    }

    Ok(None)
}

fn should_run_alt_implicit(opts: &SimOptions, state: &FallbackState) -> bool {
    matches!(
        opts.solver_mode,
        SimSolverMode::Auto | SimSolverMode::RkLike
    ) || (matches!(opts.solver_mode, SimSolverMode::Bdf) && saw_startup_or_step_issue(state))
}

fn try_alt_implicit_stage(
    ctx: &IntegrationCtx<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    if !should_run_alt_implicit(ctx.opts, state) {
        return Ok(None);
    }

    if let Some(result) =
        try_regularized_tr_bdf2(ctx, SolverStartupProfile::Default, "", true, state)?
    {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_tr_bdf2(
            ctx,
            SolverStartupProfile::RobustTinyStep,
            " (robust tiny-step retry)",
            false,
            state,
        )?
    {
        return Ok(Some(result));
    }

    if let Some(result) =
        try_regularized_esdirk34(ctx, SolverStartupProfile::Default, "", true, state)?
    {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_esdirk34(
            ctx,
            SolverStartupProfile::RobustTinyStep,
            " (robust tiny-step retry)",
            false,
            state,
        )?
    {
        return Ok(Some(result));
    }

    Ok(None)
}

fn try_auto_late_robust_bdf(
    ctx: &IntegrationCtx<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    if matches!(ctx.opts.solver_mode, SimSolverMode::Auto)
        && saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_bdf(
            ctx,
            REGULARIZATION_LEVELS,
            SolverStartupProfile::RobustTinyStep,
            " (late robust tiny-step retry)",
            false,
            state,
        )?
    {
        return Ok(Some(result));
    }
    Ok(None)
}

pub(crate) fn integrate_with_fallbacks(
    dae: &Dae,
    opts: &SimOptions,
    n_total: usize,
    mass_matrix: &MassMatrix,
    budget: &TimeoutBudget,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let mut state = FallbackState::default();
    let ctx = IntegrationCtx {
        dae,
        opts,
        n_total,
        mass_matrix,
        budget,
    };

    if let Some(result) = try_bdf_stage(&ctx, &mut state)? {
        return Ok(result);
    }

    if let Some(result) = try_alt_implicit_stage(&ctx, &mut state)? {
        return Ok(result);
    }

    if let Some(result) = try_auto_late_robust_bdf(&ctx, &mut state)? {
        return Ok(result);
    }

    Err(build_fallback_exhausted_error(&state))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_step_size_error_matches_nonzero_time() {
        let msg = "ODE solver error: Step size is too small at time = 2.0027";
        assert!(is_step_size_error(msg));
        assert!(!is_step_size_t0_error(msg));
    }

    #[test]
    fn test_is_step_size_t0_error_is_more_specific() {
        let msg = "ODE solver error: Step size is too small at time = 0";
        assert!(is_step_size_error(msg));
        assert!(is_step_size_t0_error(msg));
    }

    #[test]
    fn test_is_step_size_t0_error_does_not_match_nonzero_decimal_time() {
        let msg = "ODE solver error: Step size is too small at time = 0.20148459243126662";
        assert!(is_step_size_error(msg));
        assert!(!is_step_size_t0_error(msg));
    }

    #[test]
    fn test_parse_solver_error_time_parses_float_tokens() {
        let msg = "solver error: Step failed: ODE solver error: Step size is too small at time = 0.20000000000000015";
        let time = parse_solver_error_time(msg).expect("expected parsed time token");
        assert!((time - 0.20000000000000015).abs() < 1.0e-15);
    }

    #[test]
    fn test_classify_solver_error_variants() {
        assert_eq!(
            classify_solver_error("ODE solver error: Step size is too small at time = 0.2"),
            "step_size_too_small"
        );
        assert_eq!(
            classify_solver_error("InterpolationTimeOutsideCurrentStep"),
            "interpolation_outside_step"
        );
        assert_eq!(
            classify_solver_error("Exceeded maximum number of nonlinear solver failures"),
            "nonlinear_fail_limit"
        );
        assert_eq!(
            classify_solver_error("Exceeded maximum number of error test failures"),
            "error_test_fail_limit"
        );
    }

    #[test]
    fn test_fallback_state_attempt_history_summary_is_bounded() {
        let mut state = FallbackState::default();
        for i in 0..(MAX_RECORDED_ATTEMPT_FAILURES + 3) {
            state.record_attempt_failure(
                "BDF",
                1e-3,
                SolverStartupProfile::Default,
                "step_size_too_small",
                &format!("failure #{i}"),
            );
        }
        let summary = state.attempt_history_summary();
        assert!(summary.contains("failure #0"));
        assert!(summary.contains("more attempt failures omitted"));
    }

    #[test]
    fn test_should_fail_fast_startup_auto_when_bdf_and_tr_bdf2_fail_at_t0() {
        let opts = SimOptions::default();
        let state = FallbackState {
            saw_bdf_startup_t0: true,
            saw_tr_bdf2_startup_t0: true,
            startup_t0_step_size_attempts: 5,
            ..FallbackState::default()
        };
        assert!(should_fail_fast_startup_auto(&opts, &state));
    }

    #[test]
    fn test_should_not_fail_fast_startup_auto_when_non_startup_failure_seen() {
        let opts = SimOptions::default();
        let state = FallbackState {
            saw_bdf_startup_t0: true,
            saw_tr_bdf2_startup_t0: true,
            startup_t0_step_size_attempts: 6,
            saw_non_startup_failure: true,
            ..FallbackState::default()
        };
        assert!(!should_fail_fast_startup_auto(&opts, &state));
    }
}
