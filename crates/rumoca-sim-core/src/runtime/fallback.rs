use crate::sim_types::{OutputBuffers, SimError, SimOptions, SimSolverMode, SolverStartupProfile};

const MAX_RECORDED_ATTEMPT_FAILURES: usize = 16;
const ATTEMPT_DETAIL_MAX_CHARS: usize = 220;
const MAX_ACCEPTABLE_OUTPUT_MAGNITUDE: f64 = 1.0e120;
const MAX_ACCEPTABLE_OUTPUT_REL_GROWTH: f64 = 1.0e8;
const MAX_ACCEPTABLE_OUTPUT_ABS_GROWTH_FLOOR: f64 = 1.0e12;

pub fn is_step_size_error(msg: &str) -> bool {
    msg.to_ascii_lowercase()
        .contains("step size is too small at time =")
}

pub fn parse_solver_error_time(msg: &str) -> Option<f64> {
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

pub fn solver_error_time_is_t0(msg: &str) -> bool {
    parse_solver_error_time(msg).is_some_and(|time| time.abs() <= 1.0e-12)
}

pub fn is_step_size_t0_error(msg: &str) -> bool {
    is_step_size_error(msg) && solver_error_time_is_t0(msg)
}

pub fn is_nonlinear_fail_t0_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("maximum number of nonlinear solver failures") && solver_error_time_is_t0(msg)
}

pub fn is_error_test_fail_t0_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    lower.contains("maximum number of error test failures") && solver_error_time_is_t0(msg)
}

pub fn is_startup_t0_error(msg: &str) -> bool {
    is_step_size_t0_error(msg)
        || is_nonlinear_fail_t0_error(msg)
        || is_error_test_fail_t0_error(msg)
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

fn has_non_finite_output(buf: &OutputBuffers) -> bool {
    buf.data
        .iter()
        .any(|col| col.iter().any(|v| !v.is_finite()))
}

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

#[derive(Default)]
struct FallbackState {
    last_err: String,
    saw_startup_t0_error: bool,
    saw_step_size_error: bool,
    startup_t0_step_size_attempts: usize,
    saw_bdf_startup_t0: bool,
    saw_tr_bdf2_startup_t0: bool,
    saw_esdirk34_startup_t0: bool,
    saw_non_startup_failure: bool,
    attempt_failures: Vec<String>,
    dropped_attempt_failures: usize,
}

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

fn saw_startup_or_step_issue(state: &FallbackState) -> bool {
    state.saw_startup_t0_error || state.saw_step_size_error
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

pub struct IntegrationFallbackConfig<'a> {
    pub regularization_levels: &'a [f64],
    pub auto_bdf_regularization_levels: &'a [f64],
}

pub struct IntegrationAttemptFns<'a> {
    pub bdf: &'a dyn Fn(f64, SolverStartupProfile) -> Result<(OutputBuffers, Vec<f64>), SimError>,
    pub tr_bdf2:
        &'a dyn Fn(f64, SolverStartupProfile) -> Result<(OutputBuffers, Vec<f64>), SimError>,
    pub esdirk34:
        &'a dyn Fn(f64, SolverStartupProfile) -> Result<(OutputBuffers, Vec<f64>), SimError>,
}

fn try_regularized_method(
    method: &'static str,
    levels: &[f64],
    profile: SolverStartupProfile,
    nan_suffix: &str,
    track_startup_t0: bool,
    attempt_fn: &dyn Fn(f64, SolverStartupProfile) -> Result<(OutputBuffers, Vec<f64>), SimError>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    for &eps in levels {
        let attempt = attempt_fn(eps, profile);
        let nan_msg = match method {
            "TR-BDF2" => format!("NaN/Inf in output (TR-BDF2) at eps={eps}{nan_suffix}"),
            "ESDIRK34" => format!("NaN/Inf in output (ESDIRK34) at eps={eps}{nan_suffix}"),
            _ => format!("NaN/Inf in output at eps={eps}{nan_suffix}"),
        };
        if let Some(result) = handle_attempt(
            method,
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

fn try_bdf_stage(
    opts: &SimOptions,
    cfg: &IntegrationFallbackConfig<'_>,
    attempts: &IntegrationAttemptFns<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    if !matches!(opts.solver_mode, SimSolverMode::Auto | SimSolverMode::Bdf) {
        return Ok(None);
    }

    let bdf_levels = if matches!(opts.solver_mode, SimSolverMode::Auto) {
        cfg.auto_bdf_regularization_levels
    } else {
        cfg.regularization_levels
    };
    if let Some(result) = try_regularized_method(
        "BDF",
        bdf_levels,
        SolverStartupProfile::Default,
        "",
        true,
        attempts.bdf,
        state,
    )? {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state) {
        let (run_robust, levels, label) = match opts.solver_mode {
            SimSolverMode::Auto => (
                true,
                cfg.auto_bdf_regularization_levels,
                " (auto robust tiny-step retry)",
            ),
            SimSolverMode::Bdf => (true, cfg.regularization_levels, " (robust tiny-step retry)"),
            SimSolverMode::RkLike => (false, &[][..], ""),
        };
        if run_robust
            && let Some(result) = try_regularized_method(
                "BDF",
                levels,
                SolverStartupProfile::RobustTinyStep,
                label,
                false,
                attempts.bdf,
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
    opts: &SimOptions,
    cfg: &IntegrationFallbackConfig<'_>,
    attempts: &IntegrationAttemptFns<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    if !should_run_alt_implicit(opts, state) {
        return Ok(None);
    }

    if let Some(result) = try_regularized_method(
        "TR-BDF2",
        cfg.regularization_levels,
        SolverStartupProfile::Default,
        "",
        true,
        attempts.tr_bdf2,
        state,
    )? {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_method(
            "TR-BDF2",
            cfg.regularization_levels,
            SolverStartupProfile::RobustTinyStep,
            " (robust tiny-step retry)",
            false,
            attempts.tr_bdf2,
            state,
        )?
    {
        return Ok(Some(result));
    }

    if let Some(result) = try_regularized_method(
        "ESDIRK34",
        cfg.regularization_levels,
        SolverStartupProfile::Default,
        "",
        true,
        attempts.esdirk34,
        state,
    )? {
        return Ok(Some(result));
    }

    if saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_method(
            "ESDIRK34",
            cfg.regularization_levels,
            SolverStartupProfile::RobustTinyStep,
            " (robust tiny-step retry)",
            false,
            attempts.esdirk34,
            state,
        )?
    {
        return Ok(Some(result));
    }

    Ok(None)
}

fn try_auto_late_robust_bdf(
    opts: &SimOptions,
    cfg: &IntegrationFallbackConfig<'_>,
    attempts: &IntegrationAttemptFns<'_>,
    state: &mut FallbackState,
) -> Result<Option<(OutputBuffers, Vec<f64>)>, SimError> {
    if matches!(opts.solver_mode, SimSolverMode::Auto)
        && saw_startup_or_step_issue(state)
        && let Some(result) = try_regularized_method(
            "BDF",
            cfg.regularization_levels,
            SolverStartupProfile::RobustTinyStep,
            " (late robust tiny-step retry)",
            false,
            attempts.bdf,
            state,
        )?
    {
        return Ok(Some(result));
    }
    Ok(None)
}

pub fn integrate_with_fallbacks(
    opts: &SimOptions,
    cfg: &IntegrationFallbackConfig<'_>,
    attempts: &IntegrationAttemptFns<'_>,
) -> Result<(OutputBuffers, Vec<f64>), SimError> {
    let mut state = FallbackState::default();

    if let Some(result) = try_bdf_stage(opts, cfg, attempts, &mut state)? {
        return Ok(result);
    }

    if should_fail_fast_startup_auto(opts, &state) {
        return Err(build_startup_fail_fast_error(&state));
    }

    if let Some(result) = try_alt_implicit_stage(opts, cfg, attempts, &mut state)? {
        return Ok(result);
    }

    if should_fail_fast_startup_auto(opts, &state) {
        return Err(build_startup_fail_fast_error(&state));
    }

    if let Some(result) = try_auto_late_robust_bdf(opts, cfg, attempts, &mut state)? {
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
    fn test_parse_solver_error_time_parses_float_tokens() {
        let msg = "solver error: Step failed: ODE solver error: Step size is too small at time = 0.20000000000000015";
        let time = parse_solver_error_time(msg).expect("expected parsed time token");
        assert!((time - 0.20000000000000015).abs() < 1.0e-15);
    }
}
