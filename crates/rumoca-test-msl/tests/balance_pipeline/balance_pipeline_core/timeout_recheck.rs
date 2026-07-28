//! Separating "too slow" from "failed slowly" after a phase-monitor kill.
//!
//! The per-phase budget ([`MODEL_ATTEMPT_TIMEOUT_SECS`]) bounds how long one
//! model may sit in one pipeline phase before the monitor kills the worker.
//! That kill was recorded as `sim_timeout` regardless of *why* the phase was
//! slow, so a model that needs longer than the budget to reach a hard compiler
//! diagnostic (a structurally singular system, say) was filed in the same
//! bucket as a model whose integrator is merely slow. Measured on a full MSL
//! run, that hid a structural-failure cohort inside what read as a performance
//! pool.
//!
//! This module re-runs *only* the killed models, once, under a substantially
//! larger per-phase budget, and records what that re-run established. Two rules
//! keep the re-run a measurement rather than a way of moving the number:
//!
//! 1. The primary budget is never raised. The re-run's budget is used solely to
//!    let a diagnostic surface; it never becomes the budget a model is judged
//!    by.
//! 2. A re-run that *succeeds* is not adopted. The model exceeded the measured
//!    budget and stays `sim_timeout`; only a re-run that reaches a real failure
//!    replaces the timeout record.

use super::*;

/// Per-phase budget multiplier for the diagnostic re-run.
///
/// Sized from measurement, not taste: the killed cohort reaches its diagnostic
/// 47-80s into the Solve phase on a fully loaded host, against a 45s primary
/// budget. Four times the primary budget clears the observed worst case with
/// better than 2x headroom.
///
/// What this bounds, exactly: the re-run is monitored per phase, like the
/// primary attempt, so each phase it enters is capped at this budget and a
/// phase that overruns ends the re-run as a timeout. The models that get a
/// re-run cleared every phase before the killed one inside the *primary*
/// budget, so the re-run costs about the primary elapsed time plus this budget;
/// a re-run whose earlier phases turn slow is stopped in the first one that
/// exceeds the budget rather than being allowed to accumulate.
const TIMEOUT_RECHECK_BUDGET_MULTIPLIER: f64 = 4.0;

pub(super) fn timeout_recheck_budget_secs(primary_budget_secs: f64) -> f64 {
    primary_budget_secs * TIMEOUT_RECHECK_BUDGET_MULTIPLIER
}

/// Whether a kill in `phase` can still be holding an unreported diagnostic.
///
/// Every compile-side phase qualifies: their work is a fixed amount of
/// analysis, so more wall time only means the analysis finishes and reports
/// whatever it found. [`WorkerProgressPhase::Sim`] does not: integration
/// consumes as much time as it is given, so re-running it longer would measure
/// a different simulation than the budget the suite reports. A kill with no
/// observed phase never entered the pipeline and has nothing to re-check.
pub(super) fn timeout_recheck_applies(phase: Option<WorkerProgressPhase>) -> bool {
    match phase {
        None | Some(WorkerProgressPhase::Sim) => false,
        Some(_) => true,
    }
}

pub(super) struct TimeoutRecheckContext<'a> {
    pub(super) model_name: &'a str,
    pub(super) killed_phase: Option<WorkerProgressPhase>,
    pub(super) primary_budget_secs: f64,
    pub(super) primary_elapsed_secs: f64,
}

/// What the larger-budget re-run established.
enum TimeoutRecheckVerdict {
    /// A real failure, carrying the result that must be reported instead of
    /// the timeout.
    Diagnostic(Box<MslModelResult>),
    /// Still out of time at the larger budget, or timed out inside the solver.
    Timeout,
    /// Finished without a failure — slow, but not broken.
    Completed,
    /// The re-run itself could not be carried out.
    Unavailable(String),
}

/// Re-run a phase-monitor kill once under a larger budget and record the truth.
///
/// `timeout_result` is the record the primary attempt produced. `run_recheck`
/// receives the re-run budget in seconds and performs the actual re-run; it is
/// a parameter so the decision logic is testable without spawning a worker.
/// Models the re-check does not apply to are returned untouched, which is what
/// bounds the added suite runtime to the killed models alone.
pub(super) fn apply_timeout_recheck<F>(
    timeout_result: MslModelResult,
    ctx: TimeoutRecheckContext<'_>,
    run_recheck: F,
) -> MslModelResult
where
    F: FnOnce(f64) -> ModelWorkerRunOutcome,
{
    if !timeout_recheck_applies(ctx.killed_phase) {
        return timeout_result;
    }
    let budget_secs = timeout_recheck_budget_secs(ctx.primary_budget_secs);
    let phase_label = phase_label(ctx.killed_phase);
    println!(
        "  timeout re-check: {} killed in phase {phase_label} after {:.3}s ({:.3}s budget); re-running once with a {:.3}s budget",
        ctx.model_name, ctx.primary_elapsed_secs, ctx.primary_budget_secs, budget_secs
    );
    let started = Instant::now();
    let verdict = classify_timeout_recheck(run_recheck(budget_secs), ctx.model_name);
    let mut record = MslTimeoutRecheck {
        outcome: String::new(),
        killed_phase: ctx.killed_phase,
        primary_budget_seconds: ctx.primary_budget_secs,
        primary_elapsed_seconds: ctx.primary_elapsed_secs,
        recheck_budget_seconds: budget_secs,
        recheck_elapsed_seconds: started.elapsed().as_secs_f64(),
        diagnostic_code: None,
        diagnostic_phase: None,
        detail: None,
    };
    match verdict {
        TimeoutRecheckVerdict::Diagnostic(result) => {
            record.outcome = MSL_TIMEOUT_RECHECK_DIAGNOSTIC.to_string();
            record.diagnostic_code = recheck_diagnostic_code(&result);
            record.diagnostic_phase = Some(recheck_diagnostic_phase(&result));
            adopt_recheck_diagnostic(*result, record, ctx.model_name)
        }
        TimeoutRecheckVerdict::Timeout => {
            record.outcome = MSL_TIMEOUT_RECHECK_TIMEOUT.to_string();
            keep_timeout_result(timeout_result, record, ctx.model_name)
        }
        TimeoutRecheckVerdict::Completed => {
            record.outcome = MSL_TIMEOUT_RECHECK_COMPLETED.to_string();
            keep_timeout_result(timeout_result, record, ctx.model_name)
        }
        TimeoutRecheckVerdict::Unavailable(detail) => {
            record.outcome = MSL_TIMEOUT_RECHECK_UNAVAILABLE.to_string();
            record.detail = Some(detail);
            keep_timeout_result(timeout_result, record, ctx.model_name)
        }
    }
}

/// Replace the timeout record with the failure the re-run reached.
///
/// `timeout_phase`/`timeout_seconds` are cleared because they mean "this
/// recorded result *is* a phase kill", which is no longer true; the re-check
/// record keeps the killed phase and both budgets.
fn adopt_recheck_diagnostic(
    mut result: MslModelResult,
    record: MslTimeoutRecheck,
    model_name: &str,
) -> MslModelResult {
    println!(
        "  timeout re-check: {model_name} reached a real diagnostic in {:.3}s: {} in phase {}",
        record.recheck_elapsed_seconds,
        record.diagnostic_code.as_deref().unwrap_or("<uncoded>"),
        record.diagnostic_phase.as_deref().unwrap_or("<unknown>"),
    );
    result.timeout_phase = None;
    result.timeout_seconds = None;
    result.timeout_recheck = Some(record);
    result
}

fn keep_timeout_result(
    mut timeout_result: MslModelResult,
    record: MslTimeoutRecheck,
    model_name: &str,
) -> MslModelResult {
    println!(
        "  timeout re-check: {model_name} stays a timeout after {:.3}s ({})",
        record.recheck_elapsed_seconds, record.outcome
    );
    timeout_result.timeout_recheck = Some(record);
    timeout_result
}

fn classify_timeout_recheck(
    outcome: ModelWorkerRunOutcome,
    model_name: &str,
) -> TimeoutRecheckVerdict {
    match outcome {
        ModelWorkerRunOutcome::Completed(response) => {
            classify_recheck_result(worker_model_result_to_msl(response.result))
        }
        ModelWorkerRunOutcome::TimedOut { .. } => TimeoutRecheckVerdict::Timeout,
        ModelWorkerRunOutcome::MemoryLimitExceeded {
            elapsed_secs,
            active_phase,
            memory_limit_mb,
        } => TimeoutRecheckVerdict::Unavailable(memory_limit_message(
            model_name,
            elapsed_secs,
            memory_limit_mb,
            active_phase,
        )),
        ModelWorkerRunOutcome::Failed(error) => TimeoutRecheckVerdict::Unavailable(format!(
            "model worker failed during timeout re-check: {error}"
        )),
    }
}

fn classify_recheck_result(result: MslModelResult) -> TimeoutRecheckVerdict {
    if !recheck_result_is_failure(&result) {
        return TimeoutRecheckVerdict::Completed;
    }
    if result.sim_status.as_deref() == Some("sim_timeout") {
        return TimeoutRecheckVerdict::Timeout;
    }
    TimeoutRecheckVerdict::Diagnostic(Box::new(result))
}

fn recheck_result_is_failure(result: &MslModelResult) -> bool {
    result.error.is_some()
        || result.ir_solve_error.is_some()
        || result.ic_error.is_some()
        || result
            .sim_status
            .as_deref()
            .is_some_and(|status| status != "sim_ok")
}

fn recheck_diagnostic_code(result: &MslModelResult) -> Option<String> {
    result
        .error_code
        .clone()
        .or_else(|| result.ir_solve_error_code.clone())
        .or_else(|| result.sim_error_code.clone())
}

fn recheck_diagnostic_phase(result: &MslModelResult) -> String {
    if result.phase_reached != "Success" {
        return result.phase_reached.clone();
    }
    if result.ir_solve_error.is_some() {
        return WorkerProgressPhase::Solve.to_string();
    }
    if result.ic_error.is_some() {
        return WorkerProgressPhase::IC.to_string();
    }
    WorkerProgressPhase::Sim.to_string()
}

fn phase_label(phase: Option<WorkerProgressPhase>) -> String {
    phase.map_or_else(|| "Unknown".to_string(), |phase| phase.to_string())
}

#[cfg(test)]
mod tests;
