use super::*;

const MODEL: &str = "Modelica.Electrical.QuasiStatic.Machines.Examples.TransformerTestbench";

fn killed_in_solve_result() -> MslModelResult {
    let mut result = worker_model_result_to_msl(WorkerModelResult::phase_failure(
        MODEL.to_string(),
        "Success",
        "",
        None,
    ));
    result.error = None;
    result.compile_seconds = Some(20.0);
    result.sim_status = Some("sim_timeout".to_string());
    result.sim_error = Some("model attempt timeout".to_string());
    result.sim_seconds = Some(45.0);
    result.timeout_phase = Some(WorkerProgressPhase::Solve);
    result.timeout_seconds = Some(45.0);
    result
}

fn recheck_context(killed_phase: Option<WorkerProgressPhase>) -> TimeoutRecheckContext<'static> {
    TimeoutRecheckContext {
        model_name: MODEL,
        killed_phase,
        primary_budget_secs: 45.0,
        primary_elapsed_secs: 87.5,
    }
}

fn completed_outcome(result: WorkerModelResult) -> ModelWorkerRunOutcome {
    ModelWorkerRunOutcome::Completed(Box::new(rumoca_worker::ModelWorkerResponse {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        elapsed_secs: 61.0,
        result,
    }))
}

fn solve_failure_worker_result() -> WorkerModelResult {
    let mut result =
        WorkerModelResult::phase_failure(MODEL.to_string(), "Success", "", Some(String::new()));
    result.error = None;
    result.error_code = None;
    result.sim_status = Some("sim_solver_fail".to_string());
    result.sim_error = Some(
        "solve-IR evaluation failed: structural lowering failed: structurally singular system"
            .to_string(),
    );
    result.ir_solve_error = Some("structurally singular system: 2295 matched".to_string());
    result.ir_solve_error_code = Some("ES010".to_string());
    result
}

#[test]
fn recheck_records_the_real_diagnostic_instead_of_a_timeout() {
    let mut requested_budget = None;
    let result = apply_timeout_recheck(
        killed_in_solve_result(),
        recheck_context(Some(WorkerProgressPhase::Solve)),
        |budget_secs| {
            requested_budget = Some(budget_secs);
            completed_outcome(solve_failure_worker_result())
        },
    );

    assert_eq!(requested_budget, Some(180.0));
    assert_eq!(result.sim_status.as_deref(), Some("sim_solver_fail"));
    assert_eq!(result.timeout_phase, None);
    assert_eq!(result.timeout_seconds, None);
    let recheck = result
        .timeout_recheck
        .expect("a killed compiler phase is re-checked");
    assert_eq!(recheck.outcome, MSL_TIMEOUT_RECHECK_DIAGNOSTIC);
    assert_eq!(recheck.diagnostic_code.as_deref(), Some("ES010"));
    assert_eq!(recheck.diagnostic_phase.as_deref(), Some("Solve"));
    assert_eq!(recheck.killed_phase, Some(WorkerProgressPhase::Solve));
    assert_eq!(recheck.primary_budget_seconds, 45.0);
    assert_eq!(recheck.primary_elapsed_seconds, 87.5);
    assert_eq!(recheck.recheck_budget_seconds, 180.0);
}

#[test]
fn recheck_that_times_out_again_stays_a_timeout() {
    let result = apply_timeout_recheck(
        killed_in_solve_result(),
        recheck_context(Some(WorkerProgressPhase::Solve)),
        |budget_secs| ModelWorkerRunOutcome::TimedOut {
            elapsed_secs: 240.0,
            active_phase: Some(WorkerProgressPhase::Solve),
            phase_timeout_secs: budget_secs,
        },
    );

    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    assert_eq!(result.timeout_phase, Some(WorkerProgressPhase::Solve));
    assert_eq!(result.timeout_seconds, Some(45.0));
    let recheck = result.timeout_recheck.expect("re-check is recorded");
    assert_eq!(recheck.outcome, MSL_TIMEOUT_RECHECK_TIMEOUT);
    assert_eq!(recheck.diagnostic_code, None);
}

#[test]
fn recheck_that_finally_succeeds_does_not_upgrade_the_result() {
    let mut passing = WorkerModelResult::phase_failure(MODEL.to_string(), "Success", "", None);
    passing.error = None;
    passing.sim_status = Some("sim_ok".to_string());
    passing.ic_status = Some("ic_ok".to_string());

    let result = apply_timeout_recheck(
        killed_in_solve_result(),
        recheck_context(Some(WorkerProgressPhase::Solve)),
        |_| completed_outcome(passing),
    );

    assert_eq!(
        result.sim_status.as_deref(),
        Some("sim_timeout"),
        "a model that only passes above the measured budget still failed the budget"
    );
    assert_eq!(result.timeout_phase, Some(WorkerProgressPhase::Solve));
    let recheck = result.timeout_recheck.expect("re-check is recorded");
    assert_eq!(recheck.outcome, MSL_TIMEOUT_RECHECK_COMPLETED);
}

#[test]
fn recheck_that_hits_the_solver_wall_clock_stays_a_timeout() {
    let mut solver_timeout =
        WorkerModelResult::phase_failure(MODEL.to_string(), "Success", "", None);
    solver_timeout.error = None;
    solver_timeout.ic_status = Some("ic_ok".to_string());
    solver_timeout.sim_status = Some("sim_timeout".to_string());
    solver_timeout.sim_error = Some("timeout after 10.000s".to_string());

    let result = apply_timeout_recheck(
        killed_in_solve_result(),
        recheck_context(Some(WorkerProgressPhase::Solve)),
        |_| completed_outcome(solver_timeout),
    );

    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    let recheck = result.timeout_recheck.expect("re-check is recorded");
    assert_eq!(recheck.outcome, MSL_TIMEOUT_RECHECK_TIMEOUT);
}

#[test]
fn recheck_records_why_it_could_not_run() {
    let result = apply_timeout_recheck(
        killed_in_solve_result(),
        recheck_context(Some(WorkerProgressPhase::Solve)),
        |_| ModelWorkerRunOutcome::Failed("stdout closed".to_string()),
    );

    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    let recheck = result.timeout_recheck.expect("re-check is recorded");
    assert_eq!(recheck.outcome, MSL_TIMEOUT_RECHECK_UNAVAILABLE);
    assert!(
        recheck
            .detail
            .as_deref()
            .is_some_and(|detail| detail.contains("stdout closed")),
        "the reason the re-check could not decide is recorded"
    );
}

#[test]
fn integration_phase_kills_are_not_re_run() {
    let mut ran = false;
    let mut killed_in_sim = killed_in_solve_result();
    killed_in_sim.timeout_phase = Some(WorkerProgressPhase::Sim);

    let result = apply_timeout_recheck(
        killed_in_sim,
        recheck_context(Some(WorkerProgressPhase::Sim)),
        |_| {
            ran = true;
            ModelWorkerRunOutcome::Failed("must not run".to_string())
        },
    );

    assert!(!ran, "integration time is not a fixed amount of analysis");
    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    assert_eq!(result.timeout_recheck, None);
}

#[test]
fn kills_with_no_observed_phase_are_not_re_run() {
    let mut ran = false;
    let result = apply_timeout_recheck(killed_in_solve_result(), recheck_context(None), |_| {
        ran = true;
        ModelWorkerRunOutcome::Failed("must not run".to_string())
    });

    assert!(!ran);
    assert_eq!(result.timeout_recheck, None);
}

#[test]
fn every_compiler_phase_qualifies_for_a_re_check() {
    for phase in [
        WorkerProgressPhase::SourceRootLoad,
        WorkerProgressPhase::Compile,
        WorkerProgressPhase::Instantiate,
        WorkerProgressPhase::Typecheck,
        WorkerProgressPhase::Flatten,
        WorkerProgressPhase::ToDae,
        WorkerProgressPhase::Solve,
        WorkerProgressPhase::SimBuild,
        WorkerProgressPhase::IC,
    ] {
        assert!(
            timeout_recheck_applies(Some(phase)),
            "{phase} reaches a diagnostic given enough wall time"
        );
    }
    assert!(!timeout_recheck_applies(Some(WorkerProgressPhase::Sim)));
    assert!(!timeout_recheck_applies(None));
}
