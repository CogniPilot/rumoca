use super::*;

#[test]
fn phase_timing_totals_fold_process_isolated_worker_results() {
    let mut first = WorkerModelResult::phase_failure("A".to_string(), "ToDae", "", None);
    first.instantiate_seconds = Some(0.25);
    first.typecheck_seconds = Some(0.50);
    first.flatten_seconds = Some(1.00);
    first.dae_seconds = Some(0.125);
    let mut second = WorkerModelResult::phase_failure("B".to_string(), "Flatten", "", None);
    second.instantiate_seconds = Some(0.125);
    second.typecheck_seconds = Some(0.25);
    second.flatten_seconds = Some(0.50);

    let results = [
        worker_model_result_to_msl(first),
        worker_model_result_to_msl(second),
    ];
    let mut timings = MslPhaseTimings::default();
    update_phase_timing_totals(&mut timings, &results);

    assert_eq!(timings.compile_instantiate_seconds, 0.375);
    assert_eq!(timings.compile_instantiate_calls, 2);
    assert_eq!(timings.compile_typecheck_seconds, 0.75);
    assert_eq!(timings.compile_typecheck_calls, 2);
    assert_eq!(timings.compile_flatten_seconds, 1.5);
    assert_eq!(timings.compile_flatten_calls, 2);
    assert_eq!(timings.compile_todae_seconds, 0.125);
    assert_eq!(timings.compile_todae_calls, 1);
}

#[test]
fn compile_chunk_progress_loop_exits_promptly_after_flag_clears() {
    let compile_in_flight = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
    let compile_in_flight_flag = std::sync::Arc::clone(&compile_in_flight);
    let start = Instant::now();
    let worker = std::thread::spawn(move || {
        run_compile_chunk_progress_loop(compile_in_flight_flag, 1, 1, 1);
    });

    std::thread::sleep(Duration::from_millis(50));
    compile_in_flight.store(false, Ordering::Relaxed);
    worker.join().expect("progress logger thread should exit");

    assert!(
        start.elapsed() < Duration::from_secs(1),
        "progress logger must not add a full log-interval stall after chunk completion"
    );
}

#[test]
fn simulation_timeout_preserves_successful_compile_result() {
    let model_name = "Modelica.Mechanics.MultiBody.Examples.Loops.Engine1a";
    let temp = tempfile::tempdir().expect("temp dir");
    let mut partial = WorkerModelResult::phase_failure(model_name.to_string(), "Success", "", None);
    partial.error = None;
    partial.compile_seconds = Some(1.25);
    partial.flatten_seconds = Some(0.75);
    partial.dae_seconds = Some(0.20);
    rumoca_worker::write_model_worker_response_file(
        &temp.path().join(MODEL_WORKER_PARTIAL_RESULT_FILE),
        &rumoca_worker::ModelWorkerResponse {
            protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
            elapsed_secs: 1.25,
            result: partial,
        },
    )
    .expect("partial result should write");

    let result = simulation_timeout_model_result(
        model_name,
        temp.path(),
        21.0,
        10.0,
        Some(WorkerProgressPhase::IC),
    )
    .expect("IC timeout after compile should use partial compile result");

    assert_eq!(result.phase_reached, "Success");
    assert_eq!(result.error, None);
    assert_eq!(result.error_code, None);
    assert_eq!(
        result.sim_error_code.as_deref(),
        Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE)
    );
    assert_eq!(result.timeout_phase, Some(WorkerProgressPhase::IC));
    assert_eq!(result.timeout_seconds, Some(10.0));
    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    assert_eq!(result.ic_status.as_deref(), Some("ic_timeout"));
    assert_eq!(result.flatten_seconds, Some(0.75));
    assert_eq!(result.dae_seconds, Some(0.20));
}

/// The worker's typed classification has to survive the crossing into the
/// harness record: it is the only reason `msl_results.json` can be triaged
/// without regexing `sim_error`.
#[test]
fn worker_classification_crosses_into_the_harness_record() {
    let mut worker_row =
        WorkerModelResult::phase_failure("Modelica.A".to_string(), "Success", "", None);
    worker_row.error = None;
    worker_row.set_failure_classification(
        rumoca_worker::ModelFailureClassification::new(
            WorkerProgressPhase::Sim,
            rumoca_worker::ModelFailureBucket::RuntimeEventIteration,
        ),
        Some("EX002".to_string()),
    );
    let result = worker_model_result_to_msl(worker_row);
    assert_eq!(
        result.failure_bucket,
        Some(rumoca_worker::ModelFailureBucket::RuntimeEventIteration)
    );
    assert_eq!(
        result.owner_category,
        Some(rumoca_worker::ModelFailureOwner::Runtime)
    );
    assert_eq!(result.failure_phase, Some(WorkerProgressPhase::Sim));
    assert_eq!(result.failure_error_code.as_deref(), Some("EX002"));
}

/// A harness-side failure never carries the worker's classification, because
/// the worker was killed or never ran. The harness must mint one from the typed
/// outcome instead of leaving the record unclassifiable.
#[test]
fn harness_side_failures_are_classified_from_the_typed_outcome() {
    let result = model_worker_failure_result(
        "Modelica.A",
        MODEL_WORKER_ERROR_CODE,
        "model worker failed to start",
    );
    assert_eq!(
        result.failure_bucket,
        Some(rumoca_worker::ModelFailureBucket::HarnessFailure)
    );
    assert_eq!(
        result.owner_category,
        Some(rumoca_worker::ModelFailureOwner::Harness)
    );
    assert_eq!(
        result.failure_error_code.as_deref(),
        Some(MODEL_WORKER_ERROR_CODE)
    );
}

/// A timeout is a performance failure, not a compiler defect, and the phase it
/// stopped in is the observed one rather than a guess.
#[test]
fn timeout_classification_records_the_observed_phase_and_performance_owner() {
    let mut result = model_worker_failure_result("Modelica.A", MODEL_WORKER_ERROR_CODE, "timeout");
    set_harness_failure_classification(
        &mut result,
        Some(WorkerProgressPhase::Flatten),
        rumoca_worker::ModelFailureBucket::Timeout,
        MODEL_ATTEMPT_TIMEOUT_ERROR_CODE,
    );
    assert_eq!(
        result.failure_bucket,
        Some(rumoca_worker::ModelFailureBucket::Timeout)
    );
    assert_eq!(
        result.owner_category,
        Some(rumoca_worker::ModelFailureOwner::Performance)
    );
    assert_eq!(result.failure_phase, Some(WorkerProgressPhase::Flatten));
}

/// The in-process compile path classifies from the typed `FailedPhase`, so a
/// reworded compiler message cannot move a model between cohorts.
#[test]
fn in_process_compile_failures_are_classified_from_the_typed_phase() {
    let result = convert_phase_result(
        "Modelica.A".to_string(),
        PhaseResult::Failed {
            phase: FailedPhase::Flatten,
            error: "connect() of incompatible connectors".to_string(),
            error_code: Some("EF020".to_string()),
            diagnostics: Vec::new(),
        },
    );
    assert_eq!(
        result.failure_bucket,
        Some(rumoca_worker::ModelFailureBucket::Flatten)
    );
    assert_eq!(
        result.owner_category,
        Some(rumoca_worker::ModelFailureOwner::Flatten)
    );
    assert_eq!(result.failure_phase, Some(WorkerProgressPhase::Flatten));
    assert_eq!(result.failure_error_code.as_deref(), Some("EF020"));
}

#[test]
fn model_attempt_parent_grace_is_scoped_to_simulation() {
    assert_eq!(SIM_TIMEOUT_SECS, 12.0);
    assert_eq!(SIM_WORKER_TIMEOUT_GRACE_SECS, 2.0);
    assert_eq!(MODEL_ATTEMPT_TIMEOUT_SECS, 10.0);
    let timeouts = model_worker_phase_timeouts(MODEL_ATTEMPT_TIMEOUT_SECS, SIM_TIMEOUT_SECS);
    let elapsed_secs = 10.5;
    assert!(
        elapsed_secs
            >= timeouts.timeout_secs_for(Some(rumoca_worker::WorkerProgressPhase::Flatten))
    );
    assert_eq!(
        timeouts.timeout_secs_for(Some(rumoca_worker::WorkerProgressPhase::Sim)),
        14.0
    );
    assert!(
        elapsed_secs < timeouts.timeout_secs_for(Some(rumoca_worker::WorkerProgressPhase::Sim))
    );
}

#[test]
fn model_result_rejects_removed_timeout_recheck_schema() {
    let result = convert_phase_result(
        "Modelica.Blocks.Examples.PID_Controller".to_string(),
        PhaseResult::Failed {
            phase: FailedPhase::ToDae,
            error: "timed out".to_string(),
            error_code: Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE.to_string()),
            diagnostics: Vec::new(),
        },
    );
    let mut value = serde_json::to_value(result).expect("model result should serialize");
    value
        .as_object_mut()
        .expect("model result must serialize as an object")
        .insert("timeout_recheck".to_string(), serde_json::json!({}));

    assert!(
        serde_json::from_value::<MslModelResult>(value).is_err(),
        "removed retry schema must not be accepted as a compatibility field"
    );
}

#[test]
fn simulation_memory_limit_preserves_successful_compile_result() {
    let model_name = "Modelica.Electrical.Spice3.Examples.Spice3BenchmarkFourBitBinaryAdder";
    let temp = tempfile::tempdir().expect("temp dir");
    let mut partial = WorkerModelResult::phase_failure(model_name.to_string(), "Success", "", None);
    partial.error = None;
    partial.compile_seconds = Some(68.49);
    partial.flatten_seconds = Some(1.5);
    partial.dae_seconds = Some(2.0);
    rumoca_worker::write_model_worker_response_file(
        &temp.path().join(MODEL_WORKER_PARTIAL_RESULT_FILE),
        &rumoca_worker::ModelWorkerResponse {
            protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
            elapsed_secs: 68.49,
            result: partial,
        },
    )
    .expect("partial result should write");

    let result = simulation_memory_limit_model_result(
        model_name,
        temp.path(),
        341.0,
        6144,
        Some(WorkerProgressPhase::Solve),
    )
    .expect("Solve memory limit after compile should use partial compile result");

    assert_eq!(result.phase_reached, "Success");
    assert_eq!(result.error, None);
    assert_eq!(result.error_code, None);
    assert_eq!(result.timeout_phase, None);
    assert_eq!(result.timeout_seconds, None);
    assert_eq!(result.sim_status.as_deref(), Some("sim_solver_fail"));
    assert!(
        result
            .ir_solve_error
            .as_deref()
            .is_some_and(|error| error.contains("6144 MB resident-plus-swap limit"))
    );
    assert_eq!(result.flatten_seconds, Some(1.5));
    assert_eq!(result.dae_seconds, Some(2.0));
}

#[test]
fn compile_memory_token_limiter_releases_capacity_on_drop() {
    let limiter = std::sync::Arc::new(ResourceTokenLimiter::new(10));

    {
        let _permit = limiter.acquire(6);
        assert_eq!(limiter.available_for_tests(), 4);
    }

    assert_eq!(limiter.available_for_tests(), 10);
}

#[test]
fn compile_memory_token_limiter_caps_oversized_request_to_capacity() {
    let limiter = std::sync::Arc::new(ResourceTokenLimiter::new(10));

    {
        let _permit = limiter.acquire(64);
        assert_eq!(limiter.available_for_tests(), 0);
    }

    assert_eq!(limiter.available_for_tests(), 10);
}

#[test]
fn worker_threads_for_model_count_never_exceeds_model_count() {
    assert_eq!(worker_threads_for_model_count(14, 1), 1);
    assert_eq!(worker_threads_for_model_count(14, 3), 3);
    assert_eq!(worker_threads_for_model_count(2, 10), 2);
    assert_eq!(worker_threads_for_model_count(0, 1), 1);
}
