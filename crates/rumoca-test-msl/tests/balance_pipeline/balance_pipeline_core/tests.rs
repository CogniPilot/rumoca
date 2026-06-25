use super::*;
use rumoca_compile::compile::{CompilationResult, CompilationSummary};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use std::sync::Mutex;

struct FakeFocusedCompiler {
    uncached_called: Mutex<Vec<String>>,
}

impl FocusedClosureCompiler for FakeFocusedCompiler {
    fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport {
        self.uncached_called
            .lock()
            .expect("uncached call log should not be poisoned")
            .push(model_name.to_string());
        StrictCompileReport {
            requested_model: model_name.to_string(),
            requested_result: None,
            summary: CompilationSummary::default(),
            failures: Vec::new(),
            source_map: None,
        }
    }

    fn strict_compile_dae_for_focused_model(
        &self,
        model_name: &str,
    ) -> std::result::Result<Box<rumoca_compile::compile::DaeCompilationResult>, String> {
        self.uncached_called
            .lock()
            .expect("uncached call log should not be poisoned")
            .push(model_name.to_string());
        Err(format!("focused DAE compile failed for {model_name}"))
    }
}

fn empty_compilation_result() -> CompilationResult {
    let dae = dae::Dae::default();
    let balance_detail =
        rumoca_phase_dae::balance::balance_detail(&dae).expect("empty DAE metadata is valid");
    CompilationResult {
        flat: flat::Model::default(),
        dae,
        balance_detail,
        experiment_start_time: None,
        experiment_stop_time: None,
        experiment_tolerance: None,
        experiment_interval: None,
        rumoca_solver_fixed_step: None,
        experiment_solver: None,
    }
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
fn finalize_compile_entry_preserves_compile_that_exceeds_one_stage_budget() {
    let entry = finalize_compile_entry(
        "Modelica.Blocks.Examples.PID_Controller",
        ModelCompileOutcome::StrictReport(Box::new(StrictCompileReport {
            requested_model: "Modelica.Blocks.Examples.PID_Controller".to_string(),
            requested_result: None,
            summary: CompilationSummary::default(),
            failures: Vec::new(),
            source_map: None,
        })),
        12.5,
        10.0,
        None,
    );

    assert!(entry.remaining_budget_secs.is_none());
    match entry.compile_outcome {
        ModelCompileOutcome::StrictReport(report) => {
            assert_eq!(
                report.requested_model,
                "Modelica.Blocks.Examples.PID_Controller"
            );
        }
        _ => panic!("expected strict report"),
    }
}

#[test]
fn finalize_compile_entry_preserves_under_budget_compile_outcome() {
    let entry = finalize_compile_entry(
        "Modelica.Blocks.Examples.PID_Controller",
        ModelCompileOutcome::StrictReport(Box::new(StrictCompileReport {
            requested_model: "Modelica.Blocks.Examples.PID_Controller".to_string(),
            requested_result: None,
            summary: CompilationSummary::default(),
            failures: Vec::new(),
            source_map: None,
        })),
        2.5,
        10.0,
        None,
    );

    assert!(entry.remaining_budget_secs.is_none());
    match entry.compile_outcome {
        ModelCompileOutcome::StrictReport(report) => {
            assert_eq!(
                report.requested_model,
                "Modelica.Blocks.Examples.PID_Controller"
            );
        }
        _ => panic!("expected strict report"),
    }
}

#[test]
fn finalize_compile_entry_preserves_full_sim_timeout_after_successful_compile() {
    let entry = finalize_compile_entry(
        "Modelica.Blocks.Examples.PID_Controller",
        ModelCompileOutcome::Phase(PhaseResult::Success(Box::new(empty_compilation_result()))),
        2.5,
        10.0,
        None,
    );

    assert_eq!(entry.remaining_budget_secs, Some(10.0));
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
    assert_eq!(result.timeout_phase, Some(WorkerProgressPhase::IC));
    assert_eq!(result.timeout_seconds, Some(10.0));
    assert_eq!(result.sim_status.as_deref(), Some("sim_timeout"));
    assert_eq!(result.ic_status.as_deref(), Some("ic_timeout"));
    assert_eq!(result.flatten_seconds, Some(0.75));
    assert_eq!(result.dae_seconds, Some(0.20));
}

#[test]
fn compile_perf_retention_keeps_slow_profiles_and_deletes_fast_successes() {
    let temp = tempfile::tempdir().expect("temp dir");
    let slow_profile = temp.path().join("slow.perf.data");
    fs::write(&slow_profile, b"perf").expect("write slow profile");
    let slow_artifacts = CompilePerfArtifacts {
        profile_path: slow_profile.clone(),
        relative_path: "perf/compile/slow.perf.data".to_string(),
    };
    let success =
        ModelCompileOutcome::Phase(PhaseResult::Success(Box::new(empty_compilation_result())));

    let retained = retain_compile_perf_profile(Some(&slow_artifacts), 6.0, &success);

    assert_eq!(retained.as_deref(), Some("perf/compile/slow.perf.data"));
    assert!(slow_profile.is_file());

    let fast_profile = temp.path().join("fast.perf.data");
    fs::write(&fast_profile, b"perf").expect("write fast profile");
    let fast_artifacts = CompilePerfArtifacts {
        profile_path: fast_profile.clone(),
        relative_path: "perf/compile/fast.perf.data".to_string(),
    };

    let retained = retain_compile_perf_profile(Some(&fast_artifacts), 0.1, &success);

    assert_eq!(retained, None);
    assert!(!fast_profile.exists());
}

#[test]
fn compile_model_with_budget_timeout_uses_focused_uncached_compile_path() {
    let compiler = std::sync::Arc::new(FakeFocusedCompiler {
        uncached_called: Mutex::new(Vec::new()),
    });

    let entry = compile_model_with_budget_timeout(
        &compiler,
        "Modelica.Electrical.Digital.Examples.DFFREG",
        10.0,
        None,
        None,
    );

    assert!(entry.remaining_budget_secs.is_none());
    let calls = compiler
        .uncached_called
        .lock()
        .expect("uncached call log should not be poisoned");
    assert_eq!(
        calls.as_slice(),
        &["Modelica.Electrical.Digital.Examples.DFFREG".to_string()]
    );
}

#[test]
fn stream_compile_chunk_with_model_budgets_reports_all_indices() {
    let compiler = std::sync::Arc::new(FakeFocusedCompiler {
        uncached_called: Mutex::new(Vec::new()),
    });
    let names = vec![
        "Modelica.Blocks.Examples.PID_Controller".to_string(),
        "Modelica.Electrical.Digital.Examples.DFFREG".to_string(),
        "Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum".to_string(),
    ];
    let mut received = Vec::new();

    stream_compile_chunk_with_model_budgets(&compiler, &names, 2, 10.0, |idx, entry| {
        received.push((idx, entry.model_name));
    });

    received.sort_by_key(|(idx, _)| *idx);
    let received_names: Vec<String> = received.into_iter().map(|(_, name)| name).collect();
    assert_eq!(received_names, names);
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

#[test]
fn slow_compile_log_threshold_parses_positive_numbers_only() {
    assert_eq!(slow_compile_log_threshold_secs_from_override(None), None);
    assert_eq!(
        slow_compile_log_threshold_secs_from_override(Some("")),
        None
    );
    assert_eq!(
        slow_compile_log_threshold_secs_from_override(Some("0")),
        None
    );
    assert_eq!(
        slow_compile_log_threshold_secs_from_override(Some("-1")),
        None
    );
    assert_eq!(
        slow_compile_log_threshold_secs_from_override(Some("12.5")),
        Some(12.5)
    );
}
