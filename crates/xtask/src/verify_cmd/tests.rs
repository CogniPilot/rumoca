use super::{
    LocalMslRunPlan, MSL_FULL_TEST_FEATURE, MslCargoSetupTimingStep, MslCiEnvironment,
    MslHotspotModelResult, MslHotspotSummary, ParityConfigLock, VERIFY_SUITE_STEPS,
    VerifyMslParityArgs, VerifySuite, VerifyTimingReport, VerifyTimingStep,
    debug_msl_merge_test_command, hottest_compile_model, hottest_sim_model, local_msl_run_plan,
    msl_cache_layout_valid, prebuilt_sibling_binary, render_verify_timing_markdown,
    run_resource_monitor_loop, should_log_process_tables, write_msl_cargo_setup_timing_report,
    write_verify_timing_report,
};
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

fn step_argvs(suite: VerifySuite) -> Vec<Vec<&'static str>> {
    VERIFY_SUITE_STEPS
        .iter()
        .filter(|step| suite.includes(step))
        .map(|step| step.args.to_vec())
        .collect()
}

#[test]
fn parity_config_lock_serializes_fixed_path_harness_configuration() {
    let temp = tempfile::tempdir().expect("tempdir");
    let first = ParityConfigLock::acquire(temp.path()).expect("first lock");
    let root = temp.path().to_path_buf();
    let (started_tx, started_rx) = mpsc::channel();
    let (acquired_tx, acquired_rx) = mpsc::channel();
    let waiter = thread::spawn(move || {
        started_tx.send(()).expect("announce lock attempt");
        let second = ParityConfigLock::acquire(&root).expect("second lock");
        acquired_tx.send(()).expect("announce acquisition");
        drop(second);
    });

    started_rx.recv().expect("waiter started");
    assert_eq!(
        acquired_rx.recv_timeout(Duration::from_millis(100)),
        Err(mpsc::RecvTimeoutError::Timeout),
        "a concurrent parity invocation must wait while the config is live"
    );
    drop(first);
    acquired_rx
        .recv_timeout(Duration::from_secs(1))
        .expect("waiter acquires after the first invocation finishes");
    waiter.join().expect("waiter exits");
}

#[test]
fn quick_suite_runs_format_tests_architecture_and_msl_parity() {
    let steps = step_argvs(VerifySuite::Quick);
    assert_eq!(
        steps,
        vec![
            vec!["verify", "lint"],
            vec!["verify", "msl-parity"],
            vec!["verify", "architecture"],
            vec!["verify", "workspace"],
        ]
    );
    assert!(!steps.contains(&vec!["verify", "examples"]));
    assert!(!steps.contains(&vec!["verify", "binaries"]));
    assert!(!steps.contains(&vec!["verify", "template-runtimes"]));
    assert!(!steps.contains(&vec!["verify", "docs"]));
    assert!(!steps.contains(&vec!["vscode", "test"]));
    assert!(!steps.contains(&vec!["coverage", "run"]));
    assert!(!steps.contains(&vec!["playground", "test"]));
    assert!(!steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
}

#[test]
fn full_suite_runs_msl_parity_before_lower_signal_heavy_gates() {
    let steps = step_argvs(VerifySuite::Full);
    assert_eq!(steps.get(1), Some(&vec!["verify", "msl-parity"]));
    assert!(steps.contains(&vec!["verify", "architecture"]));
    assert!(steps.contains(&vec!["verify", "workspace"]));
    assert!(steps.contains(&vec!["verify", "examples"]));
    assert!(steps.contains(&vec!["verify", "binaries"]));
    assert!(steps.contains(&vec!["verify", "template-runtimes"]));
    assert!(steps.contains(&vec!["coverage", "run"]));
    assert!(steps.contains(&vec!["playground", "test"]));
    assert!(steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
    assert!(steps.contains(&vec!["verify", "msl-parity"]));
}

#[test]
fn focused_msl_match_does_not_imply_selected_target_success_gate() {
    let args = VerifyMslParityArgs {
        sim_match: vec!["Modelica.Blocks.Examples.BooleanNetwork1".to_string()],
        sim_match_exact: true,
        ..VerifyMslParityArgs::default()
    };
    let config = args.to_parity_config_json();

    assert!(config.get("require_selected_targets_success").is_none());
    assert_eq!(
        config
            .get("sim_match_exact")
            .and_then(serde_json::Value::as_bool),
        Some(true)
    );
    assert!(!args.requires_selected_targets_success());
    assert!(!args.uses_baseline_relative_quality_gate());
}

#[test]
fn explicit_selected_target_success_gate_is_forwarded() {
    let args = VerifyMslParityArgs {
        require_selected_targets_success: true,
        ..VerifyMslParityArgs::default()
    };
    let config = args.to_parity_config_json();

    assert_eq!(
        config
            .get("require_selected_targets_success")
            .and_then(serde_json::Value::as_bool),
        Some(true)
    );
    assert!(args.requires_selected_targets_success());
}

#[test]
fn msl_parity_config_forwards_model_worker_memory_ceiling() {
    let args = VerifyMslParityArgs {
        model_worker_memory_mb: Some(6144),
        ..VerifyMslParityArgs::default()
    };
    let config = args.to_parity_config_json();

    assert_eq!(
        config
            .get("model_worker_memory_mb")
            .and_then(serde_json::Value::as_u64),
        Some(6144)
    );
}

#[test]
fn verify_timing_markdown_preserves_step_order() {
    let report = VerifyTimingReport::new(
        VerifySuite::Quick,
        Duration::from_millis(1500),
        vec![
            VerifyTimingStep {
                label: "lint".to_string(),
                command: "cargo xtask verify lint".to_string(),
                status: "pass".to_string(),
                elapsed_seconds: 0.5,
            },
            VerifyTimingStep {
                label: "workspace tests".to_string(),
                command: "cargo xtask verify workspace".to_string(),
                status: "fail".to_string(),
                elapsed_seconds: 1.0,
            },
        ],
    );

    let markdown = render_verify_timing_markdown(&report);
    assert!(markdown.contains("# verify quick"));
    assert!(markdown.contains("- success: false"));
    assert!(
        markdown.find("| lint | pass | 0.500 |").unwrap()
            < markdown.find("| workspace tests | fail | 1.000 |").unwrap()
    );
}

#[test]
fn verify_timing_report_writes_fixed_target_artifacts() {
    let root = tempfile::tempdir().expect("temp root");
    let report = VerifyTimingReport::new(
        VerifySuite::Quick,
        Duration::from_secs(1),
        vec![VerifyTimingStep {
            label: "lint".to_string(),
            command: "cargo xtask verify lint".to_string(),
            status: "pass".to_string(),
            elapsed_seconds: 1.0,
        }],
    );

    write_verify_timing_report(root.path(), &report).expect("write timing report");

    let json_path = root.path().join("target/verify-timings/quick.json");
    let markdown_path = root.path().join("target/verify-timings/quick.md");
    assert!(json_path.is_file());
    assert!(markdown_path.is_file());
    let json = std::fs::read_to_string(json_path).expect("read timing json");
    assert!(json.contains(r#""suite": "verify quick""#));
    let markdown = std::fs::read_to_string(markdown_path).expect("read timing markdown");
    assert!(markdown.contains("| lint | pass | 1.000 |"));
}

#[test]
fn msl_cargo_setup_timing_report_writes_fixed_result_artifacts() {
    let root = tempfile::tempdir().expect("temp root");
    let results_dir = root.path().join("target/msl/results");
    let steps = vec![
        MslCargoSetupTimingStep {
            label: "build release MSL artifacts".to_string(),
            cargo_action: "build".to_string(),
            package: "rumoca-worker + rumoca-test-msl".to_string(),
            profile: "release".to_string(),
            features: vec![format!("rumoca-test-msl/{MSL_FULL_TEST_FEATURE}")],
            target_dir: root.path().join("target").display().to_string(),
            command: "\"cargo\" \"build\"".to_string(),
            status: "pass".to_string(),
            elapsed_seconds: 0.2,
        },
        MslCargoSetupTimingStep {
            label: "run release MSL test".to_string(),
            cargo_action: "run".to_string(),
            package: "rumoca-test-msl".to_string(),
            profile: "release".to_string(),
            features: vec!["msl-full-test".to_string()],
            target_dir: root.path().join("target").display().to_string(),
            command: "\"target/release/deps/msl_tests-abc\"".to_string(),
            status: "fail".to_string(),
            elapsed_seconds: 1.3,
        },
    ];

    write_msl_cargo_setup_timing_report(&results_dir, &steps)
        .expect("write MSL Cargo setup timing report");

    let json_path = results_dir.join("msl_cargo_setup_timing.json");
    let markdown_path = results_dir.join("msl_cargo_setup_timing.md");
    assert!(json_path.is_file());
    assert!(markdown_path.is_file());
    let json = std::fs::read_to_string(json_path).expect("read setup timing json");
    assert!(json.contains(r#""success": false"#));
    assert!(json.contains(r#""label": "build release MSL artifacts""#));
    assert!(json.contains(r#""package": "rumoca-worker + rumoca-test-msl""#));
    assert!(json.contains("rumoca-test-msl/msl-full-test"));
    assert!(json.contains(r#""features": ["#));
    let markdown = std::fs::read_to_string(markdown_path).expect("read setup timing markdown");
    assert!(markdown.contains("# MSL Cargo Setup Timing"));
    assert!(markdown.contains("| run release MSL test | fail | 1.300 | rumoca-test-msl |"));
    assert!(markdown.contains("| release | msl-full-test |"));
}

#[test]
fn prebuilt_sibling_binary_finds_tools_next_to_msl_tests() {
    let root = tempfile::tempdir().expect("tempdir");
    let bin_dir = root.path().join("bin");
    std::fs::create_dir_all(&bin_dir).expect("mkdir bin");
    let msl_tests = bin_dir.join("msl_tests");
    let tools = bin_dir.join("rumoca-msl-tools");
    std::fs::write(&msl_tests, "").expect("write msl_tests");
    std::fs::write(&tools, "").expect("write tools");

    assert_eq!(
        prebuilt_sibling_binary(&msl_tests, "rumoca-msl-tools"),
        Some(tools)
    );
}

#[test]
fn local_msl_run_plan_keeps_merge_and_release_paths_distinct() {
    assert_eq!(local_msl_run_plan(true), LocalMslRunPlan::MergeOnly);
    assert_eq!(local_msl_run_plan(false), LocalMslRunPlan::ReleaseArtifacts);

    let root = PathBuf::from("/workspace");
    let command = debug_msl_merge_test_command(&root, "suite::test_merge");
    assert_eq!(
        command
            .get_args()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect::<Vec<_>>(),
        [
            "test",
            "--verbose",
            "--package",
            "rumoca-test-msl",
            "--features",
            "msl-full-test",
            "--test",
            "msl_tests",
            "suite::test_merge",
            "--",
            "--nocapture",
        ]
    );
    assert_eq!(command.get_current_dir(), Some(root.as_path()));
}

#[test]
fn hotspot_selection_uses_max_compile_and_sim_wall_times() {
    let summary = MslHotspotSummary {
        model_results: vec![
            MslHotspotModelResult {
                model_name: "A".to_string(),
                compile_seconds: Some(1.5),
                sim_wall_seconds: Some(8.0),
            },
            MslHotspotModelResult {
                model_name: "B".to_string(),
                compile_seconds: Some(3.0),
                sim_wall_seconds: Some(2.0),
            },
            MslHotspotModelResult {
                model_name: "C".to_string(),
                compile_seconds: None,
                sim_wall_seconds: Some(9.0),
            },
        ],
    };

    assert_eq!(hottest_compile_model(&summary), Some(("B", 3.0)));
    assert_eq!(hottest_sim_model(&summary), Some(("C", 9.0)));
}

#[test]
fn msl_cache_layout_requires_editor_smoke_packages() {
    let temp = tempfile::tempdir().expect("tempdir");
    let msl_root = temp.path();
    std::fs::write(msl_root.join("Complex.mo"), "").expect("write Complex.mo");
    std::fs::create_dir_all(msl_root.join("Modelica 4.1.0")).expect("mkdir Modelica");
    std::fs::write(msl_root.join("Modelica 4.1.0/package.mo"), "").expect("write Modelica package");

    assert!(
        !msl_cache_layout_valid(msl_root),
        "ModelicaServices is required by editor MSL smoke asset preparation"
    );

    std::fs::create_dir_all(msl_root.join("ModelicaServices 4.1.0"))
        .expect("mkdir ModelicaServices");
    std::fs::write(msl_root.join("ModelicaServices 4.1.0/package.mo"), "")
        .expect("write ModelicaServices package");

    assert!(msl_cache_layout_valid(msl_root));
}

#[test]
fn msl_ci_environment_cleans_stale_results_before_run() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    std::fs::create_dir_all(&results_dir).expect("mkdir");
    std::fs::write(results_dir.join("stale.json"), "{}").expect("write stale file");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: results_dir.clone(),
        monitor_interval: None,
        clean_results: true,
        github_actions: false,
    };
    env.clean_stale_results().expect("cleanup should succeed");
    assert!(
        !results_dir.exists(),
        "pre-run cleanup should remove stale results directory"
    );
}

/// The preservation rules have to hold through the flag that actually
/// invokes them: a wipe that spared the cohort table in isolation but ran
/// unconditionally from `--clean-results` would still delete it on every run.
#[test]
fn msl_ci_environment_preserves_the_cohort_table_through_the_clean_results_flag() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    std::fs::create_dir_all(&results_dir).expect("mkdir");
    std::fs::write(
        results_dir.join("msl_band_table.json"),
        "{\"schema\":\"a\"}",
    )
    .expect("write band table");
    std::fs::write(results_dir.join("msl_results.json"), "{}").expect("write stale results");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: results_dir.clone(),
        monitor_interval: None,
        clean_results: true,
        github_actions: false,
    };

    env.clean_stale_results().expect("cleanup should succeed");

    assert!(
        results_dir.join("msl_band_table.json").is_file(),
        "the previous certification's cohort evidence must survive --clean-results"
    );
    assert!(
        !results_dir.join("msl_results.json").exists(),
        "everything the run regenerates must still be wiped"
    );
}

/// With the flag off, nothing is removed at all — the wipe is opt-in, so a
/// run that never asked for it cannot lose a certification.
#[test]
fn msl_ci_environment_removes_nothing_when_clean_results_is_off() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    std::fs::create_dir_all(&results_dir).expect("mkdir");
    std::fs::write(results_dir.join("msl_results.json"), "{}").expect("write results");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: results_dir.clone(),
        monitor_interval: None,
        clean_results: false,
        github_actions: false,
    };

    env.clean_stale_results().expect("cleanup should succeed");

    assert!(results_dir.join("msl_results.json").is_file());
}

#[test]
fn msl_resource_snapshot_skips_process_tables_on_github_actions() {
    let temp = tempfile::tempdir().expect("tempdir");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: temp.path().join("results"),
        monitor_interval: None,
        clean_results: false,
        github_actions: true,
    };

    assert!(!should_log_process_tables(&env));
}

#[test]
fn msl_resource_snapshot_keeps_process_tables_for_local_runs() {
    let temp = tempfile::tempdir().expect("tempdir");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: temp.path().join("results"),
        monitor_interval: None,
        clean_results: false,
        github_actions: false,
    };

    assert!(should_log_process_tables(&env));
}

#[test]
fn msl_resource_monitor_shutdown_interrupts_the_sampling_wait() {
    let temp = tempfile::tempdir().expect("tempdir");
    let env = MslCiEnvironment {
        root: PathBuf::from(temp.path()),
        results_dir: temp.path().join("results"),
        monitor_interval: Some(Duration::from_secs(30)),
        clean_results: false,
        github_actions: true,
    };
    let (stop, stop_receiver) = mpsc::channel();
    let worker = thread::spawn(move || {
        run_resource_monitor_loop(stop_receiver, Duration::from_secs(30), env);
    });
    let started = Instant::now();

    drop(stop);
    worker.join().expect("resource monitor should stop cleanly");

    assert!(
        started.elapsed() < Duration::from_secs(1),
        "monitor shutdown must not wait for the next sampling interval"
    );
}
