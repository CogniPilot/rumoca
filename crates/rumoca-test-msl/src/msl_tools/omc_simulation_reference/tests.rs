use super::*;

#[test]
fn load_simulation_targets_filters_explicit_success_non_partial() {
    let temp = tempfile::tempdir().expect("tempdir");
    let path = temp.path().join("msl_balance_results.json");
    let payload = json!({
        "model_results": [
            {"model_name":"Modelica.Blocks.Examples.PID_Controller", "phase_reached":"Success", "is_partial": false},
            {"model_name":"Modelica.Blocks.Examples.PartialThing", "phase_reached":"Success", "is_partial": true},
            {"model_name":"Modelica.Blocks.Logical.Not", "phase_reached":"Success", "is_partial": false},
            {"model_name":"Modelica.Fluid.Examples.PumpingSystem", "phase_reached":"Flatten", "is_partial": false},
            {"model_name":"Modelica.Electrical.Analog.Examples.HeatingRectifier", "phase_reached":"Success", "is_partial": false}
        ]
    });
    write_pretty_json(&path, &payload).expect("write payload");
    let names = load_simulation_targets(&path).expect("load simulation targets");
    assert_eq!(
        names,
        vec![
            "Modelica.Blocks.Examples.PID_Controller".to_string(),
            "Modelica.Electrical.Analog.Examples.HeatingRectifier".to_string()
        ]
    );
}

#[test]
fn select_models_preserves_generated_target_file_order() {
    let temp = tempfile::tempdir().expect("tempdir");
    let repo_root = temp.path().join("repo");
    let results_dir = repo_root.join("target/msl/results");
    let msl_dir = repo_root.join("target/msl/ModelicaStandardLibrary-4.1.0");
    std::fs::create_dir_all(&results_dir).expect("results dir");
    std::fs::create_dir_all(&msl_dir).expect("msl dir");
    let generated_targets = results_dir.join("msl_simulation_targets.json");
    write_pretty_json(
        &generated_targets,
        &json!({
            "model_names": [
                "Modelica.Electrical.Digital.Examples.DFFREGSRL",
                "Modelica.Blocks.Examples.BooleanNetwork1",
                "Modelica.Electrical.Digital.Examples.DFFREG"
            ]
        }),
    )
    .expect("write generated targets");

    let args = Args {
        dry_run: false,
        batch_size: 1,
        force: false,
        workers: 1,
        omc_threads: 1,
        batch_timeout_seconds: 30,
        stop_time: 1.0,
        use_experiment_stop_time: false,
        max_models: 0,
        model_regex: None,
        balance_results_file: None,
        results_dir: None,
        target_models_file: None,
        trace_exclusions_file: None,
        rumoca_sim_ok_only: false,
    };
    let paths = MslPaths {
        repo_root: repo_root.clone(),
        msl_dir,
        results_dir,
        flat_dir: repo_root.join("target/msl/results/omc_flat"),
        work_dir: repo_root.join("target/msl/results/omc_work"),
        sim_work_dir: repo_root.join("target/msl/results/omc_sim_work"),
        omc_trace_dir: repo_root.join("target/msl/results/sim_traces/omc"),
        rumoca_trace_dir: repo_root.join("target/msl/results/sim_traces/rumoca"),
    };

    let selection = select_models(&args, &paths).expect("select models");
    assert_eq!(
        selection.names,
        vec![
            "Modelica.Electrical.Digital.Examples.DFFREGSRL".to_string(),
            "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
            "Modelica.Electrical.Digital.Examples.DFFREG".to_string(),
        ]
    );
}

#[test]
fn merge_cached_results_for_resume_hydrates_missing_omc_timing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("omc_simulation_reference.json");
    let model_name = "Modelica.Blocks.Examples.PID_Controller";
    let payload = serde_json::json!({
        "models": {
            model_name: {
                "status": "success",
                "error": null,
                "sim_system_seconds": 0.25,
                "total_system_seconds": 0.5,
                "omc_wall_seconds": 0.75,
                "result_file": "Modelica.Blocks.Examples.PID_Controller_res.csv",
                "trace_file": "sim_traces/omc/Modelica.Blocks.Examples.PID_Controller.json",
                "trace_error": null,
                "rumoca_status": "sim_ok",
                "rumoca_sim_seconds": 0.4,
                "rumoca_sim_wall_seconds": 0.42,
                "rumoca_trace_file": "sim_traces/rumoca/Modelica.Blocks.Examples.PID_Controller.json",
                "rumoca_trace_error": null
            }
        }
    });
    std::fs::write(
        &path,
        serde_json::to_vec(&payload).expect("serialize payload"),
    )
    .expect("write payload");

    let mut all_results = BTreeMap::new();
    all_results.insert(
        model_name.to_string(),
        SimModelResult {
            status: "success".to_string(),
            error: None,
            sim_system_seconds: None,
            total_system_seconds: None,
            omc_wall_seconds: None,
            result_file: None,
            trace_file: None,
            trace_error: None,
            rumoca_status: None,
            rumoca_ic_status: None,
            rumoca_ic_error: None,
            rumoca_ic_seconds: None,
            rumoca_sim_seconds: None,
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: None,
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    );

    merge_cached_results_for_resume(&path, &[model_name.to_string()], &mut all_results)
        .expect("merge cached results");
    let hydrated = all_results.get(model_name).expect("missing hydrated model");
    assert_eq!(hydrated.sim_system_seconds, Some(0.25));
    assert_eq!(hydrated.total_system_seconds, Some(0.5));
    assert_eq!(hydrated.omc_wall_seconds, Some(0.75));
    assert_eq!(
        hydrated.result_file.as_deref(),
        Some("Modelica.Blocks.Examples.PID_Controller_res.csv")
    );
    assert_eq!(
        hydrated.trace_file.as_deref(),
        Some("sim_traces/omc/Modelica.Blocks.Examples.PID_Controller.json")
    );
}

#[test]
fn ensure_omc_trace_artifacts_regenerates_missing_json_from_cached_csv() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let omc_trace_dir = results_dir.join("sim_traces").join("omc");
    let sim_work_dir = results_dir.join("omc_sim_work");
    std::fs::create_dir_all(&omc_trace_dir).expect("trace dir");
    std::fs::create_dir_all(&sim_work_dir).expect("sim work dir");

    let model_name = "Modelica.Blocks.Examples.PID_Controller";
    let csv_path = sim_work_dir.join(format!("{model_name}_res.csv"));
    std::fs::write(&csv_path, "time,y\n0.0,1.0\n0.5,2.0\n1.0,3.0\n").expect("write csv");

    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: sim_work_dir.clone(),
        omc_trace_dir: omc_trace_dir.clone(),
        rumoca_trace_dir: results_dir.join("sim_traces").join("rumoca"),
    };

    let mut results = BTreeMap::new();
    results.insert(
        model_name.to_string(),
        SimModelResult {
            status: "success".to_string(),
            error: None,
            sim_system_seconds: Some(0.25),
            total_system_seconds: Some(0.5),
            omc_wall_seconds: Some(0.75),
            result_file: Some(format!("{model_name}_res.csv")),
            trace_file: Some(format!("sim_traces/omc/{model_name}.json")),
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: Some(0.01),
            rumoca_sim_seconds: Some(0.4),
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: Some(0.42),
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    );

    ensure_omc_trace_artifacts(&paths, &mut results);

    let refreshed = results.get(model_name).expect("refreshed result");
    assert_eq!(
        refreshed.trace_file.as_deref(),
        Some("sim_traces/omc/Modelica.Blocks.Examples.PID_Controller.json")
    );
    assert_eq!(refreshed.trace_error, None);

    let trace_path = omc_trace_dir.join(format!("{model_name}.json"));
    assert!(trace_path.is_file(), "missing regenerated trace json");
    let trace = load_trace_json(&trace_path).expect("load regenerated trace");
    assert_eq!(trace.times, vec![0.0, 0.5, 1.0]);
    assert_eq!(trace.names, vec!["y".to_string()]);
}

#[test]
fn ensure_omc_trace_artifacts_regenerates_missing_json_from_error_result_with_csv() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let omc_trace_dir = results_dir.join("sim_traces").join("omc");
    let sim_work_dir = results_dir.join("omc_sim_work");
    std::fs::create_dir_all(&omc_trace_dir).expect("trace dir");
    std::fs::create_dir_all(&sim_work_dir).expect("sim work dir");

    let model_name = "Modelica.Clocked.Examples.Elementary.BooleanSignals.TickBasedPulse";
    let csv_path = sim_work_dir.join(format!("{model_name}_res.csv"));
    std::fs::write(&csv_path, "time,y\n0.0,0.0\n0.5,1.0\n1.0,1.0\n").expect("write csv");

    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: sim_work_dir.clone(),
        omc_trace_dir: omc_trace_dir.clone(),
        rumoca_trace_dir: results_dir.join("sim_traces").join("rumoca"),
    };

    let mut results = BTreeMap::new();
    results.insert(
        model_name.to_string(),
        SimModelResult {
            status: "error".to_string(),
            error: Some("internal error".to_string()),
            sim_system_seconds: Some(0.25),
            total_system_seconds: Some(0.5),
            omc_wall_seconds: Some(0.75),
            result_file: Some(format!("{model_name}_res.csv")),
            trace_file: Some(format!("sim_traces/omc/{model_name}.json")),
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: Some(0.01),
            rumoca_sim_seconds: Some(0.4),
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: Some(0.42),
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    );

    ensure_omc_trace_artifacts(&paths, &mut results);

    let refreshed = results.get(model_name).expect("refreshed result");
    assert_eq!(
        refreshed.trace_file.as_deref(),
        Some(
            "sim_traces/omc/Modelica.Clocked.Examples.Elementary.BooleanSignals.TickBasedPulse.json"
        )
    );
    assert_eq!(refreshed.trace_error, None);
    let trace_path = omc_trace_dir.join(format!("{model_name}.json"));
    assert!(trace_path.is_file(), "missing regenerated trace json");
}

#[test]
fn cached_error_and_timeout_results_are_not_reusable() {
    let temp = tempfile::tempdir().expect("tempdir");
    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: temp.path().join("results"),
        flat_dir: temp.path().join("omc_flat"),
        work_dir: temp.path().join("omc_work"),
        sim_work_dir: temp.path().join("omc_sim_work"),
        omc_trace_dir: temp.path().join("sim_traces").join("omc"),
        rumoca_trace_dir: temp.path().join("sim_traces").join("rumoca"),
    };
    let model_name = "Modelica.Blocks.Examples.PID_Controller";
    for status in ["error", "timeout"] {
        let cached = SimModelResult {
            status: status.to_string(),
            error: Some("omc session port file did not appear".to_string()),
            ..empty_omc_result()
        };
        assert!(!cached_omc_result_is_reusable(&paths, model_name, &cached));
    }
}

#[test]
fn cached_success_without_materialized_trace_source_is_not_reusable() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let omc_trace_dir = results_dir.join("sim_traces").join("omc");
    let sim_work_dir = results_dir.join("omc_sim_work");
    std::fs::create_dir_all(&omc_trace_dir).expect("trace dir");
    std::fs::create_dir_all(&sim_work_dir).expect("sim work dir");

    let model_name = "Modelica.Blocks.Examples.PID_Controller";
    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir,
        flat_dir: temp.path().join("omc_flat"),
        work_dir: temp.path().join("omc_work"),
        sim_work_dir: sim_work_dir.clone(),
        omc_trace_dir: omc_trace_dir.clone(),
        rumoca_trace_dir: temp.path().join("sim_traces").join("rumoca"),
    };
    let stale_success = SimModelResult {
        status: "success".to_string(),
        error: None,
        sim_system_seconds: Some(0.25),
        total_system_seconds: Some(0.5),
        omc_wall_seconds: Some(0.75),
        result_file: Some(format!("{model_name}_res.csv")),
        trace_file: Some(format!("sim_traces/omc/{model_name}.json")),
        trace_error: None,
        rumoca_status: Some("sim_ok".to_string()),
        rumoca_ic_status: Some("ic_ok".to_string()),
        rumoca_ic_error: None,
        rumoca_ic_seconds: Some(0.01),
        rumoca_sim_seconds: Some(0.4),
        rumoca_sim_build_seconds: None,
        rumoca_sim_run_seconds: None,
        rumoca_sim_wall_seconds: Some(0.42),
        rumoca_trace_file: Some(format!("sim_traces/rumoca/{model_name}.json")),
        rumoca_trace_error: None,
    };
    assert!(!cached_omc_result_is_reusable(
        &paths,
        model_name,
        &stale_success
    ));

    std::fs::write(
        sim_work_dir.join(format!("{model_name}_res.csv")),
        "time,y\n0.0,1.0\n",
    )
    .expect("write csv");
    assert!(cached_omc_result_is_reusable(
        &paths,
        model_name,
        &stale_success
    ));

    std::fs::remove_file(sim_work_dir.join(format!("{model_name}_res.csv"))).expect("remove csv");
    write_pretty_json(
        &omc_trace_dir.join(format!("{model_name}.json")),
        &SimTrace {
            model_name: Some(model_name.to_string()),
            n_states: None,
            times: vec![0.0],
            names: vec!["y".to_string()],
            data: vec![vec![Some(1.0)]],
            variable_meta: None,
        },
    )
    .expect("write trace");
    assert!(cached_omc_result_is_reusable(
        &paths,
        model_name,
        &stale_success
    ));
}

#[test]
fn runtime_pair_rejects_invalid_values() {
    assert_eq!(runtime_pair(Some(1.0), Some(0.0)), None);
    assert_eq!(runtime_pair(Some(1.0), Some(-1.0)), None);
    assert_eq!(runtime_pair(Some(0.0), Some(1.0)), None);
    assert_eq!(runtime_pair(Some(-1.0), Some(1.0)), None);
    assert_eq!(runtime_pair(Some(f64::NAN), Some(1.0)), None);
    assert_eq!(runtime_pair(Some(1.0), Some(f64::INFINITY)), None);
    assert_eq!(runtime_pair(None, Some(1.0)), None);
    assert_eq!(runtime_pair(Some(1.0), None), None);
    assert_eq!(runtime_pair(Some(2.5), Some(5.0)), Some((5.0, 2.5)));
}

#[test]
fn compute_runtime_ratio_stats_reports_distribution() {
    let stats = compute_runtime_ratio_stats([(1.0, 2.0), (2.0, 2.0), (3.0, 2.0)].into_iter())
        .expect("ratio stats");
    assert_eq!(stats.sample_count, 3);
    assert!((stats.aggregate_ratio - 1.0).abs() < 1.0e-12);
    assert!((stats.min_ratio - 0.5).abs() < 1.0e-12);
    assert!((stats.max_ratio - 1.5).abs() < 1.0e-12);
    assert!((stats.mean_ratio - 1.0).abs() < 1.0e-12);
    assert!((stats.median_ratio - 1.0).abs() < 1.0e-12);

    let filtered = compute_runtime_ratio_stats(
        [(1.0, 2.0), (f64::INFINITY, 1.0), (2.0, 0.0), (4.0, 2.0)].into_iter(),
    )
    .expect("filtered stats");
    assert_eq!(filtered.sample_count, 2);
    assert!((filtered.aggregate_ratio - (5.0 / 4.0)).abs() < 1.0e-12);
    assert!((filtered.min_ratio - 0.5).abs() < 1.0e-12);
    assert!((filtered.max_ratio - 2.0).abs() < 1.0e-12);
}

#[test]
fn output_payload_keeps_empty_parity_diagnostics_when_omc_has_no_successes() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: results_dir.join("omc_sim_work"),
        omc_trace_dir: results_dir.join("sim_traces").join("omc"),
        rumoca_trace_dir: results_dir.join("sim_traces").join("rumoca"),
    };
    let args = Args {
        dry_run: false,
        batch_size: 1,
        force: false,
        workers: 1,
        omc_threads: 1,
        batch_timeout_seconds: 120,
        stop_time: 1.0,
        use_experiment_stop_time: true,
        max_models: 0,
        model_regex: None,
        balance_results_file: None,
        results_dir: None,
        target_models_file: None,
        trace_exclusions_file: None,
        rumoca_sim_ok_only: true,
    };
    let selection = ModelSelection {
        names: vec!["Modelica.Blocks.Examples.BooleanNetwork1".to_string()],
        source_file: temp.path().join("targets.json"),
        rule: "test".to_string(),
        selection_seconds: 0.0,
    };
    let context = FinalizeContext {
        omc_version: "OpenModelica 1.27.0~dev".to_string(),
        git_commit: "test".to_string(),
        workers: 1,
        total: 1,
        n_batches: 1,
        effective_batch_size: 1,
        elapsed_seconds: 120.0,
        cache_key: "test-key".to_string(),
    };
    let mut all_results = BTreeMap::new();
    all_results.insert(
        "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
        SimModelResult {
            status: "timeout".to_string(),
            error: Some("omc simulate exceeded 120s budget".to_string()),
            sim_system_seconds: None,
            total_system_seconds: None,
            omc_wall_seconds: Some(120.0),
            result_file: None,
            trace_file: None,
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: Some(0.1),
            rumoca_sim_seconds: Some(0.2),
            rumoca_sim_build_seconds: Some(0.05),
            rumoca_sim_run_seconds: Some(0.15),
            rumoca_sim_wall_seconds: Some(0.3),
            rumoca_trace_file: Some("sim_traces/rumoca/A.json".to_string()),
            rumoca_trace_error: None,
        },
    );
    let state = SimRunState {
        all_results,
        batch_timings: Vec::new(),
        pending_models: Vec::new(),
    };
    let metrics = compute_run_metrics(context.total, &state);
    let trace_summary = compute_trace_output_summary(&TraceQuantification::default());
    let payload = output::build_sim_output_payload(
        &args,
        &paths,
        &selection,
        &context,
        &metrics,
        &trace_summary,
        &state,
    );

    assert_eq!(payload["sim_successful"], 0);
    assert!(payload["runtime_comparison"]["ratio_stats"]["system_ratio_both_success"].is_null());
    assert!(payload["runtime_comparison"]["ratio_stats"]["wall_ratio_both_success"].is_null());
    assert_eq!(
        payload["runtime_comparison"]["diagnostics"]["both_success_model_count"],
        0
    );
    assert_eq!(
        payload["runtime_comparison"]["diagnostics"]["unavailable_reason"],
        "no_omc_rumoca_both_success_models"
    );
    assert_eq!(
        payload["runtime_comparison"]["diagnostics"]["runtime_ratio_available"],
        false
    );
}

#[test]
fn quantify_trace_differences_skips_excluded_model_before_trace_loading() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: results_dir.join("omc_sim_work"),
        omc_trace_dir: results_dir.join("sim_traces").join("omc"),
        rumoca_trace_dir: results_dir.join("sim_traces").join("rumoca"),
    };
    let model_name = "Modelica.Blocks.Examples.Noise.ImpureGenerator".to_string();
    let mut all_results = BTreeMap::new();
    all_results.insert(
        model_name.clone(),
        SimModelResult {
            status: "success".to_string(),
            error: None,
            sim_system_seconds: Some(0.1),
            total_system_seconds: Some(0.2),
            omc_wall_seconds: Some(0.21),
            result_file: None,
            trace_file: None,
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: Some(0.01),
            rumoca_sim_seconds: Some(0.1),
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: Some(0.11),
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    );
    let mut exclusions = BTreeMap::new();
    exclusions.insert(model_name.clone(), "stochastic".to_string());

    let report =
        quantify_trace_differences(&paths, &all_results, &exclusions).expect("quantify trace");

    assert!(report.models.is_empty());
    assert!(report.missing_trace.is_empty());
    assert_eq!(
        report.skipped.get(&model_name).map(String::as_str),
        Some("stochastic")
    );
}

#[test]
fn quantify_rejects_invalid_state_contract_before_no_common_variable_skip() {
    let (temp, paths, model_name) = trace_quantification_fixture("NoCommonVariables");
    write_pretty_json(
        &paths.rumoca_trace_dir.join(format!("{model_name}.json")),
        &invalid_state_contract_trace(&model_name, "x"),
    )
    .expect("write rumoca trace");
    write_pretty_json(
        &paths.omc_trace_dir.join(format!("{model_name}.json")),
        &json!({
            "model_name": model_name,
            "times": [0.0],
            "names": ["y"],
            "data": [[0.0]]
        }),
    )
    .expect("write omc trace");
    let results = trace_candidate_results(&model_name);

    let error = quantify_trace_differences(&paths, &results, &BTreeMap::new())
        .expect_err("invalid producer metadata must fail before numeric comparison skips");

    assert!(
        error
            .to_string()
            .contains("invalid state metadata contract")
    );
    drop(temp);
}

#[test]
fn quantify_rejects_invalid_state_contract_before_omc_load_skip() {
    let (temp, paths, model_name) = trace_quantification_fixture("MalformedOmcTrace");
    write_pretty_json(
        &paths.rumoca_trace_dir.join(format!("{model_name}.json")),
        &invalid_state_contract_trace(&model_name, "x"),
    )
    .expect("write rumoca trace");
    std::fs::write(
        paths.omc_trace_dir.join(format!("{model_name}.json")),
        "not json",
    )
    .expect("write malformed omc trace");
    let results = trace_candidate_results(&model_name);

    let error = quantify_trace_differences(&paths, &results, &BTreeMap::new())
        .expect_err("invalid producer metadata must fail before OMC trace loading skips");

    assert!(
        error
            .to_string()
            .contains("invalid state metadata contract")
    );
    drop(temp);
}

fn trace_quantification_fixture(name: &str) -> (tempfile::TempDir, MslPaths, String) {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let omc_trace_dir = results_dir.join("sim_traces/omc");
    let rumoca_trace_dir = results_dir.join("sim_traces/rumoca");
    std::fs::create_dir_all(&omc_trace_dir).expect("omc trace dir");
    std::fs::create_dir_all(&rumoca_trace_dir).expect("rumoca trace dir");
    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: results_dir.join("omc_sim_work"),
        omc_trace_dir,
        rumoca_trace_dir,
    };
    (temp, paths, format!("Modelica.Tests.{name}"))
}

fn invalid_state_contract_trace(model_name: &str, variable: &str) -> Value {
    json!({
        "model_name": model_name,
        "n_states": 2,
        "times": [0.0],
        "names": [variable],
        "data": [[0.0]],
        "variable_meta": [{"name": variable, "role": "state"}]
    })
}

fn trace_candidate_results(model_name: &str) -> BTreeMap<String, SimModelResult> {
    BTreeMap::from([(
        model_name.to_string(),
        SimModelResult {
            status: "success".to_string(),
            error: None,
            sim_system_seconds: None,
            total_system_seconds: None,
            omc_wall_seconds: None,
            result_file: None,
            trace_file: Some(format!("sim_traces/omc/{model_name}.json")),
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: None,
            rumoca_sim_seconds: None,
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: None,
            rumoca_trace_file: Some(format!("sim_traces/rumoca/{model_name}.json")),
            rumoca_trace_error: None,
        },
    )])
}

#[test]
fn quantify_trace_differences_includes_error_status_model_with_existing_traces() {
    let temp = tempfile::tempdir().expect("tempdir");
    let results_dir = temp.path().join("results");
    let omc_trace_dir = results_dir.join("sim_traces").join("omc");
    let rumoca_trace_dir = results_dir.join("sim_traces").join("rumoca");
    std::fs::create_dir_all(&omc_trace_dir).expect("omc trace dir");
    std::fs::create_dir_all(&rumoca_trace_dir).expect("rumoca trace dir");

    let paths = MslPaths {
        repo_root: temp.path().to_path_buf(),
        msl_dir: temp.path().join("msl"),
        results_dir: results_dir.clone(),
        flat_dir: results_dir.join("omc_flat"),
        work_dir: results_dir.join("omc_work"),
        sim_work_dir: results_dir.join("omc_sim_work"),
        omc_trace_dir: omc_trace_dir.clone(),
        rumoca_trace_dir: rumoca_trace_dir.clone(),
    };
    let model_name = "Modelica.Clocked.Examples.Elementary.RealSignals.TickBasedSine".to_string();
    let trace = SimTrace {
        model_name: Some(model_name.clone()),
        n_states: Some(0),
        times: vec![0.0, 0.5, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(1.0), Some(0.0)]],
        variable_meta: Some(Vec::new()),
    };
    write_pretty_json(&omc_trace_dir.join(format!("{model_name}.json")), &trace)
        .expect("write omc trace");
    write_pretty_json(&rumoca_trace_dir.join(format!("{model_name}.json")), &trace)
        .expect("write rumoca trace");

    let mut all_results = BTreeMap::new();
    all_results.insert(
        model_name.clone(),
        SimModelResult {
            status: "error".to_string(),
            error: Some("OMC internal error".to_string()),
            sim_system_seconds: Some(0.1),
            total_system_seconds: Some(0.2),
            omc_wall_seconds: Some(0.21),
            result_file: Some(format!("{model_name}_res.csv")),
            trace_file: Some(format!("sim_traces/omc/{model_name}.json")),
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: Some(0.01),
            rumoca_sim_seconds: Some(0.1),
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: Some(0.11),
            rumoca_trace_file: Some(format!("sim_traces/rumoca/{model_name}.json")),
            rumoca_trace_error: None,
        },
    );

    let report =
        quantify_trace_differences(&paths, &all_results, &BTreeMap::new()).expect("quantify");

    assert!(report.missing_trace.is_empty());
    assert!(report.skipped.is_empty());
    assert!(report.models.contains_key(&model_name));
}

#[test]
fn trace_output_summary_rolls_up_initial_condition_stats() {
    let rumoca = SimTrace {
        model_name: Some("M".to_string()),
        n_states: None,
        times: vec![0.0, 0.5, 1.0],
        names: vec!["x".to_string(), "y".to_string()],
        data: vec![
            vec![Some(1.0), Some(1.0), Some(1.0)],
            vec![Some(2.0), Some(2.0), Some(2.0)],
        ],
        variable_meta: None,
    };
    let omc = SimTrace {
        model_name: Some("M".to_string()),
        n_states: None,
        times: vec![0.0, 0.5, 1.0],
        names: vec!["x".to_string(), "y".to_string()],
        data: vec![
            vec![Some(0.0), Some(1.0), Some(1.0)],
            vec![Some(2.0), Some(2.0), Some(2.0)],
        ],
        variable_meta: None,
    };
    let metric = compare_model_traces("M", &rumoca, &omc).expect("compare traces");
    let mut report = TraceQuantification::default();
    report.models.insert(
        "M".to_string(),
        TraceModelMetric {
            metric,
            state_selection: None,
            rumoca_sim_wall_seconds: None,
            rumoca_sim_seconds: None,
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            omc_sim_system_seconds: None,
            omc_total_system_seconds: None,
            omc_wall_seconds: None,
        },
    );

    let summary = compute_trace_output_summary(&report);

    assert_eq!(summary.initial_condition.models_compared, 1);
    assert_eq!(summary.initial_condition.total_channels_compared, 2);
    assert_eq!(summary.initial_condition.deviation_channels_total, 1);
    assert!(summary.initial_condition.violation_mass_total > 0.0);
}

#[test]
fn load_trace_exclusions_reads_model_list() {
    let temp = tempfile::tempdir().expect("tempdir");
    let exclusions_file = temp.path().join("trace_exclusions.json");
    let payload = serde_json::json!([
        "Modelica.Blocks.Examples.Noise.ImpureGenerator",
        "Modelica.Math.Random.Examples.GenerateRandomNumbers"
    ]);
    std::fs::write(
        &exclusions_file,
        serde_json::to_vec(&payload).expect("serialize"),
    )
    .expect("write exclusions");
    let args = Args {
        dry_run: false,
        batch_size: 1,
        force: false,
        workers: 1,
        omc_threads: 1,
        batch_timeout_seconds: 1,
        stop_time: 1.0,
        use_experiment_stop_time: false,
        max_models: 0,
        model_regex: None,
        balance_results_file: None,
        results_dir: None,
        target_models_file: None,
        trace_exclusions_file: Some(exclusions_file),
        rumoca_sim_ok_only: false,
    };
    let paths = MslPaths::current();
    let exclusions = load_trace_exclusions(&args, &paths).expect("load exclusions");
    assert_eq!(exclusions.len(), 2);
    assert_eq!(
        exclusions.get("Modelica.Blocks.Examples.Noise.ImpureGenerator"),
        Some(&STOCHASTIC_TRACE_EXCLUSION_REASON.to_string())
    );
}

#[test]
fn select_omc_simulation_models_keeps_only_rumoca_trace_candidates_when_available() {
    let models = vec!["A".to_string(), "B".to_string(), "C".to_string()];
    let mut runtimes = HashMap::new();
    runtimes.insert(
        "A".to_string(),
        RumocaRuntime {
            status: "sim_ok".to_string(),
            ic_status: Some("ic_ok".to_string()),
            ic_error: None,
            ic_seconds: None,
            sim_seconds: None,
            sim_build_seconds: None,
            sim_run_seconds: None,
            sim_wall_seconds: None,
            trace_file: Some("sim_traces/rumoca/A.json".to_string()),
            trace_error: None,
            compile_seconds: None,
            scalar_equations: None,
            num_states: None,
        },
    );
    runtimes.insert(
        "B".to_string(),
        RumocaRuntime {
            status: "sim_solver_fail".to_string(),
            ic_status: Some("ic_ok".to_string()),
            ic_error: None,
            ic_seconds: None,
            sim_seconds: None,
            sim_build_seconds: None,
            sim_run_seconds: None,
            sim_wall_seconds: None,
            trace_file: None,
            trace_error: Some("solver failed".to_string()),
            compile_seconds: None,
            scalar_equations: None,
            num_states: None,
        },
    );
    runtimes.insert(
        "C".to_string(),
        RumocaRuntime {
            status: "sim_ok".to_string(),
            ic_status: Some("ic_ok".to_string()),
            ic_error: None,
            ic_seconds: None,
            sim_seconds: None,
            sim_build_seconds: None,
            sim_run_seconds: None,
            sim_wall_seconds: None,
            trace_file: None,
            trace_error: Some("missing trace".to_string()),
            compile_seconds: None,
            scalar_equations: None,
            num_states: None,
        },
    );

    let selected = select_omc_simulation_models(&models, &runtimes, true);

    assert_eq!(selected, vec!["A".to_string()]);
}

#[test]
fn select_omc_simulation_models_keeps_all_models_without_rumoca_runtime() {
    let models = vec!["A".to_string(), "B".to_string()];
    let selected = select_omc_simulation_models(&models, &HashMap::new(), true);

    assert_eq!(selected, models);
}

#[test]
fn select_omc_simulation_models_runs_all_targets_by_default() {
    let models = vec!["A".to_string(), "B".to_string()];
    let mut runtimes = HashMap::new();
    runtimes.insert(
        "A".to_string(),
        RumocaRuntime {
            status: "sim_ok".to_string(),
            ic_status: None,
            ic_error: None,
            ic_seconds: None,
            sim_seconds: None,
            sim_build_seconds: None,
            sim_run_seconds: None,
            sim_wall_seconds: None,
            trace_file: Some("sim_traces/rumoca/A.json".to_string()),
            trace_error: None,
            compile_seconds: None,
            scalar_equations: None,
            num_states: None,
        },
    );
    // B has no rumoca sim_ok trace, but the default (sim_ok_only=false) must
    // still include it so OMC has a baseline if B becomes sim_ok later.
    let selected = select_omc_simulation_models(&models, &runtimes, false);
    assert_eq!(selected, models);
}

#[test]
fn ensure_target_placeholders_preserves_full_target_denominator() {
    let mut all_results = BTreeMap::new();
    all_results.insert(
        "A".to_string(),
        SimModelResult {
            status: "success".to_string(),
            error: None,
            sim_system_seconds: None,
            total_system_seconds: None,
            omc_wall_seconds: None,
            result_file: None,
            trace_file: None,
            trace_error: None,
            rumoca_status: Some("sim_ok".to_string()),
            rumoca_ic_status: Some("ic_ok".to_string()),
            rumoca_ic_error: None,
            rumoca_ic_seconds: None,
            rumoca_sim_seconds: None,
            rumoca_sim_build_seconds: None,
            rumoca_sim_run_seconds: None,
            rumoca_sim_wall_seconds: None,
            rumoca_trace_file: Some("sim_traces/rumoca/A.json".to_string()),
            rumoca_trace_error: None,
        },
    );
    let mut runtimes = HashMap::new();
    runtimes.insert(
        "B".to_string(),
        RumocaRuntime {
            status: "sim_solver_fail".to_string(),
            ic_status: Some("ic_ok".to_string()),
            ic_error: None,
            ic_seconds: Some(0.1),
            sim_seconds: None,
            sim_build_seconds: None,
            sim_run_seconds: None,
            sim_wall_seconds: None,
            trace_file: None,
            trace_error: Some("solver failed".to_string()),
            compile_seconds: None,
            scalar_equations: None,
            num_states: None,
        },
    );
    let targets = vec!["A".to_string(), "B".to_string()];

    ensure_target_placeholders(&targets, &runtimes, &mut all_results);

    assert_eq!(all_results.len(), 2);
    assert_eq!(all_results["B"].status, "skipped");
    assert_eq!(
        all_results["B"].rumoca_status.as_deref(),
        Some("sim_solver_fail")
    );
    assert_eq!(all_results["B"].rumoca_ic_seconds, Some(0.1));
}
