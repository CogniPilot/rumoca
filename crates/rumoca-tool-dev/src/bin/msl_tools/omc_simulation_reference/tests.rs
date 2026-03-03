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
fn parse_sim_results_success_and_failure() {
    let success_entry = "MODEL:Modelica.Blocks.Examples.PID_Controller\nSIM_TIME:0.123\nTOTAL_TIME:0.456\nRESULT_FILE:Modelica.Blocks.Examples.PID_Controller_res.csv\nERROR:\n---\n";
    let success = parse_sim_entry(success_entry).expect("success entry");
    assert_eq!(success.1.status, "success");
    assert_eq!(success.1.sim_system_seconds, Some(0.123));
    assert_eq!(success.1.total_system_seconds, Some(0.456));

    let failure_entry = "MODEL:Modelica.Blocks.Examples.PID_Controller\nRESULT_FILE:Modelica.Blocks.Examples.PID_Controller_res.csv\nERROR:[/tmp/f.mo:1:1-1:2:writable] Error: Simulation failed.\n---\n";
    let failure = parse_sim_entry(failure_entry).expect("failure entry");
    assert_eq!(failure.1.status, "error");
    assert!(
        failure
            .1
            .error
            .as_deref()
            .unwrap_or_default()
            .contains("Simulation failed")
    );
}

#[test]
fn parse_omc_simulation_records_extracts_model_timings() {
    let output = r#"
record SimulationResult
resultFile = "/tmp/Modelica.Blocks.Examples.BooleanNetwork1_res.csv",
timeSimulation = 0.021,
timeTotal = 0.840
end SimulationResult;
"#;
    let records = parse_omc_simulation_records(output);
    let record = records
        .get("Modelica.Blocks.Examples.BooleanNetwork1")
        .expect("missing record");
    assert_eq!(
        record.get("result_file"),
        Some(&"/tmp/Modelica.Blocks.Examples.BooleanNetwork1_res.csv".to_string())
    );
    assert_eq!(record.get("sim_system_seconds"), Some(&"0.021".to_string()));
    assert_eq!(
        record.get("total_system_seconds"),
        Some(&"0.840".to_string())
    );
}

#[test]
fn generate_sim_script_writes_model_result_and_error_block() {
    let paths = MslPaths::current();
    let script = generate_sim_script(
        &paths,
        0,
        &[String::from("Modelica.Blocks.Examples.PID_Controller")],
        1.0,
        false,
    );
    assert!(script.contains("MODEL:Modelica.Blocks.Examples.PID_Controller"));
    assert!(script.contains("RESULT_FILE:Modelica.Blocks.Examples.PID_Controller_res.csv"));
    assert!(script.contains("ERROR:\" + err"));
}

#[test]
fn ensure_runtime_ratio_stats_present_detects_missing_stats() {
    let mut all_results = BTreeMap::new();
    all_results.insert(
        "Modelica.Blocks.Examples.PID_Controller".to_string(),
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
            rumoca_sim_seconds: Some(1.0),
            rumoca_sim_wall_seconds: Some(1.0),
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    );
    let state = SimRunState {
        all_results,
        batch_timings: Vec::new(),
        pending_batches: Vec::new(),
    };
    let metrics = compute_run_metrics(1, &state);
    let error = ensure_runtime_ratio_stats_present(&metrics, both_success_model_count(&state))
        .expect_err("missing OMC timing should fail runtime ratio check");
    assert!(
        error
            .to_string()
            .contains("missing runtime ratio stats for 1 both-success model(s)")
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
            rumoca_sim_seconds: None,
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
            rumoca_sim_seconds: Some(0.1),
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
        resume: false,
        workers: 1,
        omc_threads: 1,
        batch_timeout_seconds: 1,
        stop_time: 1.0,
        use_experiment_stop_time: false,
        max_models: 0,
        balance_results_file: None,
        target_models_file: None,
        trace_exclusions_file: Some(exclusions_file),
    };
    let paths = MslPaths::current();
    let exclusions = load_trace_exclusions(&args, &paths).expect("load exclusions");
    assert_eq!(exclusions.len(), 2);
    assert_eq!(
        exclusions.get("Modelica.Blocks.Examples.Noise.ImpureGenerator"),
        Some(&STOCHASTIC_TRACE_EXCLUSION_REASON.to_string())
    );
}
