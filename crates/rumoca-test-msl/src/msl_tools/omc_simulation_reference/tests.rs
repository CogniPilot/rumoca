use super::retry_policy::{OMC_FAILURE_RETRY_ATTEMPTS, OMC_TRANSIENT_FAILURE_RETRY_ATTEMPTS};
use super::*;
use rumoca_sim::sim_trace_compare::{TraceCertificationProfile, TraceRandomOpKind};

fn pointwise_candidate(profile: Option<TraceCertificationProfile>) -> SimTrace {
    SimTrace {
        model_name: Some("arbitrary.model.name".to_string()),
        times: vec![0.0, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(1.0)]],
        variable_meta: None,
        certification_profile: profile,
    }
}

#[test]
fn typed_stochastic_profile_is_separate_and_never_compared() {
    let profile = TraceCertificationProfile::stochastic(vec![TraceRandomOpKind::RandomResult]);
    let exit = pointwise_nonidentifiability_exit(&pointwise_candidate(Some(profile.clone())))
        .expect("valid profile")
        .expect("non-identifiable exit");
    assert_eq!(exit.kind, TraceExitKind::TraceNonidentifiable);
    assert_eq!(exit.certification_profile, Some(profile));
}

#[test]
fn malformed_profile_is_a_comparator_failure_not_an_exclusion() {
    let invalid = TraceCertificationProfile::stochastic(Vec::new());
    let exit = pointwise_nonidentifiability_exit(&pointwise_candidate(Some(invalid)))
        .expect_err("empty evidence must fail closed");
    assert_eq!(exit.kind, TraceExitKind::ComparatorFailed);
    assert!(exit.certification_profile.is_none());
}

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
            failed_attempts: 0,
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
            failed_attempts: 0,
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
fn ensure_omc_trace_artifacts_rejects_error_result_with_stale_csv() {
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
            failed_attempts: 0,
        },
    );

    ensure_omc_trace_artifacts(&paths, &mut results);

    let refreshed = results.get(model_name).expect("refreshed result");
    assert_eq!(refreshed.trace_file, None);
    assert_eq!(
        refreshed.trace_error.as_deref(),
        Some("OMC attempt status `error` is not eligible for trace provenance")
    );
    let trace_path = omc_trace_dir.join(format!("{model_name}.json"));
    assert!(
        !trace_path.exists(),
        "an unsuccessful OMC attempt must not materialize a comparable trace"
    );
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
        failed_attempts: 0,
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
            times: vec![0.0],
            names: vec!["y".to_string()],
            data: vec![vec![Some(1.0)]],
            variable_meta: None,
            certification_profile: None,
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
            failed_attempts: 0,
        },
    );
    let mut exclusions = BTreeMap::new();
    exclusions.insert(model_name.clone(), "stochastic".to_string());

    let report =
        quantify_trace_differences(&paths, &all_results, &exclusions).expect("quantify trace");

    assert!(report.models.is_empty());
    assert!(report.missing_trace.is_empty());
    assert_eq!(
        report.skipped.get(&model_name),
        Some(&TraceExitRecord::new(
            TraceExitKind::PolicyExcluded,
            "stochastic"
        )),
        "a policy exclusion must be recorded as one, so it is never read back as a \
         comparator failure"
    );
    let summary = compute_trace_output_summary(&report);
    assert_eq!(summary.skipped_models, 1);
    assert_eq!(summary.policy_excluded_models, 1);
    assert_eq!(summary.trace_nonidentifiable_models, 0);
}

#[test]
fn quantify_trace_differences_rejects_error_status_model_with_stale_traces() {
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
        times: vec![0.0, 0.5, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(1.0), Some(0.0)]],
        variable_meta: None,
        certification_profile: None,
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
            failed_attempts: 0,
        },
    );

    let report =
        quantify_trace_differences(&paths, &all_results, &BTreeMap::new()).expect("quantify");

    assert_eq!(
        report.missing_trace.get(&model_name),
        Some(&TraceExitRecord::new(
            TraceExitKind::OmcTraceMissing,
            "OMC attempt status `error` is not successful; stale trace artifacts are ineligible"
        )),
        "an OMC-side gap must be attributed to OMC, not to rumoca"
    );
    assert!(report.skipped.is_empty());
    assert!(!report.models.contains_key(&model_name));
}

#[test]
fn quantify_trace_differences_rejects_undeclared_omc_trace_file() {
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
    let model_name = "Modelica.Blocks.Examples.PID_Controller".to_string();
    let trace = SimTrace {
        model_name: Some(model_name.clone()),
        times: vec![0.0, 1.0],
        names: vec!["y".to_string()],
        data: vec![vec![Some(0.0), Some(1.0)]],
        variable_meta: None,
        certification_profile: None,
    };
    write_pretty_json(&omc_trace_dir.join(format!("{model_name}.json")), &trace)
        .expect("write undeclared omc trace");
    let rumoca_relative = format!("sim_traces/rumoca/{model_name}.json");
    write_pretty_json(&results_dir.join(&rumoca_relative), &trace)
        .expect("write declared rumoca trace");

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
            rumoca_trace_file: Some(rumoca_relative),
            rumoca_trace_error: None,
            failed_attempts: 0,
        },
    );

    let report =
        quantify_trace_differences(&paths, &all_results, &BTreeMap::new()).expect("quantify");

    assert_eq!(
        report.missing_trace.get(&model_name),
        Some(&TraceExitRecord::new(
            TraceExitKind::OmcTraceMissing,
            "successful OMC attempt did not declare a trace file"
        ))
    );
    assert!(report.models.is_empty());
}

#[test]
fn trace_output_summary_rolls_up_initial_condition_stats() {
    let rumoca = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 0.5, 1.0],
        names: vec!["x".to_string(), "y".to_string()],
        data: vec![
            vec![Some(1.0), Some(1.0), Some(1.0)],
            vec![Some(2.0), Some(2.0), Some(2.0)],
        ],
        variable_meta: None,
        certification_profile: None,
    };
    let omc = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 0.5, 1.0],
        names: vec!["x".to_string(), "y".to_string()],
        data: vec![
            vec![Some(0.0), Some(1.0), Some(1.0)],
            vec![Some(2.0), Some(2.0), Some(2.0)],
        ],
        variable_meta: None,
        certification_profile: None,
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
fn trace_output_summary_does_not_call_missing_initial_evidence_accurate() {
    let trace = |start| SimTrace {
        model_name: Some("M".to_string()),
        times: vec![start, 1.0],
        names: vec!["x".to_string()],
        data: vec![vec![Some(1.0), Some(1.0)]],
        variable_meta: None,
        certification_profile: None,
    };
    let metric =
        compare_model_traces("M", &trace(f64::from_bits(1)), &trace(0.0)).expect("compare traces");
    assert_eq!(metric.initial_condition.channels_compared, 0);
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

    assert_eq!(
        summary
            .initial_condition
            .models_with_unmeasured_initial_conditions,
        1
    );
    assert_eq!(
        summary
            .initial_condition
            .models_with_accurate_initial_conditions,
        0
    );
}

#[test]
fn load_trace_exclusions_reads_each_entrys_own_reason() {
    let temp = tempfile::tempdir().expect("tempdir");
    let exclusions_file = temp.path().join("trace_exclusions.json");
    let payload = serde_json::json!({
        "schema": "msl_trace_compare_exclusions",
        "exclusions": [
            {
                "model_name": "Modelica.Blocks.Examples.Noise.ImpureGenerator",
                "reason": "stochastic random-input model"
            },
            {
                "model_name": "Modelica.Math.Random.Examples.GenerateRandomNumbers",
                "reason": "wall-clock seeded generator"
            }
        ]
    });
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
        Some(&"stochastic random-input model".to_string()),
        "each entry keeps its own reason; one shared constant would attribute a false \
         rationale to every future exclusion"
    );
    assert_eq!(
        exclusions.get("Modelica.Math.Random.Examples.GenerateRandomNumbers"),
        Some(&"wall-clock seeded generator".to_string())
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
            failed_attempts: 0,
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

fn failure_result(status: &str, error: &str, failed_attempts: u32) -> SimModelResult {
    SimModelResult {
        status: status.to_string(),
        error: Some(error.to_string()),
        failed_attempts,
        ..empty_omc_result()
    }
}

/// A cached OMC failure must be retried until it reproduces: reusing a single
/// transient error permanently removed the model from the parity comparison.
#[test]
fn cached_error_result_is_retried_until_attempt_budget() {
    let temp = tempfile::tempdir().expect("tempdir");
    let paths = fixture_paths(temp.path());
    let model_name = "Modelica.Blocks.Examples.PID_Controller";

    for attempts in 0..OMC_FAILURE_RETRY_ATTEMPTS {
        let result = failure_result("error", "Simulation Failed: division by zero", attempts);
        assert!(
            !cached_omc_result_is_reusable(&paths, model_name, &result),
            "failure with {attempts} attempt(s) must still be retried"
        );
    }
    let settled = failure_result(
        "error",
        "Simulation Failed: division by zero",
        OMC_FAILURE_RETRY_ATTEMPTS,
    );
    assert!(cached_omc_result_is_reusable(&paths, model_name, &settled));
}

#[test]
fn transient_failure_text_gets_extended_retry_budget() {
    let temp = tempfile::tempdir().expect("tempdir");
    let paths = fixture_paths(temp.path());
    let model_name = "Modelica.Blocks.Examples.PID_Controller";

    for text in [
        "omc worker Killed",
        "out of memory while linking",
        "timed out",
    ] {
        let result = failure_result("error", text, OMC_FAILURE_RETRY_ATTEMPTS);
        assert!(
            !cached_omc_result_is_reusable(&paths, model_name, &result),
            "transient failure '{text}' must keep the larger retry budget"
        );
        let settled = failure_result("error", text, OMC_TRANSIENT_FAILURE_RETRY_ATTEMPTS);
        assert!(cached_omc_result_is_reusable(&paths, model_name, &settled));
    }

    // A timeout status is transient by construction, whatever the message says.
    let timeout = failure_result(
        "timeout",
        "omc simulate exceeded budget",
        OMC_FAILURE_RETRY_ATTEMPTS,
    );
    assert!(!cached_omc_result_is_reusable(&paths, model_name, &timeout));
}

/// The attempt counter must persist across runs or the retry loop never ends.
#[test]
fn failed_attempts_accumulate_across_runs_and_reset_on_success() {
    let mut fresh = failure_result("error", "Simulation Failed", 0);
    carry_failed_attempts(&mut fresh, None);
    assert_eq!(fresh.failed_attempts, 1);

    let prior = failure_result("error", "Simulation Failed", 3);
    let mut next = failure_result("timeout", "omc simulate exceeded budget", 0);
    carry_failed_attempts(&mut next, Some(&prior));
    assert_eq!(next.failed_attempts, 4);

    let mut recovered = SimModelResult {
        status: "success".to_string(),
        ..empty_omc_result()
    };
    carry_failed_attempts(&mut recovered, Some(&prior));
    assert_eq!(recovered.failed_attempts, 0);
}

/// Both selection branches must stay reachable: the CI lane filters to models
/// rumoca already simulates, and the nightly/local lane needs the full target
/// set so newly passing models already have an OMC baseline.
#[test]
fn select_omc_simulation_models_returns_all_targets_without_sim_ok_filter() {
    let model_names = vec![
        "Modelica.A".to_string(),
        "Modelica.B".to_string(),
        "Modelica.C".to_string(),
    ];
    let runtimes = runtime_fixture();

    assert_eq!(
        select_omc_simulation_models(&model_names, &runtimes, false),
        model_names
    );
    // An empty runtime map means the rumoca run produced nothing to filter on,
    // so the flag must not silently select zero models.
    assert_eq!(
        select_omc_simulation_models(&model_names, &HashMap::new(), true),
        model_names
    );
}

#[test]
fn select_omc_simulation_models_filters_to_trace_candidates_with_flag() {
    let model_names = vec![
        "Modelica.A".to_string(),
        "Modelica.B".to_string(),
        "Modelica.C".to_string(),
    ];
    let runtimes = runtime_fixture();

    assert_eq!(
        select_omc_simulation_models(&model_names, &runtimes, true),
        vec!["Modelica.A".to_string()]
    );
}

fn runtime_fixture() -> HashMap<String, RumocaRuntime> {
    let mut runtimes = HashMap::new();
    runtimes.insert(
        "Modelica.A".to_string(),
        RumocaRuntime {
            status: "sim_ok".to_string(),
            trace_file: Some("sim_traces/rumoca/Modelica.A.json".to_string()),
            ..RumocaRuntime::default()
        },
    );
    // sim_ok but no trace: not a comparison candidate.
    runtimes.insert(
        "Modelica.B".to_string(),
        RumocaRuntime {
            status: "sim_ok".to_string(),
            trace_file: None,
            ..RumocaRuntime::default()
        },
    );
    runtimes.insert(
        "Modelica.C".to_string(),
        RumocaRuntime {
            status: "sim_solver_fail".to_string(),
            trace_file: None,
            ..RumocaRuntime::default()
        },
    );
    runtimes
}

fn fixture_paths(root: &std::path::Path) -> MslPaths {
    MslPaths {
        repo_root: root.to_path_buf(),
        msl_dir: root.join("msl"),
        results_dir: root.join("results"),
        flat_dir: root.join("omc_flat"),
        work_dir: root.join("omc_work"),
        sim_work_dir: root.join("omc_sim_work"),
        omc_trace_dir: root.join("sim_traces").join("omc"),
        rumoca_trace_dir: root.join("sim_traces").join("rumoca"),
    }
}
