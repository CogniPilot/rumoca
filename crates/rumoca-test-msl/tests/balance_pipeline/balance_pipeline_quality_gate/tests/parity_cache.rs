use super::*;

fn valid_simulation_parity_payload() -> Value {
    json!({
        "total_models": 7,
        "runtime_comparison": { "ratio_stats": {
            "system_ratio_both_success": {
                "sample_count": 5,
                "min_ratio": 0.5,
                "median_ratio": 0.9,
                "mean_ratio": 1.0,
                "max_ratio": 1.3
            },
            "wall_ratio_both_success": {
                "sample_count": 5,
                "min_ratio": 0.4,
                "median_ratio": 0.8,
                "mean_ratio": 0.9,
                "max_ratio": 1.4
            }
        }},
        "trace_comparison": {
            "models_compared": 7,
            "missing_trace_models": 0,
            "skipped_models": 0,
            "agreement_high": 5,
            "agreement_minor": 1,
            "agreement_deviation": 1,
            "min_model_bounded_normalized_l1": 0.01,
            "median_model_bounded_normalized_l1": 0.02,
            "mean_model_bounded_normalized_l1": 0.03,
            "max_model_bounded_normalized_l1": 0.08,
            "state_selection": {
                "models_compared": 7,
                "exact_state_set_match_models": 7,
                "state_count_match_models": 7,
                "exact_state_set_match_percent": 100.0,
                "state_count_match_percent": 100.0,
                "total_rumoca_states": 7,
                "total_omc_states": 7,
                "total_matching_states": 7,
                "total_rumoca_only_states": 0,
                "total_omc_only_states": 0,
                "max_model_state_set_difference": 0
            }
        }
    })
}

#[test]
fn simulation_parity_cache_requires_runtime_and_trace_metrics() {
    fn write_payload(path: &Path, payload: &Value) {
        std::fs::write(
            path,
            serde_json::to_vec(payload).expect("serialize payload"),
        )
        .expect("write payload");
    }
    fn assert_cache_metric_check(path: &Path, payload: Value, expected: bool) {
        write_payload(path, &payload);
        let actual = simulation_parity_cache_has_required_metrics(path)
            .expect("check parity metrics payload");
        assert_eq!(actual, expected);
    }

    let dir = tempdir().expect("tempdir");
    let path = dir.path().join("omc_simulation_reference.json");

    let missing = json!({
        "runtime_comparison": { "ratio_stats": {
            "system_ratio_both_success": null,
            "wall_ratio_both_success": null
        }},
        "trace_comparison": { "models_compared": 0 }
    });
    assert_cache_metric_check(&path, missing, false);
    assert_cache_metric_check(&path, valid_simulation_parity_payload(), true);

    let mut assertion_failure = valid_simulation_parity_payload();
    assertion_failure["pipeline_progress"] = json!({ "omc_assertion_failure_models": 1 });
    assertion_failure["omc_assertion_failures"] = json!({
        "model_count": 1,
        "examples": [{
            "model_name": "ModelicaTest.AssertDemo",
            "assertions": ["assert | error | x > 0"]
        }]
    });
    assert_cache_metric_check(&path, assertion_failure, false);
}

#[test]
fn sanitize_simulation_parity_cache_payload_strips_rumoca_metrics() {
    let payload = json!({
        "runtime_comparison": {
            "ratio_stats": {
                "system_ratio_both_success": { "sample_count": 5 },
                "wall_ratio_both_success": { "sample_count": 5 }
            }
        },
        "trace_comparison": {
            "models_compared": 7
        },
        "target_selection": {
            "source_file": "/machine-specific/results/msl_simulation_targets.json",
            "rule": "explicit model list"
        },
        "models": {
            "A": {
                "status": "success",
                "trace_file": "sim_traces/omc/A.json",
                "result_file": "/machine-specific/results/omc_sim_work/A_res.csv",
                    "rumoca_status": "sim_ok",
                    "rumoca_ic_status": "ic_ok",
                    "rumoca_ic_seconds": 0.01,
                    "rumoca_sim_seconds": 1.0,
                "rumoca_sim_wall_seconds": 1.1,
                "rumoca_trace_file": "sim_traces/rumoca/A.json",
                "rumoca_trace_error": null
            }
        }
    });

    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    assert!(
        sanitized.get("runtime_comparison").is_none(),
        "simulation parity cache should not preserve runtime comparison stats"
    );
    assert!(
        sanitized.get("trace_comparison").is_none(),
        "simulation parity cache should not preserve trace comparison stats"
    );
    assert!(
        sanitized.get("target_selection").is_none(),
        "cache should not retain a machine-specific target-list path"
    );
    let model = sanitized
        .get("models")
        .and_then(Value::as_object)
        .and_then(|models| models.get("A"))
        .and_then(Value::as_object)
        .expect("sanitized cache should preserve OMC model entry");
    assert_eq!(model.get("status").and_then(Value::as_str), Some("success"));
    assert_eq!(
        model.get("trace_file").and_then(Value::as_str),
        Some("sim_traces/omc/A.json")
    );
    assert!(
        model.get("result_file").is_none(),
        "cache should not retain a disposable machine-specific result path"
    );
    assert!(
        model.get("rumoca_status").is_none(),
        "cache should strip Rumoca status"
    );
    assert!(
        model.get("rumoca_ic_status").is_none(),
        "cache should strip Rumoca IC status"
    );
    assert!(
        model.get("rumoca_sim_seconds").is_none(),
        "cache should strip Rumoca runtime"
    );
    assert!(
        model.get("rumoca_sim_wall_seconds").is_none(),
        "cache should strip Rumoca wall runtime"
    );
    assert!(
        model.get("rumoca_trace_file").is_none(),
        "cache should strip Rumoca trace file"
    );
    assert!(
        model.get("rumoca_trace_error").is_none(),
        "cache should strip Rumoca trace error"
    );
}

#[test]
fn materialize_simulation_parity_cache_entry_strips_stale_rumoca_metrics() {
    let temp = tempdir().expect("tempdir");
    let cache_path = temp.path().join("cache.json");
    let active_path = temp.path().join("active.json");
    let relative_trace = Path::new("sim_traces/omc/A.json");
    let cached_trace = cache_path.with_extension("traces").join(relative_trace);
    fs::create_dir_all(cached_trace.parent().expect("cached trace parent"))
        .expect("mkdir cached trace parent");
    fs::write(&cached_trace, b"cached OMC trace").expect("write cached OMC trace");
    fs::write(
        &cache_path,
        serde_json::to_vec_pretty(&json!({
            "cache_trace_blake3": {
                "sim_traces/omc/A.json": blake3::hash(b"cached OMC trace").to_hex().to_string()
            },
            "runtime_comparison": {
                "ratio_stats": {
                    "system_ratio_both_success": { "sample_count": 5 },
                    "wall_ratio_both_success": { "sample_count": 5 }
                }
            },
            "trace_comparison": {
                "models_compared": 7
            },
            "models": {
                "A": {
                    "status": "success",
                    "trace_file": "sim_traces/omc/A.json",
                    "rumoca_status": "sim_ok",
                    "rumoca_ic_status": "ic_ok",
                    "rumoca_trace_file": "sim_traces/rumoca/A.json"
                }
            }
        }))
        .expect("serialize cache payload"),
    )
    .expect("write cache payload");

    materialize_simulation_parity_cache_entry(&cache_path, &active_path)
        .expect("materialize sanitized cache");

    let active: Value = serde_json::from_slice(&fs::read(&active_path).expect("read active"))
        .expect("parse active payload");
    assert!(
        active.get("runtime_comparison").is_none(),
        "active simulation reference should not inherit cached runtime comparison"
    );
    assert!(
        active.get("trace_comparison").is_none(),
        "active simulation reference should not inherit cached trace comparison"
    );
    let model = active
        .get("models")
        .and_then(Value::as_object)
        .and_then(|models| models.get("A"))
        .and_then(Value::as_object)
        .expect("materialized active payload should preserve model entry");
    assert_eq!(model.get("status").and_then(Value::as_str), Some("success"));
    assert!(
        model.get("rumoca_status").is_none(),
        "active simulation reference should drop cached Rumoca status"
    );
    assert!(
        model.get("rumoca_ic_status").is_none(),
        "active simulation reference should drop cached Rumoca IC status"
    );
    assert!(
        model.get("rumoca_trace_file").is_none(),
        "active simulation reference should drop cached Rumoca trace path"
    );
}

#[test]
fn simulation_parity_cache_carries_omc_traces_between_results_runs() {
    let temp = tempdir().expect("tempdir");
    let first_results = temp.path().join("first-results");
    let second_results = temp.path().join("second-results");
    let active_path = first_results.join("omc_simulation_reference.json");
    let cache_path = temp.path().join("cache/simulation/key.json");
    let relative_trace = Path::new("sim_traces/omc/A.json");
    fs::create_dir_all(
        first_results
            .join(relative_trace)
            .parent()
            .expect("trace parent"),
    )
    .expect("mkdir trace parent");
    fs::write(first_results.join(relative_trace), b"omc trace bytes").expect("write OMC trace");
    fs::write(
        &active_path,
        serde_json::to_vec_pretty(&json!({
            "models": {
                "A": {
                    "status": "success",
                    "trace_file": "sim_traces/omc/A.json"
                }
            }
        }))
        .expect("serialize active payload"),
    )
    .expect("write active payload");

    persist_simulation_parity_cache_entry(&active_path, &cache_path)
        .expect("persist parity cache and OMC trace");
    let second_active_path = second_results.join("omc_simulation_reference.json");
    materialize_simulation_parity_cache_entry(&cache_path, &second_active_path)
        .expect("materialize parity cache and OMC trace");

    assert_eq!(
        fs::read(second_results.join(relative_trace)).expect("read restored OMC trace"),
        b"omc trace bytes"
    );
}

#[test]
fn simulation_parity_cache_rejects_missing_and_stale_traces() {
    let temp = tempdir().expect("tempdir");
    let results = temp.path().join("results");
    let active_path = results.join("omc_simulation_reference.json");
    let cache_path = temp.path().join("cache/simulation/key.json");
    let relative_trace = Path::new("sim_traces/omc/A.json");
    fs::create_dir_all(results.join(relative_trace).parent().expect("trace parent"))
        .expect("mkdir trace parent");
    fs::write(results.join(relative_trace), b"valid OMC trace").expect("write OMC trace");
    fs::write(
        &active_path,
        serde_json::to_vec_pretty(&json!({
            "msl_version": "4.1.0",
            "omc_version": "OpenModelica 1.26.1",
            "use_experiment_stop_time": true,
            "timing": { "batch_timeout_seconds": 600 },
            "models": {
                "A": {
                    "status": "success",
                    "trace_file": "sim_traces/omc/A.json"
                }
            }
        }))
        .expect("serialize active payload"),
    )
    .expect("write active payload");
    persist_simulation_parity_cache_entry(&active_path, &cache_path).expect("persist valid cache");
    let policy = SimulationParityCachePolicy {
        batch_timeout_seconds: 600,
        use_experiment_stop_time: true,
        stop_time_override: None,
    };
    let matches = || {
        simulation_parity_cache_matches(
            &cache_path,
            &["A".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            policy,
        )
        .expect("cache validation should complete")
    };
    assert!(matches(), "fresh cache trace should validate");

    let cached_trace = cache_path.with_extension("traces").join(relative_trace);
    fs::write(&cached_trace, b"stale OMC trace").expect("corrupt cached trace");
    assert!(!matches(), "digest mismatch must invalidate cache");
    fs::remove_file(&cached_trace).expect("remove cached trace");
    assert!(!matches(), "missing trace must invalidate cache");
}

#[test]
fn omc_parity_cache_is_shared_across_results_directories() {
    assert_eq!(
        omc_parity_cache_dir(),
        msl_cache_dir().join(OMC_PARITY_CACHE_DIR_REL)
    );
}

#[test]
fn parity_total_models_guard_checks_stale_and_matching_counts() {
    let path = PathBuf::from("/tmp/omc_simulation_reference.json");
    let stale = MslParityGateInput {
        total_models: Some(1),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    let err = validate_parity_total_models(&path, &stale, 180).expect_err("must fail stale count");
    assert!(
        err.to_string().contains("is stale"),
        "unexpected error: {err}"
    );
    let matching = MslParityGateInput {
        total_models: Some(180),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    validate_parity_total_models(&path, &matching, 180).expect("matching count should pass");
}

#[test]
fn parity_target_set_cache_key_is_order_insensitive() {
    let lhs = parity_target_set_cache_key(
        &["B".to_string(), "A".to_string()],
        "v4.1.0",
        "OpenModelica 1.26.1",
    );
    let rhs = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
    );
    assert_eq!(lhs, rhs, "cache key should ignore target order");
}

#[test]
fn parity_target_set_cache_key_changes_with_models_or_versions() {
    let base = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
    );
    let diff_models =
        parity_target_set_cache_key(&["A".to_string()], "4.1.0", "OpenModelica 1.26.1");
    let diff_msl = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.2.0",
        "OpenModelica 1.26.1",
    );
    let diff_omc = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.27.0",
    );
    assert_ne!(base, diff_models);
    assert_ne!(base, diff_msl);
    assert_ne!(base, diff_omc);
}

#[test]
fn simulation_parity_cache_key_changes_with_policy() {
    let base = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 600,
            use_experiment_stop_time: true,
            stop_time_override: None,
        },
    );
    let diff_timeout = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 900,
            use_experiment_stop_time: true,
            stop_time_override: None,
        },
    );
    let diff_override = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 600,
            use_experiment_stop_time: false,
            stop_time_override: Some(30.0),
        },
    );
    assert_ne!(base, diff_timeout);
    assert_ne!(base, diff_override);
}

#[test]
fn simulation_parity_cache_policy_uses_the_effective_omc_timeout() {
    assert_eq!(
        current_simulation_parity_cache_policy().batch_timeout_seconds,
        omc_sim_reference_timeout_secs()
    );
}

#[test]
fn simulation_parity_cache_matches_rejects_mismatched_policy() {
    let temp = tempdir().expect("tempdir");
    let path = temp.path().join("omc_simulation_reference.json");
    fs::write(
        &path,
        serde_json::to_vec_pretty(&json!({
            "msl_version": "4.1.0",
            "omc_version": "OpenModelica 1.26.1",
            "stop_time": 10.0,
            "use_experiment_stop_time": true,
            "timing": {
                "batch_timeout_seconds": 600
            },
            "models": {
                "A": { "status": "error" },
                "B": { "status": "error" }
            },
            "cache_trace_blake3": {}
        }))
        .expect("serialize cache payload"),
    )
    .expect("write cache payload");

    let matching = SimulationParityCachePolicy {
        batch_timeout_seconds: 600,
        use_experiment_stop_time: true,
        stop_time_override: None,
    };
    let mismatched_timeout = SimulationParityCachePolicy {
        batch_timeout_seconds: 900,
        ..matching
    };
    let mismatched_override = SimulationParityCachePolicy {
        batch_timeout_seconds: 600,
        use_experiment_stop_time: false,
        stop_time_override: Some(30.0),
    };
    assert!(
        simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            matching,
        )
        .expect("matching policy should parse"),
        "matching simulation policy should reuse cache entry"
    );
    assert!(
        !simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            mismatched_timeout,
        )
        .expect("mismatched timeout should parse"),
        "batch-timeout drift should invalidate cache entry"
    );
    assert!(
        !simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            mismatched_override,
        )
        .expect("mismatched override should parse"),
        "stop-time policy drift should invalidate cache entry"
    );
}
