use super::*;

fn provenance(
    fresh: usize,
    cached: usize,
    affinity_requested: usize,
    affinity_applied: usize,
    affinity_failed: usize,
    load_before: Option<f64>,
    load_after: Option<f64>,
) -> MslWallTimeProvenance {
    MslWallTimeProvenance {
        omc_fresh_sample_count: fresh,
        omc_cached_sample_count: cached,
        affinity_requested_worker_count: affinity_requested,
        affinity_applied_worker_count: affinity_applied,
        affinity_failed_worker_count: affinity_failed,
        normalized_load_before: load_before,
        normalized_load_after: load_after,
        workers_used: affinity_requested,
        omc_threads: 1,
    }
}

fn parity_with_provenance(
    runtime_ratio_stats: MslRuntimeRatioStatsBaseline,
    wall_time_provenance: MslWallTimeProvenance,
) -> MslParityGateInput {
    MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: Some(MslParityRuntimeContext {
            workers_used: Some(wall_time_provenance.workers_used),
            omc_threads: Some(wall_time_provenance.omc_threads),
        }),
        runtime_ratio_stats: Some(runtime_ratio_stats),
        wall_time_provenance: Some(wall_time_provenance),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    }
}

fn baseline_with_runtime(
    system_median: f64,
    wall_median: f64,
    workers_used: usize,
    omc_threads: usize,
) -> MslQualityBaseline {
    MslQualityBaseline {
        runtime_context: Some(MslParityRuntimeContext {
            workers_used: Some(workers_used),
            omc_threads: Some(omc_threads),
        }),
        runtime_ratio_stats: Some(runtime_ratio_stats(system_median, wall_median)),
        ..baseline_quality_template()
    }
}

#[test]
fn cached_omc_wall_time_regression_is_advisory_but_system_regression_blocks() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = parity_with_provenance(
        runtime_ratio_stats(1.0, 0.5),
        provenance(0, 8, 2, 2, 0, Some(0.2), Some(0.3)),
    );
    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime system speedup median"))
    );
    assert!(
        !reasons
            .iter()
            .any(|reason| reason.contains("runtime wall speedup median"))
    );
    assert!(
        wall_time_trust_decision(&baseline, Some(&parity))
            .reasons
            .iter()
            .any(|reason| reason.contains("cached"))
    );
}

#[test]
fn trusted_wall_time_regression_remains_blocking() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 0.5),
        provenance(8, 0, 2, 2, 0, Some(0.5), Some(0.6)),
    );
    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime wall speedup median"))
    );
}

#[test]
fn affinity_failure_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 1.5),
        provenance(8, 0, 2, 1, 1, Some(0.5), Some(0.6)),
    );
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("affinity"))
    );
}

#[test]
fn excessive_host_load_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 1.5),
        provenance(8, 0, 2, 2, 0, Some(1.51), Some(0.6)),
    );
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("load"))
    );
}

#[test]
fn missing_host_load_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 1.5),
        provenance(8, 0, 2, 2, 0, None, Some(0.6)),
    );
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("missing load"))
    );
}

#[test]
fn missing_provenance_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: Some(MslParityRuntimeContext {
            workers_used: Some(2),
            omc_threads: Some(1),
        }),
        runtime_ratio_stats: Some(runtime_ratio_stats(2.0, 1.5)),
        wall_time_provenance: None,
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("missing provenance"))
    );
}

#[test]
fn malformed_provenance_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let dir = tempdir().expect("tempdir");
    let path = dir.path().join("omc_simulation_reference.json");
    let mut payload = valid_simulation_parity_payload();
    payload["runtime_comparison"]["wall_time_provenance"] = json!({
        "omc_fresh_sample_count": "not-a-count",
        "omc_cached_sample_count": 0
    });
    fs::write(
        &path,
        serde_json::to_vec_pretty(&payload).expect("serialize payload"),
    )
    .expect("write payload");
    let parity = load_msl_parity_gate_input(&path).expect("load parity payload");
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("missing provenance"))
    );
}

#[test]
fn mismatched_runtime_context_makes_wall_time_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let mut parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 1.5),
        provenance(8, 0, 2, 2, 0, Some(0.5), Some(0.6)),
    );
    parity.runtime_context = Some(MslParityRuntimeContext {
        workers_used: Some(4),
        omc_threads: Some(2),
    });
    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("runtime context"))
    );
}

#[test]
fn self_consistent_wall_time_context_mismatching_baseline_is_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let mut wall_provenance = provenance(8, 0, 4, 4, 0, Some(0.5), Some(0.6));
    wall_provenance.omc_threads = 2;
    let parity = parity_with_provenance(runtime_ratio_stats(2.0, 1.5), wall_provenance);

    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("baseline runtime context"))
    );
}

#[test]
fn wall_time_fresh_count_not_covering_compared_samples_is_advisory() {
    let baseline = baseline_with_runtime(2.0, 1.5, 2, 1);
    let mut runtime = runtime_ratio_stats(2.0, 1.5);
    runtime.wall_ratio_both_success.sample_count = 10;
    let parity = parity_with_provenance(runtime, provenance(1, 0, 2, 2, 0, Some(0.5), Some(0.6)));

    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("sample count mismatch"))
    );
}

#[test]
fn missing_baseline_wall_time_runtime_context_is_advisory() {
    let baseline = MslQualityBaseline {
        runtime_ratio_stats: Some(runtime_ratio_stats(2.0, 1.5)),
        ..baseline_quality_template()
    };
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 1.5),
        provenance(8, 0, 2, 2, 0, Some(0.5), Some(0.6)),
    );

    let decision = wall_time_trust_decision(&baseline, Some(&parity));
    assert!(!decision.trusted);
    assert!(
        decision
            .reasons
            .iter()
            .any(|reason| reason.contains("baseline runtime context missing"))
    );
}
