use super::*;

fn ci_phase_budgets() -> PhaseBudgets {
    PhaseBudgets::new(20.0, 45.0).expect("valid CI evidence envelope")
}

#[test]
fn quality_evidence_phase_budgets_record_actual_overrides() {
    let wire = current_msl_quality_snapshot_json(
        &valid_summary_template(),
        None,
        false,
        ci_phase_budgets(),
    )
    .expect("serialize budget-bound snapshot");
    assert_eq!(wire["model_attempt_timeout_seconds"], 20.0);
    assert_eq!(wire["sim_timeout_seconds"], 45.0);
    assert!(wire.get("phase_budgets").is_none());
    let replay: MslQualityBaseline = serde_json::from_value(wire).expect("replay budget evidence");
    assert_eq!(replay.phase_budgets, ci_phase_budgets());
}

#[test]
fn quality_evidence_phase_budgets_require_both_wire_fields() {
    for field in ["model_attempt_timeout_seconds", "sim_timeout_seconds"] {
        let mut wire = serde_json::to_value(baseline_quality_template()).expect("baseline wire");
        wire.as_object_mut().expect("baseline object").remove(field);
        let error = serde_json::from_value::<MslQualityBaseline>(wire)
            .expect_err("missing measurement envelope must fail closed");
        assert!(error.to_string().contains(field), "{error}");
    }
}

#[test]
fn quality_evidence_phase_budgets_reject_each_mismatch() {
    let baseline = ci_phase_budgets();
    assert_eq!(baseline.mismatch_reason(baseline), None);
    for (current, field) in [
        (
            PhaseBudgets::new(10.0, 45.0),
            "model_attempt_timeout_seconds",
        ),
        (PhaseBudgets::new(20.0, 12.0), "sim_timeout_seconds"),
        (
            PhaseBudgets::new(20.0, 45.0 + 1.0e-13),
            "sim_timeout_seconds",
        ),
    ] {
        let reason = baseline
            .mismatch_reason(current.expect("valid alternative envelope"))
            .expect("different envelope");
        assert!(reason.contains(field), "{reason}");
    }
}

#[test]
fn quality_evidence_phase_budgets_reject_invalid_construction_and_wire() {
    for invalid in [0.0, -1.0, f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(PhaseBudgets::new(invalid, 45.0).is_err());
        assert!(PhaseBudgets::new(20.0, invalid).is_err());
    }
    for field in ["model_attempt_timeout_seconds", "sim_timeout_seconds"] {
        for invalid in [0.0, -1.0, f64::INFINITY] {
            let mut wire =
                serde_json::to_value(baseline_quality_template()).expect("baseline wire");
            wire[field] = serde_json::json!(invalid);
            assert!(serde_json::from_value::<MslQualityBaseline>(wire).is_err());
        }
    }
}

#[test]
fn quality_evidence_phase_budgets_context_uses_configured_envelope() {
    let attempt = model_attempt_timeout_secs();
    let simulation = sim_timeout_secs();
    for (budgets, field) in [
        (
            PhaseBudgets::new(attempt + 1.0, simulation),
            "model_attempt_timeout_seconds",
        ),
        (
            PhaseBudgets::new(attempt, simulation + 1.0),
            "sim_timeout_seconds",
        ),
    ] {
        let mut baseline = baseline_quality_template();
        baseline.phase_budgets = budgets.expect("valid alternative envelope");
        let reason =
            msl_quality_context_mismatch_reason(gate_input_with_sim_rate(8, 10), &baseline, None)
                .expect("actual configured envelope mismatch");
        assert!(reason.contains(field), "{reason}");
    }
}
