use super::gate_hole::parity_with;
use super::*;

fn timed_result(name: &str, sim_run_seconds: f64, ir_solve_seconds: f64) -> MslModelResult {
    let mut result = phase_error_result(name.to_string(), "Success", None, None);
    result.sim_status = Some("sim_ok".to_string());
    result.sim_run_seconds = Some(sim_run_seconds);
    result.ir_solve_seconds = Some(ir_solve_seconds);
    result
}

/// A strict-high completion above half of any phase budget is held back with
/// its worst phase; one within half of every budget is certified (SPEC_0050).
#[test]
fn timing_margin_holds_back_completions_above_half_a_phase_budget() {
    let mut summary = valid_summary_template();
    summary.model_results = vec![
        timed_result("Fast", 10.0, 2.0),
        timed_result("SlowSim", 36.0, 2.0),
        timed_result("SlowSolve", 1.0, 11.0),
        timed_result("AtMargin", 22.5, 10.0),
    ];
    let budgets = PhaseBudgets {
        model_attempt_seconds: 20.0,
        simulation_seconds: 45.0,
    };
    let held = timing_margin_models(
        &summary,
        ["Fast", "SlowSim", "SlowSolve", "AtMargin", "Unmeasured"],
        budgets,
    );
    assert_eq!(held.keys().collect::<Vec<_>>(), ["SlowSim", "SlowSolve"]);
    assert_eq!(held["SlowSim"].phase, "Sim");
    assert!((held["SlowSim"].budget_share - 0.8).abs() < 1e-12);
    assert_eq!(held["SlowSolve"].phase, "Solve");
}

/// The gate compares a run against the baseline's counts less the models the
/// timing margin held back, so losing exactly those models is no regression,
/// while losing a certified one still is.
#[test]
fn timing_margin_models_leave_the_trace_floors() {
    let mut baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    baseline.timing_margin_models.insert(
        "SlowSim".to_string(),
        TimingMarginEntry {
            phase: "Sim".to_string(),
            budget_share: 0.8,
        },
    );
    let parity_with = |agreement_high: usize| {
        let mut trace = trace_accuracy_baseline();
        trace.agreement_high = agreement_high;
        trace.models_compared = 10 - (8 - agreement_high);
        MslParityGateInput {
            total_models: Some(10),
            omc_version: Some("OpenModelica 1.26.1".to_string()),
            runtime_context: None,
            runtime_ratio_stats: None,
            runtime_model_ratios: IndexMap::new(),
            trace_accuracy_stats: Some(trace),
            omc_assertion_failure_models: 0,
            omc_assertion_failure_examples: Vec::new(),
        }
    };
    // allowed_drop is 1 over 10 models, so 6 is beyond the margin-held floor 7.
    for (agreement_high, regressed) in [(7, false), (6, true)] {
        let mut reasons = Vec::new();
        push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity_with(agreement_high)));
        assert_eq!(
            reasons
                .iter()
                .any(|reason| reason.contains("Trace strict-high pass count regressed")),
            regressed,
            "{agreement_high}: {reasons:?}"
        );
    }
}

/// A baseline may not both certify a model and hold it back, and its
/// certified and held-back identities together number its strict-high count.
#[test]
fn timing_margin_roster_is_disjoint_and_counted() {
    let mut baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    baseline.certified_strict_high_models = (0..7).map(|index| format!("M{index}")).collect();
    let entry = TimingMarginEntry {
        phase: "Sim".to_string(),
        budget_share: 0.9,
    };
    baseline
        .timing_margin_models
        .insert("Slow".to_string(), entry.clone());
    assert!(validate_certified_strict_high_roster(&baseline).is_ok());
    baseline
        .timing_margin_models
        .insert("M0".to_string(), entry);
    let error = validate_certified_strict_high_roster(&baseline).unwrap_err();
    assert!(error.to_string().contains("holds it back"), "{error}");
    baseline.timing_margin_models.shift_remove("M0");
    baseline.timing_margin_models.shift_remove("Slow");
    let error = validate_certified_strict_high_roster(&baseline).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("7 certified and 0 timing-margin"),
        "{error}"
    );
}

/// A run that loses exactly the held-back models to a timeout stays inside
/// the ratcheted completion and trace-accounting counts, while losing one
/// more is still a regression.
#[test]
fn timing_margin_models_leave_the_completion_and_accounting_floors() {
    let mut baseline = MslQualityBaseline {
        sim_ok: 8,
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    baseline.timing_margin_models.insert(
        "SlowSim".to_string(),
        TimingMarginEntry {
            phase: "Sim".to_string(),
            budget_share: 0.8,
        },
    );
    let allowed_drop = stage_count_allowed_drop(10);
    let sim_floor = 8 - 1 - allowed_drop;
    for (sim_ok, regressed) in [(sim_floor, false), (sim_floor - 1, true)] {
        let mut gate_input = gate_input_with_sim_rate(sim_ok, 10);
        gate_input.solve_models = baseline.solve_models;
        let notes = sim_completion_report_notes(gate_input, &baseline);
        assert_eq!(
            notes
                .iter()
                .any(|note| note.contains("Sim pass count regressed")),
            regressed,
            "{sim_ok}: {notes:?}"
        );
    }

    let accounted = trace_accounted_models(&trace_accuracy_baseline());
    let accounting_floor = accounted - 1 - TRACE_MODELS_COMPARED_ALLOWED_DROP;
    for (current, regressed) in [(accounting_floor, false), (accounting_floor - 1, true)] {
        let mut trace = trace_accuracy_baseline();
        trace.models_compared = current
            - trace_accounted_models(&MslTraceAccuracyStatsBaseline {
                models_compared: 0,
                ..trace_accuracy_baseline()
            });
        let mut reasons = Vec::new();
        push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity_with(trace)));
        assert_eq!(
            reasons
                .iter()
                .any(|reason| reason.contains("trace model accounting regressed")),
            regressed,
            "{current}: {reasons:?}"
        );
    }
}
