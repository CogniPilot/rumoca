//! Negative evidence for the closed comparator gate hole.
//!
//! Each test here pins one state that used to be representable and silent:
//! a Tier 2 run with no comparator reading that still produced a verdict, a
//! `sim_ok` movement that held the ratchet, and a strict-high collapse that no
//! gate looked at. The wrong states are unrepresentable now; these tests are
//! the proof, and they fail if any of them comes back.

use super::*;

fn trace_stats(models_compared: usize, high: usize, minor: usize) -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        models_compared,
        agreement_high: high,
        agreement_high_percent: None,
        agreement_minor: minor,
        agreement_minor_percent: None,
        agreement_deviation: models_compared.saturating_sub(high + minor),
        agreement_deviation_percent: None,
        ..trace_accuracy_baseline()
    }
}

fn parity_with(trace: MslTraceAccuracyStatsBaseline) -> MslParityGateInput {
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
}

#[test]
fn unmeasured_parity_is_a_gate_failure_on_a_baseline_relative_run() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let measurement = MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::OmcUnavailable {
        detail: "omc: command not found".to_string(),
    });

    let message =
        msl_quality_gate_failure_message(gate_input_with_sim_rate(8, 10), &baseline, &measurement)
            .expect("a run with no comparator reading must fail the gate");
    assert!(
        message.contains(PARITY_UNMEASURED_HEADLINE),
        "the gate failure must name the unmeasured state, got: {message}"
    );
    assert!(
        message.contains("omc: command not found"),
        "the gate failure must name why the comparator did not run, got: {message}"
    );
}

#[test]
fn a_perfect_run_with_no_comparator_still_fails() {
    // Every compile/balance/sim metric matches the baseline exactly. Before the
    // fix this produced a clean PASS with `sim_ok` as the only number.
    let baseline = baseline_quality_template();
    let measurement =
        MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::ReferenceAbsent {
            path: "target/msl/results/omc_simulation_reference.json".to_string(),
        });

    let message =
        msl_quality_gate_failure_message(gate_input_with_sim_rate(8, 10), &baseline, &measurement)
            .expect("a metric-clean run with no bands is still unmeasured, and unmeasured fails");
    assert!(
        message.contains(PARITY_UNMEASURED_HEADLINE),
        "got: {message}"
    );
}

#[test]
fn a_measured_run_with_stable_bands_passes() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let measurement = MslParityMeasurement::measured(parity_with(trace_accuracy_baseline()));
    assert!(measurement.is_measured());

    assert_eq!(
        msl_quality_gate_failure_message(gate_input_with_sim_rate(8, 10), &baseline, &measurement,),
        None,
        "a comparator reading that matches the baseline must pass"
    );
}

#[test]
fn a_strict_high_band_drop_fails_even_when_the_acceptable_band_holds() {
    // 8 high + 1 minor -> 6 high + 3 minor: `high + minor` is unchanged, so the
    // acceptable-band ratchet alone would call this stable. Two models stopped
    // agreeing with OMC; that is a regression.
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_stats(10, 8, 1)),
        ..baseline_quality_template()
    };
    let parity = parity_with(trace_stats(10, 6, 3));

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace strict-high pass count regressed")),
        "a strict-high band drop must fail the ratchet, got: {reasons:?}"
    );
}

#[test]
fn sim_ok_collapse_is_reported_and_never_a_gate_reason() {
    let baseline = MslQualityBaseline {
        sim_ok: 8,
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(0, 10);
    gate_input.solve_models = baseline.solve_models;
    let parity = parity_with(trace_accuracy_baseline());

    let notes = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        notes
            .iter()
            .any(|note| note.contains("Sim pass count regressed")),
        "a sim_ok collapse must still be reported, got: {notes:?}"
    );

    let reasons = msl_quality_regression_reasons(gate_input, &baseline, Some(&parity));
    assert!(
        !reasons
            .iter()
            .any(|reason| reason.contains("Sim pass count regressed")),
        "sim_ok is completion, never parity: it must not hold the ratchet, got: {reasons:?}"
    );
}

#[test]
fn the_hard_floor_reads_the_strict_high_band_not_sim_ok() {
    // 100 sim_ok, but only 5 models agree with OMC. The old floor read
    // `sim_ok >= 15` and passed this run.
    let reason = strict_high_hard_floor_reason_for(100, 5)
        .expect("100 sim_ok with 5 agreeing models must not clear the floor");
    assert!(
        reason.contains("strict-high agreement below hard floor"),
        "got: {reason}"
    );
    assert!(reason.contains("5/100"), "got: {reason}");

    assert_eq!(
        strict_high_hard_floor_reason_for(100, 15),
        None,
        "15/100 in the strict-high band clears the 15% floor"
    );
}

#[test]
fn the_hard_floor_is_silent_without_a_reading_because_unmeasured_already_fails() {
    let measurement = MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::OmcUnavailable {
        detail: "omc: command not found".to_string(),
    });
    assert_eq!(
        measurement.strict_high_models(),
        None,
        "an unmeasured run exposes no band count for the floor to read"
    );
    assert!(
        measurement.gate_failure_reason().is_some(),
        "and it is the unmeasured reason that fails the run instead"
    );
}
