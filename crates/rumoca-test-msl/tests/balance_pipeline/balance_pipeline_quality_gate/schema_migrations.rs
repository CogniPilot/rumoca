use super::*;

pub(super) fn quality_gate_v3_metric_schema_migration() -> MslMetricSchemaMigration {
    MslMetricSchemaMigration {
        from_quality_gate_version: 2,
        to_quality_gate_version: 3,
        change: "reviewed-pointwise-oracle-boundaries-v1".to_string(),
        strict_high_before: 118,
        strict_high_after: 113,
        policy_excluded_after: 9,
        excluded_strict_high_before: 5,
        excluded_non_high_before: 4,
        exclusions_file: "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json"
            .to_string(),
        exclusions_sha256: "e064ffb80771c1e231e849afcaa25cc2a08b8b7f9bf449bf8651905e5dcdc4d0"
            .to_string(),
    }
}

pub(super) fn reviewed_partial_model_names() -> IndexSet<String> {
    [
        "Modelica.Electrical.Analog.Examples.OpAmps.OpAmpCircuits.PartialOpAmp",
        "Modelica.Electrical.PowerConverters.Examples.ACAC.ExampleTemplates.Dimmer",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.Thyristor1Pulse",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.ThyristorBridge2Pulse",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.ThyristorBridge2mPulse",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.ThyristorCenterTap2Pulse",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.ThyristorCenterTap2mPulse",
        "Modelica.Electrical.PowerConverters.Examples.ACDC.ExampleTemplates.ThyristorCenterTapmPulse",
        "Modelica.Electrical.PowerConverters.Examples.DCAC.ExampleTemplates.SinglePhaseTwoLevel",
        "Modelica.Electrical.PowerConverters.Examples.DCDC.ExampleTemplates.ChopperBuckBoost",
        "Modelica.Electrical.PowerConverters.Examples.DCDC.ExampleTemplates.ChopperStepDown",
        "Modelica.Electrical.PowerConverters.Examples.DCDC.ExampleTemplates.ChopperStepUp",
        "Modelica.Electrical.PowerConverters.Examples.DCDC.ExampleTemplates.HBridge",
    ]
    .into_iter()
    .map(str::to_string)
    .collect()
}

pub(super) fn reviewed_partial_classification_migration() -> MslPartialClassificationMigration {
    MslPartialClassificationMigration {
        from_quality_gate_version: PREVIOUS_MSL_QUALITY_GATE_VERSION,
        to_quality_gate_version: MSL_QUALITY_GATE_VERSION,
        change: "source-static-partial-cohort-v1".to_string(),
        evidence_git_commit: "5394156facb1e5ff9f099f21c0e833c4870c506f".to_string(),
        sim_target_models: 566,
        partial_models_before: 11,
        partial_models_after: 13,
        affected_diagnostic_cohort: "failed-before-success-with-null-partial-classification"
            .to_string(),
        affected_models: [
            "Modelica.Electrical.Analog.Examples.OpAmps.OpAmpCircuits.PartialOpAmp",
            "Modelica.Electrical.PowerConverters.Examples.ACAC.ExampleTemplates.Dimmer",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        partial_model_names_after: reviewed_partial_model_names(),
    }
}

pub(super) fn partial_classification_context_mismatch_reason(
    baseline: &MslQualityBaseline,
) -> Option<String> {
    let expected = reviewed_partial_classification_migration();
    if baseline.partial_classification_migration.as_ref() != Some(&expected) {
        return Some(
            "partial classification migration differs from the reviewed v3-to-v4 correction"
                .to_string(),
        );
    }
    if baseline.partial_models != baseline.partial_model_names.len() {
        return Some(format!(
            "baseline partial count/roster mismatch: count={}, roster={}",
            baseline.partial_models,
            baseline.partial_model_names.len()
        ));
    }
    if baseline.partial_model_names != expected.partial_model_names_after {
        return Some(
            "baseline partial model roster differs from the reviewed v4 roster".to_string(),
        );
    }
    None
}

pub(super) fn push_partial_model_roster_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    if gate_input.partial_models != gate_input.partial_model_names.len() {
        reasons.push(format!(
            "current partial count/roster mismatch: count={}, roster={}",
            gate_input.partial_models,
            gate_input.partial_model_names.len()
        ));
    }
    let removed = baseline
        .partial_model_names
        .iter()
        .filter(|name| !gate_input.partial_model_names.contains(name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    let added = gate_input
        .partial_model_names
        .iter()
        .filter(|name| !baseline.partial_model_names.contains(name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if !removed.is_empty() {
        reasons.push(format!(
            "partial model roster lost reviewed names: {}",
            removed.join(", ")
        ));
    }
    if !added.is_empty() {
        reasons.push(format!(
            "partial model roster gained unreviewed names: {}",
            added.join(", ")
        ));
    }
}
