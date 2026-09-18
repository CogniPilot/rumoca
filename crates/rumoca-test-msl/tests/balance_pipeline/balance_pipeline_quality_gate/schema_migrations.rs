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

pub(super) fn reviewed_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = previous_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 6;
    migration.metric.to_quality_gate_version = 7;
    migration.metric.change =
        "reviewed-non-identifiable-discrete-and-internal-node-boundary-v1".to_string();
    migration.metric.strict_high_before = 166;
    migration.metric.strict_high_after = 166;
    migration.metric.policy_excluded_after = 23;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 2;
    migration.metric.exclusions_sha256 =
        "2f4742677e95825bc98f392ef063fdeb2b910b5de1b80c9e94e407a0de6f31dd".to_string();
    migration.evidence_git_commit = "9e5b5c7d52e6dde1edc6164727a5620d5e0867bc".to_string();
    migration.evidence_run = "msl-quality-0.10.0-reviewed-exclusions".to_string();
    migration.policy_excluded_before = 21;
    migration
}

pub(super) fn previous_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = base_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 5;
    migration.metric.to_quality_gate_version = 6;
    migration.metric.change = "reviewed-conditioned-observable-reference-boundary-v1".to_string();
    migration.metric.strict_high_before = 166;
    migration.metric.strict_high_after = 166;
    migration.metric.policy_excluded_after = 21;
    migration.metric.exclusions_sha256 =
        "1da770784678228aff3c0d574bb48adce7138c5c54d252ce79883dd456d135b9".to_string();
    migration.evidence_git_commit = "0de3f29c0ae9440061bef21c7eb9eb4650407015".to_string();
    migration.evidence_run = "multibody-automatic-basis-full".to_string();
    migration.policy_excluded_before = 20;
    migration
}

pub(super) fn base_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    MslReferenceBoundaryMigration {
        metric: MslMetricSchemaMigration {
            from_quality_gate_version: 4,
            to_quality_gate_version: 5,
            change: "reviewed-reference-convergence-boundary-v1".to_string(),
            strict_high_before: 160,
            strict_high_after: 160,
            policy_excluded_after: 20,
            excluded_strict_high_before: 0,
            excluded_non_high_before: 1,
            exclusions_file:
                "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json"
                    .to_string(),
            exclusions_sha256: "30f7c38e58307d6af4f69b844512679ec860f367f88670481edad5318d7d29f8"
                .to_string(),
        },
        evidence_git_commit: "3d76411c1a41a1b27e6a0ecbf1cf3e204f0b47a4".to_string(),
        evidence_run: "multibody-guarded-affine-full-11".to_string(),
        policy_excluded_before: 19,
        previous: None,
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
        from_quality_gate_version: 3,
        to_quality_gate_version: 4,
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
