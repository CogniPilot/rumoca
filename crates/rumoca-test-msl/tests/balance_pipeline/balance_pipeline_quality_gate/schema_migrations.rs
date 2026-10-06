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
    let mut migration = v13_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 13;
    migration.metric.to_quality_gate_version = 14;
    migration.metric.change = "phasor-angle-comparison-v1".to_string();
    migration.metric.strict_high_before = 298;
    migration.metric.strict_high_after = 304;
    migration.metric.policy_excluded_after = 53;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "19d0bef167c5120f371e59de5f3c4ed785779b16115f4689ebd8976be55a1a14".to_string();
    migration.evidence_git_commit = "f45f742de6ca9d824e3b707d1e91b31edd33ac7f".to_string();
    migration.evidence_run = "ang-compare-quasistatic-focused".to_string();
    migration.policy_excluded_before = 59;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v13_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v12_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 12;
    migration.metric.to_quality_gate_version = 13;
    migration.metric.change = "typed-trace-exceptions-v6".to_string();
    migration.metric.strict_high_before = 298;
    migration.metric.strict_high_after = 298;
    migration.metric.policy_excluded_after = 59;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 8;
    migration.metric.exclusions_sha256 =
        "088949f877eab0b276dc056cb54e1bc2c62b25e1813d29cd1f5c616b6909d5ed".to_string();
    migration.evidence_git_commit = "20face9056ff23ca23b71d111151d174c3755a05".to_string();
    migration.evidence_run = "ci-37253114196".to_string();
    migration.policy_excluded_before = 51;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v12_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v11_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 11;
    migration.metric.to_quality_gate_version = 12;
    migration.metric.change = "typed-trace-exceptions-v5".to_string();
    migration.metric.strict_high_before = 290;
    migration.metric.strict_high_after = 290;
    migration.metric.policy_excluded_after = 51;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "e4d219fec8391057e3e446b7e5cf83c115937e72d640eb92770bb6ddaf564bbb".to_string();
    migration.evidence_git_commit = "a6ab80615c5f5efcd174c33979ddd3b35db1cfdb".to_string();
    migration.evidence_run = "package-coverage-integration-full".to_string();
    migration.policy_excluded_before = 38;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v11_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v10_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 10;
    migration.metric.to_quality_gate_version = 11;
    migration.metric.change = "typed-trace-exceptions-v4".to_string();
    migration.metric.strict_high_before = 231;
    migration.metric.strict_high_after = 231;
    migration.metric.policy_excluded_after = 38;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "d7069c55495f78ed4f67e1833762a924dcbd719d14a36faf1f1300cda701a4e4".to_string();
    migration.evidence_git_commit = "8183e58220d930d1d2b5737a381b267881133d8a".to_string();
    migration.evidence_run = "two-tanks-near-zero-channel-full".to_string();
    migration.policy_excluded_before = 37;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v10_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v9_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 9;
    migration.metric.to_quality_gate_version = 10;
    migration.metric.change = "typed-trace-exceptions-v3".to_string();
    migration.metric.strict_high_before = 231;
    migration.metric.strict_high_after = 231;
    migration.metric.policy_excluded_after = 37;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "170408125e23ad781591e7fd5cb1f242595cbb617179df50a65705a4f709bcd2".to_string();
    migration.evidence_git_commit = "d0aa5a1e376feb893ed4eb275420c99cd7562560".to_string();
    migration.evidence_run = "controlled-tanks-reference-failure-ci-37070758328".to_string();
    migration.policy_excluded_before = 36;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v9_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v8_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 8;
    migration.metric.to_quality_gate_version = 9;
    migration.metric.change = "typed-trace-exceptions-v2".to_string();
    migration.metric.strict_high_before = 231;
    migration.metric.strict_high_after = 231;
    migration.metric.policy_excluded_after = 36;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "69d6b7259debe61e12fb9a8d3b0003115c496d4ec0f4a84ae8de5a8f9f7dcc49".to_string();
    migration.evidence_git_commit = "b4c670b55cc228fc1db13307b7252e8ba15c80f2".to_string();
    migration.evidence_run = "reviewed-reference-failure-rows-full".to_string();
    migration.policy_excluded_before = 33;
    migration.roster_additions = Vec::new();
    migration
}

pub(super) fn v8_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    let mut migration = v7_reference_boundary_migration();
    migration.previous = Some(Box::new(migration.clone()));
    migration.metric.from_quality_gate_version = 7;
    migration.metric.to_quality_gate_version = 8;
    migration.metric.change = "typed-trace-exceptions-v1".to_string();
    migration.metric.strict_high_before = 231;
    migration.metric.strict_high_after = 231;
    migration.metric.policy_excluded_after = 33;
    migration.metric.excluded_strict_high_before = 0;
    migration.metric.excluded_non_high_before = 0;
    migration.metric.exclusions_sha256 =
        "d8ddb25c1169b72c35c89f77eed12e0b5ae9de8ef4ace0ec998ecdbcb0ec9da0".to_string();
    migration.evidence_git_commit = "70fa6612a34b7e6b454672e448a747d6e4bdd440".to_string();
    migration.evidence_run = "typed-trace-exceptions-full".to_string();
    migration.policy_excluded_before = 23;
    migration.roster_additions = vec![logical_sample_roster_addition()];
    migration
}

pub(super) fn v7_reference_boundary_migration() -> MslReferenceBoundaryMigration {
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
        roster_additions: Vec::new(),
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

/// The reviewed roster addition of the typed-exception boundary: LogicalSample
/// simulates since the event-clock lowering, and its trace diverges from the
/// result OMC writes despite its backend error. A typed exception would hide
/// that Rumoca defect, so the roster names it until the kernel lane's
/// root/time-event coincidence rule fixes it.
pub(super) fn logical_sample_roster_addition() -> MslRosterAddition {
    MslRosterAddition {
        model_name: "Modelica.Clocked.Examples.Elementary.ClockSignals.LogicalSample".to_string(),
        cause:
            "root/time-event coincidence gap in the rotational clocks plus the ConjunctiveClock \
                first-tick latch"
                .to_string(),
        facts: vec![
            "rotational_clock_2 angular_offset, update_offset, sub, abs1 and less channels \
             diverge by 3.0 at t=0.3976"
                .to_string(),
            "rotational_clock_1.direction_sign.u and update_direction.y diverge by 6.0 and \
             sample_conjunctive.y by 3.54 at t=1.4"
                .to_string(),
            "OMC a96aa1a-cmake: Internal error BackendDAETransform.analyseStrongComponentBlock \
             failed (purely discrete algebraic loop through disjunctiveClock.reset_ticked); OMC \
             still writes a result file"
                .to_string(),
        ],
        owner: "kernel lane (root/time-event coincidence rule, SPEC_0044 ME-EVENT-004)".to_string(),
    }
}
