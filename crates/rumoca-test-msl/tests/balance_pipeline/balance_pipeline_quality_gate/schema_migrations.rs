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

/// The reviewed reference boundary chain (SPEC_0033, SPEC_0050), newest link
/// first. A boundary is reviewed by reviewing this file; nothing else
/// declares it.
pub(super) const MSL_REFERENCE_BOUNDARIES_FILE_REL: &str =
    "tests/msl_tests/msl_reference_boundaries.json";
pub(super) const REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION: u32 = 1;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ReferenceBoundaryChainFile {
    pub(super) schema_version: u32,
    pub(super) chain: MslReferenceBoundaryMigration,
}

/// Parse a reference boundary chain file, refusing one of another schema.
pub(super) fn parse_reference_boundary_chain(
    text: &str,
) -> Result<MslReferenceBoundaryMigration, String> {
    let file: ReferenceBoundaryChainFile = serde_json::from_str(text)
        .map_err(|error| format!("parse the reference boundary chain: {error}"))?;
    if file.schema_version != REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION {
        return Err(format!(
            "reference boundary chain schema {} is not the supported {}",
            file.schema_version, REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION
        ));
    }
    Ok(file.chain)
}

pub(super) fn reviewed_reference_boundary_migration() -> MslReferenceBoundaryMigration {
    parse_reference_boundary_chain(include_str!(
        "../../msl_tests/msl_reference_boundaries.json"
    ))
    .unwrap_or_else(|error| panic!("the reviewed reference boundary chain: {error}"))
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
        deviation_channels: None,
        severe_channels: None,
    }
}

#[test]
fn a_roster_additions_reviewed_channel_counts_survive_parse_and_serialize() {
    let text = include_str!("../../msl_tests/msl_reference_boundaries.json");
    let chain = parse_reference_boundary_chain(text).expect("the checked-in chain parses");
    let written = serde_json::to_value(&chain).expect("the chain serializes");
    let additions = written["roster_additions"]
        .as_array()
        .expect("head roster additions");
    let two_tanks = additions
        .iter()
        .find(|addition| {
            addition["model_name"] == "Modelica.Fluid.Examples.AST_BatchPlant.Test.TwoTanks"
        })
        .expect("the head boundary adds TwoTanks");
    assert_eq!(two_tanks["deviation_channels"], 11);
    assert_eq!(two_tanks["severe_channels"], 11);
}

#[test]
fn the_reviewed_chain_is_one_contiguous_checked_in_file() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(MSL_REFERENCE_BOUNDARIES_FILE_REL);
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    let chain = parse_reference_boundary_chain(&text).expect("the checked-in chain parses");
    assert_eq!(chain, reviewed_reference_boundary_migration());
    assert_eq!(
        chain.metric.to_quality_gate_version,
        MSL_QUALITY_GATE_VERSION
    );
    let mut link = &chain;
    while let Some(previous) = link.previous.as_deref() {
        assert_eq!(
            link.metric.from_quality_gate_version, previous.metric.to_quality_gate_version,
            "boundary {} does not continue the chain",
            link.metric.change
        );
        link = previous;
    }
}

#[test]
fn a_chain_file_of_another_schema_is_refused() {
    let text = include_str!("../../msl_tests/msl_reference_boundaries.json");
    let mut file: serde_json::Value = serde_json::from_str(text).unwrap();
    file["schema_version"] = serde_json::json!(REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION + 1);
    let error = parse_reference_boundary_chain(&file.to_string()).unwrap_err();
    assert!(error.contains("is not the supported"), "{error}");
}
