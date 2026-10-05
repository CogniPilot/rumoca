use super::*;

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub(super) struct ReferenceBoundaryMigration {
    #[serde(flatten)]
    metric: MetricSchemaMigration,
    evidence_git_commit: String,
    evidence_run: String,
    policy_excluded_before: usize,
    #[serde(default)]
    previous: Option<Box<ReferenceBoundaryMigration>>,
}

fn reviewed_migration() -> ReferenceBoundaryMigration {
    let mut migration = v12_migration();
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
    migration
}

fn v12_migration() -> ReferenceBoundaryMigration {
    let mut migration = v11_migration();
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
    migration
}

fn v11_migration() -> ReferenceBoundaryMigration {
    let mut migration = v10_migration();
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
    migration
}

fn v10_migration() -> ReferenceBoundaryMigration {
    let mut migration = v9_migration();
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
    migration
}

fn v9_migration() -> ReferenceBoundaryMigration {
    let mut migration = v8_migration();
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
    migration
}

fn v8_migration() -> ReferenceBoundaryMigration {
    let mut migration = v7_migration();
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
    migration
}

fn v7_migration() -> ReferenceBoundaryMigration {
    let mut migration = v6_migration();
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

fn v6_migration() -> ReferenceBoundaryMigration {
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

fn base_reference_boundary_migration() -> ReferenceBoundaryMigration {
    ReferenceBoundaryMigration {
        metric: MetricSchemaMigration {
            from_quality_gate_version: 4,
            to_quality_gate_version: 5,
            change: "reviewed-reference-convergence-boundary-v1".to_string(),
            strict_high_before: 160,
            strict_high_after: 160,
            policy_excluded_after: 20,
            excluded_strict_high_before: 0,
            excluded_non_high_before: 1,
            exclusions_file: V3_EXCLUSIONS_FILE.to_string(),
            exclusions_sha256: "30f7c38e58307d6af4f69b844512679ec860f367f88670481edad5318d7d29f8"
                .to_string(),
        },
        evidence_git_commit: "3d76411c1a41a1b27e6a0ecbf1cf3e204f0b47a4".to_string(),
        evidence_run: "multibody-guarded-affine-full-11".to_string(),
        policy_excluded_before: 19,
        previous: None,
    }
}

/// The reviewed boundary migration a baseline of `version` must carry, or `None`
/// for a version that is not a reviewed reference boundary.
fn reviewed_boundary_migration(version: u64) -> Option<ReferenceBoundaryMigration> {
    match version {
        MSL_QUALITY_GATE_VERSION => Some(reviewed_migration()),
        PREVIOUS_REFERENCE_BOUNDARY_VERSION => Some(v12_migration()),
        11 => Some(v11_migration()),
        10 => Some(v10_migration()),
        9 => Some(v9_migration()),
        PROMOTED_REFERENCE_BOUNDARY_VERSION => Some(v8_migration()),
        _ => None,
    }
}

pub(super) fn validate_reference_boundary_migration(
    baseline: &MslQualityBaselineHeader,
) -> Result<()> {
    let expected = reviewed_boundary_migration(baseline.quality_gate_version);
    ensure!(
        baseline.reference_boundary_migration == expected,
        "MSL reference boundary migration differs from the reviewed evidence for version {}",
        baseline.quality_gate_version
    );
    Ok(())
}

pub(super) fn schema_target_reaches_current(
    version: u64,
    baseline: &MslQualityBaselineHeader,
) -> bool {
    let mut target = baseline.quality_gate_version;
    let mut migration = baseline.reference_boundary_migration.as_ref();
    while let Some(boundary) = migration {
        if version == target {
            return true;
        }
        if boundary.metric.to_quality_gate_version != target
            || boundary.metric.from_quality_gate_version >= target
        {
            return false;
        }
        target = boundary.metric.from_quality_gate_version;
        migration = boundary.previous.as_deref();
    }
    version == target
}

pub(super) fn migrate_reference_boundary(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<bool> {
    if promoted.quality_gate_version == checked_in.quality_gate_version
        || !schema_target_reaches_current(promoted.quality_gate_version, checked_in)
    {
        return Ok(false);
    }
    validate_reference_boundary_migration(promoted)?;
    ensure!(
        promoted.sim_target_models == checked_in.sim_target_models
            && promoted.omc_version == checked_in.omc_version,
        "MSL reference boundary migration cannot change target or OMC context"
    );
    // This boundary earns no strict-high credit and changes no baseline floor.
    validate_migration_metric_integrity(promoted, checked_in, false, false, false)?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked_baseline() -> MslQualityBaselineHeader {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        load_baseline_header(&root.join(MSL_QUALITY_BASELINE_FALLBACK_REL)).unwrap()
    }

    #[test]
    fn reference_boundary_migration_preserves_all_existing_ratchets() {
        let checked = checked_baseline();
        for version in [4, 8, 9, 10, 11, 12] {
            preserves_ratchets_from(&checked, version);
        }
    }

    fn preserves_ratchets_from(checked: &MslQualityBaselineHeader, version: u64) {
        let mut promoted = checked.clone();
        promoted.quality_gate_version = version;
        promoted.reference_boundary_migration = reviewed_boundary_migration(version);
        assert_eq!(
            choose_baseline(&promoted, checked).unwrap(),
            BaselineChoice::CheckedInMigration
        );
        for field in ["solve", "trace", "runtime", "initial"] {
            let mut lowered = checked.clone();
            match field {
                "solve" => lowered.solve_models -= 1,
                "trace" => lowered.trace_accuracy_stats.agreement_high -= 1,
                "runtime" => lowered.runtime_ratio_stats.system_ratio_both_success.median *= 0.5,
                _ => lowered.ic_ok -= 1,
            }
            assert!(
                choose_baseline(&promoted, &lowered).is_err(),
                "lowered {field}"
            );
        }
    }

    #[test]
    fn current_reference_boundary_rejects_missing_or_forged_evidence() {
        for field in ["count", "digest", "commit", "history", "missing"] {
            let mut baseline = checked_baseline();
            let boundary = baseline.reference_boundary_migration.as_mut().unwrap();
            match field {
                "count" => boundary.metric.strict_high_after += 1,
                "digest" => boundary.metric.exclusions_sha256 = "unreviewed".to_string(),
                "commit" => boundary.evidence_git_commit = "unreviewed".to_string(),
                "history" => boundary.previous.as_mut().unwrap().metric.strict_high_after += 1,
                _ => baseline.reference_boundary_migration = None,
            }
            assert!(
                validate_reference_boundary_migration(&baseline).is_err(),
                "{field}"
            );
        }
    }

    #[test]
    fn boundary_chain_cannot_skip_or_reverse_a_schema_transition() {
        let mut baseline = checked_baseline();
        assert!(schema_target_reaches_current(4, &baseline));
        assert!(schema_target_reaches_current(5, &baseline));
        assert!(schema_target_reaches_current(6, &baseline));
        assert!(schema_target_reaches_current(7, &baseline));
        assert!(schema_target_reaches_current(8, &baseline));
        assert!(schema_target_reaches_current(9, &baseline));
        assert!(schema_target_reaches_current(10, &baseline));
        assert!(schema_target_reaches_current(11, &baseline));
        assert!(schema_target_reaches_current(12, &baseline));
        assert!(!schema_target_reaches_current(3, &baseline));
        let migration = baseline.reference_boundary_migration.as_mut().unwrap();
        migration
            .previous
            .as_mut()
            .unwrap()
            .metric
            .to_quality_gate_version = 13;
        assert!(!schema_target_reaches_current(4, &baseline));
        assert!(validate_reference_boundary_migration(&baseline).is_err());
    }

    #[test]
    fn reviewed_boundary_digest_matches_the_exact_tracked_artifact() {
        let migration = reviewed_migration();
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let bytes = fs::read(root.join(&migration.metric.exclusions_file)).unwrap();
        assert_eq!(
            format!("{:x}", Sha256::digest(bytes)),
            migration.metric.exclusions_sha256
        );
    }
}
