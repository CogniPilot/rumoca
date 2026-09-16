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
    let mut migration = previous_reference_boundary_migration();
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

fn previous_reference_boundary_migration() -> ReferenceBoundaryMigration {
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

pub(super) fn validate_reference_boundary_migration(
    baseline: &MslQualityBaselineHeader,
) -> Result<()> {
    let expected = match baseline.quality_gate_version {
        MSL_QUALITY_GATE_VERSION => Some(reviewed_migration()),
        PREVIOUS_REFERENCE_BOUNDARY_VERSION => Some(previous_reference_boundary_migration()),
        _ => None,
    };
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
        for version in [4, 5] {
            preserves_ratchets_from(&checked, version);
        }
    }

    fn preserves_ratchets_from(checked: &MslQualityBaselineHeader, version: u64) {
        let mut promoted = checked.clone();
        promoted.quality_gate_version = version;
        promoted.reference_boundary_migration =
            (version == 5).then(previous_reference_boundary_migration);
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
        assert!(!schema_target_reaches_current(3, &baseline));
        let migration = baseline.reference_boundary_migration.as_mut().unwrap();
        migration
            .previous
            .as_mut()
            .unwrap()
            .metric
            .to_quality_gate_version = 6;
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
