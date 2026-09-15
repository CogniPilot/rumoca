use super::*;

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub(super) struct ReferenceBoundaryMigration {
    #[serde(flatten)]
    metric: MetricSchemaMigration,
    evidence_git_commit: String,
    evidence_run: String,
    policy_excluded_before: usize,
}

fn reviewed_migration() -> ReferenceBoundaryMigration {
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
    }
}

pub(super) fn validate_reference_boundary_migration(
    baseline: &MslQualityBaselineHeader,
) -> Result<()> {
    if baseline.quality_gate_version != MSL_QUALITY_GATE_VERSION {
        ensure!(
            baseline.reference_boundary_migration.is_none(),
            "reference boundary migration belongs only to quality-gate version 5"
        );
        return Ok(());
    }
    ensure!(
        baseline.reference_boundary_migration.as_ref() == Some(&reviewed_migration()),
        "MSL reference boundary migration differs from the reviewed version-4 to version-5 evidence"
    );
    Ok(())
}

pub(super) fn schema_target_reaches_current(
    version: u64,
    baseline: &MslQualityBaselineHeader,
) -> bool {
    version == baseline.quality_gate_version
        || baseline
            .reference_boundary_migration
            .as_ref()
            .is_some_and(|migration| {
                version == migration.metric.from_quality_gate_version
                    && migration.metric.to_quality_gate_version == baseline.quality_gate_version
            })
}

pub(super) fn migrate_reference_boundary(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<bool> {
    let Some(migration) = checked_in.reference_boundary_migration.as_ref() else {
        return Ok(false);
    };
    if promoted.quality_gate_version != migration.metric.from_quality_gate_version {
        return Ok(false);
    }
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
        let mut promoted = checked.clone();
        promoted.quality_gate_version = 4;
        promoted.reference_boundary_migration = None;
        assert_eq!(
            choose_baseline(&promoted, &checked).unwrap(),
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
        for field in ["count", "digest", "commit", "missing"] {
            let mut baseline = checked_baseline();
            let boundary = baseline.reference_boundary_migration.as_mut().unwrap();
            match field {
                "count" => boundary.metric.strict_high_after += 1,
                "digest" => boundary.metric.exclusions_sha256 = "unreviewed".to_string(),
                "commit" => boundary.evidence_git_commit = "unreviewed".to_string(),
                _ => baseline.reference_boundary_migration = None,
            }
            assert!(
                validate_reference_boundary_migration(&baseline).is_err(),
                "{field}"
            );
        }
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
