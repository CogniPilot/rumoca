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

/// The reviewed chain, newest link first: the same checked-in file the MSL
/// quality gate loads, so the chain is declared once (xtask reads it as data
/// and depends on no rumoca crate).
const REVIEWED_REFERENCE_BOUNDARIES: &str =
    include_str!("../../../../rumoca-test-msl/tests/msl_tests/msl_reference_boundaries.json");
const REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION: u64 = 1;

#[derive(Deserialize)]
struct ReferenceBoundaryChainFile {
    schema_version: u64,
    chain: ReferenceBoundaryMigration,
}

/// Parse a reference boundary chain file, refusing one of another schema.
fn parse_reference_boundary_chain(text: &str) -> Result<ReferenceBoundaryMigration> {
    let file: ReferenceBoundaryChainFile =
        serde_json::from_str(text).context("parse the reference boundary chain")?;
    ensure!(
        file.schema_version == REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION,
        "reference boundary chain schema {} is not the supported {}",
        file.schema_version,
        REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION
    );
    Ok(file.chain)
}

fn reviewed_migration() -> ReferenceBoundaryMigration {
    // Invariant: the checked-in chain parses under its schema; the tests
    // load it on every run.
    parse_reference_boundary_chain(REVIEWED_REFERENCE_BOUNDARIES)
        .expect("the checked-in reference boundary chain parses under its schema")
}

/// The reviewed boundary migration a baseline of `version` must carry, or `None`
/// for a version that is not a reviewed reference boundary.
fn reviewed_boundary_migration(version: u64) -> Option<ReferenceBoundaryMigration> {
    if !(PROMOTED_REFERENCE_BOUNDARY_VERSION..=MSL_QUALITY_GATE_VERSION).contains(&version) {
        return None;
    }
    let mut link = Some(reviewed_migration());
    while let Some(boundary) = link {
        if boundary.metric.to_quality_gate_version == version {
            return Some(boundary);
        }
        link = boundary.previous.map(|previous| *previous);
    }
    None
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
        for version in [4, 8, 9, 10, 11, 12, 13] {
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
        assert!(schema_target_reaches_current(13, &baseline));
        assert!(schema_target_reaches_current(14, &baseline));
        assert!(!schema_target_reaches_current(3, &baseline));
        let migration = baseline.reference_boundary_migration.as_mut().unwrap();
        migration
            .previous
            .as_mut()
            .unwrap()
            .metric
            .to_quality_gate_version = 15;
        assert!(!schema_target_reaches_current(4, &baseline));
        assert!(validate_reference_boundary_migration(&baseline).is_err());
    }

    #[test]
    fn a_chain_file_of_another_schema_is_refused() {
        let mut file: serde_json::Value =
            serde_json::from_str(REVIEWED_REFERENCE_BOUNDARIES).unwrap();
        file["schema_version"] = serde_json::json!(REFERENCE_BOUNDARY_CHAIN_SCHEMA_VERSION + 1);
        let error = parse_reference_boundary_chain(&file.to_string()).unwrap_err();
        assert!(
            error.to_string().contains("is not the supported"),
            "{error}"
        );
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
