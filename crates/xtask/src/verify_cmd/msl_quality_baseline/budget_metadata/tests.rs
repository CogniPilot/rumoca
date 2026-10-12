use super::*;

const LEGACY: &[u8] = include_bytes!("fixtures/promoted-618765768.json");

fn reviewed_correction() -> BudgetMetadataCorrection {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let header = load_baseline_header(&root.join(MSL_QUALITY_BASELINE_FALLBACK_REL))
        .expect("reviewed checked baseline");
    header
        .budget_metadata_correction
        .expect("explicit metadata proof")
}

#[test]
fn quality_budget_metadata_preserves_every_nonbudget_promoted_value() {
    let original: Value = serde_json::from_slice(LEGACY).expect("actual promoted payload");
    let corrected = corrected_document(LEGACY, Some(&reviewed_correction()))
        .expect("known exact asset")
        .expect("metadata correction");
    assert_eq!(corrected["model_attempt_timeout_seconds"], 20.0);
    assert_eq!(corrected["sim_timeout_seconds"], 45.0);
    assert_eq!(corrected["flatten_models"], 541);
    assert_eq!(corrected["compiled_models"], 440);
    assert_eq!(
        corrected["certified_strict_high_models"]
            .as_array()
            .unwrap()
            .len(),
        302
    );
    let mut restored = corrected;
    restored
        .as_object_mut()
        .unwrap()
        .remove("model_attempt_timeout_seconds");
    restored["sim_timeout_seconds"] = original["sim_timeout_seconds"].clone();
    assert_eq!(
        restored, original,
        "no model, count, tolerance or source change"
    );
}

#[test]
fn quality_budget_metadata_requires_declared_exact_asset() {
    assert!(corrected_document(LEGACY, None).is_err());
    let correction = reviewed_correction();
    let mut changed_bytes = LEGACY.to_vec();
    changed_bytes.push(b'\n');
    assert!(corrected_document(&changed_bytes, Some(&correction)).is_err());
    let mut changed_source: Value = serde_json::from_slice(LEGACY).unwrap();
    changed_source["git_commit"] = "foreign".into();
    assert!(
        corrected_document(
            &serde_json::to_vec(&changed_source).unwrap(),
            Some(&correction)
        )
        .is_err()
    );
}

#[test]
fn quality_budget_metadata_passes_truthful_envelopes_without_rewriting() {
    let mut truthful = corrected_document(LEGACY, Some(&reviewed_correction()))
        .unwrap()
        .unwrap();
    truthful["sim_timeout_seconds"] = 46.0.into();
    assert!(
        corrected_document(&serde_json::to_vec(&truthful).unwrap(), None)
            .unwrap()
            .is_none()
    );
    for field in ["model_attempt_timeout_seconds", "sim_timeout_seconds"] {
        let mut invalid = truthful.clone();
        invalid[field] = 0.0.into();
        assert!(corrected_document(&serde_json::to_vec(&invalid).unwrap(), None).is_err());
    }
}

#[test]
fn quality_budget_metadata_rejects_forged_correction_authority() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let document: Value =
        serde_json::from_slice(&fs::read(root.join(MSL_QUALITY_BASELINE_FALLBACK_REL)).unwrap())
            .unwrap();
    for field in [
        "legacy_asset_sha256",
        "legacy_asset_git_commit",
        "evidence_workflow_sha256",
    ] {
        let mut wire = document["budget_metadata_correction"].clone();
        wire[field] = "foreign".into();
        assert!(serde_json::from_value::<BudgetMetadataCorrection>(wire).is_err());
    }
    for field in [
        "evidence_ci_run",
        "legacy_quality_gate_version",
        "model_attempt_timeout_seconds",
        "sim_timeout_seconds",
    ] {
        let mut wire = document["budget_metadata_correction"].clone();
        wire[field] = 1.into();
        assert!(serde_json::from_value::<BudgetMetadataCorrection>(wire).is_err());
    }
}

#[test]
fn quality_budget_metadata_keeps_raw_file_and_promoted_selection() {
    let temp = tempfile::tempdir().unwrap();
    let raw = temp.path().join("raw.json");
    fs::write(&raw, LEGACY).unwrap();
    let derived =
        selected_promoted_baseline(temp.path(), raw.clone(), Some(&reviewed_correction())).unwrap();
    assert_ne!(derived, raw);
    assert_eq!(fs::read(&raw).unwrap(), LEGACY);
    let checked_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let checked =
        load_baseline_header(&checked_root.join(MSL_QUALITY_BASELINE_FALLBACK_REL)).unwrap();
    let corrected = load_promoted_baseline_header(&derived).unwrap();
    assert_eq!(
        choose_baseline(&corrected, &checked).unwrap(),
        BaselineChoice::Promoted
    );
}
