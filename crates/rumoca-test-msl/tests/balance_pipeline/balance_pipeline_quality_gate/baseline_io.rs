//! Baseline paths and the checked reference read boundary.

use super::*;

pub(in super::super) fn msl_quality_baseline_path() -> PathBuf {
    if let Some(path) = quality_baseline_file_override() {
        return path;
    }
    Path::new(env!("CARGO_MANIFEST_DIR")).join(MSL_QUALITY_BASELINE_FILE_REL)
}

pub(in super::super) fn msl_quality_current_path() -> PathBuf {
    msl_results_dir().join(MSL_QUALITY_CURRENT_FILE_REL)
}

pub(in super::super) fn msl_simulation_targets_path() -> PathBuf {
    msl_results_dir().join(MSL_SIM_TARGETS_FILE_REL)
}

pub(in super::super) fn omc_parity_cache_dir() -> PathBuf {
    // Results directories are intentionally disposable and often unique per
    // diagnostic run. Keep the content-keyed OMC cache at the relocatable MSL
    // cache root so a fresh results directory can reuse the same verified
    // reference without coupling it to a machine-specific path.
    msl_cache_dir().join(OMC_PARITY_CACHE_DIR_REL)
}

pub(in super::super) fn load_msl_quality_baseline(path: &Path) -> io::Result<MslQualityBaseline> {
    let raw = fs::read(path)?;
    let document: serde_json::Value = serde_json::from_slice(&raw)
        .map_err(|error| io::Error::other(format!("invalid MSL quality baseline JSON: {error}")))?;
    validate_runtime_baseline_evidence(&document)?;
    let baseline: MslQualityBaseline = serde_json::from_slice(&raw)
        .map_err(|error| io::Error::other(format!("invalid MSL quality baseline JSON: {error}")))?;
    validate_certified_strict_high_roster(&baseline)?;
    validate_unexcepted_non_high_roster(&baseline)?;
    Ok(baseline)
}

pub(in super::super) fn omc_simulation_reference_path() -> PathBuf {
    msl_results_dir().join(OMC_SIM_REFERENCE_FILE_REL)
}

pub(in super::super) fn json_usize_field(root: &serde_json::Value, key: &str) -> Option<usize> {
    root.get(key)?
        .as_u64()
        .and_then(|value| usize::try_from(value).ok())
}

pub(in super::super) fn json_f64_field(root: &serde_json::Value, key: &str) -> Option<f64> {
    root.get(key)?.as_f64()
}

pub(super) fn json_f64_field_any(root: &serde_json::Value, keys: &[&str]) -> Option<f64> {
    keys.iter().find_map(|key| json_f64_field(root, key))
}
