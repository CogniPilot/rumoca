//! Per-invocation configuration for the MSL parity harness.
//!
//! `cargo test` cannot forward custom CLI arguments to a libtest harness, so
//! `cargo xtask verify msl-parity` writes this configuration to a fixed path
//! (`<workspace>/target/msl/parity-config.json`) before invoking the gate, and
//! the harness reads it once here. This keeps the user-facing surface a set of
//! documented `xtask` flags while the on-disk file is purely a private
//! xtask->harness channel (the libtest equivalent of an argv).
//!
//! Every field is optional: an absent or empty file yields the documented
//! defaults (a full root-examples parity run), so a bare
//! `cargo test -p rumoca-test-msl ...` Just Works with no config present.

use super::*;
use std::sync::OnceLock;

/// Fixed config path shared with `cargo xtask verify msl-parity`.
pub(crate) fn parity_config_path() -> PathBuf {
    msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR")).join("parity-config.json")
}

/// Deserialized MSL parity configuration. All fields optional; `None` selects
/// the harness default for that knob.
#[derive(Debug, Default, Clone, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub(crate) struct MslParityConfig {
    /// Results directory for JSON, markdown, trace, and debug artifacts. Relative
    /// paths are resolved from the workspace root.
    pub results_dir: Option<PathBuf>,
    /// `"root-examples"` (default) or `"default-simulation-targets"`.
    pub target_scope: Option<String>,
    /// `"full"` (default), `"short"`, or `"long"`.
    pub sim_set: Option<String>,
    /// Short/long lexical subset size (default [`SIM_SET_LIMIT_DEFAULT`]).
    pub sim_set_limit: Option<usize>,
    /// Explicit simulation-targets file (absolute, or relative to the
    /// workspace / harness crate).
    pub sim_targets_file: Option<PathBuf>,
    /// Include ModelicaTest sources in the discovered model set.
    pub include_modelica_test: Option<bool>,
    /// Require every selected simulation target to simulate successfully; used
    /// by the focused CI ModelicaTest gate in place of the baseline gate.
    pub require_selected_targets_success: Option<bool>,
    /// Substring (or exact, see `sim_match_exact`) model-name subset filter.
    pub sim_match: Option<Vec<String>>,
    /// Treat `sim_match` entries as exact model names rather than substrings.
    pub sim_match_exact: Option<bool>,
    /// Cap on the number of models after subset filtering.
    pub sim_limit: Option<usize>,
    /// Compile/balance stage worker count (default: host-derived).
    pub stage_parallelism: Option<usize>,
    /// Simulation worker count (default: stage parallelism, memory-capped).
    pub sim_parallelism: Option<usize>,
    /// Per-sim-worker address-space cap in MB (default: uncapped).
    pub sim_worker_memory_mb: Option<usize>,
    /// Total simulation memory budget in MB; caps the sim worker count.
    pub sim_total_memory_mb: Option<usize>,
    /// Resolved quality baseline JSON file. Relative paths are resolved from
    /// the workspace root.
    pub quality_baseline_file: Option<PathBuf>,
    /// Opt into the generated simulation-targets file when no explicit file or
    /// committed file applies.
    pub generated_sim_targets_file: Option<bool>,
    /// Force regeneration of the OMC simulation reference cache.
    pub force_omc_parity_refresh: Option<bool>,
    /// OMC reference-generation worker count.
    pub omc_parity_workers: Option<usize>,
    /// Whole-stage OMC reference-generation timeout in seconds.
    pub omc_sim_reference_batch_timeout_secs: Option<u64>,
}

/// Load (once) the MSL parity configuration from [`parity_config_path`]. A
/// missing file is the common case (local `cargo test`) and yields defaults; a
/// present-but-invalid file is a hard error so a mis-written config never runs
/// the wrong gate silently.
pub(crate) fn parity_config() -> &'static MslParityConfig {
    static CONFIG: OnceLock<MslParityConfig> = OnceLock::new();
    CONFIG.get_or_init(|| {
        let path = parity_config_path();
        let Ok(raw) = fs::read_to_string(&path) else {
            return MslParityConfig::default();
        };
        if raw.trim().is_empty() {
            return MslParityConfig::default();
        }
        match serde_json::from_str(&raw) {
            Ok(config) => {
                println!("MSL parity config loaded from {}", path.display());
                config
            }
            Err(err) => panic!("invalid MSL parity config '{}': {err}", path.display()),
        }
    })
}

pub(crate) fn msl_results_dir() -> PathBuf {
    let Some(path) = parity_config().results_dir.as_ref() else {
        return get_msl_cache_dir().join("results");
    };
    if path.as_os_str().is_empty() {
        return get_msl_cache_dir().join("results");
    }
    if path.is_absolute() {
        return path.clone();
    }
    workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR")).join(path)
}

pub(crate) fn quality_baseline_file_override() -> Option<PathBuf> {
    let path = parity_config().quality_baseline_file.as_ref()?;
    if path.as_os_str().is_empty() {
        return None;
    }
    if path.is_absolute() {
        return Some(path.clone());
    }
    Some(workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR")).join(path))
}
