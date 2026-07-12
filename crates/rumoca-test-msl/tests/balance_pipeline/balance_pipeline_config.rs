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

/// The `rumoca-test-msl` crate directory, resolved at RUNTIME.
///
/// Every path the harness reads or writes (the parity config, sim-target JSONs,
/// the `target/msl` cache/results, the quality baselines) hangs off this. A
/// normal `cargo test` build bakes the correct `CARGO_MANIFEST_DIR`, but a
/// PREBUILT binary (built once by Nix/crane and run in a different checkout)
/// bakes the build-sandbox path — gone, or read-only in the Nix store, at
/// runtime. So resolve it from the runtime workspace instead: walk up from the
/// CWD to the `[workspace]` root and take `crates/rumoca-test-msl`. This equals
/// the baked value for in-tree `cargo test` (whose CWD is the crate dir, one hop
/// under the workspace root) and is correct for the prebuilt binary (whose CWD
/// the xtask sets to the workspace root). Falls back to the compile-time manifest
/// dir if the CWD isn't inside a rumoca workspace.
pub(crate) fn msl_crate_manifest_dir() -> PathBuf {
    fn workspace_crate_from(start: &std::path::Path) -> Option<PathBuf> {
        start.ancestors().find_map(|dir| {
            let is_workspace_root = dir.join("Cargo.toml").is_file()
                && fs::read_to_string(dir.join("Cargo.toml"))
                    .map(|s| s.contains("[workspace]"))
                    .unwrap_or(false);
            let crate_dir = dir.join("crates/rumoca-test-msl");
            (is_workspace_root && crate_dir.join("Cargo.toml").is_file()).then_some(crate_dir)
        })
    }
    std::env::current_dir()
        .ok()
        .and_then(|cwd| workspace_crate_from(&cwd))
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")))
}

/// `target/msl` cache dir under the runtime workspace (relocatable).
pub(crate) fn msl_cache_dir() -> PathBuf {
    msl_cache_dir_from_manifest(&msl_crate_manifest_dir().to_string_lossy())
}

/// The runtime workspace root (relocatable).
pub(crate) fn msl_workspace_root() -> PathBuf {
    workspace_root_from_manifest_dir(&msl_crate_manifest_dir().to_string_lossy())
}

/// Fixed config path shared with `cargo xtask verify msl-parity`.
pub(crate) fn parity_config_path() -> PathBuf {
    msl_cache_dir().join("parity-config.json")
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
    /// 1-based shard index for a sharded parity run (`--shard m/n` → `m`). The
    /// model set (already ordered slowest-first) is striped round-robin so this
    /// shard keeps every `shard_count`-th model starting at `shard_index - 1`.
    pub shard_index: Option<usize>,
    /// Total shard count for a sharded parity run (`--shard m/n` → `n`).
    pub shard_count: Option<usize>,
    /// Fan-in directory holding each shard's `shard-*/msl_results.json`. When
    /// set, the merge-and-gate entry loads + concatenates those partials,
    /// recomputes the full-set aggregates, and runs the quality gate once on the
    /// merged results (the un-sharded gate's job). Kept OUT of the shard/subset
    /// predicate so the merged run enforces the real baseline ratchet.
    pub merge_shards_dir: Option<PathBuf>,
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
    msl_workspace_root().join(path)
}

pub(crate) fn quality_baseline_file_override() -> Option<PathBuf> {
    let path = parity_config().quality_baseline_file.as_ref()?;
    if path.as_os_str().is_empty() {
        return None;
    }
    if path.is_absolute() {
        return Some(path.clone());
    }
    Some(msl_workspace_root().join(path))
}

/// Fan-in directory (`--merge-shards <dir>`) holding each shard's
/// `shard-*/msl_results.json`. Relative paths resolve from the workspace root
/// (like [`msl_results_dir`]) so the value is independent of the test process's
/// CWD — the CI passes a workspace-root-relative `target/msl/shards`, but libtest
/// runs with CWD set to the crate directory.
pub(crate) fn merge_shards_dir() -> Option<PathBuf> {
    let path = parity_config().merge_shards_dir.as_ref()?;
    if path.as_os_str().is_empty() {
        return None;
    }
    if path.is_absolute() {
        return Some(path.clone());
    }
    Some(msl_workspace_root().join(path))
}

#[test]
fn msl_parity_config_accepts_omc_and_shard_fields() {
    let config: MslParityConfig = serde_json::from_str(
        r#"{
            "force_omc_parity_refresh": true,
            "omc_parity_workers": 6,
            "omc_sim_reference_batch_timeout_secs": 900,
            "shard_index": 2,
            "shard_count": 4,
            "merge_shards_dir": "target/msl/shards"
        }"#,
    )
    .expect("OMC and shard parity fields should deserialize together");

    assert_eq!(config.force_omc_parity_refresh, Some(true));
    assert_eq!(config.omc_parity_workers, Some(6));
    assert_eq!(config.omc_sim_reference_batch_timeout_secs, Some(900));
    assert_eq!(config.shard_index, Some(2));
    assert_eq!(config.shard_count, Some(4));
    assert_eq!(
        config.merge_shards_dir,
        Some(PathBuf::from("target/msl/shards"))
    );
}
