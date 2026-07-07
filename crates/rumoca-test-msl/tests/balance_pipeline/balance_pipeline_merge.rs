//! Fan-in "merge and gate" entry for sharded MSL parity runs.
//!
//! A sharded run (`verify msl-parity --shard m/n`) produces N partial
//! `msl_results.json` files, one per shard, each of which skips the aggregate
//! quality ratchet. This module loads those partials, concatenates them into the
//! full result set, recomputes the run-wide aggregates by reusing the normal
//! summary builder ([`finalize_msl_summary_from_results`]), and then runs the
//! quality gate ONCE on the merged results — the job the un-sharded gate used to
//! do inline. Almost everything here is reuse; the only new logic is loading and
//! concatenating the shard files.

use super::*;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Concatenate per-shard summaries into one full-set summary, reusing the normal
/// aggregate builder. Pure (no I/O), so the merge arithmetic is unit-testable.
///
/// The per-model `model_results` and `sim_target_models` are disjoint stripes
/// that tile the full set, so they concatenate. `total_models` is per-shard →
/// summed. `total_mo_files` / `parse_errors` / `class_type_counts` are full-set
/// discovery values (discovery runs before striping) identical across shards →
/// taken from the first shard and asserted equal.
fn merge_shard_summaries(shards: Vec<MslSummary>) -> Result<MslSummary, String> {
    let Some(first) = shards.first() else {
        return Err("merge-shards: no shard summaries provided".to_string());
    };
    let total_mo_files = first.total_mo_files;
    let parse_errors = first.parse_errors;
    let class_type_counts = first.class_type_counts.clone();

    let mut total_models = 0usize;
    for (idx, shard) in shards.iter().enumerate() {
        if shard.total_mo_files != total_mo_files || shard.parse_errors != parse_errors {
            return Err(format!(
                "merge-shards: shard {idx} discovery mismatch \
                 (total_mo_files {} vs {}, parse_errors {} vs {}) — \
                 shards must come from one source root",
                shard.total_mo_files, total_mo_files, shard.parse_errors, parse_errors
            ));
        }
        total_models += shard.total_models;
    }

    let mut model_results = Vec::new();
    let mut sim_target_models = Vec::new();
    for shard in shards {
        model_results.extend(shard.model_results);
        sim_target_models.extend(shard.sim_target_models);
    }

    let inputs = MslSummaryInputs {
        total_mo_files,
        parse_errors,
        total_models,
        class_type_counts,
    };
    Ok(finalize_msl_summary_from_results(
        model_results,
        sim_target_models,
        inputs,
        MslPhaseTimings::default(),
        Instant::now(),
    ))
}

/// Load every `<dir>/shard-*/msl_results.json` into an [`MslSummary`], ordered by
/// shard directory name for determinism.
fn load_shard_summaries(dir: &Path) -> Result<Vec<MslSummary>, String> {
    let mut shard_dirs: Vec<PathBuf> = std::fs::read_dir(dir)
        .map_err(|e| format!("merge-shards: cannot read {}: {e}", dir.display()))?
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| {
            path.is_dir()
                && path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.starts_with("shard-"))
        })
        .collect();
    shard_dirs.sort();
    if shard_dirs.is_empty() {
        return Err(format!(
            "merge-shards: no shard-*/ directories under {}",
            dir.display()
        ));
    }

    let mut summaries = Vec::with_capacity(shard_dirs.len());
    for shard_dir in shard_dirs {
        let file = shard_dir.join("msl_results.json");
        let raw = std::fs::read_to_string(&file)
            .map_err(|e| format!("merge-shards: cannot read {}: {e}", file.display()))?;
        let summary: MslSummary = serde_json::from_str(&raw)
            .map_err(|e| format!("merge-shards: invalid {}: {e}", file.display()))?;
        println!(
            "Loaded {} ({} model results)",
            shard_dir.display(),
            summary.model_results.len()
        );
        summaries.push(summary);
    }
    Ok(summaries)
}

/// Fan-in entry: load the shard partials, merge them, and run the real quality
/// gate once on the merged full-set summary. Invoked by
/// `cargo xtask verify msl-parity --merge-shards <dir>` (which is a "full-shaped"
/// run so `should_skip_msl_quality_gate()` is false and the baseline ratchet
/// actually runs).
#[cfg(feature = "msl-full-test")]
#[test]
fn test_msl_merge_and_gate() {
    let dir = merge_shards_dir()
        .expect("test_msl_merge_and_gate requires --merge-shards <dir> in the parity config");
    let shards = load_shard_summaries(&dir).expect("load shard summaries");
    println!("Merging {} shards from {}", shards.len(), dir.display());
    let merged = merge_shard_summaries(shards).expect("merge shard summaries");
    println!(
        "Merged full set: {} models, {} sim targets, total_models {}",
        merged.model_results.len(),
        merged.sim_target_models.len(),
        merged.total_models
    );

    // Reuse the un-sharded write + validate + snapshot + gate sequence.
    write_msl_results(&merged).expect("write merged msl_results.json + reports");
    assert_valid_msl_summary(&merged);
    if merged.sim_attempted > 0 {
        write_current_msl_quality_snapshot(&merged).expect("write merged quality snapshot");
    }
    enforce_msl_quality_gate(&merged).expect("merged MSL quality gate");
    println!(
        "Merged MSL quality gate passed ({} sim_ok / {} attempted)",
        merged.sim_ok, merged.sim_attempted
    );
}

#[cfg(test)]
fn model_result(name: &str) -> MslModelResult {
    let mut result = phase_error_result(name.to_string(), "Success", None, None);
    result.is_balanced = Some(true);
    result.initial_balance_ok = Some(true);
    result.ir_solve_file = Some(format!("ir_solve/{name}.json"));
    result.ic_status = Some("ic_ok".to_string());
    result.sim_status = Some("sim_ok".to_string());
    result
}

#[test]
fn merge_shard_summaries_concatenates_and_sums() {
    let mut a = empty_summary(100, 2);
    a.total_models = 3;
    a.sim_target_models = vec!["A".into(), "C".into()];
    a.model_results = vec![model_result("A"), model_result("C")];

    let mut b = empty_summary(100, 2);
    b.total_models = 2;
    b.sim_target_models = vec!["B".into()];
    b.model_results = vec![model_result("B")];

    let merged = merge_shard_summaries(vec![a, b]).expect("merge");
    assert_eq!(merged.total_models, 5, "total_models summed across shards");
    assert_eq!(
        merged.total_mo_files, 100,
        "full-set discovery carried over"
    );
    assert_eq!(merged.parse_errors, 2, "full-set discovery carried over");
    assert_eq!(
        merged.model_results.len(),
        3,
        "model_results concatenated (stripes tile the set)"
    );
    assert_eq!(merged.sim_target_models.len(), 3, "sim targets unioned");
    assert_eq!(merged.sim_attempted, 3, "sim statuses recomputed");
    assert_eq!(merged.sim_ok, 3, "sim successes recomputed");
}

#[test]
fn merge_shard_summaries_rejects_discovery_mismatch() {
    // Shards from different source roots (differing full-set discovery) must not
    // silently merge into a bogus aggregate.
    let a = empty_summary(100, 2);
    let b = empty_summary(999, 2);
    assert!(merge_shard_summaries(vec![a, b]).is_err());
    assert!(
        merge_shard_summaries(vec![]).is_err(),
        "empty input rejected"
    );
}
