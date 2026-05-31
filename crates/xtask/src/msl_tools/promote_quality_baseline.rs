use anyhow::{Context, Result, bail};
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};

use super::common::MslPaths;

const DEFAULT_BASELINE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";
const DEFAULT_CURRENT_REL: &str = "msl_quality_current.json";
const EXPECTED_QUALITY_GATE_VERSION: u64 = 1;
const RUN_SCOPE_FULL: &str = "full";
const RUN_SCOPE_PARTIAL: &str = "partial";

#[derive(Debug, Parser, Clone)]
pub struct Args {
    /// Source quality snapshot JSON (defaults to target/msl/results/msl_quality_current.json)
    #[arg(long)]
    pub source: Option<PathBuf>,
    /// Destination committed baseline JSON
    #[arg(long)]
    pub baseline: Option<PathBuf>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let source = args
        .source
        .unwrap_or_else(|| paths.results_dir.join(DEFAULT_CURRENT_REL));
    let baseline = args
        .baseline
        .unwrap_or_else(|| paths.repo_root.join(DEFAULT_BASELINE_REL));

    if !source.is_file() {
        bail!(
            "current quality snapshot not found: {}. run the MSL test first.",
            source.display()
        );
    }

    let snapshot_text = fs::read_to_string(&source)
        .with_context(|| format!("failed to read {}", source.display()))?;
    let snapshot: serde_json::Value = serde_json::from_str(&snapshot_text)
        .with_context(|| format!("invalid JSON in {}", source.display()))?;
    ensure_promotable_quality_snapshot(&snapshot, &source)?;

    if let Some(parent) = baseline.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&baseline, snapshot_text)
        .with_context(|| format!("failed to write {}", baseline.display()))?;

    println!("Promoted MSL quality baseline:");
    println!("  source: {}", source.display());
    println!("  baseline: {}", baseline.display());
    Ok(())
}

fn ensure_promotable_quality_snapshot(snapshot: &serde_json::Value, source: &Path) -> Result<()> {
    let Some(root) = snapshot.as_object() else {
        bail!("cannot promote {}: expected JSON object", source.display());
    };
    match root
        .get("quality_gate_version")
        .and_then(serde_json::Value::as_u64)
    {
        Some(EXPECTED_QUALITY_GATE_VERSION) => {}
        Some(version) => bail!(
            "cannot promote {}: unsupported quality_gate_version={} (expected {})",
            source.display(),
            version,
            EXPECTED_QUALITY_GATE_VERSION
        ),
        None => bail!(
            "cannot promote {}: missing numeric quality_gate_version",
            source.display()
        ),
    }
    match root.get("run_scope").and_then(serde_json::Value::as_str) {
        Some(RUN_SCOPE_FULL) => {}
        Some(RUN_SCOPE_PARTIAL) => bail!(
            "cannot promote partial MSL quality snapshot {}. rerun the full baseline without focused RUMOCA_MSL_SIM_* filters.",
            source.display()
        ),
        Some(scope) => bail!(
            "cannot promote {}: run_scope must be '{}', got '{}'",
            source.display(),
            RUN_SCOPE_FULL,
            scope
        ),
        None => bail!("cannot promote {}: missing run_scope", source.display()),
    }
    match root.get("omc_version").and_then(serde_json::Value::as_str) {
        Some(version) if !version.trim().is_empty() => {}
        Some(_) => bail!(
            "cannot promote {}: omc_version must be a non-empty string",
            source.display()
        ),
        None => bail!("cannot promote {}: missing omc_version", source.display()),
    }
    let Some(partial) = root.get("partial") else {
        return Ok(());
    };
    match partial.as_bool() {
        Some(false) => Ok(()),
        Some(true) => bail!(
            "cannot promote partial MSL quality snapshot {}. rerun the full baseline without focused RUMOCA_MSL_SIM_* filters.",
            source.display()
        ),
        None => bail!(
            "cannot promote {}: field 'partial' must be a boolean when present",
            source.display()
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::ensure_promotable_quality_snapshot;
    use serde_json::json;
    use std::path::PathBuf;

    fn full_snapshot() -> serde_json::Value {
        json!({
            "quality_gate_version": 1,
            "run_scope": "full",
            "omc_version": "OpenModelica 1.26.1",
            "sim_ok": 120
        })
    }

    #[test]
    fn promotable_quality_snapshot_allows_missing_or_false_partial_marker_for_full_run() {
        let source = PathBuf::from("msl_quality_current.json");
        ensure_promotable_quality_snapshot(&full_snapshot(), &source)
            .expect("full scope snapshot is promotable");
        let mut explicit_false = full_snapshot();
        explicit_false["partial"] = json!(false);
        ensure_promotable_quality_snapshot(&explicit_false, &source)
            .expect("explicit false marker means full baseline snapshot");
    }

    #[test]
    fn promotable_quality_snapshot_rejects_partial_scope() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(
            &json!({
                "quality_gate_version": 1,
                "run_scope": "partial"
            }),
            &source,
        )
        .expect_err("partial snapshots must not be promotable");
        assert!(
            err.to_string().contains("cannot promote partial"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_non_boolean_partial_marker() {
        let source = PathBuf::from("msl_quality_current.json");
        let mut snapshot = full_snapshot();
        snapshot["partial"] = json!("yes");
        let err = ensure_promotable_quality_snapshot(&snapshot, &source)
            .expect_err("partial marker must be boolean");
        assert!(
            err.to_string().contains("must be a boolean"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_missing_gate_metadata() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(&json!({ "run_scope": "full" }), &source)
            .expect_err("gate version is required");
        assert!(
            err.to_string().contains("quality_gate_version"),
            "unexpected error: {err}"
        );

        let err =
            ensure_promotable_quality_snapshot(&json!({ "quality_gate_version": 1 }), &source)
                .expect_err("run_scope is required");
        assert!(
            err.to_string().contains("run_scope"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_missing_omc_version() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(
            &json!({
                "quality_gate_version": 1,
                "run_scope": "full"
            }),
            &source,
        )
        .expect_err("OMC version is required for promoted baselines");
        assert!(
            err.to_string().contains("omc_version"),
            "unexpected error: {err}"
        );

        let mut blank = full_snapshot();
        blank["omc_version"] = json!(" ");
        let err = ensure_promotable_quality_snapshot(&blank, &source)
            .expect_err("OMC version must be non-empty");
        assert!(
            err.to_string().contains("non-empty"),
            "unexpected error: {err}"
        );
    }
}
