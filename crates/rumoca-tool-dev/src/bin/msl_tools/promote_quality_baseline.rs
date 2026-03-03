use anyhow::{Context, Result, bail};
use clap::Parser;
use std::fs;
use std::path::PathBuf;

use super::common::MslPaths;

const DEFAULT_BASELINE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";
const DEFAULT_CURRENT_REL: &str = "msl_quality_current.json";

#[derive(Debug, Parser, Clone)]
pub(crate) struct Args {
    /// Source quality snapshot JSON (defaults to target/msl/results/msl_quality_current.json)
    #[arg(long)]
    pub source: Option<PathBuf>,
    /// Destination committed baseline JSON
    #[arg(long)]
    pub baseline: Option<PathBuf>,
}

pub(crate) fn run(args: Args) -> Result<()> {
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
    let _: serde_json::Value = serde_json::from_str(&snapshot_text)
        .with_context(|| format!("invalid JSON in {}", source.display()))?;

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
