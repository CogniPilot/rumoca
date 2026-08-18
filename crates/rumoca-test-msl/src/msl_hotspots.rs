//! Select and profile the slowest compile and simulation entries from the
//! latest MSL parity results.

use anyhow::{Context, Result};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

use crate::msl_flamegraph::{self, MslFlamegraphArgs, MslFlamegraphMode};
use crate::msl_tools::common::MslPaths;

#[derive(Debug, Deserialize)]
struct MslHotspotSummary {
    model_results: Vec<MslHotspotModelResult>,
}

#[derive(Debug, Deserialize)]
struct MslHotspotModelResult {
    model_name: String,
    #[serde(default)]
    compile_seconds: Option<f64>,
    #[serde(default)]
    sim_wall_seconds: Option<f64>,
}

pub fn run(root: &Path) -> Result<()> {
    let summary = load_latest_summary(root)?;
    let source_root = MslPaths::current().msl_dir;
    anyhow::ensure!(
        source_root.is_dir(),
        "missing MSL source root {}; run `cargo make modelica-deps`",
        source_root.display()
    );

    let (compile_model, compile_seconds) = hottest_compile_model(&summary)
        .context("latest MSL results did not contain per-model compile timings")?;
    println!(
        "Generating compile flamegraph for hottest model: {compile_model} ({compile_seconds:.2}s)"
    );
    run_flamegraph(
        root,
        compile_model,
        MslFlamegraphMode::Compile,
        &source_root,
    )?;

    let (sim_model, sim_seconds) = hottest_sim_model(&summary)
        .context("latest MSL results did not contain per-model simulation timings")?;
    println!("Generating simulation flamegraph for hottest model: {sim_model} ({sim_seconds:.2}s)");
    run_flamegraph(root, sim_model, MslFlamegraphMode::Simulate, &source_root)
}

fn latest_results_path(root: &Path) -> PathBuf {
    root.join("target/msl/results/msl_results.json")
}

fn load_latest_summary(root: &Path) -> Result<MslHotspotSummary> {
    let path = latest_results_path(root);
    let raw = fs::read_to_string(&path).with_context(|| {
        format!(
            "missing hotspot source data; run `cargo make verify-msl-parity` so {} exists",
            path.display()
        )
    })?;
    serde_json::from_str(&raw).with_context(|| format!("failed to parse {}", path.display()))
}

fn hottest_compile_model(summary: &MslHotspotSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .compile_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn hottest_sim_model(summary: &MslHotspotSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .sim_wall_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn run_flamegraph(
    root: &Path,
    model: &str,
    mode: MslFlamegraphMode,
    source_root: &Path,
) -> Result<()> {
    msl_flamegraph::run(
        MslFlamegraphArgs {
            model: model.to_owned(),
            mode,
            source_root: Some(source_root.to_path_buf()),
            output: None,
            freq: 99,
            no_inline: false,
            stop_time: None,
        },
        root,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn selects_slowest_compile_and_simulation_independently() {
        let summary = MslHotspotSummary {
            model_results: vec![
                MslHotspotModelResult {
                    model_name: "CompileSlow".to_owned(),
                    compile_seconds: Some(4.0),
                    sim_wall_seconds: Some(1.0),
                },
                MslHotspotModelResult {
                    model_name: "SimSlow".to_owned(),
                    compile_seconds: Some(2.0),
                    sim_wall_seconds: Some(9.0),
                },
            ],
        };

        assert_eq!(hottest_compile_model(&summary), Some(("CompileSlow", 4.0)));
        assert_eq!(hottest_sim_model(&summary), Some(("SimSlow", 9.0)));
    }
}
