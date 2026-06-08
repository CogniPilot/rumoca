use anyhow::{Context, Result};
use serde::Serialize;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

use crate::run_status;

#[derive(Debug, Clone, Serialize)]
pub(super) struct MslCargoSetupTimingStep {
    pub(super) label: String,
    pub(super) cargo_action: String,
    pub(super) package: String,
    pub(super) profile: String,
    pub(super) features: Vec<String>,
    pub(super) target_dir: String,
    pub(super) command: String,
    pub(super) status: String,
    pub(super) elapsed_seconds: f64,
}

#[derive(Debug, Clone)]
pub(super) struct MslCargoSetupStepMetadata {
    label: String,
    cargo_action: String,
    package: String,
    profile: String,
    features: Vec<String>,
    target_dir: String,
}

impl MslCargoSetupStepMetadata {
    pub(super) fn new(
        label: &str,
        cargo_action: &str,
        package: &str,
        profile: &str,
        features: Vec<String>,
        target_dir: &Path,
    ) -> Self {
        Self {
            label: label.to_string(),
            cargo_action: cargo_action.to_string(),
            package: package.to_string(),
            profile: profile.to_string(),
            features,
            target_dir: target_dir.display().to_string(),
        }
    }
}

#[derive(Debug, Serialize)]
struct MslCargoSetupTimingReport {
    success: bool,
    total_elapsed_seconds: f64,
    steps: Vec<MslCargoSetupTimingStep>,
}

impl MslCargoSetupTimingReport {
    fn new(steps: Vec<MslCargoSetupTimingStep>) -> Self {
        let total_elapsed_seconds = steps.iter().map(|step| step.elapsed_seconds).sum();
        Self {
            success: steps.iter().all(|step| step.status == "pass"),
            total_elapsed_seconds,
            steps,
        }
    }
}

pub(super) fn run_msl_cargo_setup_step(
    steps: &mut Vec<MslCargoSetupTimingStep>,
    metadata: MslCargoSetupStepMetadata,
    command: Command,
) -> Result<()> {
    let command_display = format!("{command:?}");
    let started = Instant::now();
    let result = run_status_logged(command);
    steps.push(MslCargoSetupTimingStep {
        label: metadata.label,
        cargo_action: metadata.cargo_action,
        package: metadata.package,
        profile: metadata.profile,
        features: metadata.features,
        target_dir: metadata.target_dir,
        command: command_display,
        status: if result.is_ok() { "pass" } else { "fail" }.to_string(),
        elapsed_seconds: elapsed_seconds(started.elapsed()),
    });
    result
}

fn run_status_logged(command: Command) -> Result<()> {
    println!("Running command: {command:?}");
    run_status(command)
}

pub(super) fn write_msl_cargo_setup_timing_report(
    results_dir: &Path,
    steps: &[MslCargoSetupTimingStep],
) -> Result<()> {
    let report = MslCargoSetupTimingReport::new(steps.to_vec());
    fs::create_dir_all(results_dir)
        .with_context(|| format!("failed to create {}", results_dir.display()))?;
    let json_path = results_dir.join("msl_cargo_setup_timing.json");
    let markdown_path = results_dir.join("msl_cargo_setup_timing.md");
    let json = serde_json::to_string_pretty(&report)
        .context("failed to serialize MSL Cargo setup timing")?;
    fs::write(&json_path, json)
        .with_context(|| format!("failed to write {}", json_path.display()))?;
    fs::write(
        &markdown_path,
        render_msl_cargo_setup_timing_markdown(&report),
    )
    .with_context(|| format!("failed to write {}", markdown_path.display()))?;
    println!(
        "MSL Cargo setup timing report written to {} and {}",
        json_path.display(),
        markdown_path.display()
    );
    Ok(())
}

fn render_msl_cargo_setup_timing_markdown(report: &MslCargoSetupTimingReport) -> String {
    let mut lines = vec![
        "# MSL Cargo Setup Timing".to_string(),
        String::new(),
        format!("- success: {}", report.success),
        format!(
            "- total_elapsed_seconds: {:.3}",
            report.total_elapsed_seconds
        ),
        String::new(),
        "| Step | Status | Seconds | Package | Profile | Features | Target Dir | Command |"
            .to_string(),
        "|---|---|---:|---|---|---|---|---|".to_string(),
    ];
    lines.extend(report.steps.iter().map(|step| {
        format!(
            "| {} | {} | {:.3} | {} | {} | {} | {} | `{}` |",
            step.label,
            step.status,
            step.elapsed_seconds,
            step.package,
            step.profile,
            format_feature_list(&step.features),
            step.target_dir,
            step.command
        )
    }));
    lines.push(String::new());
    lines.join("\n")
}

fn format_feature_list(features: &[String]) -> String {
    if features.is_empty() {
        "none".to_string()
    } else {
        features.join(", ")
    }
}

fn elapsed_seconds(elapsed: Duration) -> f64 {
    elapsed.as_secs_f64()
}
