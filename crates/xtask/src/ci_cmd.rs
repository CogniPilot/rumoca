//! `cargo xtask ci`: read-only views of the hosted CI through the `gh` CLI.

use anyhow::{Context, Result, bail};
use clap::{Args, Subcommand};
use serde_json::Value;
use std::process::Command;
use std::time::Duration;

#[derive(Debug, Args)]
pub(crate) struct CiArgs {
    #[command(subcommand)]
    pub(crate) command: CiCommand,
}

#[derive(Debug, Subcommand, PartialEq, Eq)]
pub(crate) enum CiCommand {
    /// Wait for the `main` runs of a commit and print their conclusions and
    /// failed jobs
    Watch(CiWatchArgs),
}

#[derive(Debug, Args, PartialEq, Eq)]
pub(crate) struct CiWatchArgs {
    /// Commit whose runs on `main` are watched
    pub(crate) sha: String,
    /// Seconds between polls while a run is pending
    #[arg(long, default_value_t = 60)]
    pub(crate) interval_secs: u64,
    /// Report the current state once instead of waiting for completion
    #[arg(long)]
    pub(crate) once: bool,
}

/// One workflow run of the watched commit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CiRun {
    pub(crate) id: u64,
    pub(crate) workflow: String,
    pub(crate) status: String,
    pub(crate) conclusion: String,
}

impl CiRun {
    fn completed(&self) -> bool {
        self.status == "completed"
    }

    fn succeeded(&self) -> bool {
        matches!(self.conclusion.as_str(), "success" | "skipped" | "neutral")
    }
}

/// The runs of `gh run list --json databaseId,workflowName,status,conclusion`.
pub(crate) fn parse_runs(json: &str) -> Result<Vec<CiRun>> {
    let value: Value = serde_json::from_str(json).context("parse `gh run list` output")?;
    Ok(value
        .as_array()
        .into_iter()
        .flatten()
        .map(|run| CiRun {
            id: run["databaseId"].as_u64().unwrap_or_default(),
            workflow: run["workflowName"].as_str().unwrap_or_default().to_string(),
            status: run["status"].as_str().unwrap_or_default().to_string(),
            conclusion: run["conclusion"].as_str().unwrap_or_default().to_string(),
        })
        .collect())
}

/// Names of the jobs of `gh run view --json jobs` that did not succeed.
pub(crate) fn failed_jobs(json: &str) -> Result<Vec<String>> {
    let value: Value = serde_json::from_str(json).context("parse `gh run view` output")?;
    Ok(value["jobs"]
        .as_array()
        .into_iter()
        .flatten()
        .filter(|job| {
            matches!(
                job["conclusion"].as_str(),
                Some("failure" | "timed_out" | "cancelled")
            )
        })
        .filter_map(|job| job["name"].as_str().map(str::to_string))
        .collect())
}

pub(crate) fn run(args: CiArgs) -> Result<()> {
    match args.command {
        CiCommand::Watch(args) => watch(&args),
    }
}

fn gh(args: &[&str]) -> Result<String> {
    let output = Command::new("gh")
        .args(args)
        .output()
        .context("run `gh` (the GitHub CLI must be installed and authenticated)")?;
    if !output.status.success() {
        bail!(
            "gh {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn watch(args: &CiWatchArgs) -> Result<()> {
    loop {
        let runs = parse_runs(&gh(&[
            "run",
            "list",
            "--commit",
            &args.sha,
            "--branch",
            "main",
            "--json",
            "databaseId,workflowName,status,conclusion",
        ])?)?;
        let pending = runs.is_empty() || runs.iter().any(|run| !run.completed());
        if pending && !args.once {
            println!(
                "{} run(s) for {}; {} pending",
                runs.len(),
                args.sha,
                runs.iter().filter(|run| !run.completed()).count()
            );
            std::thread::sleep(Duration::from_secs(args.interval_secs));
            continue;
        }
        return report(&runs);
    }
}

fn report(runs: &[CiRun]) -> Result<()> {
    if runs.is_empty() {
        bail!("no run on main for this commit");
    }
    let mut failed = false;
    for run in runs {
        println!(
            "{} ({}): {} {}",
            run.workflow, run.id, run.status, run.conclusion
        );
        if run.completed() && !run.succeeded() {
            failed = true;
            let jobs = failed_jobs(&gh(&[
                "run",
                "view",
                &run.id.to_string(),
                "--json",
                "jobs",
            ])?)?;
            for job in jobs {
                println!("  failed job: {job}");
            }
        }
    }
    if failed {
        bail!("a main run of this commit did not succeed");
    }
    Ok(())
}

#[cfg(test)]
mod tests;
