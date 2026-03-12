use anyhow::{Context, Result};
use clap::{Args, Subcommand};
use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crate::{CheckRustFileLinesArgs, cmd_check_rust_file_lines, run_status, test_cmd};

#[derive(Debug, Args, Clone)]
pub(crate) struct VerifyArgs {
    #[command(subcommand)]
    pub(crate) command: VerifyCommand,
}

#[derive(Debug, Subcommand, Clone, Copy, PartialEq, Eq)]
pub(crate) enum VerifyCommand {
    /// Rust formatting, line-count policy, traversal policy, and clippy
    Lint,
    /// Workspace tests that mirror the main test matrix
    Workspace,
    /// Full verification suite mirrored by GitHub CI
    Full,
    /// Full local verification suite, excluding the slow 180-model MSL parity gate
    Quick,
    /// Verify the primary binaries build
    Binaries,
    /// Rustdoc/docs gate with warnings denied
    Docs,
    /// Full MSL/OMC parity gate harness
    MslParity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VerifyStep {
    label: &'static str,
    args: &'static [&'static str],
    include_in_quick: bool,
}

const VERIFY_SUITE_STEPS: &[VerifyStep] = &[
    VerifyStep {
        label: "lint",
        args: &["verify", "lint"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "workspace tests",
        args: &["verify", "workspace"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "binary build",
        args: &["verify", "binaries"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage run",
        args: &["coverage", "run"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage report",
        args: &["coverage", "report"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "coverage gate",
        args: &[
            "coverage",
            "gate",
            "--allowed-workspace-line-coverage-drop",
            "6.0",
        ],
        include_in_quick: true,
    },
    VerifyStep {
        label: "docs",
        args: &["verify", "docs"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "VS Code gate",
        args: &["vscode", "test"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "WASM gate",
        args: &["wasm", "test"],
        include_in_quick: true,
    },
    VerifyStep {
        label: "MSL parity",
        args: &["verify", "msl-parity"],
        include_in_quick: false,
    },
];

#[derive(Debug, Clone, Copy)]
enum VerifySuite {
    Full,
    Quick,
}

impl VerifySuite {
    fn label(self) -> &'static str {
        match self {
            Self::Full => "verify full",
            Self::Quick => "verify quick",
        }
    }

    fn includes(self, step: &VerifyStep) -> bool {
        match self {
            Self::Full => true,
            Self::Quick => step.include_in_quick,
        }
    }
}

pub(crate) fn run(args: VerifyArgs, root: &Path) -> Result<()> {
    match args.command {
        VerifyCommand::Lint => run_lint_job(root),
        VerifyCommand::Workspace => test_cmd::run_workspace_tests(root),
        VerifyCommand::Full => run_verify_suite(root, VerifySuite::Full),
        VerifyCommand::Quick => run_verify_suite(root, VerifySuite::Quick),
        VerifyCommand::Binaries => test_cmd::run_workspace_binary_build(root),
        VerifyCommand::Docs => test_cmd::run_workspace_docs(root),
        VerifyCommand::MslParity => run_msl_quality_gate(root),
    }
}

fn run_verify_suite(root: &Path, suite: VerifySuite) -> Result<()> {
    println!("Running `{}` suite...", suite.label());
    for step in VERIFY_SUITE_STEPS
        .iter()
        .filter(|step| suite.includes(step))
    {
        run_rum_step(root, step)?;
    }
    println!("`{}` suite passed.", suite.label());
    Ok(())
}

fn run_rum_step(root: &Path, step: &VerifyStep) -> Result<()> {
    println!("Running {}: `rum {}`", step.label, step.args.join(" "));
    let rum_exe =
        std::env::current_exe().context("failed to resolve current rum executable for step run")?;
    let mut cmd = Command::new(rum_exe);
    cmd.args(step.args).current_dir(root);
    run_status(cmd)
}

fn run_lint_job(root: &Path) -> Result<()> {
    test_cmd::run_workspace_fmt_check(root)?;
    cmd_check_rust_file_lines(CheckRustFileLinesArgs {
        max_lines: 2000,
        all_files: true,
    })?;

    let mut traversal = Command::new("cargo");
    traversal
        .arg("run")
        .arg("--quiet")
        .arg("--bin")
        .arg("rumoca-traversal-policy-check")
        .current_dir(root);
    run_status(traversal)?;

    test_cmd::run_workspace_clippy(root)
}

fn run_msl_quality_gate(root: &Path) -> Result<()> {
    let ci_env = MslCiEnvironment::from_env(root);
    ci_env.print_notice();
    ci_env.clean_stale_results()?;
    let _cleanup = MslResultsCleanupGuard::new(ci_env.results_dir.clone(), ci_env.clean_results);
    let _monitor = MslResourceMonitor::start(ci_env.clone());

    let mut build_sim_worker = Command::new("cargo");
    build_sim_worker
        .arg("build")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--bin")
        .arg("rumoca-sim-worker")
        .current_dir(root);
    run_status_logged(build_sim_worker)?;

    let mut build_msl_tools = Command::new("cargo");
    build_msl_tools
        .arg("build")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-tool-dev")
        .arg("--bin")
        .arg("rumoca-msl-tools")
        .current_dir(root);
    run_status_logged(build_msl_tools)?;

    let mut gate = Command::new("cargo");
    gate.arg("test")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--test")
        .arg("msl_tests")
        .arg("balance_pipeline::balance_pipeline_core::test_msl_all")
        .arg("--")
        .arg("--ignored")
        .arg("--nocapture")
        .env(
            "RUMOCA_SIM_WORKER_EXE",
            root.join("target/release/rumoca-sim-worker"),
        )
        .env(
            "RUMOCA_MSL_TOOLS_EXE",
            root.join("target/release/rumoca-msl-tools"),
        )
        .env("RUST_BACKTRACE", "full")
        .env("RUMOCA_MSL_CACHE_DIR", root.join("target/msl"))
        .current_dir(root);
    run_status_logged(gate)
}

fn run_status_logged(command: Command) -> Result<()> {
    println!("Running command: {command:?}");
    run_status(command)
}

#[derive(Debug, Clone)]
struct MslCiEnvironment {
    root: std::path::PathBuf,
    results_dir: std::path::PathBuf,
    monitor_interval: Option<Duration>,
    clean_results: bool,
    github_actions: bool,
}

impl MslCiEnvironment {
    fn from_env(root: &Path) -> Self {
        let results_dir = std::env::var_os("RUMOCA_CI_MSL_RESULTS_DIR")
            .filter(|value| !value.is_empty())
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| root.join("target/msl/results"));
        Self {
            root: root.to_path_buf(),
            results_dir,
            monitor_interval: env_var_nonzero_u64("RUMOCA_CI_RESOURCE_MONITOR_SECS")
                .map(Duration::from_secs),
            clean_results: env_var_bool("RUMOCA_CI_CLEAN_MSL_RESULTS"),
            github_actions: std::env::var_os("GITHUB_ACTIONS").is_some(),
        }
    }

    fn print_notice(&self) {
        if !self.github_actions {
            return;
        }
        println!(
            "GitHub Actions note: workflow concurrency may cancel this job when a newer push or force-push reaches the same branch/PR, even if CPU, disk, and memory look healthy."
        );
    }

    fn clean_stale_results(&self) -> Result<()> {
        if !self.clean_results || !self.results_dir.is_dir() {
            return Ok(());
        }
        eprintln!(
            "Removing stale MSL results directory before run: {}",
            self.results_dir.display()
        );
        print_results_dir_summary("cleanup-start", &self.results_dir);
        fs::remove_dir_all(&self.results_dir).with_context(|| {
            format!(
                "failed to remove stale MSL results directory '{}'",
                self.results_dir.display()
            )
        })
    }
}

struct MslResultsCleanupGuard {
    results_dir: std::path::PathBuf,
    enabled: bool,
}

impl MslResultsCleanupGuard {
    fn new(results_dir: std::path::PathBuf, enabled: bool) -> Self {
        Self {
            results_dir,
            enabled,
        }
    }
}

impl Drop for MslResultsCleanupGuard {
    fn drop(&mut self) {
        if !self.enabled || !self.results_dir.is_dir() {
            return;
        }
        eprintln!(
            "Cleaning MSL results directory: {}",
            self.results_dir.display()
        );
        print_results_dir_summary("cleanup-before", &self.results_dir);
        if let Err(error) = fs::remove_dir_all(&self.results_dir) {
            eprintln!(
                "WARNING: failed to remove MSL results directory '{}': {error}",
                self.results_dir.display()
            );
            return;
        }
        eprintln!(
            "Removed MSL results directory: {}",
            self.results_dir.display()
        );
    }
}

struct MslResourceMonitor {
    config: MslCiEnvironment,
    done: Arc<AtomicBool>,
    worker: Option<JoinHandle<()>>,
}

impl MslResourceMonitor {
    fn start(config: MslCiEnvironment) -> Self {
        print_resource_snapshot("initial", &config);
        let Some(interval) = config.monitor_interval else {
            return Self {
                config,
                done: Arc::new(AtomicBool::new(true)),
                worker: None,
            };
        };

        let done = Arc::new(AtomicBool::new(false));
        let done_flag = Arc::clone(&done);
        let config_for_worker = config.clone();
        let worker = thread::spawn(move || {
            run_resource_monitor_loop(done_flag, interval, config_for_worker);
        });
        Self {
            config,
            done,
            worker: Some(worker),
        }
    }
}

impl Drop for MslResourceMonitor {
    fn drop(&mut self) {
        self.done.store(true, Ordering::Relaxed);
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
        print_resource_snapshot("final", &self.config);
    }
}

fn run_resource_monitor_loop(
    done_flag: Arc<AtomicBool>,
    interval: Duration,
    config: MslCiEnvironment,
) {
    while !done_flag.load(Ordering::Relaxed) {
        thread::sleep(interval);
        if done_flag.load(Ordering::Relaxed) {
            break;
        }
        print_resource_snapshot("periodic", &config);
    }
}

fn env_var_nonzero_u64(key: &str) -> Option<u64> {
    std::env::var(key)
        .ok()
        .as_deref()
        .and_then(parse_nonzero_u64)
}

fn env_var_bool(key: &str) -> bool {
    std::env::var(key)
        .ok()
        .as_deref()
        .is_some_and(parse_truthy_bool)
}

fn parse_nonzero_u64(raw: &str) -> Option<u64> {
    raw.trim().parse::<u64>().ok().filter(|value| *value > 0)
}

fn parse_truthy_bool(raw: &str) -> bool {
    matches!(
        raw.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn print_resource_snapshot(phase: &str, config: &MslCiEnvironment) {
    eprintln!("== MSL Resource Snapshot ({phase}) ==");
    log_command_output("date", "date", ["-Is"]);
    eprintln!("workspace_root={}", config.root.display());
    if let Some(interval) = config.monitor_interval {
        eprintln!("resource_monitor_interval_secs={}", interval.as_secs());
    }
    log_command_output("nproc", "nproc", std::iter::empty::<&str>());
    log_command_output("uptime", "uptime", std::iter::empty::<&str>());
    log_command_output("free -h", "free", ["-h"]);
    log_command_output("df -h", "df", ["-h", ".", "/tmp"]);
    log_command_output("df -ih", "df", ["-ih", ".", "/tmp"]);
    log_path_size("target", &config.root.join("target"));
    log_path_size("target/msl", &config.root.join("target/msl"));
    log_path_size("msl_results", &config.results_dir);
    print_results_dir_summary("results-breakdown", &config.results_dir);
    log_top_processes("top-by-mem", "--sort=-%mem");
    log_top_processes("top-by-cpu", "--sort=-%cpu");
}

fn log_path_size(label: &str, path: &Path) {
    if !path.exists() {
        return;
    }
    let output = Command::new("du").arg("-sh").arg(path).output();
    match output {
        Ok(output) if output.status.success() => {
            let summary = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if !summary.is_empty() {
                eprintln!("{label}: {summary} ({})", path.display());
            }
        }
        Ok(_) | Err(_) => {
            eprintln!("{label}: {}", path.display());
        }
    }
}

fn print_results_dir_summary(label: &str, results_dir: &Path) {
    if !results_dir.is_dir() {
        return;
    }
    eprintln!("{label}:");
    let entries = match fs::read_dir(results_dir) {
        Ok(entries) => entries,
        Err(error) => {
            eprintln!("  failed to read '{}': {error}", results_dir.display());
            return;
        }
    };
    for entry in entries.flatten() {
        let path = entry.path();
        log_path_size("  entry", &path);
    }
}

fn log_top_processes(label: &str, sort_flag: &str) {
    let output = Command::new("ps")
        .args(["-eo", "pid,ppid,%cpu,%mem,rss,vsz,etime,comm", sort_flag])
        .output();
    let Ok(output) = output else {
        return;
    };
    if !output.status.success() {
        return;
    }
    eprintln!("{label}:");
    for line in String::from_utf8_lossy(&output.stdout).lines().take(15) {
        eprintln!("  {line}");
    }
}

fn log_command_output<I, S>(label: &str, program: &str, args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let output = Command::new(program).args(args).output();
    let Ok(output) = output else {
        return;
    };
    if !output.status.success() {
        return;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    if stdout.trim().is_empty() {
        return;
    }
    eprintln!("{label}:");
    for line in stdout.lines() {
        eprintln!("  {line}");
    }
}

#[cfg(test)]
mod tests {
    use super::{
        MslCiEnvironment, VERIFY_SUITE_STEPS, VerifySuite, parse_nonzero_u64, parse_truthy_bool,
    };
    use std::path::PathBuf;

    fn step_argvs(suite: VerifySuite) -> Vec<Vec<&'static str>> {
        VERIFY_SUITE_STEPS
            .iter()
            .filter(|step| suite.includes(step))
            .map(|step| step.args.to_vec())
            .collect()
    }

    #[test]
    fn quick_suite_skips_msl_parity() {
        let steps = step_argvs(VerifySuite::Quick);
        assert!(!steps.contains(&vec!["verify", "msl-parity"]));
        assert!(steps.contains(&vec!["coverage", "run"]));
        assert!(steps.contains(&vec!["vscode", "test"]));
        assert!(steps.contains(&vec!["wasm", "test"]));
    }

    #[test]
    fn full_suite_includes_full_msl_parity() {
        let steps = step_argvs(VerifySuite::Full);
        assert!(steps.contains(&vec!["verify", "msl-parity"]));
        assert_eq!(steps.last(), Some(&vec!["verify", "msl-parity"]));
    }

    #[test]
    fn bool_env_parser_accepts_common_truthy_values() {
        assert!(parse_truthy_bool("yes"));
        assert!(parse_truthy_bool("TRUE"));
        assert!(!parse_truthy_bool("no"));
    }

    #[test]
    fn nonzero_u64_env_parser_rejects_zero_and_invalid_values() {
        assert_eq!(parse_nonzero_u64("30"), Some(30));
        assert_eq!(parse_nonzero_u64("0"), None);
        assert_eq!(parse_nonzero_u64("abc"), None);
    }

    #[test]
    fn msl_ci_environment_cleans_stale_results_before_run() {
        let temp = tempfile::tempdir().expect("tempdir");
        let results_dir = temp.path().join("results");
        std::fs::create_dir_all(&results_dir).expect("mkdir");
        std::fs::write(results_dir.join("stale.json"), "{}").expect("write stale file");
        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: results_dir.clone(),
            monitor_interval: None,
            clean_results: true,
            github_actions: false,
        };
        env.clean_stale_results().expect("cleanup should succeed");
        assert!(
            !results_dir.exists(),
            "pre-run cleanup should remove stale results directory"
        );
    }
}
