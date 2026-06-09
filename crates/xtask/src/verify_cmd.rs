use anyhow::{Context, Result, ensure};
use clap::{Args, Subcommand};
use serde::Serialize;
use std::ffi::OsStr;
use std::fs;
use std::io::{Cursor, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::{
    command_exists, exe_name, lsp_benchmark_cmd, modelica_dependency_cache, msl_flamegraph_cmd,
    run_status, run_status_quiet, test_cmd, vscode_cmd,
};

mod msl_cargo_setup_timing;

use msl_cargo_setup_timing::{
    MslCargoSetupStepMetadata, MslCargoSetupTimingStep, run_msl_cargo_setup_step,
    write_msl_cargo_setup_timing_report,
};

const MSL_VERSION: &str = "4.1.0";
const MSL_RELEASE_ZIP_URL: &str = "https://github.com/modelica/ModelicaStandardLibrary/releases/download/v4.1.0/ModelicaStandardLibrary_v4.1.0.zip";
const MSL_MODELICA_DIR_NAME: &str = "Modelica 4.1.0";
const MSL_MODELICA_SERVICES_DIR_NAME: &str = "ModelicaServices 4.1.0";

#[derive(Debug, Args, Clone)]
pub(crate) struct VerifyArgs {
    #[command(subcommand)]
    pub(crate) command: VerifyCommand,
}

#[derive(Debug, Args, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct VerifyEditorRuntimeArgs {
    /// On Ubuntu/Debian Linux hosts, install missing headless VS Code smoke prerequisites (`xvfb`, `xauth`) before running
    #[arg(long)]
    pub(crate) install_prereqs: bool,
    /// Fail (instead of skipping) when an editor runtime smoke prerequisite is missing; used by CI to enforce real runtime coverage
    #[arg(long)]
    pub(crate) require_runtimes: bool,
}

#[derive(Debug, Args, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) struct VerifySuiteArgs {
    /// Stop at the first failing step instead of running every step and
    /// reporting an aggregate pass/fail summary at the end
    #[arg(long)]
    pub(crate) early_exit: bool,
}

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub(crate) struct VerifyMslParityArgs {
    /// Emit periodic CPU/disk/memory resource samples at this interval in seconds (0 disables monitoring)
    #[arg(long, default_value_t = 0)]
    monitor_interval_secs: u64,
    /// Remove any stale MSL results directory before running the gate
    #[arg(long)]
    clean_results: bool,
    /// Directory for MSL parity result JSON (defaults to `<root>/target/msl/results`)
    #[arg(long)]
    results_dir: Option<PathBuf>,
    /// Model selection scope: `root-examples` (default) or `default-simulation-targets`
    #[arg(long)]
    target_scope: Option<String>,
    /// Simulation set: `full` (default), `short`, or `long`
    #[arg(long)]
    sim_set: Option<String>,
    /// Short/long lexical subset size
    #[arg(long)]
    sim_set_limit: Option<usize>,
    /// Explicit simulation-targets JSON file (absolute or workspace-relative)
    #[arg(long)]
    sim_targets_file: Option<PathBuf>,
    /// Include ModelicaTest sources in the discovered model set
    #[arg(long)]
    include_modelica_test: bool,
    /// Require every selected simulation target to simulate successfully (focused gate)
    #[arg(long)]
    require_selected_targets_success: bool,
    /// Restrict to models whose name matches one of these patterns (repeatable)
    #[arg(long = "sim-match")]
    sim_match: Vec<String>,
    /// Treat `--sim-match` values as exact model names instead of substrings
    #[arg(long)]
    sim_match_exact: bool,
    /// Cap the number of models after subset filtering
    #[arg(long)]
    sim_limit: Option<usize>,
    /// Compile/balance stage worker count (default: host-derived)
    #[arg(long)]
    stage_parallelism: Option<usize>,
    /// Simulation worker count (default: stage parallelism, memory-capped)
    #[arg(long)]
    sim_parallelism: Option<usize>,
    /// Per-sim-worker address-space cap in MB
    #[arg(long)]
    sim_worker_memory_mb: Option<usize>,
    /// Total simulation memory budget in MB (caps the sim worker count)
    #[arg(long)]
    sim_total_memory_mb: Option<usize>,
}

impl VerifyMslParityArgs {
    /// Serialize the harness-facing knobs into the JSON config that the libtest
    /// gate reads (the libtest equivalent of forwarding argv). Only explicitly
    /// set fields are written; absent fields fall back to the harness defaults.
    fn to_parity_config_json(&self) -> serde_json::Value {
        let mut config = serde_json::Map::new();
        if let Some(value) = &self.results_dir {
            config.insert(
                "results_dir".into(),
                value.to_string_lossy().into_owned().into(),
            );
        }
        if let Some(value) = &self.target_scope {
            config.insert("target_scope".into(), value.clone().into());
        }
        if let Some(value) = &self.sim_set {
            config.insert("sim_set".into(), value.clone().into());
        }
        if let Some(value) = self.sim_set_limit {
            config.insert("sim_set_limit".into(), value.into());
        }
        if let Some(value) = &self.sim_targets_file {
            config.insert(
                "sim_targets_file".into(),
                value.to_string_lossy().into_owned().into(),
            );
        }
        if self.include_modelica_test {
            config.insert("include_modelica_test".into(), true.into());
        }
        if self.requires_selected_targets_success() {
            config.insert("require_selected_targets_success".into(), true.into());
        }
        if !self.sim_match.is_empty() {
            config.insert("sim_match".into(), self.sim_match.clone().into());
        }
        if self.sim_match_exact {
            config.insert("sim_match_exact".into(), true.into());
        }
        if let Some(value) = self.sim_limit {
            config.insert("sim_limit".into(), value.into());
        }
        if let Some(value) = self.stage_parallelism {
            config.insert("stage_parallelism".into(), value.into());
        }
        if let Some(value) = self.sim_parallelism {
            config.insert("sim_parallelism".into(), value.into());
        }
        if let Some(value) = self.sim_worker_memory_mb {
            config.insert("sim_worker_memory_mb".into(), value.into());
        }
        if let Some(value) = self.sim_total_memory_mb {
            config.insert("sim_total_memory_mb".into(), value.into());
        }
        serde_json::Value::Object(config)
    }

    fn requires_selected_targets_success(&self) -> bool {
        self.require_selected_targets_success
            || self.sim_targets_file.is_some()
            || !self.sim_match.is_empty()
            || self.sim_limit.is_some()
    }
}

/// Fixed path the libtest harness reads its per-invocation config from
/// (`<workspace>/target/msl/parity-config.json`), matching
/// `balance_pipeline_config::parity_config_path` on the harness side.
fn parity_config_path(root: &Path) -> PathBuf {
    root.join("target/msl/parity-config.json")
}

/// Write the parity config to the fixed path before the libtest gate runs. The
/// file is rewritten every invocation so a previous run's config never leaks.
fn write_parity_config(root: &Path, args: &VerifyMslParityArgs) -> Result<()> {
    let path = parity_config_path(root);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let json = serde_json::to_string_pretty(&args.to_parity_config_json())?;
    fs::write(&path, json).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

#[derive(Debug, Subcommand, Clone, PartialEq, Eq)]
pub(crate) enum VerifyCommand {
    /// Architecture-hardening gates (env-var registry, file-size, layering)
    Architecture,
    /// Rust formatting, traversal policy, and clippy
    Lint,
    /// Workspace tests that mirror the main test matrix
    Workspace,
    /// Environment-dependent example template runtime checks
    TemplateRuntimes,
    /// Fetch example Modelica library caches and compile example models
    Examples,
    /// Real VS Code extension-host MSL smoke check
    VscodeMsl(VerifyEditorRuntimeArgs),
    /// Full-MSL LSP timings plus headless VS Code and WASM runtime smoke
    LspMslCompletionTimings(VerifyEditorRuntimeArgs),
    /// Real browser-hosted WASM editor MSL smoke check
    WasmEditorMsl,
    /// Real VS Code and WASM editor MSL smoke checks
    EditorMsl(VerifyEditorRuntimeArgs),
    /// Full local/CI verification suite, including the long MSL parity gate
    Full(VerifySuiteArgs),
    /// Risk-focused verification suite for local development
    Quick(VerifySuiteArgs),
    /// Verify the primary binaries build
    Binaries,
    /// Rustdoc/docs gate with warnings denied
    Docs,
    /// Full MSL/OMC parity gate harness
    MslParity(Box<VerifyMslParityArgs>),
    /// Generate real flamegraph SVGs for the hottest compile and sim models from the latest MSL run
    MslHotspots,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VerifyStep {
    label: &'static str,
    args: &'static [&'static str],
    include_in_full: bool,
    include_in_quick: bool,
}

const VERIFY_SUITE_STEPS: &[VerifyStep] = &[
    // Lint runs first so deterministic policy failures surface before the
    // heavier test/build steps.
    VerifyStep {
        label: "lint",
        args: &["verify", "lint"],
        include_in_full: true,
        include_in_quick: true,
    },
    // MSL parity is the highest-signal gate for compiler/simulator changes, so
    // it runs before lower-risk heavyweight surfaces.
    VerifyStep {
        label: "MSL parity",
        args: &["verify", "msl-parity"],
        include_in_full: true,
        include_in_quick: true,
    },
    VerifyStep {
        label: "architecture/file-size gates",
        args: &["verify", "architecture"],
        include_in_full: true,
        include_in_quick: true,
    },
    VerifyStep {
        label: "workspace tests",
        args: &["verify", "workspace"],
        include_in_full: true,
        include_in_quick: true,
    },
    VerifyStep {
        label: "example smoke tests",
        args: &["verify", "examples"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "binary build",
        args: &["verify", "binaries"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "template runtime checks",
        args: &["verify", "template-runtimes"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "coverage run",
        args: &["coverage", "run"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "coverage report",
        args: &["coverage", "report"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "coverage gate",
        args: &[
            "coverage",
            "gate",
            "--allowed-workspace-line-coverage-drop",
            "3.0",
        ],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "docs",
        args: &["verify", "docs"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "VS Code gate",
        args: &["vscode", "test"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "WASM gate",
        args: &["wasm", "test"],
        include_in_full: true,
        include_in_quick: false,
    },
    VerifyStep {
        label: "full-MSL LSP/editor gate",
        args: &["verify", "lsp-msl-completion-timings"],
        include_in_full: true,
        include_in_quick: false,
    },
];

const WASM_SMOKE_SERVER_READY_PATH: &str = "/editors/wasm/index.html";
const WASM_SMOKE_SERVER_START_ATTEMPTS: usize = 3;
const WASM_SMOKE_SERVER_START_TIMEOUT: Duration = Duration::from_secs(20);
const MSL_RESOURCE_CPU_SAMPLE_INTERVAL: Duration = Duration::from_secs(30);
/// Floor on how often the *periodic* resource snapshot is printed. The monitor
/// loop still wakes on the (smaller) configured interval so shutdown stays
/// responsive, but the verbose snapshot is throttled to at most once per this
/// window to avoid flooding the long MSL-gate log.
const MSL_RESOURCE_PERIODIC_MIN_INTERVAL: Duration = Duration::from_secs(120);

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
            Self::Full => step.include_in_full,
            Self::Quick => step.include_in_quick,
        }
    }
}

pub(crate) fn run(args: VerifyArgs, root: &Path) -> Result<()> {
    match args.command {
        VerifyCommand::Lint => run_lint_job(root),
        VerifyCommand::Workspace => test_cmd::run_workspace_tests(root),
        VerifyCommand::TemplateRuntimes => run_template_runtime_checks(root),
        VerifyCommand::Examples => run_examples_smoke(root),
        VerifyCommand::VscodeMsl(args) => run_vscode_editor_msl_smoke(root, args.install_prereqs),
        VerifyCommand::LspMslCompletionTimings(args) => {
            run_lsp_msl_completion_timings(root, args.install_prereqs, args.require_runtimes)
        }
        VerifyCommand::WasmEditorMsl => run_wasm_editor_msl_smoke(root),
        VerifyCommand::EditorMsl(args) => run_editor_msl_smoke(root, args.install_prereqs),
        VerifyCommand::Architecture => test_cmd::run_architecture_gates(root),
        VerifyCommand::Full(args) => run_verify_suite(root, VerifySuite::Full, args.early_exit),
        VerifyCommand::Quick(args) => run_verify_suite(root, VerifySuite::Quick, args.early_exit),
        VerifyCommand::Binaries => test_cmd::run_workspace_binary_build(root),
        VerifyCommand::Docs => test_cmd::run_workspace_docs(root),
        VerifyCommand::MslParity(args) => run_msl_quality_gate(root, &args),
        VerifyCommand::MslHotspots => run_msl_hotspot_flamegraphs(root),
    }
}

fn run_examples_smoke(root: &Path) -> Result<()> {
    modelica_dependency_cache::ensure_example_libraries(root, false)?;
    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--package")
        .arg("rumoca")
        .arg("--features")
        .arg("examples-smoke-tests")
        .arg("--test")
        .arg("examples_smoke")
        .arg("--")
        .arg("--nocapture")
        .current_dir(root);
    run_status(cmd)
}

#[derive(Debug, serde::Deserialize)]
struct MslHotspotSummary {
    model_results: Vec<MslHotspotModelResult>,
}

#[derive(Debug, serde::Deserialize)]
struct MslHotspotModelResult {
    model_name: String,
    #[serde(default)]
    compile_seconds: Option<f64>,
    #[serde(default)]
    sim_wall_seconds: Option<f64>,
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

fn latest_msl_results_path(root: &Path) -> PathBuf {
    root.join("target/msl/results/msl_results.json")
}

fn load_latest_msl_hotspot_summary(root: &Path) -> Result<MslHotspotSummary> {
    let results_path = latest_msl_results_path(root);
    let raw = fs::read_to_string(&results_path)
        .with_context(|| format!("failed to read {}", results_path.display()))?;
    serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse {}", results_path.display()))
}

fn run_msl_hotspot_flamegraphs(root: &Path) -> Result<()> {
    let summary = load_latest_msl_hotspot_summary(root).with_context(|| {
        format!(
            "missing hotspot source data. Run the full MSL parity test first so {} exists.",
            latest_msl_results_path(root).display()
        )
    })?;
    let source_root = cached_msl_source_root(root)?;

    let Some((compile_model, compile_seconds)) = hottest_compile_model(&summary) else {
        anyhow::bail!("latest MSL results did not contain per-model compile timings");
    };
    println!(
        "Generating compile flamegraph for hottest model: {} ({:.2}s)",
        compile_model, compile_seconds
    );
    msl_flamegraph_cmd::run(
        msl_flamegraph_cmd::MslFlamegraphArgs {
            model: compile_model.to_string(),
            mode: msl_flamegraph_cmd::MslFlamegraphMode::Compile,
            source_root: Some(source_root.clone()),
            output: None,
            freq: 99,
            no_inline: false,
            stop_time: None,
        },
        root,
    )?;

    let Some((sim_model, sim_seconds)) = hottest_sim_model(&summary) else {
        anyhow::bail!("latest MSL results did not contain per-model simulation timings");
    };
    println!(
        "Generating simulation flamegraph for hottest model: {} ({:.2}s)",
        sim_model, sim_seconds
    );
    msl_flamegraph_cmd::run(
        msl_flamegraph_cmd::MslFlamegraphArgs {
            model: sim_model.to_string(),
            mode: msl_flamegraph_cmd::MslFlamegraphMode::Simulate,
            source_root: Some(source_root),
            output: None,
            freq: 99,
            no_inline: false,
            stop_time: None,
        },
        root,
    )
}

fn run_template_runtime_checks(root: &Path) -> Result<()> {
    trim_template_runtime_artifacts(root)?;

    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--verbose")
        .arg("-p")
        .arg("rumoca")
        .arg("--features")
        .arg("template-runtime-tests")
        .arg("--test")
        .arg("template_target_ci")
        .arg("--test")
        .arg("standalone_template_regression")
        .arg("--test")
        .arg("sympy_template_regression")
        .arg("--test")
        .arg("symforce_template_regression")
        .arg("--test")
        .arg("backend_template_runtime_regression")
        .arg("--")
        .arg("--nocapture")
        .current_dir(root);
    run_status(cmd)?;
    trim_template_runtime_artifacts(root)
}

fn trim_template_runtime_artifacts(root: &Path) -> Result<()> {
    let target_dir = cargo_target_dir(root);
    remove_dir_if_exists(&target_dir.join("debug").join("incremental"))?;

    let deps_dir = target_dir.join("debug").join("deps");
    if !deps_dir.is_dir() {
        return Ok(());
    }

    let test_stems = [
        "template_target_ci",
        "standalone_template_regression",
        "sympy_template_regression",
        "symforce_template_regression",
        "backend_template_runtime_regression",
    ];
    for entry in fs::read_dir(&deps_dir)
        .with_context(|| format!("read Cargo deps directory {}", deps_dir.display()))?
    {
        let entry = entry.with_context(|| format!("read entry in {}", deps_dir.display()))?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if test_stems.iter().any(|stem| name.starts_with(stem)) {
            remove_path(entry.path())?;
        }
    }
    Ok(())
}

fn cargo_target_dir(root: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .map(|path| {
            if path.is_absolute() {
                path
            } else {
                root.join(path)
            }
        })
        .unwrap_or_else(|| root.join("target"))
}

fn remove_dir_if_exists(path: &Path) -> Result<()> {
    if path.is_dir() {
        fs::remove_dir_all(path).with_context(|| format!("remove {}", path.display()))?;
    }
    Ok(())
}

fn remove_path(path: PathBuf) -> Result<()> {
    if path.is_dir() {
        fs::remove_dir_all(&path).with_context(|| format!("remove {}", path.display()))?;
    } else if path.exists() {
        fs::remove_file(&path).with_context(|| format!("remove {}", path.display()))?;
    }
    Ok(())
}

fn run_editor_msl_smoke(root: &Path, install_prereqs: bool) -> Result<()> {
    run_vscode_editor_msl_smoke(root, install_prereqs)?;
    run_wasm_editor_msl_smoke(root)
}

fn run_vscode_editor_msl_smoke(root: &Path, install_prereqs: bool) -> Result<()> {
    let msl_root = cached_msl_source_root(root)?;
    vscode_cmd::run_vscode_msl_smoke(root, &msl_root, install_prereqs)
}

fn run_wasm_editor_msl_smoke(root: &Path) -> Result<()> {
    let msl_root = cached_msl_source_root(root)?;
    run_wasm_browser_msl_smoke(root, &msl_root)
}

fn run_verify_suite(root: &Path, suite: VerifySuite, early_exit: bool) -> Result<()> {
    println!("Running `{}` suite...", suite.label());
    let steps: Vec<&VerifyStep> = VERIFY_SUITE_STEPS
        .iter()
        .filter(|step| suite.includes(step))
        .collect();

    // Default: run every step to the end so a single early failure does not
    // hide later results (and force a full re-run). `--early-exit` restores the
    // fail-fast behavior. Either way the suite fails if any step failed.
    let mut failures: Vec<(&str, anyhow::Error)> = Vec::new();
    let mut timing_steps = Vec::new();
    let suite_started = Instant::now();
    for step in steps {
        let step_outcome = run_timed_rum_step(root, step);
        timing_steps.push(step_outcome.timing);
        if let Some(error) = step_outcome.error {
            if early_exit {
                failures.push((step.label, error));
                break;
            }
            eprintln!("verify step `{}` FAILED: {error:#}", step.label);
            failures.push((step.label, error));
        }
    }
    let timing_report = VerifyTimingReport::new(suite, suite_started.elapsed(), timing_steps);
    write_verify_timing_report(root, &timing_report)?;

    if failures.is_empty() {
        println!("`{}` suite passed.", suite.label());
        return Ok(());
    }
    if early_exit && failures.len() == 1 {
        let (label, error) = failures.remove(0);
        return Err(error.context(format!("verify step `{label}` failed")));
    }

    let labels = failures
        .iter()
        .map(|(label, _)| *label)
        .collect::<Vec<_>>()
        .join(", ");
    anyhow::bail!(
        "`{}` suite failed: {} step(s) failed: {}",
        suite.label(),
        failures.len(),
        labels
    );
}

struct VerifyStepOutcome {
    timing: VerifyTimingStep,
    error: Option<anyhow::Error>,
}

fn run_timed_rum_step(root: &Path, step: &VerifyStep) -> VerifyStepOutcome {
    let started = Instant::now();
    match run_rum_step(root, step) {
        Ok(()) => VerifyStepOutcome {
            timing: VerifyTimingStep::new(step, "pass", started.elapsed()),
            error: None,
        },
        Err(error) => VerifyStepOutcome {
            timing: VerifyTimingStep::new(step, "fail", started.elapsed()),
            error: Some(error),
        },
    }
}

fn run_rum_step(root: &Path, step: &VerifyStep) -> Result<()> {
    println!(
        "Running {}: `cargo xtask {}`",
        step.label,
        step.args.join(" ")
    );
    let xtask_exe = resolve_xtask_cli_executable(root)?;
    let mut cmd = Command::new(xtask_exe);
    cmd.args(step.args).current_dir(root);
    run_status(cmd)
}

#[derive(Debug, Serialize)]
struct VerifyTimingReport {
    suite: String,
    success: bool,
    total_elapsed_seconds: f64,
    steps: Vec<VerifyTimingStep>,
}

impl VerifyTimingReport {
    fn new(suite: VerifySuite, elapsed: Duration, steps: Vec<VerifyTimingStep>) -> Self {
        Self {
            suite: suite.label().to_string(),
            success: steps.iter().all(|step| step.status == "pass"),
            total_elapsed_seconds: elapsed_seconds(elapsed),
            steps,
        }
    }
}

#[derive(Debug, Serialize)]
struct VerifyTimingStep {
    label: String,
    command: String,
    status: String,
    elapsed_seconds: f64,
}

impl VerifyTimingStep {
    fn new(step: &VerifyStep, status: &str, elapsed: Duration) -> Self {
        Self {
            label: step.label.to_string(),
            command: format!("cargo xtask {}", step.args.join(" ")),
            status: status.to_string(),
            elapsed_seconds: elapsed_seconds(elapsed),
        }
    }
}

fn elapsed_seconds(elapsed: Duration) -> f64 {
    elapsed.as_secs_f64()
}

fn write_verify_timing_report(root: &Path, report: &VerifyTimingReport) -> Result<()> {
    let output_dir = root.join("target/verify-timings");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    let stem = report
        .suite
        .strip_prefix("verify ")
        .unwrap_or(report.suite.as_str());
    let json_path = output_dir.join(format!("{stem}.json"));
    let markdown_path = output_dir.join(format!("{stem}.md"));
    let json = serde_json::to_string_pretty(report).context("failed to serialize verify timing")?;
    fs::write(&json_path, json)
        .with_context(|| format!("failed to write {}", json_path.display()))?;
    fs::write(&markdown_path, render_verify_timing_markdown(report))
        .with_context(|| format!("failed to write {}", markdown_path.display()))?;
    println!(
        "Verify timing report written to {} and {}",
        json_path.display(),
        markdown_path.display()
    );
    Ok(())
}

fn render_verify_timing_markdown(report: &VerifyTimingReport) -> String {
    let mut lines = vec![
        format!("# {}", report.suite),
        String::new(),
        format!("- success: {}", report.success),
        format!(
            "- total_elapsed_seconds: {:.3}",
            report.total_elapsed_seconds
        ),
        String::new(),
        "| Step | Status | Seconds | Command |".to_string(),
        "|---|---:|---:|---|".to_string(),
    ];
    for step in &report.steps {
        lines.push(format!(
            "| {} | {} | {:.3} | `{}` |",
            step.label, step.status, step.elapsed_seconds, step.command
        ));
    }
    lines.push(String::new());
    lines.join("\n")
}

fn cached_msl_source_root(root: &Path) -> Result<PathBuf> {
    let msl_root = root.join(format!("target/msl/ModelicaStandardLibrary-{MSL_VERSION}"));
    if msl_cache_layout_valid(&msl_root) {
        return Ok(msl_root);
    }

    download_standard_msl_release(&msl_root)?;
    Ok(msl_root)
}

fn msl_cache_layout_valid(msl_root: &Path) -> bool {
    msl_root.join("Complex.mo").is_file()
        && msl_root
            .join(MSL_MODELICA_DIR_NAME)
            .join("package.mo")
            .is_file()
        && msl_root
            .join(MSL_MODELICA_SERVICES_DIR_NAME)
            .join("package.mo")
            .is_file()
}

fn download_standard_msl_release(msl_root: &Path) -> Result<()> {
    println!(
        "MSL {MSL_VERSION} cache missing or incomplete at {}; downloading official release zip...",
        msl_root.display()
    );

    let response = ureq::get(MSL_RELEASE_ZIP_URL)
        .call()
        .with_context(|| format!("failed to download MSL release from {MSL_RELEASE_ZIP_URL}"))?;
    let content_len = response
        .header("content-length")
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(0);
    let mut data = Vec::with_capacity(content_len);
    response
        .into_reader()
        .read_to_end(&mut data)
        .context("failed to read MSL release zip response")?;

    if msl_root.exists() {
        fs::remove_dir_all(msl_root)
            .with_context(|| format!("failed to remove stale MSL cache {}", msl_root.display()))?;
    }
    fs::create_dir_all(msl_root)
        .with_context(|| format!("failed to create MSL cache {}", msl_root.display()))?;
    extract_msl_release_zip(&data, msl_root)?;

    ensure!(
        msl_cache_layout_valid(msl_root),
        "downloaded MSL cache at {} is incomplete; expected Complex.mo, {}, and {}",
        msl_root.display(),
        msl_root
            .join(MSL_MODELICA_DIR_NAME)
            .join("package.mo")
            .display(),
        msl_root
            .join(MSL_MODELICA_SERVICES_DIR_NAME)
            .join("package.mo")
            .display()
    );
    println!("MSL {MSL_VERSION} cached at {}", msl_root.display());
    Ok(())
}

fn extract_msl_release_zip(data: &[u8], msl_root: &Path) -> Result<()> {
    let cursor = Cursor::new(data);
    let mut archive = zip::ZipArchive::new(cursor).context("failed to open MSL release zip")?;

    for index in 0..archive.len() {
        let mut entry = archive
            .by_index(index)
            .with_context(|| format!("failed to read MSL zip entry {index}"))?;
        let relative_path = entry
            .enclosed_name()
            .with_context(|| format!("invalid MSL zip entry path: {}", entry.name()))?;
        let output_path = msl_root.join(relative_path);

        if entry.is_dir() {
            fs::create_dir_all(&output_path).with_context(|| {
                format!("failed to create MSL directory {}", output_path.display())
            })?;
            continue;
        }

        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create MSL directory {}", parent.display()))?;
        }
        let mut output = fs::File::create(&output_path)
            .with_context(|| format!("failed to create MSL file {}", output_path.display()))?;
        std::io::copy(&mut entry, &mut output)
            .with_context(|| format!("failed to extract MSL file {}", output_path.display()))?;
    }

    Ok(())
}

fn run_lsp_msl_completion_timings(
    root: &Path,
    install_prereqs: bool,
    require_runtimes: bool,
) -> Result<()> {
    let msl_root = cached_msl_source_root(root)?;
    lsp_benchmark_cmd::run_lsp_msl_completion_timings(
        root,
        &msl_root,
        install_prereqs,
        require_runtimes,
    )
}

pub(crate) fn can_launch_wasm_browser_msl_smoke() -> bool {
    command_exists("node") && detect_browser_binary().is_ok() && can_bind_local_browser_port()
}

fn run_wasm_browser_msl_smoke(root: &Path, msl_root: &Path) -> Result<()> {
    let output_dir = root.join("target/editor-msl-smoke");
    let _ = run_wasm_browser_msl_smoke_report(root, msl_root, &output_dir)?;
    Ok(())
}

pub(crate) fn run_wasm_browser_msl_smoke_report(
    root: &Path,
    msl_root: &Path,
    output_dir: &Path,
) -> Result<WasmSmokeSummary> {
    prepare_wasm_browser_smoke_assets(root, msl_root)?;
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    let (port, _child_guard) = start_wasm_smoke_server(root)?;

    let wasm_dir = root.join("editors/wasm");
    ensure_wasm_browser_smoke_npm_dependencies(&wasm_dir)?;
    let browser = detect_browser_binary()?;
    let result_path = output_dir.join("wasm-browser-result.json");
    let smoke_model = "SmokeHarness";
    let smoke_url = format!(
        "http://127.0.0.1:{port}/editors/wasm/index.html?rumoca_smoke=1&smoke_model={smoke_model}&smoke_source_url=/target/editor-msl-smoke/SmokeHarness.mo&smoke_package_archive_url=/target/editor-msl-smoke/msl-slice.zip&smoke_compile_timeout_ms=300000"
    );
    let mut smoke = Command::new("node");
    smoke
        .arg("tests/run_browser_msl_smoke.mjs")
        .arg("--browser-binary")
        .arg(&browser)
        .arg("--smoke-url")
        .arg(&smoke_url)
        .arg("--smoke-result")
        .arg(&result_path)
        .current_dir(&wasm_dir);
    run_status_quiet(smoke)
        .with_context(|| "failed to launch Playwright-driven wasm editor smoke".to_string())?;
    let callback = fs::read_to_string(&result_path)
        .with_context(|| format!("failed to read wasm smoke result {}", result_path.display()))
        .and_then(|raw| {
            serde_json::from_str::<WasmSmokeCallback>(&raw)
                .context("failed to parse wasm smoke result JSON")
        })?;
    anyhow::ensure!(
        callback.status == "pass",
        "wasm browser smoke reported '{}' with payload {}",
        callback.status,
        serde_json::to_string_pretty(&callback.payload)
            .unwrap_or_else(|_| String::from("<unserializable>"))
    );
    Ok(callback.payload)
}

fn resolve_xtask_cli_executable(root: &Path) -> Result<PathBuf> {
    let repo_debug_bin = root.join("target/debug").join(exe_name("xtask"));
    if repo_debug_bin.is_file() {
        return Ok(repo_debug_bin);
    }
    std::env::current_exe().context("failed to resolve current xtask executable")
}

fn ensure_wasm_browser_smoke_npm_dependencies(wasm_dir: &Path) -> Result<()> {
    let playwright_package = wasm_dir.join("node_modules/playwright-core/package.json");
    if playwright_package.is_file() {
        return Ok(());
    }

    println!("Installing WASM browser smoke npm dependencies...");
    let mut npm = Command::new("npm");
    if wasm_dir.join("package-lock.json").is_file() {
        npm.arg("ci");
    } else {
        npm.arg("install");
    }
    npm.current_dir(wasm_dir);
    run_status(npm)
}

fn prepare_wasm_browser_smoke_assets(root: &Path, msl_root: &Path) -> Result<PathBuf> {
    let smoke_dir = root.join("target/editor-msl-smoke");
    if smoke_dir.exists() {
        fs::remove_dir_all(&smoke_dir)
            .with_context(|| format!("failed to remove {}", smoke_dir.display()))?;
    }
    let modelica_dir = smoke_dir.join("Modelica");
    let services_dir = smoke_dir.join("ModelicaServices");
    copy_dir_recursive(&msl_root.join("Modelica 4.1.0"), &modelica_dir)?;
    copy_dir_recursive(&msl_root.join("ModelicaServices 4.1.0"), &services_dir)?;
    fs::copy(msl_root.join("Complex.mo"), smoke_dir.join("Complex.mo"))
        .with_context(|| "failed to stage Complex.mo for wasm smoke".to_string())?;
    fs::write(smoke_dir.join("SmokeHarness.mo"), WASM_BROWSER_SMOKE_SOURCE)
        .with_context(|| "failed to stage browser smoke source".to_string())?;

    let mut zip = Command::new("zip");
    zip.arg("-qr")
        .arg("msl-slice.zip")
        .arg("Modelica")
        .arg("ModelicaServices")
        .arg("Complex.mo")
        .current_dir(&smoke_dir);
    run_status(zip)?;

    Ok(smoke_dir)
}

const WASM_BROWSER_SMOKE_SOURCE: &str = r#"model SmokeHarness
  Modelica.Electrical.Analog.Sources.ConstantVoltage source(V=1);
  Modelica.Electrical.Analog.Basic.Resistor resistor(R=1);
  Modelica.Electrical.Analog.Basic.Ground ground;
equation
  connect(source.p, resistor.p);
  connect(resistor.n, ground.p);
  connect(source.n, ground.p);
end SmokeHarness;
"#;

fn copy_dir_recursive(source: &Path, dest: &Path) -> Result<()> {
    anyhow::ensure!(
        source.is_dir(),
        "missing directory required for wasm smoke: {}",
        source.display()
    );
    for entry in walkdir::WalkDir::new(source) {
        let entry = entry
            .with_context(|| format!("failed to walk source directory {}", source.display()))?;
        let relative = entry.path().strip_prefix(source).with_context(|| {
            format!(
                "failed to strip source prefix {} from {}",
                source.display(),
                entry.path().display()
            )
        })?;
        let target = dest.join(relative);
        if entry.file_type().is_dir() {
            fs::create_dir_all(&target)
                .with_context(|| format!("failed to create {}", target.display()))?;
            continue;
        }
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::copy(entry.path(), &target).with_context(|| {
            format!(
                "failed to copy '{}' to '{}'",
                entry.path().display(),
                target.display()
            )
        })?;
    }
    Ok(())
}

fn reserve_local_port() -> Result<u16> {
    let listener = TcpListener::bind(("127.0.0.1", 0)).context("failed to bind local port")?;
    let port = listener
        .local_addr()
        .context("failed to inspect reserved port")?
        .port();
    drop(listener);
    Ok(port)
}

fn can_bind_local_browser_port() -> bool {
    TcpListener::bind(("127.0.0.1", 0)).is_ok()
}

fn start_wasm_smoke_server(root: &Path) -> Result<(u16, ChildGuard)> {
    let xtask_exe = resolve_xtask_cli_executable(root)?;
    println!("Prebuilding WASM module for browser smoke...");
    let mut build = Command::new(&xtask_exe);
    build
        .arg("wasm")
        .arg("build")
        // Browser smoke validates the default editor/runtime flow on the
        // non-threaded package (the build default, no --rayon).
        .current_dir(root);
    run_status(build).with_context(|| {
        "failed to prebuild `cargo xtask wasm build` for browser smoke".to_string()
    })?;
    let mut last_error = None;

    for attempt in 1..=WASM_SMOKE_SERVER_START_ATTEMPTS {
        let port = reserve_local_port()?;
        let mut server = Command::new(&xtask_exe);
        server
            .arg("wasm")
            .arg("edit")
            .arg("--skip-build")
            .arg("--port")
            .arg(port.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .current_dir(root);
        let child = server
            .spawn()
            .context("failed to launch `cargo xtask wasm edit` for browser smoke")?;
        let mut child_guard = ChildGuard::new(child);

        match wait_for_http_ready(
            &mut child_guard,
            port,
            WASM_SMOKE_SERVER_READY_PATH,
            WASM_SMOKE_SERVER_START_TIMEOUT,
        ) {
            Ok(()) => return Ok((port, child_guard)),
            Err(error) => last_error = Some(format!("attempt {attempt}: {error:#}")),
        }
    }

    anyhow::bail!(
        "failed to start wasm smoke server after {} attempts: {}",
        WASM_SMOKE_SERVER_START_ATTEMPTS,
        last_error.unwrap_or_else(|| "unknown startup failure".to_string())
    )
}

fn wait_for_http_ready(
    child_guard: &mut ChildGuard,
    port: u16,
    path: &str,
    timeout: Duration,
) -> Result<()> {
    let started = Instant::now();
    while started.elapsed() < timeout {
        if http_get_ok(port, path) {
            return Ok(());
        }
        if child_guard.has_exited()? {
            anyhow::bail!(
                "wasm smoke server exited before responding on http://127.0.0.1:{port}{path}"
            );
        }
        thread::sleep(Duration::from_millis(250));
    }
    anyhow::bail!("timed out waiting for wasm smoke server on http://127.0.0.1:{port}{path}")
}

fn http_get_ok(port: u16, path: &str) -> bool {
    let Ok(mut stream) = TcpStream::connect(("127.0.0.1", port)) else {
        return false;
    };
    let request =
        format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n\r\n");
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut response = String::new();
    if stream.read_to_string(&mut response).is_err() {
        return false;
    }
    response.starts_with("HTTP/1.1 200")
}

fn detect_browser_binary() -> Result<String> {
    ["google-chrome", "chromium", "chromium-browser"]
        .into_iter()
        .find(|program| {
            Command::new(program)
                .arg("--version")
                .output()
                .is_ok_and(|output| output.status.success())
        })
        .map(ToOwned::to_owned)
        .context("missing browser for wasm smoke (expected google-chrome/chromium)")
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WasmSmokeSummary {
    pub(crate) model_name: Option<String>,
    pub(crate) source_root_count: Option<u64>,
    pub(crate) status_text: Option<String>,
    pub(crate) open_ms: Option<u64>,
    pub(crate) code_lens_ms: Option<u64>,
    pub(crate) code_lens_count: Option<u64>,
    pub(crate) archive_load_ms: Option<u64>,
    pub(crate) source_root_load_ms: Option<u64>,
    pub(crate) source_root_load_completion_count: Option<u64>,
    pub(crate) source_root_expected_completion_present: Option<bool>,
    pub(crate) source_root_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) compile_ms: Option<u64>,
    pub(crate) completion_ms: Option<u64>,
    pub(crate) completion_count: Option<u64>,
    pub(crate) expected_completion_present: Option<bool>,
    pub(crate) cold_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) warm_completion_ms: Option<u64>,
    pub(crate) warm_completion_count: Option<u64>,
    pub(crate) warm_expected_completion_present: Option<bool>,
    pub(crate) warm_stage_timings: Option<vscode_cmd::VscodeStageTimingSummary>,
    pub(crate) hover_ms: Option<u64>,
    pub(crate) hover_count: Option<u64>,
    pub(crate) expected_hover_present: Option<bool>,
    pub(crate) definition_ms: Option<u64>,
    pub(crate) definition_count: Option<u64>,
    pub(crate) expected_definition_present: Option<bool>,
    pub(crate) cross_file_definition_present: Option<bool>,
    pub(crate) error: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
struct WasmSmokeCallback {
    status: String,
    payload: WasmSmokeSummary,
}

struct ChildGuard {
    child: Option<Child>,
}

impl ChildGuard {
    fn new(child: Child) -> Self {
        Self { child: Some(child) }
    }

    fn has_exited(&mut self) -> Result<bool> {
        let Some(child) = self.child.as_mut() else {
            return Ok(true);
        };
        Ok(child.try_wait()?.is_some())
    }
}

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let Some(mut child) = self.child.take() else {
            return;
        };
        let _ = child.kill();
        let _ = child.wait();
    }
}

fn run_lint_policy_checks(root: &Path) -> Result<()> {
    let fmt_root = root.to_path_buf();
    let fmt_check = thread::spawn(move || test_cmd::run_workspace_fmt_check(&fmt_root));
    let policy_checks = thread::spawn(xtask::run_traversal_policy_check);

    join_lint_policy_check("rustfmt", fmt_check)?;
    join_lint_policy_check("policy checks", policy_checks)
}

fn join_lint_policy_check(label: &str, handle: JoinHandle<Result<()>>) -> Result<()> {
    handle
        .join()
        .unwrap_or_else(|_| anyhow::bail!("{label} panicked"))?;
    Ok(())
}

fn run_lint_job(root: &Path) -> Result<()> {
    run_lint_policy_checks(root)?;
    test_cmd::run_workspace_clippy(root)
}

fn run_msl_quality_gate(root: &Path, args: &VerifyMslParityArgs) -> Result<()> {
    let ci_env = MslCiEnvironment::from_args(root, args);
    ci_env.print_notice();
    ci_env.clean_stale_results()?;
    write_parity_config(root, args)?;
    let _cleanup = MslResultsCleanupGuard::new(ci_env.results_dir.clone(), ci_env.clean_results);
    let _monitor = MslResourceMonitor::start(ci_env.clone());
    let mut cargo_setup_steps = Vec::new();

    let result = run_msl_quality_gate_cargo_commands(root, &mut cargo_setup_steps);
    let write_result = write_msl_cargo_setup_timing_report(&ci_env.results_dir, &cargo_setup_steps);
    if result.is_ok() {
        write_result?;
    } else if let Err(error) = write_result {
        eprintln!("failed to write MSL Cargo setup timing report: {error:#}");
    }
    result
}

fn run_msl_quality_gate_cargo_commands(
    root: &Path,
    cargo_setup_steps: &mut Vec<MslCargoSetupTimingStep>,
) -> Result<()> {
    let target_dir = cargo_target_dir(root);
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
    let result = run_msl_cargo_setup_step(
        cargo_setup_steps,
        MslCargoSetupStepMetadata::new(
            "build rumoca-sim-worker",
            "build",
            "rumoca-test-msl",
            "release",
            Vec::new(),
            &target_dir,
        ),
        build_sim_worker,
    );
    result?;

    let mut build_msl_tools = Command::new("cargo");
    build_msl_tools
        .arg("build")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("xtask")
        .arg("--bin")
        .arg("rumoca-msl-tools")
        .current_dir(root);
    let result = run_msl_cargo_setup_step(
        cargo_setup_steps,
        MslCargoSetupStepMetadata::new(
            "build rumoca-msl-tools",
            "build",
            "xtask",
            "release",
            Vec::new(),
            &target_dir,
        ),
        build_msl_tools,
    );
    result?;

    let mut gate = Command::new("cargo");
    gate.arg("test")
        .arg("--verbose")
        .arg("--release")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--features")
        .arg("msl-full-test")
        .arg("--test")
        .arg("msl_tests")
        .arg("balance_pipeline::balance_pipeline_core::test_msl_all")
        .arg("--")
        .arg("--nocapture")
        .env("RUST_BACKTRACE", "full")
        .current_dir(root);
    run_msl_cargo_setup_step(
        cargo_setup_steps,
        MslCargoSetupStepMetadata::new(
            "run release MSL test",
            "test",
            "rumoca-test-msl",
            "release",
            vec!["msl-full-test".to_string()],
            &target_dir,
        ),
        gate,
    )
}

#[derive(Debug, Clone)]
struct MslCiEnvironment {
    root: std::path::PathBuf,
    results_dir: std::path::PathBuf,
    monitor_interval: Option<Duration>,
    clean_results: bool,
    github_actions: bool,
}

const MSL_RESULTS_PRESERVED_DIRS: &[&str] = &["omc_parity_cache"];

fn should_preserve_msl_results_entry(entry_path: &Path) -> bool {
    entry_path.is_dir()
        && entry_path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| MSL_RESULTS_PRESERVED_DIRS.contains(&name))
}

fn clean_msl_results_dir(results_dir: &Path) -> std::io::Result<()> {
    if !results_dir.is_dir() {
        return Ok(());
    }

    for entry in fs::read_dir(results_dir)? {
        let entry = entry?;
        let path = entry.path();
        if should_preserve_msl_results_entry(&path) {
            continue;
        }
        if path.is_dir() {
            fs::remove_dir_all(&path)?;
        } else {
            fs::remove_file(&path)?;
        }
    }

    if fs::read_dir(results_dir)?.next().is_none() {
        fs::remove_dir(results_dir)?;
    }

    Ok(())
}

impl MslCiEnvironment {
    fn from_args(root: &Path, args: &VerifyMslParityArgs) -> Self {
        let results_dir = args
            .results_dir
            .clone()
            .unwrap_or_else(|| root.join("target/msl/results"));
        Self {
            root: root.to_path_buf(),
            results_dir,
            monitor_interval: (args.monitor_interval_secs > 0)
                .then(|| Duration::from_secs(args.monitor_interval_secs)),
            clean_results: args.clean_results,
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
        clean_msl_results_dir(&self.results_dir).with_context(|| {
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
        if let Err(error) = clean_msl_results_dir(&self.results_dir) {
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
        print_resource_snapshot("initial", &config, true);
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
        print_resource_snapshot("final", &self.config, true);
    }
}

fn run_resource_monitor_loop(
    done_flag: Arc<AtomicBool>,
    interval: Duration,
    config: MslCiEnvironment,
) {
    let mut last_cpu_sample = Instant::now();
    let mut last_print: Option<Instant> = None;
    while !done_flag.load(Ordering::Relaxed) {
        thread::sleep(interval);
        if done_flag.load(Ordering::Relaxed) {
            break;
        }
        // Throttle the (verbose) periodic snapshot to the floor, independent of
        // the wake interval, so a small `--monitor-interval-secs` does not spam.
        if last_print.is_some_and(|at| at.elapsed() < MSL_RESOURCE_PERIODIC_MIN_INTERVAL) {
            continue;
        }
        let include_cpu = last_cpu_sample.elapsed() >= MSL_RESOURCE_CPU_SAMPLE_INTERVAL;
        print_resource_snapshot("periodic", &config, include_cpu);
        last_print = Some(Instant::now());
        if include_cpu {
            last_cpu_sample = Instant::now();
        }
    }
}

fn print_resource_snapshot(phase: &str, config: &MslCiEnvironment, include_cpu: bool) {
    // Periodic snapshots fire on a timer for the whole (long) MSL gate, so keep
    // them to a compact memory/disk pulse; the full breakdown (per-result-entry
    // sizes, inode usage, host facts) is only emitted for the initial/final ones.
    let concise = phase == "periodic";
    eprintln!("== MSL Resource Snapshot ({phase}) ==");
    if !concise {
        log_command_output("date", "date", ["-Is"]);
        eprintln!("workspace_root={}", config.root.display());
        if let Some(interval) = config.monitor_interval {
            eprintln!("resource_monitor_interval_secs={}", interval.as_secs());
        }
        log_command_output("nproc", "nproc", std::iter::empty::<&str>());
    }
    log_command_output("free -h", "free", ["-h"]);
    log_command_output("df -h", "df", ["-h", ".", "/tmp"]);
    log_path_size("target/msl", &config.root.join("target/msl"));
    log_path_size("msl_results", &config.results_dir);
    if !concise {
        log_command_output("uptime", "uptime", std::iter::empty::<&str>());
        log_command_output("df -ih", "df", ["-ih", ".", "/tmp"]);
        log_path_size("target", &config.root.join("target"));
        print_results_dir_summary("results-breakdown", &config.results_dir);
    }
    if !should_log_process_tables(config) {
        return;
    }
    log_top_processes("top-by-mem", "--sort=-%mem");
    if include_cpu {
        log_top_processes("top-by-cpu", "--sort=-%cpu");
    }
}

fn should_log_process_tables(config: &MslCiEnvironment) -> bool {
    !config.github_actions
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
        MslCargoSetupTimingStep, MslCiEnvironment, MslHotspotModelResult, MslHotspotSummary,
        VERIFY_SUITE_STEPS, VerifyMslParityArgs, VerifySuite, VerifyTimingReport, VerifyTimingStep,
        hottest_compile_model, hottest_sim_model, msl_cache_layout_valid,
        render_verify_timing_markdown, should_log_process_tables,
        write_msl_cargo_setup_timing_report, write_verify_timing_report,
    };
    use std::path::PathBuf;
    use std::time::Duration;

    fn step_argvs(suite: VerifySuite) -> Vec<Vec<&'static str>> {
        VERIFY_SUITE_STEPS
            .iter()
            .filter(|step| suite.includes(step))
            .map(|step| step.args.to_vec())
            .collect()
    }

    #[test]
    fn quick_suite_runs_format_tests_architecture_and_msl_parity() {
        let steps = step_argvs(VerifySuite::Quick);
        assert_eq!(
            steps,
            vec![
                vec!["verify", "lint"],
                vec!["verify", "msl-parity"],
                vec!["verify", "architecture"],
                vec!["verify", "workspace"],
            ]
        );
        assert!(!steps.contains(&vec!["verify", "examples"]));
        assert!(!steps.contains(&vec!["verify", "binaries"]));
        assert!(!steps.contains(&vec!["verify", "template-runtimes"]));
        assert!(!steps.contains(&vec!["verify", "docs"]));
        assert!(!steps.contains(&vec!["vscode", "test"]));
        assert!(!steps.contains(&vec!["coverage", "run"]));
        assert!(!steps.contains(&vec!["wasm", "test"]));
        assert!(!steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
    }

    #[test]
    fn full_suite_runs_msl_parity_before_lower_signal_heavy_gates() {
        let steps = step_argvs(VerifySuite::Full);
        assert_eq!(steps.get(1), Some(&vec!["verify", "msl-parity"]));
        assert!(steps.contains(&vec!["verify", "architecture"]));
        assert!(steps.contains(&vec!["verify", "workspace"]));
        assert!(steps.contains(&vec!["verify", "examples"]));
        assert!(steps.contains(&vec!["verify", "binaries"]));
        assert!(steps.contains(&vec!["verify", "template-runtimes"]));
        assert!(steps.contains(&vec!["coverage", "run"]));
        assert!(steps.contains(&vec!["verify", "lsp-msl-completion-timings"]));
        assert!(steps.contains(&vec!["verify", "msl-parity"]));
    }

    #[test]
    fn focused_msl_match_requires_selected_targets_success() {
        let args = VerifyMslParityArgs {
            sim_match: vec!["Modelica.Blocks.Examples.BooleanNetwork1".to_string()],
            sim_match_exact: true,
            ..VerifyMslParityArgs::default()
        };
        let config = args.to_parity_config_json();

        assert_eq!(
            config
                .get("require_selected_targets_success")
                .and_then(serde_json::Value::as_bool),
            Some(true)
        );
        assert_eq!(
            config
                .get("sim_match_exact")
                .and_then(serde_json::Value::as_bool),
            Some(true)
        );
    }

    #[test]
    fn verify_timing_markdown_preserves_step_order() {
        let report = VerifyTimingReport::new(
            VerifySuite::Quick,
            Duration::from_millis(1500),
            vec![
                VerifyTimingStep {
                    label: "lint".to_string(),
                    command: "cargo xtask verify lint".to_string(),
                    status: "pass".to_string(),
                    elapsed_seconds: 0.5,
                },
                VerifyTimingStep {
                    label: "workspace tests".to_string(),
                    command: "cargo xtask verify workspace".to_string(),
                    status: "fail".to_string(),
                    elapsed_seconds: 1.0,
                },
            ],
        );

        let markdown = render_verify_timing_markdown(&report);
        assert!(markdown.contains("# verify quick"));
        assert!(markdown.contains("- success: false"));
        assert!(
            markdown.find("| lint | pass | 0.500 |").unwrap()
                < markdown.find("| workspace tests | fail | 1.000 |").unwrap()
        );
    }

    #[test]
    fn verify_timing_report_writes_fixed_target_artifacts() {
        let root = tempfile::tempdir().expect("temp root");
        let report = VerifyTimingReport::new(
            VerifySuite::Quick,
            Duration::from_secs(1),
            vec![VerifyTimingStep {
                label: "lint".to_string(),
                command: "cargo xtask verify lint".to_string(),
                status: "pass".to_string(),
                elapsed_seconds: 1.0,
            }],
        );

        write_verify_timing_report(root.path(), &report).expect("write timing report");

        let json_path = root.path().join("target/verify-timings/quick.json");
        let markdown_path = root.path().join("target/verify-timings/quick.md");
        assert!(json_path.is_file());
        assert!(markdown_path.is_file());
        let json = std::fs::read_to_string(json_path).expect("read timing json");
        assert!(json.contains(r#""suite": "verify quick""#));
        let markdown = std::fs::read_to_string(markdown_path).expect("read timing markdown");
        assert!(markdown.contains("| lint | pass | 1.000 |"));
    }

    #[test]
    fn msl_cargo_setup_timing_report_writes_fixed_result_artifacts() {
        let root = tempfile::tempdir().expect("temp root");
        let results_dir = root.path().join("target/msl/results");
        let steps = vec![
            MslCargoSetupTimingStep {
                label: "build rumoca-sim-worker".to_string(),
                cargo_action: "build".to_string(),
                package: "rumoca-test-msl".to_string(),
                profile: "release".to_string(),
                features: Vec::new(),
                target_dir: root.path().join("target").display().to_string(),
                command: "\"cargo\" \"build\"".to_string(),
                status: "pass".to_string(),
                elapsed_seconds: 0.2,
            },
            MslCargoSetupTimingStep {
                label: "run release MSL test".to_string(),
                cargo_action: "test".to_string(),
                package: "rumoca-test-msl".to_string(),
                profile: "release".to_string(),
                features: vec!["msl-full-test".to_string()],
                target_dir: root.path().join("target").display().to_string(),
                command: "\"cargo\" \"test\"".to_string(),
                status: "fail".to_string(),
                elapsed_seconds: 1.3,
            },
        ];

        write_msl_cargo_setup_timing_report(&results_dir, &steps)
            .expect("write MSL Cargo setup timing report");

        let json_path = results_dir.join("msl_cargo_setup_timing.json");
        let markdown_path = results_dir.join("msl_cargo_setup_timing.md");
        assert!(json_path.is_file());
        assert!(markdown_path.is_file());
        let json = std::fs::read_to_string(json_path).expect("read setup timing json");
        assert!(json.contains(r#""success": false"#));
        assert!(json.contains(r#""label": "build rumoca-sim-worker""#));
        assert!(json.contains(r#""package": "rumoca-test-msl""#));
        assert!(json.contains(r#""features": ["#));
        let markdown = std::fs::read_to_string(markdown_path).expect("read setup timing markdown");
        assert!(markdown.contains("# MSL Cargo Setup Timing"));
        assert!(markdown.contains("| run release MSL test | fail | 1.300 | rumoca-test-msl |"));
        assert!(markdown.contains("| release | msl-full-test |"));
    }

    #[test]
    fn hotspot_selection_uses_max_compile_and_sim_wall_times() {
        let summary = MslHotspotSummary {
            model_results: vec![
                MslHotspotModelResult {
                    model_name: "A".to_string(),
                    compile_seconds: Some(1.5),
                    sim_wall_seconds: Some(8.0),
                },
                MslHotspotModelResult {
                    model_name: "B".to_string(),
                    compile_seconds: Some(3.0),
                    sim_wall_seconds: Some(2.0),
                },
                MslHotspotModelResult {
                    model_name: "C".to_string(),
                    compile_seconds: None,
                    sim_wall_seconds: Some(9.0),
                },
            ],
        };

        assert_eq!(hottest_compile_model(&summary), Some(("B", 3.0)));
        assert_eq!(hottest_sim_model(&summary), Some(("C", 9.0)));
    }

    #[test]
    fn msl_cache_layout_requires_editor_smoke_packages() {
        let temp = tempfile::tempdir().expect("tempdir");
        let msl_root = temp.path();
        std::fs::write(msl_root.join("Complex.mo"), "").expect("write Complex.mo");
        std::fs::create_dir_all(msl_root.join("Modelica 4.1.0")).expect("mkdir Modelica");
        std::fs::write(msl_root.join("Modelica 4.1.0/package.mo"), "")
            .expect("write Modelica package");

        assert!(
            !msl_cache_layout_valid(msl_root),
            "ModelicaServices is required by editor MSL smoke asset preparation"
        );

        std::fs::create_dir_all(msl_root.join("ModelicaServices 4.1.0"))
            .expect("mkdir ModelicaServices");
        std::fs::write(msl_root.join("ModelicaServices 4.1.0/package.mo"), "")
            .expect("write ModelicaServices package");

        assert!(msl_cache_layout_valid(msl_root));
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

    #[test]
    fn msl_ci_environment_preserves_keyed_omc_parity_cache() {
        let temp = tempfile::tempdir().expect("tempdir");
        let results_dir = temp.path().join("results");
        let parity_cache_dir = results_dir.join("omc_parity_cache");
        std::fs::create_dir_all(&parity_cache_dir).expect("mkdir parity cache");
        std::fs::write(results_dir.join("stale.json"), "{}").expect("write stale file");
        std::fs::write(parity_cache_dir.join("compile.json"), "{}").expect("write cache file");

        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: results_dir.clone(),
            monitor_interval: None,
            clean_results: true,
            github_actions: false,
        };
        env.clean_stale_results().expect("cleanup should succeed");

        assert!(
            results_dir.is_dir(),
            "results dir should remain when keyed parity cache is preserved"
        );
        assert!(
            parity_cache_dir.join("compile.json").is_file(),
            "cleanup should preserve keyed OMC parity cache contents"
        );
        assert!(
            !results_dir.join("stale.json").exists(),
            "cleanup should remove stale non-cache artifacts"
        );
    }

    #[test]
    fn msl_resource_snapshot_skips_process_tables_on_github_actions() {
        let temp = tempfile::tempdir().expect("tempdir");
        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: temp.path().join("results"),
            monitor_interval: None,
            clean_results: false,
            github_actions: true,
        };

        assert!(!should_log_process_tables(&env));
    }

    #[test]
    fn msl_resource_snapshot_keeps_process_tables_for_local_runs() {
        let temp = tempfile::tempdir().expect("tempdir");
        let env = MslCiEnvironment {
            root: PathBuf::from(temp.path()),
            results_dir: temp.path().join("results"),
            monitor_interval: None,
            clean_results: false,
            github_actions: false,
        };

        assert!(should_log_process_tables(&env));
    }
}
