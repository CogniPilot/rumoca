//! MSL parity-gate configuration, baseline selection, and harness execution.

mod msl_cargo_setup_timing;
mod msl_local_run;
mod msl_quality_baseline;
mod msl_results_cleanup;
mod parity_budgets;
mod parity_comparator;

use anyhow::{Context, Result};
use clap::Args;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use msl_cargo_setup_timing::{
    MslCargoSetupStepMetadata, MslCargoSetupTimingStep, run_msl_cargo_setup_step,
    run_msl_cargo_setup_step_with, write_msl_cargo_setup_timing_report,
};
use msl_local_run::{
    MSL_BUILD_PROFILE, MslTestBinaries, msl_test_binary_command, optimized_msl_artifact_build,
    run_optimized_msl_artifact_build,
};
use msl_quality_baseline::resolve_msl_quality_baseline;
use msl_results_cleanup::clean_msl_results_dir;
use parity_comparator::check_comparator_evidence;

const MSL_FULL_TEST_FEATURE: &str = "msl-full-test";
const MSL_RESOURCE_CPU_SAMPLE_INTERVAL: Duration = Duration::from_secs(30);
const MSL_RESOURCE_PERIODIC_MIN_INTERVAL: Duration = Duration::from_secs(120);

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub struct MslParityArgs {
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
    /// Per-model compile/simulation worker resident-plus-swap ceiling in MB
    #[arg(long)]
    model_worker_memory_mb: Option<usize>,
    /// Simulation worker count (default: at most four, memory-capped)
    #[arg(long)]
    sim_parallelism: Option<usize>,
    /// Per-sim-worker address-space cap in MB
    #[arg(long)]
    sim_worker_memory_mb: Option<usize>,
    /// Total simulation memory budget in MB (caps the sim worker count)
    #[arg(long)]
    sim_total_memory_mb: Option<usize>,
    #[command(flatten)]
    budgets: parity_budgets::MslParityBudgetArgs,
    /// Explicit MSL quality baseline JSON for baseline-relative gates
    #[arg(long)]
    quality_baseline: Option<PathBuf>,
    /// Use the checked-in MSL quality baseline instead of downloading the latest promoted asset
    #[arg(long)]
    no_remote_quality_baseline: bool,
    /// Run only shard `m` of `n` (`--shard m/n`, 1-based). The slowest-first
    /// model set is striped round-robin across shards so the slow/timeout tail
    /// spreads evenly. A shard skips the aggregate baseline ratchet; the fan-in
    /// `verify msl-parity --merge-shards <dir>` job runs the gate once on the
    /// merged results.
    #[arg(long, value_name = "M/N")]
    shard: Option<String>,
    /// Fan-in mode: merge the shard partials under DIR (`shard-*/msl_results.json`)
    /// into the full set and run the quality gate ONCE on the merged results.
    /// Used by the sharded MSL CI after all `--shard` jobs finish.
    #[arg(long, value_name = "DIR", conflicts_with = "shard")]
    merge_shards: Option<PathBuf>,
    /// Run a prebuilt `msl_tests` libtest binary (built once by Nix/crane and
    /// shared through CI) instead of recompiling the workspace. The gate does its
    /// normal config/baseline setup, then executes this binary directly — no
    /// `cargo test`, so no workspace compile + LTO in the consuming job.
    #[arg(long, value_name = "PATH")]
    prebuilt_test_binary: Option<PathBuf>,
    /// Path to the prebuilt `rumoca-worker` binary the harness should spawn for
    /// isolated per-model compile/lower runs. Required by CI prebuilt harnesses;
    /// local `cargo test` resolves it through Cargo's normal test-binary env.
    #[arg(long, value_name = "PATH", requires = "prebuilt_test_binary")]
    prebuilt_model_worker: Option<PathBuf>,
    /// Path to the prebuilt `rumoca-sim-worker` binary the harness should spawn
    /// (used with `--prebuilt-test-binary` for the sim-running jobs; the harness
    /// resolves it via `CARGO_BIN_EXE_rumoca-sim-worker`). Not needed for the
    /// fan-in merge, which runs no simulations.
    #[arg(long, value_name = "PATH", requires = "prebuilt_test_binary")]
    prebuilt_sim_worker: Option<PathBuf>,
    /// Accept a cohort-shaped run whose OMC comparator produced no agreement
    /// bands. The run still prints "parity unmeasured: comparator did not run"
    /// and still reports no parity number; this only stops that from failing
    /// the command. Without it, an unmeasured cohort run is an error.
    #[arg(long)]
    allow_unmeasured_parity: bool,
}

impl MslParityArgs {
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
        if let Some(value) = self.model_worker_memory_mb {
            config.insert("model_worker_memory_mb".into(), value.into());
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
        self.budgets.insert_into(&mut config);
        if let Some(value) = &self.quality_baseline {
            config.insert(
                "quality_baseline_file".into(),
                value.to_string_lossy().into_owned().into(),
            );
        }
        // Validated by `parse_shard` (called on the run path before this), so a
        // malformed `--shard` never reaches here as a silent no-op.
        if let Ok(Some((index, count))) = self.parse_shard() {
            config.insert("shard_index".into(), index.into());
            config.insert("shard_count".into(), count.into());
        }
        if let Some(dir) = &self.merge_shards {
            config.insert(
                "merge_shards_dir".into(),
                dir.to_string_lossy().into_owned().into(),
            );
        }
        serde_json::Value::Object(config)
    }

    /// Parse `--shard m/n` into a validated 1-based `(m, n)` pair, or `None` when
    /// unset. Errors on a malformed pair so a typo never silently runs the full
    /// set (or an empty shard).
    pub(crate) fn parse_shard(&self) -> Result<Option<(usize, usize)>> {
        let Some(raw) = self.shard.as_deref() else {
            return Ok(None);
        };
        let (m, n) = raw
            .split_once('/')
            .with_context(|| format!("--shard must be 'm/n', got '{raw}'"))?;
        let index: usize = m
            .trim()
            .parse()
            .with_context(|| format!("--shard index 'm' must be a positive integer, got '{m}'"))?;
        let count: usize = n
            .trim()
            .parse()
            .with_context(|| format!("--shard count 'n' must be a positive integer, got '{n}'"))?;
        anyhow::ensure!(
            count >= 1 && index >= 1 && index <= count,
            "--shard m/n requires 1 <= m <= n (n >= 1), got {index}/{count}"
        );
        Ok(Some((index, count)))
    }

    /// Whether this run accepts a cohort-shaped result with no OMC comparison.
    pub(crate) fn allows_unmeasured_parity(&self) -> bool {
        self.allow_unmeasured_parity
    }

    fn requires_selected_targets_success(&self) -> bool {
        self.require_selected_targets_success
    }

    fn uses_baseline_relative_quality_gate(&self) -> bool {
        if self.requires_selected_targets_success()
            || self.sim_targets_file.is_some()
            || !self.sim_match.is_empty()
            || self.sim_limit.is_some()
        {
            return false;
        }
        // A shard runs only its stripe, so it never enforces the aggregate
        // baseline gate — the fan-in merge job does that once on the full set.
        if self.shard.is_some() {
            return false;
        }
        if !matches!(self.target_scope.as_deref(), None | Some("root-examples")) {
            return false;
        }
        matches!(self.sim_set.as_deref(), None | Some("full"))
    }
}

/// Fixed path the libtest harness reads its per-invocation config from
/// (`<workspace>/target/msl/parity-config.json`), matching
/// `balance_pipeline_config::parity_config_path` on the harness side.
fn parity_config_path(root: &Path) -> PathBuf {
    root.join("target/msl/parity-config.json")
}

/// Exclusive ownership of the fixed gate-to-libtest parity-config channel.
///
/// Libtest cannot receive these knobs through argv, so SPEC_0018 permits one
/// inspectable fixed-path file. The lock must outlive both writing and every
/// consumer: otherwise a concurrent focused run can replace the selected model
/// set while the first harness is starting and silently certify the wrong run.
struct ParityConfigLock {
    _file: fs::File,
}

impl ParityConfigLock {
    fn acquire(root: &Path) -> Result<Self> {
        let path = root.join("target/msl/parity-config.lock");
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)
            .with_context(|| format!("failed to open {}", path.display()))?;
        file.lock()
            .with_context(|| format!("failed to lock {}", path.display()))?;
        Ok(Self { _file: file })
    }
}

/// Write the parity config to the fixed path before the libtest gate runs. The
/// file is rewritten every invocation so a previous run's config never leaks.
fn write_parity_config(root: &Path, args: &MslParityArgs) -> Result<()> {
    let path = parity_config_path(root);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let mut config = args.clone();
    if args.quality_baseline.is_some() || args.uses_baseline_relative_quality_gate() {
        config.quality_baseline = Some(resolve_msl_quality_baseline(root, args)?);
    }
    let json = serde_json::to_string_pretty(&config.to_parity_config_json())?;
    fs::write(&path, json).with_context(|| format!("failed to write {}", path.display()))?;
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

pub fn run(root: &Path, args: &MslParityArgs) -> Result<()> {
    // Fail fast on a malformed `--shard m/n` before any expensive setup.
    args.parse_shard()?;
    let ci_env = MslCiEnvironment::from_args(root, args);
    ci_env.print_notice();
    ci_env.clean_stale_results()?;
    // Held through the libtest run and comparator-evidence check: the config is
    // an argv-equivalent channel, so changing it while any consumer is alive
    // would change the meaning of that invocation.
    let _parity_config_lock = ParityConfigLock::acquire(root)?;
    write_parity_config(root, args)?;
    let _cleanup = MslResultsCleanupGuard::new(ci_env.results_dir.clone(), ci_env.clean_results);
    let _monitor = MslResourceMonitor::start(ci_env.clone());
    let mut cargo_setup_steps = Vec::new();

    let merge_only = args.merge_shards.is_some();
    let test_target = if merge_only {
        "balance_pipeline::balance_pipeline_merge::test_msl_merge_and_gate"
    } else {
        "balance_pipeline::balance_pipeline_core::test_msl_all"
    };
    let result = if let Some(binary) = args.prebuilt_test_binary.as_deref() {
        run_prebuilt_msl_test(
            root,
            binary,
            args.prebuilt_model_worker.as_deref(),
            args.prebuilt_sim_worker.as_deref(),
            test_target,
            &mut cargo_setup_steps,
        )
    } else {
        run_msl_quality_gate_cargo_commands(root, test_target, merge_only, &mut cargo_setup_steps)
    };
    let write_result = write_msl_cargo_setup_timing_report(&ci_env.results_dir, &cargo_setup_steps);
    if result.is_ok() {
        write_result?;
    } else if let Err(error) = write_result {
        eprintln!("failed to write MSL Cargo setup timing report: {error:#}");
    }
    result?;
    // Second, independent boundary: the harness gate can be skipped (shards,
    // focused runs), but "did anything get compared against OMC?" is answered
    // from what landed on disk, for every cohort-shaped run.
    check_comparator_evidence(&ci_env.results_dir, args.allows_unmeasured_parity())
}

/// Run a specific libtest from a prebuilt `msl_tests` binary (built once by
/// Nix/crane and shared through CI) instead of recompiling. The gate's config +
/// baseline setup has already run, so this only executes the binary with the
/// right test filter — no `cargo test`, hence no workspace compile + LTO in the
/// consuming job. Sim-running gates spawn `rumoca-sim-worker`, which the harness
/// resolves via `CARGO_BIN_EXE_rumoca-sim-worker`; point that at the prebuilt one.
fn run_prebuilt_msl_test(
    root: &Path,
    binary: &Path,
    model_worker: Option<&Path>,
    sim_worker: Option<&Path>,
    test_target: &str,
    cargo_setup_steps: &mut Vec<MslCargoSetupTimingStep>,
) -> Result<()> {
    let tools = prebuilt_sibling_binary(binary, "rumoca-msl-tools");
    let binaries = MslTestBinaries {
        test_binary: binary,
        model_worker,
        sim_worker,
        msl_tools: tools.as_deref(),
    };
    run_msl_test_binary(
        root,
        binaries,
        test_target,
        MslTestRunSource::Prebuilt,
        cargo_setup_steps,
    )
}

fn prebuilt_sibling_binary(binary: &Path, name: &str) -> Option<PathBuf> {
    let candidate = binary.parent()?.join(name);
    candidate.is_file().then_some(candidate)
}

fn run_msl_quality_gate_cargo_commands(
    root: &Path,
    test_target: &str,
    merge_only: bool,
    cargo_setup_steps: &mut Vec<MslCargoSetupTimingStep>,
) -> Result<()> {
    let target_dir = cargo_target_dir(root);

    // The merge-and-gate fan-in entry runs NO simulations: it loads the per-shard
    // `msl_results.json`, concatenates them, and runs the quality ratchet on the
    // merged aggregate. So it needs neither the optimized `rumoca-sim-worker` /
    // `rumoca-msl-tools` binaries (only the sharded sim run spawns those) nor an
    // optimized build of the harness. Building just the merge test in debug
    // avoids rebuilding the whole workspace in the fan-in job,
    // which otherwise runs sequentially after the shards and inflates the gate.
    if local_msl_run_plan(merge_only) == LocalMslRunPlan::ReleaseArtifacts {
        // Include the integration test and every spawned runtime in one Cargo
        // graph. The test's dev-dependencies participate in feature unification;
        // separate binary builds therefore cannot be reused reliably even when
        // their top-level feature flag matches this test.
        let build = optimized_msl_artifact_build(root);
        let artifacts = run_msl_cargo_setup_step_with(
            cargo_setup_steps,
            MslCargoSetupStepMetadata::new(
                "build optimized MSL artifacts",
                "build",
                "rumoca-worker + rumoca-test-msl",
                MSL_BUILD_PROFILE,
                vec![format!("rumoca-test-msl/{MSL_FULL_TEST_FEATURE}")],
                &target_dir,
            ),
            build,
            run_optimized_msl_artifact_build,
        )?;
        return run_msl_test_binary(
            root,
            artifacts.binaries(),
            test_target,
            MslTestRunSource::Optimized,
            cargo_setup_steps,
        );
    }

    let gate = debug_msl_merge_test_command(root, test_target);
    run_msl_cargo_setup_step(
        cargo_setup_steps,
        MslCargoSetupStepMetadata::new(
            "run debug MSL merge test",
            "test",
            "rumoca-test-msl",
            "debug",
            vec![MSL_FULL_TEST_FEATURE.to_string()],
            &target_dir,
        ),
        gate,
    )
}

fn debug_msl_merge_test_command(root: &Path, test_target: &str) -> Command {
    let mut command = Command::new("cargo");
    command
        .arg("test")
        .arg("--verbose")
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--features")
        .arg(MSL_FULL_TEST_FEATURE)
        .arg("--test")
        .arg("msl_tests")
        .arg(test_target)
        .arg("--")
        .arg("--nocapture")
        .env("RUST_BACKTRACE", "full")
        .current_dir(root);
    command
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LocalMslRunPlan {
    MergeOnly,
    ReleaseArtifacts,
}

fn local_msl_run_plan(merge_only: bool) -> LocalMslRunPlan {
    if merge_only {
        LocalMslRunPlan::MergeOnly
    } else {
        LocalMslRunPlan::ReleaseArtifacts
    }
}

#[derive(Debug, Clone, Copy)]
enum MslTestRunSource {
    Prebuilt,
    Optimized,
}

impl MslTestRunSource {
    fn label(self) -> &'static str {
        match self {
            Self::Prebuilt => "run prebuilt MSL test",
            Self::Optimized => "run optimized MSL test",
        }
    }

    fn profile(self) -> &'static str {
        match self {
            Self::Prebuilt => "prebuilt",
            Self::Optimized => MSL_BUILD_PROFILE,
        }
    }
}

fn run_msl_test_binary(
    root: &Path,
    binaries: MslTestBinaries<'_>,
    test_target: &str,
    source: MslTestRunSource,
    cargo_setup_steps: &mut Vec<MslCargoSetupTimingStep>,
) -> Result<()> {
    let target_dir = cargo_target_dir(root);
    let run = msl_test_binary_command(root, binaries, test_target)?;
    run_msl_cargo_setup_step(
        cargo_setup_steps,
        MslCargoSetupStepMetadata::new(
            source.label(),
            "run",
            "rumoca-test-msl",
            source.profile(),
            vec![MSL_FULL_TEST_FEATURE.to_string()],
            &target_dir,
        ),
        run,
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

impl MslCiEnvironment {
    fn from_args(root: &Path, args: &MslParityArgs) -> Self {
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
    stop: Option<mpsc::Sender<()>>,
    worker: Option<JoinHandle<()>>,
}

impl MslResourceMonitor {
    fn start(config: MslCiEnvironment) -> Self {
        print_resource_snapshot("initial", &config, true);
        let Some(interval) = config.monitor_interval else {
            return Self {
                config,
                stop: None,
                worker: None,
            };
        };

        let (stop, stop_receiver) = mpsc::channel();
        let config_for_worker = config.clone();
        let worker = thread::spawn(move || {
            run_resource_monitor_loop(stop_receiver, interval, config_for_worker);
        });
        Self {
            config,
            stop: Some(stop),
            worker: Some(worker),
        }
    }
}

impl Drop for MslResourceMonitor {
    fn drop(&mut self) {
        self.stop.take();
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
        print_resource_snapshot("final", &self.config, true);
    }
}

fn run_resource_monitor_loop(
    stop: mpsc::Receiver<()>,
    interval: Duration,
    config: MslCiEnvironment,
) {
    let mut last_cpu_sample = Instant::now();
    let mut last_print: Option<Instant> = None;
    while let Err(RecvTimeoutError::Timeout) = stop.recv_timeout(interval) {
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
    // `df` is the bounded disk-capacity monitor. Recursively walking the large
    // shared target trees here used to add minutes to otherwise focused gates.
    eprintln!(
        "target_msl_path={}",
        config.root.join("target/msl").display()
    );
    eprintln!("msl_results_path={}", config.results_dir.display());
    if !concise {
        log_command_output("uptime", "uptime", std::iter::empty::<&str>());
        log_command_output("df -ih", "df", ["-ih", ".", "/tmp"]);
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
        match entry.metadata() {
            Ok(metadata) if metadata.is_file() => {
                eprintln!("  file: {} bytes ({})", metadata.len(), path.display());
            }
            Ok(metadata) if metadata.is_dir() => {
                eprintln!("  dir: {}", path.display());
            }
            Ok(_) | Err(_) => eprintln!("  entry: {}", path.display()),
        }
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
