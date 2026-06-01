use super::common::{
    AUTO_WORKERS_DEFAULT, BATCH_SIZE_OMC_SIMULATION_DEFAULT, BATCH_TIMEOUT_SECONDS_DEFAULT,
    BatchElapsedStats, BatchTimingDetail, MSL_VERSION, MslPaths, OMC_THREADS_DEFAULT,
    SIM_STOP_TIME_DEFAULT, choose_effective_batch_size, get_git_commit, get_omc_version,
    has_fatal_omc_error, load_target_models, msl_load_lines, round3, summarize_batch_timings,
    summarize_omc_error, unix_timestamp_seconds, write_pretty_json,
};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use rumoca_sim::sim_trace_compare::{
    ModelDeviationMetric, SimTrace, compare_model_traces, load_trace_json,
};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

mod omc_session;
mod output;
mod runtime;
mod speed_report;
mod state_selection;
#[cfg(test)]
mod tests;
use omc_session::{OmcEvalError, OmcSession, OmcSimOutcome};
use output::{
    build_sim_output_payload, compute_trace_output_summary, print_summary, write_trace_report,
};
use runtime::{
    attach_rumoca_runtime, ensure_target_placeholders, load_rumoca_runtime,
    path_for_rumoca_results, select_omc_simulation_models,
};
use state_selection::StateSelectionMetric;

const DEFAULT_TRACE_EXCLUSIONS_FILE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json";
const STOCHASTIC_TRACE_EXCLUSION_REASON: &str =
    "stochastic random-input model; skipped until generator + seed parity is implemented";

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Write the sample OMC command stream instead of running OMC.
    #[arg(long, default_value_t = false)]
    dry_run: bool,
    /// Internal scheduling chunk size; sessions run one model at a time, so this
    /// is hidden and should be left at the default.
    #[arg(long, default_value_t = BATCH_SIZE_OMC_SIMULATION_DEFAULT, hide = true)]
    batch_size: usize,
    /// Re-run OMC for every model, ignoring the cached reference. By default the
    /// cached OMC results are reused as long as the OMC version and MSL source
    /// are unchanged, so developers do not pay for a full OMC run every time.
    #[arg(long, default_value_t = false)]
    force: bool,
    /// Parallel persistent OMC worker sessions (0 = auto: physical cores minus
    /// headroom on large hosts; each worker is pinned to one core).
    #[arg(long, default_value_t = AUTO_WORKERS_DEFAULT)]
    workers: usize,
    /// Thread cap applied to each OMC process (OMP/BLAS); keep at 1 so the pool,
    /// not OMC, owns parallelism.
    #[arg(long, default_value_t = OMC_THREADS_DEFAULT)]
    omc_threads: usize,
    /// Per-model wall timeout (seconds) for one OMC compile+simulate; on timeout
    /// the session is killed (with its process group) and respawned.
    #[arg(long = "model-timeout-seconds", value_name = "SECONDS", default_value_t = BATCH_TIMEOUT_SECONDS_DEFAULT)]
    batch_timeout_seconds: u64,
    /// stopTime passed to OMC simulate()
    #[arg(long, default_value_t = SIM_STOP_TIME_DEFAULT)]
    stop_time: f64,
    /// Use model annotation(experiment(StopTime=...)) when available
    #[arg(long, default_value_t = false)]
    use_experiment_stop_time: bool,
    /// Limit target model count (0 = all)
    #[arg(long, default_value_t = 0)]
    max_models: usize,
    /// Only run models whose fully-qualified name matches this regex. Lets an
    /// agent scope a run to a small subset, e.g. --model-regex 'Rotational|Spice3'.
    #[arg(long)]
    model_regex: Option<String>,
    /// Balance results JSON used for target selection
    #[arg(long)]
    balance_results_file: Option<PathBuf>,
    /// Directory for OMC reference output, trace reports, and intermediate work
    #[arg(long)]
    results_dir: Option<PathBuf>,
    /// Optional explicit model list JSON (array or object.model_names)
    #[arg(long)]
    target_models_file: Option<PathBuf>,
    /// Optional trace-exclusion model list JSON (array or object.model_names)
    #[arg(long)]
    trace_exclusions_file: Option<PathBuf>,
    /// Only build OMC references for models rumoca already simulates (`sim_ok`).
    /// This is the fast CI subset; by default OMC runs on every target so that
    /// newly-passing rumoca models already have an OMC baseline to compare to.
    #[arg(long, default_value_t = false)]
    rumoca_sim_ok_only: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct SimModelResult {
    status: String,
    error: Option<String>,
    sim_system_seconds: Option<f64>,
    total_system_seconds: Option<f64>,
    omc_wall_seconds: Option<f64>,
    result_file: Option<String>,
    trace_file: Option<String>,
    trace_error: Option<String>,
    rumoca_status: Option<String>,
    rumoca_ic_status: Option<String>,
    rumoca_ic_error: Option<String>,
    rumoca_ic_seconds: Option<f64>,
    rumoca_sim_seconds: Option<f64>,
    rumoca_sim_build_seconds: Option<f64>,
    rumoca_sim_run_seconds: Option<f64>,
    rumoca_sim_wall_seconds: Option<f64>,
    rumoca_trace_file: Option<String>,
    rumoca_trace_error: Option<String>,
}

#[derive(Debug, Clone)]
struct SimRunState {
    all_results: BTreeMap<String, SimModelResult>,
    // One per-model timing record. The shared `BatchTimingDetail` type is reused
    // (the compile reference genuinely batches); here each entry is one model.
    batch_timings: Vec<BatchTimingDetail>,
    // The session pool pulls models one at a time, so the queue is a flat model
    // list — no `PendingBatch` wrapping (that is the compile reference's model).
    pending_models: Vec<String>,
}

#[derive(Debug, Clone, Default)]
struct TraceQuantification {
    models: BTreeMap<String, TraceModelMetric>,
    missing_trace: BTreeMap<String, String>,
    skipped: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize)]
struct TraceModelMetric {
    #[serde(flatten)]
    metric: ModelDeviationMetric,
    #[serde(skip_serializing_if = "Option::is_none")]
    state_selection: Option<StateSelectionMetric>,
    rumoca_sim_wall_seconds: Option<f64>,
    rumoca_sim_seconds: Option<f64>,
    rumoca_sim_build_seconds: Option<f64>,
    rumoca_sim_run_seconds: Option<f64>,
    omc_sim_system_seconds: Option<f64>,
    omc_total_system_seconds: Option<f64>,
    omc_wall_seconds: Option<f64>,
}

#[derive(Debug, Clone)]
struct ModelSelection {
    names: Vec<String>,
    source_file: PathBuf,
    rule: String,
    selection_seconds: f64,
}

#[derive(Debug, Clone)]
struct RumocaRuntime {
    status: String,
    ic_status: Option<String>,
    ic_error: Option<String>,
    ic_seconds: Option<f64>,
    sim_seconds: Option<f64>,
    sim_build_seconds: Option<f64>,
    sim_run_seconds: Option<f64>,
    sim_wall_seconds: Option<f64>,
    trace_file: Option<String>,
    trace_error: Option<String>,
    /// Rumoca front-to-DAE compile seconds, for the speed comparison table.
    compile_seconds: Option<f64>,
    /// Flattened scalar-equation count: the square system size both compilers
    /// process. This is the primary scalability axis (continuous states are a
    /// poor proxy because most MSL example models are algebraic, 0 states).
    scalar_equations: Option<usize>,
    /// Continuous state count, kept as a secondary scaling view.
    num_states: Option<usize>,
}

#[derive(Debug, Clone)]
struct FinalizeContext {
    omc_version: String,
    git_commit: String,
    workers: usize,
    total: usize,
    n_batches: usize,
    effective_batch_size: usize,
    elapsed_seconds: f64,
    /// Cache invalidation key (OMC version + MSL source fingerprint) stored in
    /// the output JSON so a later run can detect OMC/MSL changes and re-run.
    cache_key: String,
}

#[derive(Debug, Clone)]
struct RunMetrics {
    sim_successful: usize,
    sim_failed: usize,
    sim_timed_out: usize,
    success_rate: f64,
    total_omc_sim_system_seconds: f64,
    total_omc_total_system_seconds: f64,
    total_omc_wall_seconds: f64,
    total_rumoca_sim_seconds: f64,
    total_rumoca_sim_build_seconds: f64,
    total_rumoca_sim_run_seconds: f64,
    total_rumoca_sim_wall_seconds: f64,
    system_ratio_all_positive: Option<RuntimeRatioStats>,
    system_ratio_both_success: Option<RuntimeRatioStats>,
    wall_ratio_all_positive: Option<RuntimeRatioStats>,
    wall_ratio_both_success: Option<RuntimeRatioStats>,
    ran_batches: usize,
    skipped_batches: usize,
    batch_stats: Option<BatchElapsedStats>,
}

#[derive(Debug, Clone, Serialize)]
struct RuntimeRatioStats {
    sample_count: usize,
    aggregate_ratio: f64,
    min_ratio: f64,
    max_ratio: f64,
    mean_ratio: f64,
    median_ratio: f64,
}

#[derive(Debug, Clone)]
struct TraceOutputSummary {
    models_compared: usize,
    missing_trace_models: usize,
    skipped_models: usize,
    agreement_high: usize,
    agreement_minor: usize,
    agreement_deviation: usize,
    agreement_high_percent: f64,
    agreement_minor_percent: f64,
    agreement_deviation_percent: f64,
    total_channels_compared: usize,
    bad_channels_total: usize,
    severe_channels_total: usize,
    bad_channels_percent: f64,
    severe_channels_percent: f64,
    violation_mass_total: f64,
    violation_mass_mean_per_model: f64,
    violation_mass_mean_per_channel: f64,
    models_with_bad_channel: usize,
    models_with_severe_channel: usize,
    models_with_any_channel_deviation: usize,
    models_with_any_channel_deviation_percent: f64,
    models_with_no_severe_channel: usize,
    models_with_no_severe_channel_percent: f64,
    max_model_channel_deviation_percent: f64,
    min_model_bounded_normalized_l1: f64,
    median_model_bounded_normalized_l1: f64,
    mean_model_bounded_normalized_l1: f64,
    max_model_bounded_normalized_l1: f64,
    mean_model_mean_channel_bounded_normalized_l1: f64,
    max_model_max_channel_bounded_normalized_l1: f64,
    initial_condition: InitialConditionSummary,
    state_selection: state_selection::StateSelectionSummary,
}

#[derive(Debug, Clone, Default, Serialize)]
struct InitialConditionSummary {
    models_compared: usize,
    models_with_accurate_initial_conditions: usize,
    models_with_initial_condition_deviation: usize,
    accurate_initial_conditions_percent: f64,
    models_with_initial_condition_deviation_percent: f64,
    total_channels_compared: usize,
    high_channels_total: usize,
    near_channels_total: usize,
    deviation_channels_total: usize,
    severe_channels_total: usize,
    high_channels_percent: f64,
    near_channels_percent: f64,
    deviation_channels_percent: f64,
    severe_channels_percent: f64,
    violation_mass_total: f64,
    violation_mass_mean_per_model: f64,
    violation_mass_mean_per_channel: f64,
    mean_channel_bounded_normalized_error: f64,
    max_channel_bounded_normalized_error: f64,
}

pub fn run(args: Args) -> Result<()> {
    let paths = args
        .results_dir
        .as_deref()
        .map_or_else(MslPaths::current, |dir| {
            MslPaths::current().with_results_dir(dir)
        });
    ensure_msl_available(&paths)?;
    // Warm OMC sessions are heavyweight (one omc process each), so size the pool
    // like the rumoca warm-worker stage: physical cores, with headroom reserved
    // on large hosts. (`resolve_worker_count` is the older logical-core policy.)
    let workers = rumoca_worker::warm_worker_pool_size(args.workers);
    let omc_version = get_omc_version();
    let git_commit = get_git_commit(&paths.repo_root);

    std::fs::create_dir_all(&paths.results_dir)
        .with_context(|| format!("failed to create '{}'", paths.results_dir.display()))?;
    std::fs::create_dir_all(&paths.sim_work_dir)
        .with_context(|| format!("failed to create '{}'", paths.sim_work_dir.display()))?;
    prepare_omc_trace_dir(&args, &paths)?;

    let selection = select_models(&args, &paths)?;
    let model_names = filter_models_by_regex(
        truncate_models(selection.names.clone(), args.max_models),
        &args,
    )?;
    let total = model_names.len();
    let rumoca_runtimes = load_rumoca_runtime(path_for_rumoca_results(&paths))?;
    let omc_model_names =
        select_omc_simulation_models(&model_names, &rumoca_runtimes, args.rumoca_sim_ok_only);
    let omc_total = omc_model_names.len();
    let effective_batch_size = choose_effective_batch_size(omc_total, args.batch_size, workers)?;
    let n_batches = if omc_total == 0 {
        0
    } else {
        omc_total.div_ceil(effective_batch_size)
    };
    print_selection_summary(&args, workers, &selection, total, effective_batch_size);
    print_omc_simulation_scope(total, omc_total, args.rumoca_sim_ok_only);

    if args.dry_run {
        return run_dry_run(&paths, &omc_model_names, &args);
    }

    let overall_start = Instant::now();
    // OMC results are cached: reuse them as long as the OMC version and MSL
    // source are unchanged, so developers do not re-run OMC on every invocation.
    // `--force` re-runs everything.
    let omc_ref_json = paths.results_dir.join("omc_simulation_reference.json");
    let cache_key = omc_reference_cache_key(&omc_version, &paths.msl_dir);
    let cache_valid =
        !args.force && cached_reference_cache_key(&omc_ref_json) == Some(cache_key.clone());
    println!(
        "OMC reference cache: {} (key {})",
        if args.force {
            "forced refresh (--force)"
        } else if cache_valid {
            "valid — reusing cached OMC results for unchanged models"
        } else {
            "stale or absent — OMC or MSL changed, re-running"
        },
        &cache_key[..cache_key.len().min(16)]
    );
    let mut state = prepare_run_state(&omc_model_names);
    if cache_valid {
        merge_cached_results_for_resume(&omc_ref_json, &omc_model_names, &mut state.all_results)?;
    }
    run_session_pending(&args, workers, &paths.sim_work_dir, &mut state, cache_valid)?;
    ensure_omc_trace_artifacts(&paths, &mut state.all_results);
    attach_rumoca_runtime(&rumoca_runtimes, &mut state.all_results);
    ensure_target_placeholders(&model_names, &rumoca_runtimes, &mut state.all_results);
    let trace_exclusions = load_trace_exclusions(&args, &paths)?;
    let trace_report = quantify_trace_differences(&paths, &state.all_results, &trace_exclusions)?;
    let context = FinalizeContext {
        omc_version,
        git_commit,
        workers,
        total,
        n_batches,
        effective_batch_size,
        elapsed_seconds: overall_start.elapsed().as_secs_f64(),
        cache_key,
    };
    finalize_and_write_output(
        &args,
        &paths,
        &selection,
        context,
        state,
        trace_report,
        &rumoca_runtimes,
    )
}

fn ensure_msl_available(paths: &MslPaths) -> Result<()> {
    if paths.msl_dir.exists() {
        return Ok(());
    }
    bail!(
        "MSL directory not found: {}. Run an MSL test first to populate cache.",
        paths.msl_dir.display()
    );
}

fn prepare_omc_trace_dir(args: &Args, paths: &MslPaths) -> Result<()> {
    // Keep cached OMC traces by default (they are reused with the cached
    // results); only wipe them on a forced full re-run.
    if args.force && paths.omc_trace_dir.exists() {
        std::fs::remove_dir_all(&paths.omc_trace_dir)
            .with_context(|| format!("failed to remove '{}'", paths.omc_trace_dir.display()))?;
    }
    std::fs::create_dir_all(&paths.omc_trace_dir)
        .with_context(|| format!("failed to create '{}'", paths.omc_trace_dir.display()))
}

fn select_models(args: &Args, paths: &MslPaths) -> Result<ModelSelection> {
    let start = Instant::now();
    if let Some(target_file) = args.target_models_file.clone() {
        let resolved = resolve_optional_path(&paths.repo_root, target_file);
        let names = load_target_models(&resolved)?;
        return Ok(ModelSelection {
            names,
            source_file: resolved,
            rule: "explicit model list from --target-models-file".to_string(),
            selection_seconds: start.elapsed().as_secs_f64(),
        });
    }

    let generated_targets = paths.results_dir.join("msl_simulation_targets.json");
    if generated_targets.is_file() {
        let names = load_target_models(&generated_targets)?;
        return Ok(ModelSelection {
            names,
            source_file: generated_targets,
            rule: "default model list from target/msl/results/msl_simulation_targets.json"
                .to_string(),
            selection_seconds: start.elapsed().as_secs_f64(),
        });
    }

    let committed_targets = paths
        .repo_root
        .join("crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets.json");
    if committed_targets.is_file() {
        let names = load_target_models(&committed_targets)?;
        return Ok(ModelSelection {
            names,
            source_file: committed_targets,
            rule: "default committed MSL simulation target list".to_string(),
            selection_seconds: start.elapsed().as_secs_f64(),
        });
    }

    let balance_file = resolve_optional_path(
        &paths.repo_root,
        args.balance_results_file
            .clone()
            .unwrap_or_else(|| paths.results_dir.join("msl_balance_results.json")),
    );
    let names = load_simulation_targets(&balance_file)?;
    Ok(ModelSelection {
        names,
        source_file: balance_file,
        rule:
            "phase_reached=Success && is_partial=false && model_name matches Modelica.*.Examples.*"
                .to_string(),
        selection_seconds: start.elapsed().as_secs_f64(),
    })
}

fn resolve_optional_path(repo_root: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        repo_root.join(path)
    }
}

fn load_simulation_targets(path: &Path) -> Result<Vec<String>> {
    if !path.exists() {
        bail!(
            "balance results file not found: {}. Run msl balance test first.",
            path.display()
        );
    }
    let payload: Value = serde_json::from_str(
        &std::fs::read_to_string(path)
            .with_context(|| format!("failed to read '{}'", path.display()))?,
    )
    .with_context(|| format!("failed to parse '{}'", path.display()))?;
    let model_results = payload
        .get("model_results")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let mut names = Vec::new();
    for model in model_results {
        let Some(name) = model.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        let phase_ok = model
            .get("phase_reached")
            .and_then(Value::as_str)
            .is_some_and(|phase| phase == "Success");
        let is_partial = model
            .get("is_partial")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if phase_ok && !is_partial && is_explicit_msl_example_model(name) {
            names.push(name.to_string());
        }
    }
    Ok(names)
}

fn is_explicit_msl_example_model(model_name: &str) -> bool {
    model_name.starts_with("Modelica.") && model_name.contains(".Examples.")
}

fn filter_models_by_regex(names: Vec<String>, args: &Args) -> Result<Vec<String>> {
    let Some(pattern) = args.model_regex.as_deref() else {
        return Ok(names);
    };
    let regex =
        regex::Regex::new(pattern).with_context(|| format!("invalid --model-regex '{pattern}'"))?;
    let filtered: Vec<String> = names
        .into_iter()
        .filter(|name| regex.is_match(name))
        .collect();
    println!(
        "Model regex filter '{pattern}': {} model(s) selected",
        filtered.len()
    );
    Ok(filtered)
}

fn truncate_models(mut names: Vec<String>, max_models: usize) -> Vec<String> {
    if max_models > 0 {
        names.truncate(max_models);
    }
    names
}

fn print_selection_summary(
    args: &Args,
    workers: usize,
    selection: &ModelSelection,
    total: usize,
    batch_size: usize,
) {
    println!("OMC version: {}", get_omc_version());
    println!("Batch workers: {workers} (requested {})", args.workers);
    println!("OMC process thread cap: {}", args.omc_threads);
    println!("Target set source: {}", selection.source_file.display());
    println!("Target selection rule: {}", selection.rule);
    println!("Total target models: {total}");
    println!(
        "Target discovery/selection time: {:.2}s",
        selection.selection_seconds
    );
    println!("Batch size: {batch_size}");
    if args.use_experiment_stop_time {
        println!("stopTime policy: model annotation experiment(StopTime) when available");
    } else {
        println!("stopTime: {}", args.stop_time);
    }
}

fn print_omc_simulation_scope(total_models: usize, omc_models: usize, sim_ok_only: bool) {
    if sim_ok_only {
        println!(
            "OMC simulation scope: {omc_models}/{total_models} target models (restricted to rumoca sim_ok models via --rumoca-sim-ok-only)"
        );
    } else {
        println!(
            "OMC simulation scope: {omc_models}/{total_models} target models (all targets; OMC baseline built even for models rumoca cannot yet simulate)"
        );
    }
}

fn run_dry_run(paths: &MslPaths, model_names: &[String], args: &Args) -> Result<()> {
    // Emit the exact command stream a persistent session worker would send:
    // the one-time MSL loads followed by a `simulate(...)` per model.
    let sample = model_names.iter().take(10).cloned().collect::<Vec<_>>();
    let mut lines = msl_load_lines(paths);
    for model in &sample {
        if args.use_experiment_stop_time {
            lines.push(format!(
                "simulate({model}, outputFormat=\"csv\", fileNamePrefix=\"{model}\");"
            ));
        } else {
            lines.push(format!(
                "simulate({model}, stopTime={}, outputFormat=\"csv\", fileNamePrefix=\"{model}\");",
                args.stop_time
            ));
        }
    }
    let mos_file = paths.sim_work_dir.join("sim_dry_run_sample.mos");
    std::fs::write(&mos_file, lines.join("\n"))
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;
    println!(
        "Dry run: sample session commands written to {}",
        mos_file.display()
    );
    Ok(())
}

/// Cache key for the OMC reference: OMC version plus a fingerprint of the MSL
/// source. Any OMC upgrade or MSL `.mo` edit (content or size) changes the key
/// and forces a re-run; otherwise cached per-model results are reused.
fn omc_reference_cache_key(omc_version: &str, msl_dir: &Path) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(MSL_VERSION.as_bytes());
    hasher.update(b"\0");
    hasher.update(omc_version.as_bytes());
    hasher.update(b"\0");
    let mut files: Vec<(String, u64, u128)> = Vec::new();
    for entry in walkdir::WalkDir::new(msl_dir).into_iter().flatten() {
        if !entry.file_type().is_file()
            || entry.path().extension().and_then(|ext| ext.to_str()) != Some("mo")
        {
            continue;
        }
        let Ok(meta) = entry.metadata() else { continue };
        let rel = entry
            .path()
            .strip_prefix(msl_dir)
            .unwrap_or(entry.path())
            .to_string_lossy()
            .into_owned();
        let mtime = meta
            .modified()
            .ok()
            .and_then(|time| time.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|delta| delta.as_nanos())
            .unwrap_or(0);
        files.push((rel, meta.len(), mtime));
    }
    files.sort();
    for (rel, len, mtime) in &files {
        hasher.update(rel.as_bytes());
        hasher.update(&len.to_le_bytes());
        hasher.update(&mtime.to_le_bytes());
    }
    hasher.finalize().to_hex().to_string()
}

/// Read the `cache_key` field previously written to the OMC reference JSON.
fn cached_reference_cache_key(omc_ref_json: &Path) -> Option<String> {
    let text = std::fs::read_to_string(omc_ref_json).ok()?;
    let value: Value = serde_json::from_str(&text).ok()?;
    value
        .get("cache_key")
        .and_then(Value::as_str)
        .map(ToOwned::to_owned)
}

fn prepare_run_state(model_names: &[String]) -> SimRunState {
    SimRunState {
        all_results: BTreeMap::new(),
        batch_timings: Vec::new(),
        pending_models: model_names.to_vec(),
    }
}

/// Outcome of simulating one model inside a persistent session worker.
struct SessionModelOutcome {
    idx: usize,
    model: String,
    result: SimModelResult,
    elapsed_seconds: f64,
    timed_out: bool,
}

/// Shared, immutable context handed to each session worker thread.
struct SessionWorkerCtx<'a> {
    models: &'a [String],
    next: &'a AtomicUsize,
    tx: &'a mpsc::Sender<SessionModelOutcome>,
    msl_exprs: &'a [String],
    work_dir: &'a Path,
    stop_time: f64,
    use_experiment: bool,
    omc_threads: usize,
    sim_timeout: Duration,
    startup_timeout: Duration,
    load_timeout: Duration,
    /// CPU core to pin this worker (and the OMC process it spawns) to, keeping
    /// caches warm and avoiding SMT-sibling oversubscription. `None` when more
    /// workers than cores were requested.
    cpu_core_id: Option<usize>,
}

/// Persistent-session execution path: the OMC analogue of the rumoca warm
/// worker queue. Each worker thread owns one [`OmcSession`] that loads the MSL
/// once, then pulls models from a shared atomic index and simulates them. On a
/// per-model timeout (or transport failure) the session is killed and the next
/// model respawns a fresh one, so a single hung model never blocks the others.
fn run_session_pending(
    args: &Args,
    workers: usize,
    work_dir: &Path,
    state: &mut SimRunState,
    reuse_cached: bool,
) -> Result<()> {
    // When the cache is valid (OMC + MSL unchanged, no --force) the cached OMC
    // reference has already been merged into `all_results`, so skip any model
    // that already has a real cached result instead of re-running OMC.
    let reuse_skip: BTreeSet<String> = if reuse_cached {
        state
            .all_results
            .iter()
            .filter(|(_, result)| matches!(result.status.as_str(), "success" | "error" | "timeout"))
            .map(|(name, _)| name.clone())
            .collect()
    } else {
        BTreeSet::new()
    };
    let models: Vec<String> = std::mem::take(&mut state.pending_models)
        .into_iter()
        .filter(|model| !reuse_skip.contains(model))
        .collect();
    if !reuse_skip.is_empty() {
        println!(
            "OMC session pool: reusing {} cached OMC result(s) (OMC + MSL unchanged)",
            reuse_skip.len()
        );
    }
    if models.is_empty() {
        return Ok(());
    }

    let paths = MslPaths::current();
    let msl_exprs = msl_load_lines(&paths);
    // Mirror the rumoca warm-worker pool: one heavyweight OMC session per real
    // CPU core (not per SMT sibling), reserving headroom cores on large hosts so
    // the machine stays usable, and pin each worker to its core to keep caches
    // warm. `args.workers == 0` selects the auto/headroom default.
    let physical_cores = rumoca_worker::physical_cpu_core_count()
        .unwrap_or_else(|| rumoca_worker::available_cpu_core_ids().len())
        .max(1);
    let worker_count = workers.max(1).min(models.len());
    let core_plan = rumoca_worker::cpu_core_plan(worker_count);
    let pinned = core_plan.iter().filter(|core| core.is_some()).count();
    let sim_timeout = Duration::from_secs(args.batch_timeout_seconds.max(1));
    let startup_timeout = Duration::from_secs(60);
    let load_timeout = Duration::from_secs(120);
    let total = models.len();
    println!(
        "OMC session pool: {worker_count} persistent omc worker(s) ({pinned} pinned; {physical_cores} physical cores, headroom reserved) over {total} models (MSL loaded once per worker)"
    );

    let models = Arc::new(models);
    let next = Arc::new(AtomicUsize::new(0));
    let (tx, rx) = mpsc::channel::<SessionModelOutcome>();
    let mut handles = Vec::with_capacity(worker_count);
    for worker_idx in 0..worker_count {
        let models = Arc::clone(&models);
        let next = Arc::clone(&next);
        let tx = tx.clone();
        let msl_exprs = msl_exprs.clone();
        let work_dir = work_dir.to_path_buf();
        let stop_time = args.stop_time;
        let use_experiment = args.use_experiment_stop_time;
        let omc_threads = args.omc_threads;
        let cpu_core_id = core_plan.get(worker_idx).copied().flatten();
        handles.push(thread::spawn(move || {
            run_one_session_worker(SessionWorkerCtx {
                models: &models,
                next: &next,
                tx: &tx,
                msl_exprs: &msl_exprs,
                work_dir: &work_dir,
                stop_time,
                use_experiment,
                omc_threads,
                sim_timeout,
                startup_timeout,
                load_timeout,
                cpu_core_id,
            });
        }));
    }
    drop(tx);

    let mut completed = 0usize;
    for outcome in rx {
        completed += 1;
        state.batch_timings.push(BatchTimingDetail {
            batch_idx: outcome.idx,
            requested_models: 1,
            parsed_models: usize::from(outcome.result.status == "success"),
            elapsed_seconds: round3(outcome.elapsed_seconds),
            timed_out: outcome.timed_out,
            skipped: false,
        });
        state.all_results.insert(outcome.model, outcome.result);
        if completed.is_multiple_of(25) || completed == total {
            println!("  OMC session progress: {completed}/{total} models");
        }
    }
    for handle in handles {
        let _ = handle.join();
    }
    Ok(())
}

fn run_one_session_worker(ctx: SessionWorkerCtx<'_>) {
    // Pin before spawning OMC: on Linux the omc child inherits this thread's
    // CPU affinity, so the whole session stays on one core.
    if let Some(core_id) = ctx.cpu_core_id
        && let Err(error) = rumoca_worker::pin_current_thread_to_cpu_core(core_id)
    {
        eprintln!("  OMC session worker: failed to pin to CPU core {core_id}: {error}");
    }
    let mut session: Option<OmcSession> = None;
    // A long-lived omc interactive process accumulates memory across simulate()
    // calls (loaded result sets, codegen scratch), so recycle the session every
    // few models. This bounds RSS while still amortizing the MSL load across the
    // batch (one reload per recycle, not per model).
    let mut models_on_session = 0usize;
    loop {
        let idx = ctx.next.fetch_add(1, AtomicOrdering::Relaxed);
        let Some(model) = ctx.models.get(idx) else {
            break;
        };
        if session.is_none() {
            match OmcSession::spawn(
                ctx.work_dir,
                ctx.msl_exprs,
                ctx.omc_threads,
                ctx.startup_timeout,
                ctx.load_timeout,
            ) {
                Ok(spawned) => {
                    session = Some(spawned);
                    models_on_session = 0;
                }
                Err(error) => {
                    let _ = ctx.tx.send(SessionModelOutcome {
                        idx,
                        model: model.clone(),
                        result: session_error_result(format!("omc session spawn failed: {error}")),
                        elapsed_seconds: 0.0,
                        timed_out: false,
                    });
                    continue;
                }
            }
        }
        let live = session.as_mut().expect("session present after spawn");
        let start = Instant::now();
        let sim = live.simulate_model(model, ctx.stop_time, ctx.use_experiment, ctx.sim_timeout);
        let elapsed = start.elapsed().as_secs_f64();
        let (result, timed_out) = match sim {
            Ok(outcome) => (build_session_model_result(&outcome, elapsed), false),
            Err(OmcEvalError::Timeout) => {
                // Poisoned REQ socket after a hung model: kill and respawn.
                if let Some(mut dead) = session.take() {
                    dead.kill();
                }
                (session_timeout_result(elapsed, ctx.sim_timeout), true)
            }
            Err(OmcEvalError::Io(error)) => {
                if let Some(mut dead) = session.take() {
                    dead.kill();
                }
                (
                    session_error_result(format!("omc session io error: {error}")),
                    false,
                )
            }
        };
        let _ = ctx.tx.send(SessionModelOutcome {
            idx,
            model: model.clone(),
            result,
            elapsed_seconds: elapsed,
            timed_out,
        });
        // `session` is still alive only on the success path (timeout/io already
        // took and killed it). Recycle it once it has handled enough models.
        if session.is_some() {
            models_on_session += 1;
            if models_on_session >= SESSION_RECYCLE_MODELS
                && let Some(mut spent) = session.take()
            {
                spent.kill();
            }
        }
    }
}

/// Recycle an omc session after this many models to bound its memory growth.
const SESSION_RECYCLE_MODELS: usize = 25;

/// Build a [`SimModelResult`] from a session simulate outcome, mirroring the
/// success/error classification of the former script-parsing path.
fn build_session_model_result(outcome: &OmcSimOutcome, elapsed: f64) -> SimModelResult {
    let mut error_text = outcome.error.clone();
    let messages = outcome.messages.trim();
    let messages_indicate_failure =
        messages.contains("Simulation Failed") || messages.contains("does not exist");
    if messages_indicate_failure {
        if !error_text.trim().is_empty() {
            error_text.push('\n');
        }
        error_text.push_str(messages);
    }
    let assertion_failures = omc_assertion_failure_lines(&error_text);
    let fatal = has_fatal_omc_error(&error_text)
        || !assertion_failures.is_empty()
        || messages_indicate_failure;
    let status = if fatal { "error" } else { "success" };
    let error = if fatal {
        Some(summarize_omc_error_with_assertions(
            &error_text,
            &assertion_failures,
        ))
    } else {
        None
    };
    SimModelResult {
        status: status.to_string(),
        error,
        sim_system_seconds: outcome.timing.time_simulation,
        total_system_seconds: outcome.timing.time_total,
        omc_wall_seconds: Some(round3(elapsed)),
        result_file: outcome.result_file.clone(),
        ..empty_omc_result()
    }
}

fn session_timeout_result(elapsed: f64, sim_timeout: Duration) -> SimModelResult {
    SimModelResult {
        status: "timeout".to_string(),
        error: Some(format!(
            "omc simulate exceeded {:.0}s budget (session killed and respawned)",
            sim_timeout.as_secs_f64()
        )),
        omc_wall_seconds: Some(round3(elapsed)),
        ..empty_omc_result()
    }
}

fn session_error_result(message: String) -> SimModelResult {
    SimModelResult {
        status: "error".to_string(),
        error: Some(message),
        ..empty_omc_result()
    }
}

/// A blank OMC result with every field unset; spread with `..` to fill in only
/// the fields a given outcome knows about.
fn empty_omc_result() -> SimModelResult {
    SimModelResult {
        status: String::new(),
        error: None,
        sim_system_seconds: None,
        total_system_seconds: None,
        omc_wall_seconds: None,
        result_file: None,
        trace_file: None,
        trace_error: None,
        rumoca_status: None,
        rumoca_ic_status: None,
        rumoca_ic_error: None,
        rumoca_ic_seconds: None,
        rumoca_sim_seconds: None,
        rumoca_sim_build_seconds: None,
        rumoca_sim_run_seconds: None,
        rumoca_sim_wall_seconds: None,
        rumoca_trace_file: None,
        rumoca_trace_error: None,
    }
}

fn summarize_omc_error_with_assertions(error_text: &str, assertion_failures: &[String]) -> String {
    let mut summary = summarize_omc_error(error_text, "");
    let missing_assertions = assertion_failures
        .iter()
        .filter(|assertion| !summary.contains(assertion.as_str()))
        .collect::<Vec<_>>();
    if !missing_assertions.is_empty() {
        if !summary.is_empty() {
            summary.push('\n');
        }
        summary.push_str(
            &missing_assertions
                .into_iter()
                .map(|assertion| format!("assertion: {assertion}"))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
    summary
}

fn omc_assertion_failure_lines(error_text: &str) -> Vec<String> {
    let mut lines = Vec::new();
    for raw_line in error_text.lines() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }
        let lower = line.to_ascii_lowercase();
        if lower.contains("assert")
            && (lower.contains("error") || lower.contains("violat") || lower.contains("fail"))
        {
            lines.push(line.to_string());
        }
    }
    lines.sort();
    lines.dedup();
    lines
}

fn ensure_omc_trace_artifacts(paths: &MslPaths, results: &mut BTreeMap<String, SimModelResult>) {
    for (model_name, result) in results {
        if !omc_result_can_produce_trace(result)
            || omc_trace_artifact_exists(paths, model_name, result)
        {
            continue;
        }
        let (trace_file, trace_error) =
            write_omc_trace_artifact(paths, model_name, result.result_file.as_deref());
        result.trace_file = trace_file;
        result.trace_error = trace_error;
    }
}

fn omc_trace_artifact_exists(paths: &MslPaths, model_name: &str, result: &SimModelResult) -> bool {
    resolve_declared_omc_trace_path(paths, model_name, result).is_some_and(|path| path.is_file())
}

fn omc_result_can_produce_trace(result: &SimModelResult) -> bool {
    result.status == "success" || result.result_file.is_some() || result.trace_file.is_some()
}

fn omc_model_is_trace_candidate(result: &SimModelResult) -> bool {
    result.rumoca_status.as_deref() == Some("sim_ok") && omc_result_can_produce_trace(result)
}

fn resolve_declared_omc_trace_path(
    paths: &MslPaths,
    model_name: &str,
    model: &SimModelResult,
) -> Option<PathBuf> {
    if let Some(trace_file) = model.trace_file.as_ref() {
        let path = PathBuf::from(trace_file);
        if path.is_absolute() {
            return Some(path);
        }
        return Some(paths.results_dir.join(path));
    }
    Some(paths.omc_trace_dir.join(format!("{model_name}.json")))
}

fn write_omc_trace_artifact(
    paths: &MslPaths,
    model_name: &str,
    result_file: Option<&str>,
) -> (Option<String>, Option<String>) {
    let Some(csv_path) = resolve_result_file_path(paths, model_name, result_file) else {
        return (None, Some("missing result CSV file".to_string()));
    };
    let Ok(trace) = load_omc_csv_trace(model_name, &csv_path) else {
        return (None, Some("failed to load CSV trace".to_string()));
    };
    let relative = PathBuf::from("sim_traces")
        .join("omc")
        .join(format!("{model_name}.json"));
    let trace_path = paths.results_dir.join(&relative);
    if let Err(error) = write_pretty_json(&trace_path, &trace) {
        return (None, Some(format!("failed to write trace JSON: {error}")));
    }
    (Some(relative.to_string_lossy().replace('\\', "/")), None)
}

fn resolve_result_file_path(
    paths: &MslPaths,
    model_name: &str,
    result_file: Option<&str>,
) -> Option<PathBuf> {
    let mut candidates = Vec::new();
    if let Some(result_file) = result_file {
        let mut candidate = PathBuf::from(result_file);
        if !candidate.is_absolute() {
            candidate = paths.sim_work_dir.join(candidate);
        }
        candidates.push(candidate.clone());
        if candidate.extension().and_then(|ext| ext.to_str()) != Some("csv") {
            candidates.push(candidate.with_extension("csv"));
        }
    }
    candidates.push(paths.sim_work_dir.join(format!("{model_name}_res.csv")));
    candidates.into_iter().find(|path| path.is_file())
}

fn load_omc_csv_trace(model_name: &str, csv_path: &Path) -> Result<SimTrace> {
    let content = std::fs::read_to_string(csv_path)
        .with_context(|| format!("failed to read '{}'", csv_path.display()))?;
    let mut rows = content.lines();
    let Some(header_row) = rows.next() else {
        bail!("empty CSV header");
    };
    let headers = parse_csv_row(header_row);
    let Some(time_index) = headers
        .iter()
        .position(|name| name.eq_ignore_ascii_case("time"))
    else {
        bail!("CSV has no 'time' column");
    };
    let value_indices = headers
        .iter()
        .enumerate()
        .filter(|(index, _)| *index != time_index)
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    let names = value_indices
        .iter()
        .map(|index| headers[*index].to_string())
        .collect::<Vec<_>>();
    let mut times = Vec::new();
    let mut data = vec![Vec::new(); value_indices.len()];
    for row in rows {
        let values = parse_csv_row(row);
        let Some(time_value) = values.get(time_index).and_then(|raw| parse_finite_f64(raw)) else {
            continue;
        };
        times.push(time_value);
        for (output_idx, source_idx) in value_indices.iter().enumerate() {
            let parsed = values
                .get(*source_idx)
                .and_then(|raw| parse_finite_f64(raw));
            data[output_idx].push(parsed);
        }
    }
    if times.is_empty() {
        bail!("no numeric rows in CSV trace");
    }
    Ok(SimTrace {
        model_name: Some(model_name.to_string()),
        times,
        names,
        data,
        variable_meta: None,
    })
}

fn parse_csv_row(row: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut chars = row.chars().peekable();
    let mut in_quotes = false;
    while let Some(ch) = chars.next() {
        if ch == '"' {
            if in_quotes && chars.peek() == Some(&'"') {
                current.push('"');
                let _ = chars.next();
            } else {
                in_quotes = !in_quotes;
            }
            continue;
        }
        if ch == ',' && !in_quotes {
            fields.push(current.trim().to_string());
            current.clear();
            continue;
        }
        current.push(ch);
    }
    fields.push(current.trim().to_string());
    fields
}

fn merge_cached_results_for_resume(
    cached_reference_path: &Path,
    target_models: &[String],
    all_results: &mut BTreeMap<String, SimModelResult>,
) -> Result<()> {
    if !cached_reference_path.is_file() {
        return Ok(());
    }
    let payload: Value = serde_json::from_str(
        &std::fs::read_to_string(cached_reference_path).with_context(|| {
            format!(
                "failed to read cached OMC parity file '{}'",
                cached_reference_path.display()
            )
        })?,
    )
    .with_context(|| {
        format!(
            "failed to parse cached OMC parity file '{}'",
            cached_reference_path.display()
        )
    })?;
    let Some(models) = payload.get("models").and_then(Value::as_object) else {
        return Ok(());
    };
    for model_name in target_models {
        let Some(cached) = models.get(model_name) else {
            continue;
        };
        let Ok(cached_result) = serde_json::from_value::<SimModelResult>(cached.clone()) else {
            continue;
        };
        match all_results.get_mut(model_name) {
            Some(current) => hydrate_omc_fields_from_cached(current, &cached_result),
            None => {
                all_results.insert(model_name.clone(), cached_result);
            }
        }
    }
    Ok(())
}
fn hydrate_omc_fields_from_cached(current: &mut SimModelResult, cached: &SimModelResult) {
    if current.error.is_none() {
        current.error = cached.error.clone();
    }
    if current.sim_system_seconds.is_none() {
        current.sim_system_seconds = cached.sim_system_seconds;
    }
    if current.total_system_seconds.is_none() {
        current.total_system_seconds = cached.total_system_seconds;
    }
    if current.omc_wall_seconds.is_none() {
        current.omc_wall_seconds = cached.omc_wall_seconds;
    }
    if current.result_file.is_none() {
        current.result_file = cached.result_file.clone();
    }
    if current.trace_file.is_none() {
        current.trace_file = cached.trace_file.clone();
    }
    if current.trace_error.is_none() {
        current.trace_error = cached.trace_error.clone();
    }
}
fn load_trace_exclusions(args: &Args, paths: &MslPaths) -> Result<BTreeMap<String, String>> {
    let default_file = paths.repo_root.join(DEFAULT_TRACE_EXCLUSIONS_FILE_REL);
    let file = args
        .trace_exclusions_file
        .clone()
        .map(|path| resolve_optional_path(&paths.repo_root, path))
        .unwrap_or(default_file);
    if !file.is_file() {
        return Ok(BTreeMap::new());
    }
    let names = load_target_models(&file).with_context(|| {
        format!(
            "failed to load trace exclusions model list from '{}'",
            file.display()
        )
    })?;
    if names.is_empty() {
        return Ok(BTreeMap::new());
    }
    let exclusions = names
        .into_iter()
        .map(|name| (name, STOCHASTIC_TRACE_EXCLUSION_REASON.to_string()))
        .collect::<BTreeMap<_, _>>();
    println!(
        "Trace comparison exclusions loaded: {} model(s) from {}",
        exclusions.len(),
        file.display()
    );
    Ok(exclusions)
}

fn quantify_trace_differences(
    paths: &MslPaths,
    all_results: &BTreeMap<String, SimModelResult>,
    trace_exclusions: &BTreeMap<String, String>,
) -> Result<TraceQuantification> {
    let mut report = TraceQuantification::default();
    for (model_name, omc_model) in all_results {
        if !omc_model_is_trace_candidate(omc_model) {
            continue;
        }
        if let Some(reason) = trace_exclusions.get(model_name) {
            report.skipped.insert(model_name.clone(), reason.clone());
            continue;
        }
        let Some(rumoca_trace_path) = resolve_rumoca_trace_path(paths, model_name, omc_model)
        else {
            report
                .missing_trace
                .insert(model_name.clone(), "missing rumoca trace path".to_string());
            continue;
        };
        let Some(omc_trace_path) = resolve_omc_trace_path(paths, model_name, omc_model) else {
            report
                .missing_trace
                .insert(model_name.clone(), "missing omc trace path".to_string());
            continue;
        };
        let rumoca_trace = match load_trace_json(&rumoca_trace_path) {
            Ok(trace) => trace,
            Err(error) => {
                report.missing_trace.insert(
                    model_name.clone(),
                    format!("failed to load rumoca trace: {error}"),
                );
                continue;
            }
        };
        let omc_trace = match load_trace_json(&omc_trace_path) {
            Ok(trace) => trace,
            Err(error) => {
                report.missing_trace.insert(
                    model_name.clone(),
                    format!("failed to load omc trace: {error}"),
                );
                continue;
            }
        };
        let metric = match compare_model_traces(model_name, &rumoca_trace, &omc_trace) {
            Ok(metric) => metric,
            Err(error) => {
                report
                    .skipped
                    .insert(model_name.clone(), format!("trace compare failed: {error}"));
                continue;
            }
        };
        let state_selection =
            state_selection::compare_model_state_selection(paths, model_name, &rumoca_trace);
        report.models.insert(
            model_name.clone(),
            TraceModelMetric {
                metric,
                state_selection,
                rumoca_sim_wall_seconds: omc_model.rumoca_sim_wall_seconds,
                rumoca_sim_seconds: omc_model.rumoca_sim_seconds,
                rumoca_sim_build_seconds: omc_model.rumoca_sim_build_seconds,
                rumoca_sim_run_seconds: omc_model.rumoca_sim_run_seconds,
                omc_sim_system_seconds: omc_model.sim_system_seconds,
                omc_total_system_seconds: omc_model.total_system_seconds,
                omc_wall_seconds: omc_model.omc_wall_seconds,
            },
        );
    }
    write_trace_report(paths, all_results, &report)?;
    Ok(report)
}

fn resolve_rumoca_trace_path(
    paths: &MslPaths,
    model_name: &str,
    model: &SimModelResult,
) -> Option<PathBuf> {
    if let Some(trace_file) = model.rumoca_trace_file.as_ref() {
        let path = PathBuf::from(trace_file);
        if path.is_absolute() {
            return Some(path);
        }
        return Some(paths.results_dir.join(path));
    }
    let fallback = paths.rumoca_trace_dir.join(format!("{model_name}.json"));
    if fallback.is_file() {
        Some(fallback)
    } else {
        None
    }
}

fn resolve_omc_trace_path(
    paths: &MslPaths,
    model_name: &str,
    model: &SimModelResult,
) -> Option<PathBuf> {
    resolve_declared_omc_trace_path(paths, model_name, model).filter(|path| path.is_file())
}

fn metric_distribution(values: impl Iterator<Item = f64>) -> Option<(f64, f64, f64, f64)> {
    let mut collected = values.filter(|value| value.is_finite()).collect::<Vec<_>>();
    if collected.is_empty() {
        return None;
    }
    collected.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
    let len = collected.len();
    let median = if len.is_multiple_of(2) {
        (collected[len / 2 - 1] + collected[len / 2]) / 2.0
    } else {
        collected[len / 2]
    };
    let min = *collected.first().unwrap_or(&0.0);
    let max = *collected.last().unwrap_or(&0.0);
    let mean = collected.iter().sum::<f64>() / len as f64;
    Some((min, median, mean, max))
}

fn finalize_and_write_output(
    args: &Args,
    paths: &MslPaths,
    selection: &ModelSelection,
    context: FinalizeContext,
    mut state: SimRunState,
    trace_report: TraceQuantification,
    rumoca_runtimes: &HashMap<String, RumocaRuntime>,
) -> Result<()> {
    state.batch_timings.sort_by_key(|batch| batch.batch_idx);
    let metrics = compute_run_metrics(context.total, &state);
    ensure_runtime_ratio_stats_present(&metrics, both_success_model_count(&state))?;
    let trace_summary = compute_trace_output_summary(&trace_report);
    let output = build_sim_output_payload(
        args,
        paths,
        selection,
        &context,
        &metrics,
        &trace_summary,
        &state,
    );
    let output_file = paths.results_dir.join("omc_simulation_reference.json");
    write_pretty_json(&output_file, &output)?;
    print_summary(&output_file, &context, &metrics, &trace_summary, &state);
    let agreeing_models = agreeing_model_names(&trace_report);
    speed_report::write_and_print_speed_comparison(
        paths,
        rumoca_runtimes,
        &state,
        &agreeing_models,
    )?;
    Ok(())
}

/// Models whose rumoca trace is in high or near (minor) agreement with OMC,
/// using the same per-model channel-distribution classifier as the headline
/// trace gate. Speed/scalability comparisons are restricted to this set so we
/// only compare compile times for models where BOTH tools produced a trace and
/// the results actually match.
fn agreeing_model_names(trace_report: &TraceQuantification) -> BTreeSet<String> {
    use rumoca_sim::sim_trace_compare::{
        AgreementBand, MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE, MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
        MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE, MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
        classify_trace_metric_channel_distribution,
    };
    trace_report
        .models
        .iter()
        .filter(|(_, model)| {
            matches!(
                classify_trace_metric_channel_distribution(
                    &model.metric,
                    MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
                    MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
                    MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
                    MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
                ),
                AgreementBand::HighAgreement | AgreementBand::MinorAgreement
            )
        })
        .map(|(name, _)| name.clone())
        .collect()
}

fn both_success_model_count(state: &SimRunState) -> usize {
    state
        .all_results
        .values()
        .filter(|result| {
            result.status == "success" && result.rumoca_status.as_deref() == Some("sim_ok")
        })
        .count()
}

fn ensure_runtime_ratio_stats_present(
    metrics: &RunMetrics,
    both_success_models: usize,
) -> Result<()> {
    if both_success_models == 0 {
        return Ok(());
    }
    let mut missing = Vec::new();
    if metrics.system_ratio_both_success.is_none() {
        missing.push("system_ratio_both_success");
    }
    if metrics.wall_ratio_both_success.is_none() {
        missing.push("wall_ratio_both_success");
    }
    if missing.is_empty() {
        return Ok(());
    }
    bail!(
        "missing runtime ratio stats for {} both-success model(s): {}. \
         Ensure OMC solver + external wall timings are captured before writing parity output.",
        both_success_models,
        missing.join(", ")
    );
}

fn compute_run_metrics(total: usize, state: &SimRunState) -> RunMetrics {
    let sim_successful = state
        .all_results
        .values()
        .filter(|result| result.status == "success")
        .count();
    let sim_failed = state
        .all_results
        .values()
        .filter(|result| result.status == "error")
        .count();
    let sim_timed_out = state
        .all_results
        .values()
        .filter(|result| result.status == "timeout")
        .count();
    let success_rate = if total == 0 {
        0.0
    } else {
        (sim_successful as f64 / total as f64) * 100.0
    };
    let runtime_totals = compute_runtime_totals(state);
    let ratio_stats = compute_runtime_ratio_buckets(state);
    let ran_batches = state
        .batch_timings
        .iter()
        .filter(|batch| !batch.skipped)
        .count();
    let skipped_batches = state
        .batch_timings
        .iter()
        .filter(|batch| batch.skipped)
        .count();
    RunMetrics {
        sim_successful,
        sim_failed,
        sim_timed_out,
        success_rate,
        total_omc_sim_system_seconds: runtime_totals.total_omc_sim_system_seconds,
        total_omc_total_system_seconds: runtime_totals.total_omc_total_system_seconds,
        total_omc_wall_seconds: runtime_totals.total_omc_wall_seconds,
        total_rumoca_sim_seconds: runtime_totals.total_rumoca_sim_seconds,
        total_rumoca_sim_build_seconds: runtime_totals.total_rumoca_sim_build_seconds,
        total_rumoca_sim_run_seconds: runtime_totals.total_rumoca_sim_run_seconds,
        total_rumoca_sim_wall_seconds: runtime_totals.total_rumoca_sim_wall_seconds,
        system_ratio_all_positive: ratio_stats.system_ratio_all_positive,
        system_ratio_both_success: ratio_stats.system_ratio_both_success,
        wall_ratio_all_positive: ratio_stats.wall_ratio_all_positive,
        wall_ratio_both_success: ratio_stats.wall_ratio_both_success,
        ran_batches,
        skipped_batches,
        batch_stats: summarize_batch_timings(&state.batch_timings),
    }
}

#[derive(Debug, Clone, Copy)]
struct RuntimeTotals {
    total_omc_sim_system_seconds: f64,
    total_omc_total_system_seconds: f64,
    total_omc_wall_seconds: f64,
    total_rumoca_sim_seconds: f64,
    total_rumoca_sim_build_seconds: f64,
    total_rumoca_sim_run_seconds: f64,
    total_rumoca_sim_wall_seconds: f64,
}

#[derive(Debug, Clone)]
struct RuntimeRatioBuckets {
    system_ratio_all_positive: Option<RuntimeRatioStats>,
    system_ratio_both_success: Option<RuntimeRatioStats>,
    wall_ratio_all_positive: Option<RuntimeRatioStats>,
    wall_ratio_both_success: Option<RuntimeRatioStats>,
}

fn compute_runtime_totals(state: &SimRunState) -> RuntimeTotals {
    RuntimeTotals {
        total_omc_sim_system_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.status == "success")
                .filter_map(|result| result.sim_system_seconds),
        ),
        total_omc_total_system_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.status == "success")
                .filter_map(|result| result.total_system_seconds),
        ),
        total_omc_wall_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.status == "success")
                .filter_map(|result| result.omc_wall_seconds),
        ),
        total_rumoca_sim_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.rumoca_status.as_deref() == Some("sim_ok"))
                .filter_map(|result| result.rumoca_sim_seconds),
        ),
        total_rumoca_sim_build_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.rumoca_status.as_deref() == Some("sim_ok"))
                .filter_map(|result| result.rumoca_sim_build_seconds),
        ),
        total_rumoca_sim_run_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.rumoca_status.as_deref() == Some("sim_ok"))
                .filter_map(rumoca_runtime_sim_seconds),
        ),
        total_rumoca_sim_wall_seconds: sum_metric(
            state
                .all_results
                .values()
                .filter(|result| result.rumoca_status.as_deref() == Some("sim_ok"))
                .filter_map(|result| result.rumoca_sim_wall_seconds),
        ),
    }
}

fn compute_runtime_ratio_buckets(state: &SimRunState) -> RuntimeRatioBuckets {
    let system_ratio_all_positive =
        compute_runtime_ratio_stats(state.all_results.values().filter_map(|result| {
            runtime_pair(
                rumoca_runtime_sim_seconds(result),
                result.sim_system_seconds,
            )
        }));
    let wall_ratio_all_positive =
        compute_runtime_ratio_stats(state.all_results.values().filter_map(|result| {
            runtime_pair(result.rumoca_sim_wall_seconds, result.omc_wall_seconds)
        }));
    let both_success = state.all_results.values().filter(|result| {
        result.status == "success" && result.rumoca_status.as_deref() == Some("sim_ok")
    });
    let system_ratio_both_success =
        compute_runtime_ratio_stats(both_success.clone().filter_map(|result| {
            runtime_pair(
                rumoca_runtime_sim_seconds(result),
                result.sim_system_seconds,
            )
        }));
    let wall_ratio_both_success = compute_runtime_ratio_stats(both_success.filter_map(|result| {
        runtime_pair(result.rumoca_sim_wall_seconds, result.omc_wall_seconds)
    }));
    RuntimeRatioBuckets {
        system_ratio_all_positive,
        system_ratio_both_success,
        wall_ratio_all_positive,
        wall_ratio_both_success,
    }
}

fn rumoca_runtime_sim_seconds(result: &SimModelResult) -> Option<f64> {
    result.rumoca_sim_run_seconds.or(result.rumoca_sim_seconds)
}

fn sum_metric(values: impl Iterator<Item = f64>) -> f64 {
    values.filter(|value| value.is_finite()).sum::<f64>()
}

fn runtime_pair(rumoca: Option<f64>, omc: Option<f64>) -> Option<(f64, f64)> {
    let rumoca = rumoca?;
    let omc = omc?;
    if !rumoca.is_finite() || !omc.is_finite() || rumoca <= 0.0 || omc <= 0.0 {
        return None;
    }
    // Ratio semantics are OMC/Rumoca so values > 1 mean Rumoca is faster.
    Some((omc, rumoca))
}

fn compute_runtime_ratio_stats(
    pairs: impl Iterator<Item = (f64, f64)>,
) -> Option<RuntimeRatioStats> {
    let mut ratios = Vec::new();
    let mut omc_sum = 0.0_f64;
    let mut rumoca_sum = 0.0_f64;
    for (omc, rumoca) in pairs {
        let ratio = omc / rumoca;
        if !ratio.is_finite() {
            continue;
        }
        ratios.push(ratio);
        omc_sum += omc;
        rumoca_sum += rumoca;
    }
    if ratios.is_empty() || !rumoca_sum.is_finite() || rumoca_sum <= 0.0 {
        return None;
    }
    ratios.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
    let sample_count = ratios.len();
    let mean_ratio = ratios.iter().sum::<f64>() / sample_count as f64;
    let median_ratio = if sample_count.is_multiple_of(2) {
        (ratios[sample_count / 2 - 1] + ratios[sample_count / 2]) / 2.0
    } else {
        ratios[sample_count / 2]
    };
    Some(RuntimeRatioStats {
        sample_count,
        aggregate_ratio: omc_sum / rumoca_sum,
        min_ratio: *ratios.first().unwrap_or(&0.0),
        max_ratio: *ratios.last().unwrap_or(&0.0),
        mean_ratio,
        median_ratio,
    })
}

fn parse_finite_f64(raw: &str) -> Option<f64> {
    raw.parse::<f64>().ok().filter(|value| value.is_finite())
}
