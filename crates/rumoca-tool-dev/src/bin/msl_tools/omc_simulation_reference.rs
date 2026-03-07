use crate::common::{
    AUTO_WORKERS_DEFAULT, BATCH_SIZE_OMC_SIMULATION_DEFAULT, BATCH_TIMEOUT_SECONDS_DEFAULT,
    BatchElapsedStats, BatchTimingDetail, MSL_VERSION, MslPaths, OMC_THREADS_DEFAULT, PendingBatch,
    SIM_STOP_TIME_DEFAULT, apply_omc_thread_env, choose_effective_batch_size, get_git_commit,
    get_omc_version, has_fatal_omc_error, load_target_models, msl_load_lines, resolve_worker_count,
    round3, run_command_with_timeout, run_parallel_batches_with_progress, summarize_batch_timings,
    summarize_omc_error, unix_timestamp_seconds, write_pretty_json,
};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use rumoca_session::analysis::{
    ModelDeviationMetric, SimTrace, compare_model_traces, load_trace_json,
};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

mod output;
#[cfg(test)]
mod tests;
use output::{
    build_sim_output_payload, compute_trace_output_summary, print_summary, write_trace_report,
};

const DEFAULT_TRACE_EXCLUSIONS_FILE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json";
const STOCHASTIC_TRACE_EXCLUSION_REASON: &str =
    "stochastic random-input model; skipped until generator + seed parity is implemented";

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct Args {
    /// Generate .mos scripts only
    #[arg(long, default_value_t = false)]
    dry_run: bool,
    /// Models per batch
    #[arg(long, default_value_t = BATCH_SIZE_OMC_SIMULATION_DEFAULT)]
    batch_size: usize,
    /// Skip completed batches
    #[arg(long, default_value_t = false)]
    resume: bool,
    /// Parallel OMC batches (0 = auto)
    #[arg(long, default_value_t = AUTO_WORKERS_DEFAULT)]
    workers: usize,
    /// Thread cap applied to each spawned OMC process (OMP/BLAS)
    #[arg(long, default_value_t = OMC_THREADS_DEFAULT)]
    omc_threads: usize,
    /// Timeout per batch in seconds (with default batch_size=1, this is per model)
    #[arg(long, default_value_t = BATCH_TIMEOUT_SECONDS_DEFAULT)]
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
    /// Balance results JSON used for target selection
    #[arg(long)]
    balance_results_file: Option<PathBuf>,
    /// Optional explicit model list JSON (array or object.model_names)
    #[arg(long)]
    target_models_file: Option<PathBuf>,
    /// Optional trace-exclusion model list JSON (array or object.model_names)
    #[arg(long)]
    trace_exclusions_file: Option<PathBuf>,
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
    rumoca_sim_seconds: Option<f64>,
    rumoca_sim_wall_seconds: Option<f64>,
    rumoca_trace_file: Option<String>,
    rumoca_trace_error: Option<String>,
}

#[derive(Debug, Clone)]
struct SimBatchRunOutput {
    requested_models: usize,
    parsed_models: usize,
    elapsed_seconds: f64,
    timed_out: bool,
    results: BTreeMap<String, SimModelResult>,
}

#[derive(Debug, Clone)]
struct SimRunState {
    all_results: BTreeMap<String, SimModelResult>,
    batch_timings: Vec<BatchTimingDetail>,
    pending_batches: Vec<PendingBatch>,
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
    rumoca_sim_wall_seconds: Option<f64>,
    rumoca_sim_seconds: Option<f64>,
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
    sim_seconds: Option<f64>,
    sim_wall_seconds: Option<f64>,
    trace_file: Option<String>,
    trace_error: Option<String>,
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
    models_with_any_channel_deviation: usize,
    models_with_any_channel_deviation_percent: f64,
    max_model_channel_deviation_percent: f64,
    min_model_bounded_normalized_l1: f64,
    median_model_bounded_normalized_l1: f64,
    mean_model_bounded_normalized_l1: f64,
    max_model_bounded_normalized_l1: f64,
    mean_model_mean_channel_bounded_normalized_l1: f64,
    max_model_max_channel_bounded_normalized_l1: f64,
}

fn simulation_stop_time_override() -> Option<f64> {
    std::env::var("RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE")
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|value| value.is_finite() && *value > 0.0)
}

pub(crate) fn run(args: Args) -> Result<()> {
    let mut args = args;
    if let Some(stop_time_override) = simulation_stop_time_override() {
        args.stop_time = stop_time_override;
        args.use_experiment_stop_time = false;
        println!(
            "OMC stopTime override active: {} (via RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE)",
            stop_time_override
        );
    }

    let paths = MslPaths::current();
    ensure_msl_available(&paths)?;
    let workers = resolve_worker_count(args.workers)?;
    let omc_version = get_omc_version();
    let git_commit = get_git_commit(&paths.repo_root);

    std::fs::create_dir_all(&paths.results_dir)
        .with_context(|| format!("failed to create '{}'", paths.results_dir.display()))?;
    std::fs::create_dir_all(&paths.sim_work_dir)
        .with_context(|| format!("failed to create '{}'", paths.sim_work_dir.display()))?;
    prepare_omc_trace_dir(&args, &paths)?;

    let selection = select_models(&args, &paths)?;
    let model_names = truncate_models(selection.names.clone(), args.max_models);
    let total = model_names.len();
    let effective_batch_size = choose_effective_batch_size(total, args.batch_size, workers)?;
    let n_batches = if total == 0 {
        0
    } else {
        total.div_ceil(effective_batch_size)
    };
    print_selection_summary(&args, workers, &selection, total, effective_batch_size);

    if args.dry_run {
        return run_dry_run(&paths, &model_names, &args);
    }

    let overall_start = Instant::now();
    let missing_omc_wall = models_missing_omc_wall_timing(
        &paths.results_dir.join("omc_simulation_reference.json"),
        &model_names,
    )?;
    let mut state = prepare_run_state(
        &args,
        &model_names,
        &paths.sim_work_dir,
        effective_batch_size,
        &missing_omc_wall,
    );
    merge_cached_results_for_resume(
        &paths.results_dir.join("omc_simulation_reference.json"),
        &model_names,
        &mut state.all_results,
    )?;
    run_pending_batches(&args, workers, &paths.sim_work_dir, &mut state)?;
    attach_rumoca_runtime(&paths, &mut state.all_results)?;
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
    };
    finalize_and_write_output(&args, &paths, &selection, context, state, trace_report)
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
    if !args.resume && paths.omc_trace_dir.exists() {
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
        let mut names = load_target_models(&resolved)?;
        names.sort();
        return Ok(ModelSelection {
            names,
            source_file: resolved,
            rule: "explicit model list from --target-models-file".to_string(),
            selection_seconds: start.elapsed().as_secs_f64(),
        });
    }

    let generated_targets = paths.results_dir.join("msl_simulation_targets.json");
    if generated_targets.is_file() {
        let mut names = load_target_models(&generated_targets)?;
        names.sort();
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
        .join("crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets_180.json");
    if committed_targets.is_file() {
        let mut names = load_target_models(&committed_targets)?;
        names.sort();
        return Ok(ModelSelection {
            names,
            source_file: committed_targets,
            rule: "default committed 180-model target list".to_string(),
            selection_seconds: start.elapsed().as_secs_f64(),
        });
    }

    let balance_file = resolve_optional_path(
        &paths.repo_root,
        args.balance_results_file
            .clone()
            .unwrap_or_else(|| paths.results_dir.join("msl_balance_results.json")),
    );
    let mut names = load_simulation_targets(&balance_file)?;
    names.sort();
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

fn run_dry_run(paths: &MslPaths, model_names: &[String], args: &Args) -> Result<()> {
    let sample = model_names.iter().take(10).cloned().collect::<Vec<_>>();
    let script = generate_sim_script(
        paths,
        0,
        &sample,
        args.stop_time,
        args.use_experiment_stop_time,
    );
    let mos_file = paths.sim_work_dir.join("sim_dry_run_sample.mos");
    std::fs::write(&mos_file, script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;
    println!("Dry run: sample script written to {}", mos_file.display());
    Ok(())
}

fn prepare_run_state(
    args: &Args,
    model_names: &[String],
    work_dir: &Path,
    batch_size: usize,
    models_missing_omc_wall: &BTreeSet<String>,
) -> SimRunState {
    let mut all_results = BTreeMap::new();
    let mut batch_timings = Vec::new();
    let mut pending_batches = Vec::new();
    let n_batches = if model_names.is_empty() {
        0
    } else {
        model_names.len().div_ceil(batch_size)
    };
    for batch_idx in 0..n_batches {
        let start_idx = batch_idx * batch_size;
        let end_idx = (start_idx + batch_size).min(model_names.len());
        let models = model_names[start_idx..end_idx].to_vec();
        if args.resume
            && let Some((parsed, timing)) =
                try_resume_completed_batch(work_dir, batch_idx, &models, models_missing_omc_wall)
        {
            all_results.extend(parsed);
            batch_timings.push(timing);
            continue;
        }
        pending_batches.push(PendingBatch {
            batch_idx,
            start_idx,
            end_idx,
            models,
        });
    }
    SimRunState {
        all_results,
        batch_timings,
        pending_batches,
    }
}

fn try_resume_completed_batch(
    work_dir: &Path,
    batch_idx: usize,
    models: &[String],
    models_missing_omc_wall: &BTreeSet<String>,
) -> Option<(BTreeMap<String, SimModelResult>, BatchTimingDetail)> {
    let parsed = parse_sim_results(work_dir, batch_idx);
    if parsed.len() != models.len() {
        return None;
    }
    let missing_omc_wall_in_batch = models
        .iter()
        .any(|model| models_missing_omc_wall.contains(model));
    if missing_omc_wall_in_batch {
        println!(
            "  Batch {batch_idx}: rerunning (missing OMC wall timing in cached parity data, {} models)",
            parsed.len()
        );
        return None;
    }
    println!(
        "  Batch {batch_idx}: skipped (already complete, {} models)",
        parsed.len()
    );
    let timing = BatchTimingDetail {
        batch_idx,
        requested_models: models.len(),
        parsed_models: models.len(),
        elapsed_seconds: 0.0,
        timed_out: false,
        skipped: true,
    };
    Some((parsed, timing))
}

fn models_missing_omc_wall_timing(
    cached_reference_path: &Path,
    target_models: &[String],
) -> Result<BTreeSet<String>> {
    if !cached_reference_path.is_file() {
        return Ok(target_models.iter().cloned().collect());
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
    let models = payload
        .get("models")
        .and_then(Value::as_object)
        .cloned()
        .unwrap_or_default();
    let mut missing = BTreeSet::new();
    for model_name in target_models {
        let Some(model) = models.get(model_name) else {
            missing.insert(model_name.clone());
            continue;
        };
        let status = model
            .get("status")
            .and_then(Value::as_str)
            .unwrap_or_default();
        if status != "success" {
            continue;
        }
        let has_omc_wall = model
            .get("omc_wall_seconds")
            .and_then(Value::as_f64)
            .is_some_and(|value| value.is_finite() && value >= 0.0);
        if !has_omc_wall {
            missing.insert(model_name.clone());
        }
    }
    Ok(missing)
}

fn run_pending_batches(
    args: &Args,
    workers: usize,
    work_dir: &Path,
    state: &mut SimRunState,
) -> Result<()> {
    if workers == 1 {
        run_batches_serial(args, work_dir, state)?;
        return Ok(());
    }
    println!(
        "Running {} batches with {workers} workers...",
        state.pending_batches.len()
    );
    let total_batches = state.pending_batches.len();
    let work_dir = work_dir.to_path_buf();
    let paths = MslPaths::current();
    let args = args.clone();
    let outputs = run_parallel_batches_with_progress(
        state.pending_batches.clone(),
        workers,
        move |batch| run_sim_batch(&paths, &args, &work_dir, batch),
        move |batch, output| {
            println!(
                "  Done batch {}/{} (models {}-{}): {}/{} in {:.1}s [{}]",
                batch.batch_idx + 1,
                total_batches,
                batch.start_idx + 1,
                batch.end_idx,
                output.parsed_models,
                output.requested_models,
                output.elapsed_seconds,
                if output.timed_out { "timeout" } else { "ok" }
            );
        },
    )?;
    consume_batch_outputs(state, outputs);
    Ok(())
}

fn run_batches_serial(args: &Args, work_dir: &Path, state: &mut SimRunState) -> Result<()> {
    let paths = MslPaths::current();
    for batch in state.pending_batches.clone() {
        println!(
            "Processing batch {}/{} (models {}-{})...",
            batch.batch_idx + 1,
            state.pending_batches.len(),
            batch.start_idx + 1,
            batch.end_idx
        );
        let output = run_sim_batch(&paths, args, work_dir, batch.clone())?;
        println!(
            "  Batch {}: {}/{} in {:.1}s [{}]",
            batch.batch_idx,
            output.parsed_models,
            output.requested_models,
            output.elapsed_seconds,
            if output.timed_out { "timeout" } else { "ok" }
        );
        state.all_results.extend(output.results);
        state.batch_timings.push(BatchTimingDetail {
            batch_idx: batch.batch_idx,
            requested_models: output.requested_models,
            parsed_models: output.parsed_models,
            elapsed_seconds: round3(output.elapsed_seconds),
            timed_out: output.timed_out,
            skipped: false,
        });
    }
    Ok(())
}

fn consume_batch_outputs(state: &mut SimRunState, outputs: Vec<(PendingBatch, SimBatchRunOutput)>) {
    for (batch, output) in outputs {
        state.all_results.extend(output.results);
        state.batch_timings.push(BatchTimingDetail {
            batch_idx: batch.batch_idx,
            requested_models: output.requested_models,
            parsed_models: output.parsed_models,
            elapsed_seconds: round3(output.elapsed_seconds),
            timed_out: output.timed_out,
            skipped: false,
        });
    }
}

fn run_sim_batch(
    paths: &MslPaths,
    args: &Args,
    work_dir: &Path,
    batch: PendingBatch,
) -> Result<SimBatchRunOutput> {
    let script = generate_sim_script(
        paths,
        batch.batch_idx,
        &batch.models,
        args.stop_time,
        args.use_experiment_stop_time,
    );
    let mos_file = work_dir.join(format!("sim_batch_{}.mos", batch.batch_idx));
    std::fs::write(&mos_file, script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;
    if args.dry_run {
        return Ok(SimBatchRunOutput {
            requested_models: batch.models.len(),
            parsed_models: 0,
            elapsed_seconds: 0.0,
            timed_out: false,
            results: BTreeMap::new(),
        });
    }

    let start = Instant::now();
    let mut command = Command::new("omc");
    command.arg(&mos_file).current_dir(work_dir);
    apply_omc_thread_env(&mut command, args.omc_threads);
    let run = run_command_with_timeout(
        &mut command,
        Duration::from_secs(args.batch_timeout_seconds),
    )
    .with_context(|| {
        format!(
            "failed to run OMC simulation batch '{}'",
            mos_file.display()
        )
    })?;
    let elapsed_seconds = start.elapsed().as_secs_f64();

    let mut results = parse_sim_results(work_dir, batch.batch_idx);
    let omc_records = parse_omc_simulation_records(&format!("{}\n{}", run.stdout, run.stderr));
    attach_omc_record_metrics(&mut results, &batch.models, &omc_records);
    attach_omc_traces(paths, &mut results);
    fill_missing_batch_entries(&mut results, &batch.models, run.timed_out);
    attach_omc_wall_seconds(&mut results, &batch.models, elapsed_seconds);

    Ok(SimBatchRunOutput {
        requested_models: batch.models.len(),
        parsed_models: results.len(),
        elapsed_seconds,
        timed_out: run.timed_out,
        results,
    })
}

fn attach_omc_wall_seconds(
    results: &mut BTreeMap<String, SimModelResult>,
    batch_models: &[String],
    elapsed_seconds: f64,
) {
    if batch_models.len() != 1 || !elapsed_seconds.is_finite() || elapsed_seconds < 0.0 {
        return;
    }
    if let Some(result) = batch_models.first().and_then(|name| results.get_mut(name)) {
        result.omc_wall_seconds = Some(elapsed_seconds);
    }
}

fn generate_sim_script(
    paths: &MslPaths,
    batch_idx: usize,
    batch_models: &[String],
    stop_time: f64,
    use_experiment_stop_time: bool,
) -> String {
    let mut lines = msl_load_lines(paths);
    let check_file = paths
        .sim_work_dir
        .join(format!("sim_batch_{batch_idx}.txt"));
    lines.push(format!("writeFile(\"{}\", \"\");", check_file.display()));
    for model in batch_models {
        let result_file = format!("{model}_res.csv");
        if use_experiment_stop_time {
            lines.push(format!(
                "simRes := simulate({model}, outputFormat=\"csv\", fileNamePrefix=\"{model}\");"
            ));
        } else {
            lines.push(format!(
                "simRes := simulate({model}, stopTime={stop_time}, outputFormat=\"csv\", fileNamePrefix=\"{model}\");"
            ));
        }
        lines.push("err := getErrorString();".to_string());
        lines.push(format!(
            "writeFile(\"{}\", \"MODEL:{model}\\nRESULT_FILE:{result_file}\\nERROR:\" + err + \"\\n---\\n\", append=true);",
            check_file.display()
        ));
    }
    lines.join("\n")
}

fn parse_sim_results(work_dir: &Path, batch_idx: usize) -> BTreeMap<String, SimModelResult> {
    let check_file = work_dir.join(format!("sim_batch_{batch_idx}.txt"));
    if !check_file.exists() {
        return BTreeMap::new();
    }
    let Ok(content) = std::fs::read_to_string(&check_file) else {
        return BTreeMap::new();
    };
    let mut results = BTreeMap::new();
    for entry in content.split("---\n") {
        let Some((model_name, result)) = parse_sim_entry(entry) else {
            continue;
        };
        results.insert(model_name, result);
    }
    results
}

fn parse_sim_entry(entry: &str) -> Option<(String, SimModelResult)> {
    let mut model_name = None;
    let mut error_text = String::new();
    let mut result_file = None;
    let mut sim_time = None;
    let mut total_time = None;
    let mut mode = SimEntryMode::None;
    for raw_line in entry.lines() {
        if let Some(value) = raw_line.strip_prefix("MODEL:") {
            model_name = Some(value.trim().to_string());
            mode = SimEntryMode::None;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("SIM_TIME:") {
            sim_time = parse_finite_f64(value.trim());
            mode = SimEntryMode::None;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("TOTAL_TIME:") {
            total_time = parse_finite_f64(value.trim());
            mode = SimEntryMode::None;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("RESULT_FILE:") {
            let value = value.trim();
            result_file = if value.is_empty() {
                None
            } else {
                Some(value.to_string())
            };
            mode = SimEntryMode::None;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("ERROR:") {
            append_line(&mut error_text, value);
            mode = SimEntryMode::Error;
            continue;
        }
        if mode == SimEntryMode::Error {
            append_line(&mut error_text, raw_line);
        }
    }
    let model_name = model_name?;
    let fatal = has_fatal_omc_error(&error_text);
    let status = if fatal { "error" } else { "success" };
    let error = if fatal {
        Some(summarize_omc_error(&error_text, ""))
    } else {
        None
    };
    Some((
        model_name,
        SimModelResult {
            status: status.to_string(),
            error,
            sim_system_seconds: sim_time,
            total_system_seconds: total_time,
            omc_wall_seconds: None,
            result_file,
            trace_file: None,
            trace_error: None,
            rumoca_status: None,
            rumoca_sim_seconds: None,
            rumoca_sim_wall_seconds: None,
            rumoca_trace_file: None,
            rumoca_trace_error: None,
        },
    ))
}

fn parse_omc_simulation_records(output_text: &str) -> HashMap<String, HashMap<String, String>> {
    let mut records = HashMap::new();
    let mut remaining = output_text;
    while let Some(start_idx) = remaining.find("record SimulationResult") {
        remaining = &remaining[start_idx..];
        let Some(end_idx) = remaining.find("end SimulationResult;") else {
            break;
        };
        let block = &remaining[..end_idx];
        let result_file = extract_record_field_string(block, "resultFile");
        let sim_time = extract_record_field_string(block, "timeSimulation");
        let total_time = extract_record_field_string(block, "timeTotal");
        if let Some(model_name) = result_file.as_deref().and_then(model_name_from_result_file) {
            let mut map = HashMap::new();
            if let Some(result_file) = result_file {
                map.insert("result_file".to_string(), result_file);
            }
            if let Some(sim_time) = sim_time {
                map.insert("sim_system_seconds".to_string(), sim_time);
            }
            if let Some(total_time) = total_time {
                map.insert("total_system_seconds".to_string(), total_time);
            }
            records.insert(model_name, map);
        }
        remaining = &remaining[end_idx + "end SimulationResult;".len()..];
    }
    records
}

fn extract_record_field_string(block: &str, field: &str) -> Option<String> {
    let needle = format!("{field} =");
    let start = block.find(&needle)?;
    let value_text = block[start + needle.len()..].trim_start();
    let value_end = value_text.find(',').unwrap_or(value_text.len());
    let raw = value_text[..value_end].trim();
    let unquoted = raw.trim_matches('"').trim();
    if unquoted.is_empty() {
        None
    } else {
        Some(unquoted.to_string())
    }
}

fn model_name_from_result_file(result_file: &str) -> Option<String> {
    let file_name = Path::new(result_file).file_name()?.to_string_lossy();
    for suffix in ["_res.csv", "_res.mat", "_res.plt"] {
        if let Some(stripped) = file_name.strip_suffix(suffix) {
            return Some(stripped.to_string());
        }
    }
    None
}

fn attach_omc_record_metrics(
    results: &mut BTreeMap<String, SimModelResult>,
    batch_models: &[String],
    records: &HashMap<String, HashMap<String, String>>,
) {
    for model_name in batch_models {
        let Some(record) = records.get(model_name) else {
            if let Some(result) = results.get_mut(model_name)
                && result.result_file.is_none()
            {
                result.result_file = Some(format!("{model_name}_res.csv"));
            }
            continue;
        };
        let result = results
            .entry(model_name.clone())
            .or_insert_with(|| SimModelResult {
                status: "success".to_string(),
                error: None,
                sim_system_seconds: None,
                total_system_seconds: None,
                omc_wall_seconds: None,
                result_file: Some(format!("{model_name}_res.csv")),
                trace_file: None,
                trace_error: None,
                rumoca_status: None,
                rumoca_sim_seconds: None,
                rumoca_sim_wall_seconds: None,
                rumoca_trace_file: None,
                rumoca_trace_error: None,
            });
        result.status = "success".to_string();
        result.error = None;
        if let Some(value) = record
            .get("sim_system_seconds")
            .and_then(|value| parse_finite_f64(value))
        {
            result.sim_system_seconds = Some(value);
        }
        if let Some(value) = record
            .get("total_system_seconds")
            .and_then(|value| parse_finite_f64(value))
        {
            result.total_system_seconds = Some(value);
        }
        if let Some(value) = record.get("result_file").cloned() {
            result.result_file = Some(value);
        }
    }
}

fn attach_omc_traces(paths: &MslPaths, results: &mut BTreeMap<String, SimModelResult>) {
    for (model_name, result) in results {
        if result.status != "success" {
            continue;
        }
        let (trace_file, trace_error) =
            write_omc_trace_artifact(paths, model_name, result.result_file.as_deref());
        result.trace_file = trace_file;
        result.trace_error = trace_error;
    }
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
    (Some(relative.display().to_string()), None)
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

fn fill_missing_batch_entries(
    results: &mut BTreeMap<String, SimModelResult>,
    batch_models: &[String],
    timed_out: bool,
) {
    for model_name in batch_models {
        if results.contains_key(model_name) {
            continue;
        }
        results.insert(
            model_name.clone(),
            SimModelResult {
                status: if timed_out {
                    "timeout".to_string()
                } else {
                    "error".to_string()
                },
                error: Some(if timed_out {
                    "batch timeout".to_string()
                } else {
                    "missing batch result".to_string()
                }),
                sim_system_seconds: None,
                total_system_seconds: None,
                omc_wall_seconds: None,
                result_file: None,
                trace_file: None,
                trace_error: None,
                rumoca_status: None,
                rumoca_sim_seconds: None,
                rumoca_sim_wall_seconds: None,
                rumoca_trace_file: None,
                rumoca_trace_error: None,
            },
        );
    }
}

fn attach_rumoca_runtime(
    paths: &MslPaths,
    all_results: &mut BTreeMap<String, SimModelResult>,
) -> Result<()> {
    let runtimes = load_rumoca_runtime(path_for_rumoca_results(paths))?;
    for (model_name, result) in all_results {
        let Some(runtime) = runtimes.get(model_name) else {
            continue;
        };
        result.rumoca_status = Some(runtime.status.clone());
        result.rumoca_sim_seconds = runtime.sim_seconds;
        result.rumoca_sim_wall_seconds = runtime.sim_wall_seconds;
        result.rumoca_trace_file = runtime.trace_file.clone();
        result.rumoca_trace_error = runtime.trace_error.clone();
    }
    Ok(())
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
fn path_for_rumoca_results(paths: &MslPaths) -> PathBuf {
    paths.results_dir.join("msl_results.json")
}

fn load_rumoca_runtime(path: PathBuf) -> Result<HashMap<String, RumocaRuntime>> {
    if !path.exists() {
        return Ok(HashMap::new());
    }
    let payload: Value = serde_json::from_str(
        &std::fs::read_to_string(&path)
            .with_context(|| format!("failed to read '{}'", path.display()))?,
    )
    .with_context(|| format!("failed to parse '{}'", path.display()))?;
    let model_results = payload
        .get("model_results")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let mut runtimes = HashMap::new();
    for model in model_results {
        let Some(name) = model.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        let Some(status) = model.get("sim_status").and_then(Value::as_str) else {
            continue;
        };
        runtimes.insert(
            name.to_string(),
            RumocaRuntime {
                status: status.to_string(),
                sim_seconds: parse_json_float(model.get("sim_seconds")),
                sim_wall_seconds: parse_json_float(model.get("sim_wall_seconds")),
                trace_file: model
                    .get("sim_trace_file")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
                trace_error: model
                    .get("sim_trace_error")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
            },
        );
    }
    Ok(runtimes)
}

fn parse_json_float(value: Option<&Value>) -> Option<f64> {
    value
        .and_then(Value::as_f64)
        .filter(|value| value.is_finite())
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
        if omc_model.status != "success" {
            continue;
        }
        if omc_model.rumoca_status.as_deref() != Some("sim_ok") {
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
        report.models.insert(
            model_name.clone(),
            TraceModelMetric {
                metric,
                rumoca_sim_wall_seconds: omc_model.rumoca_sim_wall_seconds,
                rumoca_sim_seconds: omc_model.rumoca_sim_seconds,
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
    if let Some(trace_file) = model.trace_file.as_ref() {
        let path = PathBuf::from(trace_file);
        if path.is_absolute() {
            return Some(path);
        }
        return Some(paths.results_dir.join(path));
    }
    let fallback = paths.omc_trace_dir.join(format!("{model_name}.json"));
    if fallback.is_file() {
        Some(fallback)
    } else {
        None
    }
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
    print_summary(&output_file, &context, &metrics, &trace_summary);
    Ok(())
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
            runtime_pair(result.rumoca_sim_seconds, result.sim_system_seconds)
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
            runtime_pair(result.rumoca_sim_seconds, result.sim_system_seconds)
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

fn append_line(buffer: &mut String, line: &str) {
    if !buffer.is_empty() {
        buffer.push('\n');
    }
    buffer.push_str(line.trim_end());
}

fn parse_finite_f64(raw: &str) -> Option<f64> {
    raw.parse::<f64>().ok().filter(|value| value.is_finite())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SimEntryMode {
    None,
    Error,
}
