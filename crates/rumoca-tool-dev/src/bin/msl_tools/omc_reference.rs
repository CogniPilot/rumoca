use crate::common::{
    AUTO_WORKERS_DEFAULT, BATCH_SIZE_OMC_REFERENCE_DEFAULT, BatchTimingDetail, EXCLUDE_PREFIXES,
    MSL_TOP_PACKAGES, MSL_VERSION, MslPaths, OMC_THREADS_DEFAULT, PendingBatch,
    apply_omc_thread_env, choose_effective_batch_size, get_git_commit, get_omc_version,
    has_fatal_omc_error, load_target_models, msl_load_lines, resolve_worker_count, round3,
    run_command_with_timeout, run_parallel_batches_with_progress, summarize_batch_timings,
    summarize_omc_error, unix_timestamp_seconds, write_pretty_json,
};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use serde::Serialize;
use serde_json::json;
use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

const DISCOVER_TIMEOUT_SECONDS: u64 = 300;
// Match rumoca simulation worker defaults: 5s model budget + 1s process grace.
const MODEL_TIMEOUT_SECONDS_DEFAULT: u64 = 30;
const WORKER_TIMEOUT_GRACE_SECONDS: u64 = 1;

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct Args {
    /// Generate .mos scripts only
    #[arg(long, default_value_t = false)]
    dry_run: bool,
    /// Models per OMC invocation (default: 1 for model-level isolation)
    #[arg(long, default_value_t = BATCH_SIZE_OMC_REFERENCE_DEFAULT)]
    batch_size: usize,
    /// Skip already completed batches
    #[arg(long, default_value_t = false)]
    resume: bool,
    /// Parallel OMC batches (0 = auto)
    #[arg(long, default_value_t = AUTO_WORKERS_DEFAULT)]
    workers: usize,
    /// Thread cap applied to each spawned OMC process (OMP/BLAS)
    #[arg(long, default_value_t = OMC_THREADS_DEFAULT)]
    omc_threads: usize,
    /// OMC model timeout in seconds (legacy alias: --batch-timeout-seconds)
    #[arg(
        long = "model-timeout-seconds",
        alias = "batch-timeout-seconds",
        default_value_t = MODEL_TIMEOUT_SECONDS_DEFAULT
    )]
    model_timeout_seconds: u64,
    /// Limit number of discovered models (0 = all)
    #[arg(long, default_value_t = 0)]
    max_models: usize,
    /// Optional explicit target model list JSON
    #[arg(long)]
    target_models_file: Option<PathBuf>,
    /// Optional run id for work-file isolation under `omc_work/RUN_ID`
    #[arg(long)]
    run_id: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct ModelResult {
    status: String,
    equations: Option<usize>,
    variables: Option<usize>,
    trivial: Option<usize>,
    error: Option<String>,
    flat_file: String,
}

#[derive(Debug, Clone)]
struct BatchRunOutput {
    requested_models: usize,
    parsed_models: usize,
    elapsed_seconds: f64,
    timed_out: bool,
    results: BTreeMap<String, ModelResult>,
}

#[derive(Debug, Clone)]
struct ModelSelection {
    names: Vec<String>,
    source: String,
    source_file: Option<PathBuf>,
    discover_seconds: f64,
}

pub(crate) fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    ensure_msl_available(&paths)?;
    let workers = resolve_worker_count(args.workers)?;
    let omc_version = get_omc_version();
    let git_commit = get_git_commit(&paths.repo_root);
    println!("OMC version: {omc_version}");
    println!("Workers: {workers} (requested {})", args.workers);
    println!("OMC process thread cap: {}", args.omc_threads);

    let run_id = resolve_run_id(&args);
    let run_work_dir = paths.work_dir_for_run_id(run_id.as_deref());
    std::fs::create_dir_all(&paths.flat_dir)
        .with_context(|| format!("failed to create '{}'", paths.flat_dir.display()))?;
    std::fs::create_dir_all(&run_work_dir)
        .with_context(|| format!("failed to create '{}'", run_work_dir.display()))?;
    println!("Work directory: {}", run_work_dir.display());

    let selection = select_models(&args, &paths, &run_work_dir)?;
    let model_names = truncate_models(selection.names.clone(), args.max_models);
    let total = model_names.len();
    let effective_batch_size = choose_effective_batch_size(total, args.batch_size, workers)?;
    print_selection_summary(&selection, total, args.batch_size, effective_batch_size);

    if args.dry_run {
        return run_dry_run(&paths, &run_work_dir, &model_names);
    }

    let n_batches = if total == 0 {
        0
    } else {
        total.div_ceil(effective_batch_size)
    };
    let overall_start = Instant::now();
    let mut run_state = prepare_run_state(&args, &model_names, &run_work_dir, effective_batch_size);
    run_pending_batches(&args, workers, &run_work_dir, &mut run_state)?;
    let overall_elapsed = overall_start.elapsed().as_secs_f64();
    let context = FinalizeContext {
        omc_version,
        git_commit,
        total,
        n_batches,
        workers,
        effective_batch_size,
        overall_elapsed,
    };
    finalize_and_write_output(&args, &paths, &selection, context, run_state)
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

fn resolve_run_id(args: &Args) -> Option<String> {
    if let Some(run_id) = args.run_id.clone() {
        let trimmed = run_id.trim().to_string();
        return if trimmed.is_empty() {
            None
        } else {
            Some(trimmed)
        };
    }
    if args.resume {
        return None;
    }
    Some(format!(
        "run_{}_{}",
        unix_timestamp_seconds(),
        std::process::id()
    ))
}

fn select_models(args: &Args, paths: &MslPaths, run_work_dir: &Path) -> Result<ModelSelection> {
    let target_file = resolve_target_models_path(args, paths);
    if target_file.exists() {
        let names = load_target_models(&target_file)?;
        println!(
            "Loaded {} target models from {}",
            names.len(),
            target_file.display()
        );
        return Ok(ModelSelection {
            names,
            source: format!("target_file:{}", target_file.display()),
            source_file: Some(target_file),
            discover_seconds: 0.0,
        });
    }
    let start = Instant::now();
    let names = discover_models(paths, run_work_dir, args.omc_threads)?;
    let elapsed = start.elapsed().as_secs_f64();
    Ok(ModelSelection {
        names,
        source: "discovery".to_string(),
        source_file: None,
        discover_seconds: elapsed,
    })
}

fn resolve_target_models_path(args: &Args, paths: &MslPaths) -> PathBuf {
    let generated_targets = paths.results_dir.join("msl_simulation_targets.json");
    let default_path = if generated_targets.is_file() {
        generated_targets
    } else {
        paths
            .repo_root
            .join("crates/rumoca-test-msl/tests/msl_tests/msl_simulation_targets_180.json")
    };
    let path = args
        .target_models_file
        .clone()
        .unwrap_or(default_path)
        .to_path_buf();
    if path.is_absolute() {
        path
    } else {
        paths.repo_root.join(path)
    }
}

fn truncate_models(mut names: Vec<String>, max_models: usize) -> Vec<String> {
    if max_models > 0 {
        names.truncate(max_models);
    }
    names
}

fn print_selection_summary(
    selection: &ModelSelection,
    total: usize,
    requested_batch_size: usize,
    effective_batch_size: usize,
) {
    println!("Total models to process: {total}");
    if selection.source == "discovery" {
        println!("Model discovery time: {:.1}s", selection.discover_seconds);
    } else {
        println!("Target source: {}", selection.source);
    }
    if requested_batch_size != effective_batch_size {
        println!("Adjusted batch size: {requested_batch_size} -> {effective_batch_size}");
    } else {
        println!("Batch size: {effective_batch_size}");
    }
}

fn run_dry_run(paths: &MslPaths, work_dir: &Path, model_names: &[String]) -> Result<()> {
    let sample = model_names.iter().take(10).cloned().collect::<Vec<_>>();
    let script = generate_check_script(paths, 0, &sample, work_dir);
    let mos_file = work_dir.join("dry_run_sample.mos");
    std::fs::write(&mos_file, script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;
    println!("Dry run: sample script written to {}", mos_file.display());
    Ok(())
}

#[derive(Debug, Clone)]
struct RunState {
    all_results: BTreeMap<String, ModelResult>,
    batch_timings: Vec<BatchTimingDetail>,
    pending_batches: Vec<PendingBatch>,
}

#[derive(Debug, Clone)]
struct FinalizeContext {
    omc_version: String,
    git_commit: String,
    total: usize,
    n_batches: usize,
    workers: usize,
    effective_batch_size: usize,
    overall_elapsed: f64,
}

fn prepare_run_state(
    args: &Args,
    model_names: &[String],
    work_dir: &Path,
    batch_size: usize,
) -> RunState {
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
        if args.resume {
            let parsed = parse_check_results(work_dir, batch_idx);
            if parsed.len() == models.len() {
                println!(
                    "  Batch {batch_idx}: skipped (already complete, {} models)",
                    parsed.len()
                );
                all_results.extend(parsed);
                batch_timings.push(BatchTimingDetail {
                    batch_idx,
                    requested_models: models.len(),
                    parsed_models: models.len(),
                    elapsed_seconds: 0.0,
                    timed_out: false,
                    skipped: true,
                });
                continue;
            }
        }
        pending_batches.push(PendingBatch {
            batch_idx,
            start_idx,
            end_idx,
            models,
        });
    }

    RunState {
        all_results,
        batch_timings,
        pending_batches,
    }
}

fn run_pending_batches(
    args: &Args,
    workers: usize,
    work_dir: &Path,
    state: &mut RunState,
) -> Result<()> {
    if workers == 1 {
        for batch in state.pending_batches.clone() {
            println!(
                "Processing batch {}/{} (models {}-{})...",
                batch.batch_idx + 1,
                state.pending_batches.len(),
                batch.start_idx + 1,
                batch.end_idx
            );
            let run = run_batch(
                batch.clone(),
                work_dir,
                args.model_timeout_seconds,
                args.omc_threads,
                args.dry_run,
            )?;
            println!(
                "  Batch {}: {}/{} in {:.1}s [{}]",
                batch.batch_idx,
                run.parsed_models,
                run.requested_models,
                run.elapsed_seconds,
                if run.timed_out { "timeout" } else { "ok" }
            );
            state.all_results.extend(run.results);
            state.batch_timings.push(BatchTimingDetail {
                batch_idx: batch.batch_idx,
                requested_models: run.requested_models,
                parsed_models: run.parsed_models,
                elapsed_seconds: round3(run.elapsed_seconds),
                timed_out: run.timed_out,
                skipped: false,
            });
        }
        return Ok(());
    }

    println!(
        "Running {} batches with {workers} workers...",
        state.pending_batches.len()
    );
    let total_batches = state.pending_batches.len();
    let work_dir = work_dir.to_path_buf();
    let timeout = args.model_timeout_seconds;
    let omc_threads = args.omc_threads;
    let results = run_parallel_batches_with_progress(
        state.pending_batches.clone(),
        workers,
        move |batch| run_batch(batch, &work_dir, timeout, omc_threads, false),
        move |batch, run| {
            println!(
                "  Done batch {}/{} (models {}-{}): {}/{} in {:.1}s [{}]",
                batch.batch_idx + 1,
                total_batches,
                batch.start_idx + 1,
                batch.end_idx,
                run.parsed_models,
                run.requested_models,
                run.elapsed_seconds,
                if run.timed_out { "timeout" } else { "ok" }
            );
        },
    )?;
    for (batch, run) in results {
        state.all_results.extend(run.results);
        state.batch_timings.push(BatchTimingDetail {
            batch_idx: batch.batch_idx,
            requested_models: run.requested_models,
            parsed_models: run.parsed_models,
            elapsed_seconds: round3(run.elapsed_seconds),
            timed_out: run.timed_out,
            skipped: false,
        });
    }
    Ok(())
}

fn run_batch(
    batch: PendingBatch,
    work_dir: &Path,
    model_timeout_seconds: u64,
    omc_threads: usize,
    dry_run: bool,
) -> Result<BatchRunOutput> {
    let paths = MslPaths::current();
    let script = generate_check_script(&paths, batch.batch_idx, &batch.models, work_dir);
    let mos_file = work_dir.join(format!("batch_{}.mos", batch.batch_idx));
    std::fs::write(&mos_file, script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;
    if dry_run {
        return Ok(BatchRunOutput {
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
    apply_omc_thread_env(&mut command, omc_threads);
    let process_timeout_seconds = model_timeout_seconds + WORKER_TIMEOUT_GRACE_SECONDS;
    let run = run_command_with_timeout(&mut command, Duration::from_secs(process_timeout_seconds))
        .with_context(|| format!("failed to execute omc for '{}'", mos_file.display()))?;
    let elapsed_seconds = start.elapsed().as_secs_f64();
    let mut results = parse_check_results(work_dir, batch.batch_idx);
    let parsed_models = results.len();
    if parsed_models != batch.models.len() && !run.timed_out {
        for model in &batch.models {
            if results.contains_key(model) {
                continue;
            }
            println!("  warning: missing result entry for {model}");
        }
    }
    fill_missing_batch_entries(
        &mut results,
        &batch.models,
        run.timed_out,
        model_timeout_seconds,
        process_timeout_seconds,
    );
    if run.timed_out {
        println!("  warning: batch {} timed out", batch.batch_idx);
    }
    Ok(BatchRunOutput {
        requested_models: batch.models.len(),
        parsed_models,
        elapsed_seconds,
        timed_out: run.timed_out,
        results,
    })
}

fn finalize_and_write_output(
    args: &Args,
    paths: &MslPaths,
    selection: &ModelSelection,
    context: FinalizeContext,
    mut state: RunState,
) -> Result<()> {
    state.batch_timings.sort_by_key(|batch| batch.batch_idx);
    let batch_elapsed_stats = summarize_batch_timings(&state.batch_timings);
    let successful = state
        .all_results
        .values()
        .filter(|result| result.status == "success")
        .count();
    let failed = state
        .all_results
        .values()
        .filter(|result| result.status == "error")
        .count();
    let flat_available = state
        .all_results
        .keys()
        .filter(|model| {
            let path = paths.flat_dir.join(format!("{model}.mo"));
            path.is_file() && path.metadata().map(|meta| meta.len() > 0).unwrap_or(false)
        })
        .count();
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

    let output = json!({
        "msl_version": MSL_VERSION,
        "omc_version": context.omc_version,
        "git_commit": context.git_commit,
        "target_source": selection.source,
        "target_models_file": selection.source_file.as_ref().map(|path| path.display().to_string()),
        "total_models": context.total,
        "processed": state.all_results.len(),
        "successful": successful,
        "failed": failed,
        "flat_available": flat_available,
        "elapsed_seconds": round3(context.overall_elapsed),
        "timing": {
            "discover_seconds": round3(selection.discover_seconds),
            "batch_size_requested": args.batch_size,
            "batch_size_effective": context.effective_batch_size,
            "model_timeout_seconds": args.model_timeout_seconds,
            "worker_timeout_grace_seconds": WORKER_TIMEOUT_GRACE_SECONDS,
            "worker_process_timeout_seconds": args.model_timeout_seconds + WORKER_TIMEOUT_GRACE_SECONDS,
            "workers_requested": args.workers,
            "workers_used": context.workers,
            "omc_threads": args.omc_threads,
            "batches_total": context.n_batches,
            "batches_ran": ran_batches,
            "batches_skipped": skipped_batches,
            "batch_elapsed_stats": batch_elapsed_stats,
            "batch_details": state.batch_timings,
        },
        "models": state.all_results,
    });
    let output_file = paths.results_dir.join("omc_reference.json");
    write_pretty_json(&output_file, &output)?;
    print_summary(
        &output_file,
        context.total,
        successful,
        failed,
        flat_available,
        context.overall_elapsed,
        batch_elapsed_stats.as_ref(),
    );
    Ok(())
}

fn print_summary(
    output_file: &Path,
    total: usize,
    successful: usize,
    failed: usize,
    flat_available: usize,
    elapsed_seconds: f64,
    batch_stats: Option<&crate::common::BatchElapsedStats>,
) {
    println!();
    println!("Results saved to {}", output_file.display());
    println!("  Total models: {total}");
    println!("  Successful (eq/var counts): {successful}");
    println!("  Failed: {failed}");
    println!("  Flat files available: {flat_available}");
    println!(
        "  Elapsed: {:.1}s ({:.2}s/model)",
        elapsed_seconds,
        elapsed_seconds / total.max(1) as f64
    );
    if let Some(stats) = batch_stats {
        println!(
            "  Batch timings (s): min={}, median={}, mean={}, max={}",
            stats.min, stats.median, stats.mean, stats.max
        );
    }
}

fn generate_check_script(
    paths: &MslPaths,
    batch_idx: usize,
    batch_models: &[String],
    work_dir: &Path,
) -> String {
    let mut lines = msl_load_lines(paths);
    let check_file = work_dir.join(format!("check_batch_{batch_idx}.txt"));
    lines.push(format!("writeFile(\"{}\", \"\");", check_file.display()));
    for model in batch_models {
        let flat_file = paths.flat_dir.join(format!("{model}.mo"));
        lines.push("getErrorString();".to_string());
        lines.push(format!("res := checkModel({model});"));
        lines.push("checkErr := getErrorString();".to_string());
        lines.push(format!("flat := instantiateModel({model});"));
        lines.push("instErr := getErrorString();".to_string());
        lines.push(format!("writeFile(\"{}\", flat);", flat_file.display()));
        lines.push(format!(
            "writeFile(\"{}\", \"MODEL:{model}\\nRESULT:\" + res + \"\\nERROR:\" + checkErr + \"\\nINST_ERROR:\" + instErr + \"\\n---\\n\", append=true);",
            check_file.display()
        ));
    }
    lines.join("\n")
}

fn parse_check_results(work_dir: &Path, batch_idx: usize) -> BTreeMap<String, ModelResult> {
    let check_file = work_dir.join(format!("check_batch_{batch_idx}.txt"));
    if !check_file.exists() {
        return BTreeMap::new();
    }
    let Ok(content) = std::fs::read_to_string(&check_file) else {
        return BTreeMap::new();
    };
    parse_check_results_from_content(&content)
}

fn parse_check_results_from_content(content: &str) -> BTreeMap<String, ModelResult> {
    let mut results = BTreeMap::new();
    for entry in content.split("---\n") {
        let Some((model_name, result)) = parse_check_entry(entry) else {
            continue;
        };
        results.insert(model_name, result);
    }
    results
}

fn parse_check_entry(entry: &str) -> Option<(String, ModelResult)> {
    let mut model_name = None;
    let mut result = String::new();
    let mut check_error = String::new();
    let mut inst_error = String::new();
    let mut mode = EntryMode::None;

    for raw_line in entry.lines() {
        if let Some(value) = raw_line.strip_prefix("MODEL:") {
            model_name = Some(value.trim().to_string());
            mode = EntryMode::None;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("RESULT:") {
            append_line(&mut result, value);
            mode = EntryMode::Result;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("ERROR:") {
            append_line(&mut check_error, value);
            mode = EntryMode::Error;
            continue;
        }
        if let Some(value) = raw_line.strip_prefix("INST_ERROR:") {
            append_line(&mut inst_error, value);
            mode = EntryMode::InstError;
            continue;
        }
        match mode {
            EntryMode::Result => append_line(&mut result, raw_line),
            EntryMode::Error => append_line(&mut check_error, raw_line),
            EntryMode::InstError => append_line(&mut inst_error, raw_line),
            EntryMode::None => {}
        }
    }

    let model_name = model_name?;
    let combined_error = combine_errors(&check_error, &inst_error);
    let fatal = has_fatal_omc_error(&combined_error);
    let flat_file = format!("omc_flat/{model_name}.mo");
    let Some((equations, variables)) = extract_equation_variable_counts(&result).filter(|_| !fatal)
    else {
        return Some((
            model_name,
            ModelResult {
                status: "error".to_string(),
                equations: None,
                variables: None,
                trivial: None,
                error: Some(summarize_omc_error(&combined_error, &result)),
                flat_file,
            },
        ));
    };
    Some((
        model_name,
        ModelResult {
            status: "success".to_string(),
            equations: Some(equations),
            variables: Some(variables),
            trivial: Some(extract_trivial_count(&result)),
            error: None,
            flat_file,
        },
    ))
}

fn combine_errors(check_error: &str, inst_error: &str) -> String {
    [check_error.trim(), inst_error.trim()]
        .iter()
        .filter(|part| !part.is_empty())
        .map(|part| (*part).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn append_line(buffer: &mut String, line: &str) {
    if !buffer.is_empty() {
        buffer.push('\n');
    }
    buffer.push_str(line.trim_end());
}

fn fill_missing_batch_entries(
    results: &mut BTreeMap<String, ModelResult>,
    batch_models: &[String],
    timed_out: bool,
    model_timeout_seconds: u64,
    process_timeout_seconds: u64,
) {
    for model_name in batch_models {
        if results.contains_key(model_name) {
            continue;
        }
        results.insert(
            model_name.clone(),
            ModelResult {
                status: "error".to_string(),
                equations: None,
                variables: None,
                trivial: None,
                error: Some(if timed_out {
                    format!(
                        "timeout: omc invocation exceeded model timeout {}s (process timeout {}s)",
                        model_timeout_seconds, process_timeout_seconds
                    )
                } else {
                    "error: missing omc result entry".to_string()
                }),
                flat_file: format!("omc_flat/{model_name}.mo"),
            },
        );
    }
}

fn extract_equation_variable_counts(result_text: &str) -> Option<(usize, usize)> {
    for line in result_text.lines() {
        if !line.contains("equation(s) and") || !line.contains("variable(s)") {
            continue;
        }
        let values = line
            .split(|ch: char| !ch.is_ascii_digit())
            .filter(|part| !part.is_empty())
            .filter_map(|part| part.parse::<usize>().ok())
            .collect::<Vec<_>>();
        if values.len() >= 2 {
            return Some((values[0], values[1]));
        }
    }
    None
}

fn extract_trivial_count(result_text: &str) -> usize {
    for line in result_text.lines() {
        if !line.contains("trivial equation(s)") {
            continue;
        }
        let Some(value) = line
            .split(|ch: char| !ch.is_ascii_digit())
            .find(|part| !part.is_empty())
        else {
            continue;
        };
        if let Ok(parsed) = value.parse::<usize>() {
            return parsed;
        }
    }
    0
}

fn discover_models(paths: &MslPaths, work_dir: &Path, omc_threads: usize) -> Result<Vec<String>> {
    std::fs::create_dir_all(work_dir)
        .with_context(|| format!("failed to create '{}'", work_dir.display()))?;
    let names_file = work_dir.join("all_model_names.txt");
    let discover_script = generate_discovery_script(paths, &names_file);
    let mos_file = work_dir.join("discover.mos");
    std::fs::write(&mos_file, discover_script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;

    println!("Discovering models via OMC getClassNames...");
    let mut command = Command::new("omc");
    command.arg(&mos_file).current_dir(work_dir);
    apply_omc_thread_env(&mut command, omc_threads);
    let run = run_command_with_timeout(&mut command, Duration::from_secs(DISCOVER_TIMEOUT_SECONDS))
        .with_context(|| format!("failed to run '{}'", mos_file.display()))?;
    if !names_file.exists() {
        let stdout_head = run.stdout.lines().take(20).collect::<Vec<_>>().join("\n");
        let stderr_head = run.stderr.lines().take(20).collect::<Vec<_>>().join("\n");
        bail!(
            "OMC failed to generate model names file.\nstdout:\n{stdout_head}\nstderr:\n{stderr_head}"
        );
    }
    let content = std::fs::read_to_string(&names_file)
        .with_context(|| format!("failed to read '{}'", names_file.display()))?;
    let names = filter_discovered_model_names(&content);
    println!(
        "  Found {} models/blocks/classes after filtering",
        names.len()
    );
    Ok(names)
}

fn generate_discovery_script(paths: &MslPaths, names_file: &Path) -> String {
    let mut lines = msl_load_lines(paths);
    lines.push("allNames := \"\";".to_string());
    for package in MSL_TOP_PACKAGES {
        lines.push(format!(
            "names := getClassNames({package}, recursive=true, qualified=true);"
        ));
        lines.push("for n in names loop".to_string());
        lines.push(
            "  allNames := allNames + typeNameString(n) + \"\\t\" + getClassRestriction(n) + \"\\n\";"
                .to_string(),
        );
        lines.push("end for;".to_string());
    }
    lines.push(format!(
        "writeFile(\"{}\", allNames);",
        names_file.display()
    ));
    lines.join("\n")
}

fn filter_discovered_model_names(content: &str) -> Vec<String> {
    let allowed: HashSet<&str> = ["model", "block", "class"].into_iter().collect();
    let mut names = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let mut parts = trimmed.splitn(2, '\t');
        let Some(name) = parts.next().map(str::trim) else {
            continue;
        };
        let restriction = parts.next().map(str::trim).unwrap_or("");
        if !allowed.contains(restriction) {
            continue;
        }
        if EXCLUDE_PREFIXES
            .iter()
            .any(|prefix| name.starts_with(prefix))
        {
            continue;
        }
        names.push(name.to_string());
    }
    names
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EntryMode {
    None,
    Result,
    Error,
    InstError,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_success_without_error_diagnostic() {
        let entry = "MODEL:Demo.M\nRESULT:Check of Demo.M completed successfully.\nClass Demo.M has 3 equation(s) and 3 variable(s).\n1 of these are trivial equation(s).\nERROR:\n---\n";
        let results = parse_check_results_from_content(entry);
        let result = results.get("Demo.M").expect("missing model");
        assert_eq!(result.status, "success");
        assert_eq!(result.equations, Some(3));
        assert_eq!(result.variables, Some(3));
        assert_eq!(result.trivial, Some(1));
    }

    #[test]
    fn parse_counts_with_fatal_error_as_failure() {
        let entry = "MODEL:Demo.M\nRESULT:Check of Demo.M completed successfully.\nClass Demo.M has 11 equation(s) and 11 variable(s).\n6 of these are trivial equation(s).\nERROR:[/tmp/file.mo:1:1-1:10:writable] Error: Illegal to instantiate partial class Demo.\n---\n";
        let results = parse_check_results_from_content(entry);
        let result = results.get("Demo.M").expect("missing model");
        assert_eq!(result.status, "error");
        assert!(
            result
                .error
                .as_deref()
                .unwrap_or_default()
                .contains("Illegal")
        );
        assert!(result.equations.is_none());
    }

    #[test]
    fn warning_only_diagnostics_still_success() {
        let entry = "MODEL:Demo.M\nRESULT:Check of Demo.M completed successfully.\nClass Demo.M has 4 equation(s) and 4 variable(s).\n2 of these are trivial equation(s).\nERROR:Warning: Requested package Modelica of version trunk.\n---\n";
        let results = parse_check_results_from_content(entry);
        let result = results.get("Demo.M").expect("missing model");
        assert_eq!(result.status, "success");
        assert_eq!(result.equations, Some(4));
        assert_eq!(result.variables, Some(4));
    }

    #[test]
    fn fill_missing_results_marks_timeout_entries_as_errors() {
        let mut results = BTreeMap::new();
        results.insert(
            "Demo.OK".to_string(),
            ModelResult {
                status: "success".to_string(),
                equations: Some(1),
                variables: Some(1),
                trivial: Some(0),
                error: None,
                flat_file: "omc_flat/Demo.OK.mo".to_string(),
            },
        );
        let batch = vec!["Demo.OK".to_string(), "Demo.Timeout".to_string()];
        fill_missing_batch_entries(&mut results, &batch, true, 5, 6);
        let timeout = results.get("Demo.Timeout").expect("missing timeout model");
        assert_eq!(timeout.status, "error");
        assert!(
            timeout
                .error
                .as_deref()
                .unwrap_or_default()
                .contains("timeout")
        );
    }
}
