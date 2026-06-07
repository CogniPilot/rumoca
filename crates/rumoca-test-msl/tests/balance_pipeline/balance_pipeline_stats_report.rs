use super::*;

// =============================================================================
// Human-readable simulation/timing/failure reporting
// =============================================================================

const MSL_PARITY_TIMING_VERSION: u32 = 1;
const MSL_PARITY_TIMING_JSON_REL: &str = "msl_parity_timing.json";
const MSL_PARITY_TIMING_MARKDOWN_REL: &str = "msl_parity_timing.md";

pub(super) fn print_simulation_results(summary: &MslSummary) {
    println!("Simulation Results:");
    println!("  - Attempted: {}", summary.sim_attempted);
    println!("  - sim_ok: {}", summary.sim_ok);
    println!("  - sim_nan: {}", summary.sim_nan);
    println!("  - sim_solver_fail: {}", summary.sim_solver_fail);
    println!("  - sim_timeout: {}", summary.sim_timeout);
    println!("  - sim_balance_fail: {}", summary.sim_balance_fail);
    println!("  - ic_attempted: {}", summary.ic_attempted);
    println!("  - ic_ok: {}", summary.ic_ok);
    println!("  - ic_solver_fail: {}", summary.ic_solver_fail);
    println!(
        "  - Total sim solver time: {:.2}s (sum of per-model worker-reported solver runtime)",
        summary.total_sim_seconds
    );
    println!(
        "  - Total sim wall/system time: {:.2}s (sum of per-model process wall time)",
        summary.total_sim_wall_seconds
    );
    let trace_count = summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_file.is_some())
        .count();
    let trace_write_fail_count = summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_error.is_some())
        .count();
    println!("  - Trace files written: {}", trace_count);
    println!("  - Trace write errors: {}", trace_write_fail_count);
    if summary.sim_attempted > 0 {
        let ic_rate = if summary.ic_attempted == 0 {
            0.0
        } else {
            (summary.ic_ok as f64 / summary.ic_attempted as f64) * 100.0
        };
        let sim_rate = (summary.sim_ok as f64 / summary.sim_attempted as f64) * 100.0;
        println!(
            "  - Initialization success rate: {:.1}% ({}/{})",
            ic_rate, summary.ic_ok, summary.ic_attempted
        );
        println!(
            "  - Simulation success rate: {:.1}% ({}/{})",
            sim_rate, summary.sim_ok, summary.sim_attempted
        );
    }
    // Print per-model simulation details
    for result in &summary.model_results {
        if let Some(ref status) = result.sim_status {
            let error_str = result
                .sim_error
                .as_deref()
                .map(|e| format!(" — {}", e))
                .unwrap_or_default();
            println!("  [{}] {}{error_str}", status, result.model_name,);
        }
    }
    println!();
}

fn hottest_compile_model(summary: &MslSummary) -> Option<(&str, f64)> {
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

fn hottest_sim_model(summary: &MslSummary) -> Option<(&str, f64)> {
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

fn print_profiler_follow_ups(summary: &MslSummary) {
    println!("Profiler Follow-Ups:");
    let compile_profile_count = summary
        .model_results
        .iter()
        .filter(|result| result.compile_perf_profile_file.is_some())
        .count();
    let sim_profile_count = summary
        .model_results
        .iter()
        .filter(|result| result.sim_perf_profile_file.is_some())
        .count();
    println!(
        "  - Retained perf profiles: compile={compile_profile_count}, sim={sim_profile_count}"
    );
    println!(
        "    Enable compile perf-profile auto-retention by editing the COMPILE_PERF_RECORD constant in balance_pipeline_core.rs"
    );
    if let Some((model_name, seconds)) = hottest_compile_model(summary) {
        println!("  - Hottest compile model: {model_name} ({seconds:.2}s)");
        println!("    cargo xtask repo msl flamegraph --model {model_name} --mode compile");
    }
    if let Some((model_name, seconds)) = hottest_sim_model(summary) {
        println!("  - Hottest sim model: {model_name} ({seconds:.2}s)");
        println!("    cargo xtask repo msl flamegraph --model {model_name} --mode simulate");
    }
    println!();
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingReport {
    version: u32,
    git_commit: String,
    msl_version: String,
    success: bool,
    harness_total_seconds: f64,
    core_pipeline: MslPhaseTimings,
    counts: MslParityTimingCounts,
    workers: MslParityTimingWorkers,
    hotspots: MslParityTimingHotspots,
    stages: Vec<MslParityTimingStage>,
}

impl MslParityTimingReport {
    fn new(summary: &MslSummary) -> Self {
        Self {
            version: MSL_PARITY_TIMING_VERSION,
            git_commit: summary.git_commit.clone(),
            msl_version: summary.msl_version.clone(),
            success: false,
            harness_total_seconds: summary.timings.core_pipeline_seconds,
            core_pipeline: summary.timings.clone(),
            counts: MslParityTimingCounts::from_summary(summary),
            workers: MslParityTimingWorkers::current(summary),
            hotspots: MslParityTimingHotspots::from_summary(summary),
            stages: Vec::new(),
        }
    }

    fn record_stage(&mut self, label: &str, status: &str, elapsed: Duration) {
        let elapsed_seconds = elapsed.as_secs_f64();
        self.harness_total_seconds += elapsed_seconds;
        self.stages.push(MslParityTimingStage {
            label: label.to_string(),
            status: status.to_string(),
            elapsed_seconds,
        });
    }

    fn mark_success(&mut self) {
        self.success = self.stages.iter().all(|stage| stage.status == "pass");
    }
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingCounts {
    total_models: usize,
    compiled_models: usize,
    solve_models: usize,
    balanced_models: usize,
    sim_target_models: usize,
    sim_attempted: usize,
    sim_ok: usize,
    sim_solver_fail: usize,
    sim_timeout: usize,
    ic_attempted: usize,
    ic_ok: usize,
    trace_files_written: usize,
    trace_write_errors: usize,
}

impl MslParityTimingCounts {
    fn from_summary(summary: &MslSummary) -> Self {
        Self {
            total_models: summary.total_models,
            compiled_models: summary.compiled_models,
            solve_models: solve_model_count(summary),
            balanced_models: summary.balanced_models,
            sim_target_models: summary.sim_target_models.len(),
            sim_attempted: summary.sim_attempted,
            sim_ok: summary.sim_ok,
            sim_solver_fail: summary.sim_solver_fail,
            sim_timeout: summary.sim_timeout,
            ic_attempted: summary.ic_attempted,
            ic_ok: summary.ic_ok,
            trace_files_written: trace_file_count(summary),
            trace_write_errors: trace_write_error_count(summary),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingWorkers {
    rumoca_workers: usize,
    omc_workers: usize,
    omc_threads: usize,
}

impl MslParityTimingWorkers {
    fn current(summary: &MslSummary) -> Self {
        Self {
            rumoca_workers: summary.timings.worker_threads,
            omc_workers: current_omc_parity_workers(),
            omc_threads: current_omc_parity_threads(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingHotspots {
    hottest_compile_model: Option<MslParityTimingHotspot>,
    hottest_sim_model: Option<MslParityTimingHotspot>,
}

impl MslParityTimingHotspots {
    fn from_summary(summary: &MslSummary) -> Self {
        Self {
            hottest_compile_model: hottest_compile_model(summary)
                .map(MslParityTimingHotspot::from_model_seconds),
            hottest_sim_model: hottest_sim_model(summary)
                .map(MslParityTimingHotspot::from_model_seconds),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingHotspot {
    model: String,
    seconds: f64,
}

impl MslParityTimingHotspot {
    fn from_model_seconds((model, seconds): (&str, f64)) -> Self {
        Self {
            model: model.to_string(),
            seconds,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct MslParityTimingStage {
    label: String,
    status: String,
    elapsed_seconds: f64,
}

fn trace_file_count(summary: &MslSummary) -> usize {
    summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_file.is_some())
        .count()
}

fn trace_write_error_count(summary: &MslSummary) -> usize {
    summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_error.is_some())
        .count()
}

fn solve_model_count(summary: &MslSummary) -> usize {
    summary
        .model_results
        .iter()
        .filter(|result| result.ir_solve_file.is_some() || result.ir_solve_seconds.is_some())
        .count()
}

fn run_timed_parity_stage<F>(
    report: &mut MslParityTimingReport,
    label: &str,
    mut run: F,
) -> io::Result<()>
where
    F: FnMut() -> io::Result<()>,
{
    let started = Instant::now();
    match run() {
        Ok(()) => {
            report.record_stage(label, "pass", started.elapsed());
            Ok(())
        }
        Err(error) => {
            report.record_stage(label, "fail", started.elapsed());
            Err(error)
        }
    }
}

fn run_timed_parity_stage_or_panic<F>(
    report: &mut MslParityTimingReport,
    label: &str,
    context: &str,
    run: F,
) where
    F: FnMut() -> io::Result<()>,
{
    if let Err(error) = run_timed_parity_stage(report, label, run) {
        write_msl_parity_timing_report(report).expect("Failed to write MSL parity timing report");
        panic!("{context}: {error}");
    }
}

fn write_msl_parity_timing_report(report: &MslParityTimingReport) -> io::Result<()> {
    let json_path = msl_results_dir().join(MSL_PARITY_TIMING_JSON_REL);
    let markdown_path = msl_results_dir().join(MSL_PARITY_TIMING_MARKDOWN_REL);
    if let Some(parent) = json_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(report).map_err(|error| {
        io::Error::other(format!(
            "failed to serialize MSL parity timing JSON: {error}"
        ))
    })?;
    fs::write(&json_path, json)?;
    fs::write(&markdown_path, render_msl_parity_timing_markdown(report))?;
    println!(
        "MSL parity timing report written to {} and {}",
        json_path.display(),
        markdown_path.display()
    );
    Ok(())
}

fn render_msl_parity_timing_markdown(report: &MslParityTimingReport) -> String {
    let mut lines = vec![
        "# MSL Parity Timing".to_string(),
        String::new(),
        format!("- success: {}", report.success),
        format!("- git_commit: {}", report.git_commit),
        format!("- msl_version: {}", report.msl_version),
        format!(
            "- harness_total_seconds: {:.3}",
            report.harness_total_seconds
        ),
        String::new(),
        "| Stage | Status | Seconds |".to_string(),
        "|---|---:|---:|".to_string(),
        format!(
            "| core pipeline | pass | {:.3} |",
            report.core_pipeline.core_pipeline_seconds
        ),
    ];
    for stage in &report.stages {
        lines.push(format!(
            "| {} | {} | {:.3} |",
            stage.label, stage.status, stage.elapsed_seconds
        ));
    }
    lines.extend([
        String::new(),
        "## Counts".to_string(),
        String::new(),
        format!("- total_models: {}", report.counts.total_models),
        format!("- compiled_models: {}", report.counts.compiled_models),
        format!("- solve_models: {}", report.counts.solve_models),
        format!("- balanced_models: {}", report.counts.balanced_models),
        format!("- sim_target_models: {}", report.counts.sim_target_models),
        format!("- sim_attempted: {}", report.counts.sim_attempted),
        format!("- sim_ok: {}", report.counts.sim_ok),
        format!("- sim_solver_fail: {}", report.counts.sim_solver_fail),
        format!("- sim_timeout: {}", report.counts.sim_timeout),
        format!("- ic_attempted: {}", report.counts.ic_attempted),
        format!("- ic_ok: {}", report.counts.ic_ok),
        format!(
            "- trace_files_written: {}",
            report.counts.trace_files_written
        ),
        format!("- trace_write_errors: {}", report.counts.trace_write_errors),
        String::new(),
        "## Workers".to_string(),
        String::new(),
        format!("- rumoca_workers: {}", report.workers.rumoca_workers),
        format!("- omc_workers: {}", report.workers.omc_workers),
        format!("- omc_threads: {}", report.workers.omc_threads),
    ]);
    if let Some(hotspot) = &report.hotspots.hottest_compile_model {
        lines.push(format!(
            "- hottest_compile_model: {} ({:.3}s)",
            hotspot.model, hotspot.seconds
        ));
    }
    if let Some(hotspot) = &report.hotspots.hottest_sim_model {
        lines.push(format!(
            "- hottest_sim_model: {} ({:.3}s)",
            hotspot.model, hotspot.seconds
        ));
    }
    lines.push(String::new());
    lines.join("\n")
}

fn last_stage_seconds(report: &MslParityTimingReport) -> f64 {
    report
        .stages
        .last()
        .map_or(0.0, |stage| stage.elapsed_seconds)
}

fn run_simulation_parity_stages(summary: &MslSummary, report: &mut MslParityTimingReport) {
    let parity_start = Instant::now();
    let _parity_watchdog = StageAbortWatchdog::new("parity_stage", 7200);
    println!("MSL parity stage: ensuring OMC references + trace comparison...");
    run_timed_parity_stage_or_panic(
        report,
        "omc_reference_and_trace_compare",
        "Failed to ensure required OMC parity references",
        || ensure_required_msl_parity_references(summary),
    );
    run_timed_parity_stage_or_panic(
        report,
        "package_trace_accuracy_report",
        "Failed to write MSL package trace accuracy report",
        || write_msl_package_trace_accuracy_report(summary).map(|_| ()),
    );
    println!(
        "MSL parity stage: completed in {:.2}s",
        parity_start.elapsed().as_secs_f64()
    );
}

fn run_quality_snapshot_stage(summary: &MslSummary, report: &mut MslParityTimingReport) {
    let _snapshot_watchdog = StageAbortWatchdog::new("quality_snapshot_write", 300);
    println!("MSL parity stage: writing current quality snapshot...");
    run_timed_parity_stage_or_panic(
        report,
        "quality_snapshot_write",
        "Failed to write current MSL quality snapshot",
        || write_current_msl_quality_snapshot(summary),
    );
    println!(
        "MSL parity stage: quality snapshot written in {:.2}s",
        last_stage_seconds(report)
    );
}

fn run_quality_gate_stage(summary: &MslSummary, report: &mut MslParityTimingReport) {
    if should_skip_msl_quality_gate() {
        println!(
            "MSL quality gate: baseline delta checks skipped for focused/non-baseline run (committed target scope, explicit target file, subset, or non-default sim set)."
        );
        run_timed_parity_stage_or_panic(
            report,
            "quality_gate_eval_partial",
            "Failed to run MSL quality gate",
            || enforce_msl_quality_gate(summary),
        );
        return;
    }
    let _quality_gate_watchdog = StageAbortWatchdog::new("quality_gate_eval", 300);
    println!("MSL quality gate: evaluating baseline deltas...");
    run_timed_parity_stage_or_panic(
        report,
        "quality_gate_eval",
        "Failed to run MSL quality gate",
        || enforce_msl_quality_gate(summary),
    );
    println!(
        "MSL quality gate: completed in {:.2}s",
        last_stage_seconds(report)
    );
}

fn finalize_msl_parity_timing_report(report: &mut MslParityTimingReport) {
    report.mark_success();
    write_msl_parity_timing_report(report).expect("Failed to write MSL parity timing report");
}

fn print_simulatable_compilation_rate(summary: &MslSummary) {
    let attempted_standalone = summary.compiled_models
        + summary.resolve_failed
        + summary.instantiate_failed
        + summary.typecheck_failed
        + summary.flatten_failed
        + summary.todae_failed;
    let compile_rate = if attempted_standalone > 0 {
        (summary.compiled_models as f64 / attempted_standalone as f64) * 100.0
    } else {
        0.0
    };

    println!(
        "\nSimulatable compilation rate: {:.1}% ({} compiled / {} simulatable models)",
        compile_rate, summary.compiled_models, attempted_standalone
    );
    println!(
        "Non-simulatable non-partial models (excluded from simulatable denominator): {}",
        summary.non_sim_models
    );
}

pub(super) fn print_final_stats(summary: &MslSummary) {
    let write_start = Instant::now();
    write_msl_results(summary).expect("Failed to write results");
    let json_write_seconds = write_start.elapsed().as_secs_f64();
    let mut timing_report = MslParityTimingReport::new(summary);
    timing_report.record_stage(
        "results_json_write",
        "pass",
        Duration::from_secs_f64(json_write_seconds),
    );
    println!("  - JSON results write: {:.2}s", json_write_seconds);
    println!(
        "  - Core + JSON write subtotal: {:.2}s",
        summary.timings.core_pipeline_seconds + json_write_seconds
    );
    assert_valid_msl_summary(summary);
    if summary.sim_attempted > 0 {
        run_simulation_parity_stages(summary, &mut timing_report);
        run_quality_snapshot_stage(summary, &mut timing_report);
    }
    run_quality_gate_stage(summary, &mut timing_report);
    finalize_msl_parity_timing_report(&mut timing_report);
    print_simulatable_compilation_rate(summary);
}

fn env_flag_enabled(name: &str) -> bool {
    std::env::var(name)
        .ok()
        .map(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

pub(super) fn print_timing_breakdown(summary: &MslSummary) {
    println!("Performance Snapshot:");
    if summary.timings.compile_chunk_count <= 1 {
        println!(
            "  - Compile scheduling: global work queue ({} model scope)",
            summary.timings.compile_batch_size
        );
    } else {
        println!(
            "  - Compile chunking: {} chunk(s) of up to {} model(s)",
            summary.timings.compile_chunk_count, summary.timings.compile_batch_size
        );
    }
    println!("  - Worker threads: {}", summary.timings.worker_threads);
    println!(
        "  - Compile-scope throughput: {:.2} models/s",
        summary.total_models as f64 / summary.timings.frontend_compile_seconds.max(f64::EPSILON)
    );
    if summary.timings.render_and_write_seconds > 0.0 && summary.sim_attempted > 0 {
        println!(
            "  - Sim/render throughput: {:.2} models/s",
            summary.sim_attempted as f64 / summary.timings.render_and_write_seconds
        );
    }
    println!(
        "  - Core pipeline subtotal: {:.2}s",
        summary.timings.core_pipeline_seconds
    );
    println!();
    print_profiler_follow_ups(summary);
    println!("Timing Breakdown:");
    println!(
        "  - Pipeline compile (parse+session+compile): {:.2}s",
        summary.timings.frontend_compile_seconds
    );
    println!("  - Parse: {:.2}s", summary.timings.parse_seconds);
    println!(
        "  - Session build: {:.2}s",
        summary.timings.session_build_seconds
    );
    println!("  - Compile only: {:.2}s", summary.timings.compile_seconds);
    println!(
        "  - Compile phase totals: instantiate {:.2}s ({} calls), typecheck {:.2}s ({} calls), flatten {:.2}s ({} calls), todae {:.2}s ({} calls)",
        summary.timings.compile_instantiate_seconds,
        summary.timings.compile_instantiate_calls,
        summary.timings.compile_typecheck_seconds,
        summary.timings.compile_typecheck_calls,
        summary.timings.compile_flatten_seconds,
        summary.timings.compile_flatten_calls,
        summary.timings.compile_todae_seconds,
        summary.timings.compile_todae_calls
    );
    println!(
        "  - Flatten subpasses: connections {:.2}s ({} calls), eval fallback {:.2}s ({} calls)",
        summary.timings.flatten_connections_seconds,
        summary.timings.flatten_connections_calls,
        summary.timings.flatten_eval_fallback_seconds,
        summary.timings.flatten_eval_fallback_calls
    );
    println!(
        "  - File generation (render + write): {:.2}s",
        summary.timings.render_and_write_seconds
    );
    println!(
        "  - Summary aggregation: {:.2}s",
        summary.timings.summarize_seconds
    );
    println!(
        "  - Core pipeline subtotal (before JSON write): {:.2}s",
        summary.timings.core_pipeline_seconds
    );
}

/// Print detailed failure and error information from the balance summary.
pub(super) fn print_failure_details(summary: &MslSummary) {
    print_blocking_phase_failure_counts(summary);

    // Print first few failures by phase (skip NeedsInner since those aren't failures)
    for (phase, failures) in &summary.failures_by_phase {
        if phase == "NeedsInner" || phase == "NonSim" {
            continue;
        }
        println!("\nFirst 5 {} failures:", phase);
        for model in failures.iter().take(5) {
            println!("  - {}", model);
        }
        if failures.len() > 5 {
            println!("  ... and {} more", failures.len() - 5);
        }
    }

    // Print first few unbalanced models
    if !summary.unbalanced_list.is_empty() {
        println!("\nFirst 5 unbalanced models:");
        for model in summary.unbalanced_list.iter().take(5) {
            println!("  - {}", model);
        }
        if summary.unbalanced_list.len() > 5 {
            println!("  ... and {} more", summary.unbalanced_list.len() - 5);
        }
    }

    if !summary.initial_unbalanced_list.is_empty() {
        println!("\nFirst 5 models with initial-balance deficits:");
        for model in summary.initial_unbalanced_list.iter().take(5) {
            println!("  - {}", model);
        }
        if summary.initial_unbalanced_list.len() > 5 {
            println!(
                "  ... and {} more",
                summary.initial_unbalanced_list.len() - 5
            );
        }
    }

    print_flatten_error_categories(summary);
    print_unsupported_feature_counts(summary);
    print_error_code_counts(summary);
    print_common_undefined_variables(summary);
}

fn print_blocking_phase_failure_counts(summary: &MslSummary) {
    let mut phase_counts: Vec<_> = summary
        .failures_by_phase
        .iter()
        .filter(|(phase, _)| phase.as_str() != "NeedsInner" && phase.as_str() != "NonSim")
        .map(|(phase, failures)| (phase.as_str(), failures.len()))
        .collect();
    phase_counts.sort_by(|(a_phase, a_count), (b_phase, b_count)| {
        b_count.cmp(a_count).then_with(|| a_phase.cmp(b_phase))
    });
    if !phase_counts.is_empty() {
        println!("\n=== Blocking Phase Failure Counts ===");
        for (phase, count) in &phase_counts {
            println!("  {phase}: {count}");
        }
    }
}

fn print_flatten_error_categories(summary: &MslSummary) {
    if !summary.error_categories.is_empty() {
        println!("\n=== Flatten Error Categories ===");
        let mut sorted: Vec<_> = summary.error_categories.iter().collect();
        sorted.sort_by_key(|(_, errors)| std::cmp::Reverse(errors.len()));

        for (category, errors) in &sorted {
            println!("\n--- {} ({} errors) ---", category, errors.len());
            for (model, error) in errors.iter().take(5) {
                let short_error = truncate_error(error, 100);
                println!("  {}: {}", model, short_error);
            }
            if errors.len() > 5 {
                println!("  ... and {} more", errors.len() - 5);
            }
        }

        // Summary by category
        println!("\n=== Error Summary by Category ===");
        for (category, errors) in &sorted {
            let pct = errors.len() as f64 / summary.flatten_failed as f64 * 100.0;
            println!("{:25} {:>5} ({:>5.1}%)", category, errors.len(), pct);
        }
    }
}

fn print_unsupported_feature_counts(summary: &MslSummary) {
    if !summary.unsupported_feature_counts.is_empty() {
        println!("\n=== Unsupported Feature Counts ===");
        let mut sorted_features: Vec<_> = summary.unsupported_feature_counts.iter().collect();
        sorted_features.sort_by(|(a_feature, a_count), (b_feature, b_count)| {
            b_count.cmp(a_count).then_with(|| a_feature.cmp(b_feature))
        });
        for (feature, count) in sorted_features.iter().take(15) {
            println!("  {}: {}", feature, count);
        }
    }

    if !summary.unsupported_feature_counts_by_backend.is_empty() {
        println!("\n=== Unsupported Feature Counts By Target ===");
        let mut sorted_backends: Vec<_> = summary
            .unsupported_feature_counts_by_backend
            .iter()
            .collect();
        sorted_backends.sort_by_key(|(backend, _)| *backend);
        for (backend, feature_counts) in sorted_backends.iter().take(10) {
            println!("  {backend}:");
            let mut sorted_features: Vec<_> = feature_counts.iter().collect();
            sorted_features.sort_by(|(a_feature, a_count), (b_feature, b_count)| {
                b_count.cmp(a_count).then_with(|| a_feature.cmp(b_feature))
            });
            for (feature, count) in sorted_features.iter().take(10) {
                println!("    {}: {}", feature, count);
            }
        }
    }
}

fn print_error_code_counts(summary: &MslSummary) {
    if !summary.error_code_counts.is_empty() {
        println!("\n=== Error Code Counts ===");
        let mut sorted_codes: Vec<_> = summary.error_code_counts.iter().collect();
        sorted_codes.sort_by(|(a_code, a_count), (b_code, b_count)| {
            b_count.cmp(a_count).then_with(|| a_code.cmp(b_code))
        });
        for (code, count) in sorted_codes.iter().take(15) {
            println!("  {}: {}", code, count);
        }
    }
}

fn print_common_undefined_variables(summary: &MslSummary) {
    if !summary.undefined_vars.is_empty() {
        println!("\n=== Most Common Undefined Variables ===");
        let mut sorted_vars: Vec<_> = summary.undefined_vars.iter().collect();
        sorted_vars.sort_by(|a, b| b.1.cmp(a.1));
        for (var, count) in sorted_vars.iter().take(10) {
            println!("  {} ({}x)", var, count);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn timing_report_for_render_test() -> MslParityTimingReport {
        let mut report = MslParityTimingReport {
            version: MSL_PARITY_TIMING_VERSION,
            git_commit: "abc123".to_string(),
            msl_version: "v4.1.0".to_string(),
            success: false,
            harness_total_seconds: 2.0,
            core_pipeline: MslPhaseTimings {
                core_pipeline_seconds: 2.0,
                ..MslPhaseTimings::default()
            },
            counts: MslParityTimingCounts {
                total_models: 566,
                compiled_models: 487,
                solve_models: 391,
                balanced_models: 474,
                sim_target_models: 566,
                sim_attempted: 397,
                sim_ok: 156,
                sim_solver_fail: 235,
                sim_timeout: 6,
                ic_attempted: 252,
                ic_ok: 231,
                trace_files_written: 155,
                trace_write_errors: 0,
            },
            workers: MslParityTimingWorkers {
                rumoca_workers: 14,
                omc_workers: 14,
                omc_threads: 1,
            },
            hotspots: MslParityTimingHotspots {
                hottest_compile_model: Some(MslParityTimingHotspot {
                    model: "Modelica.X".to_string(),
                    seconds: 24.0,
                }),
                hottest_sim_model: None,
            },
            stages: Vec::new(),
        };
        report.record_stage(
            "omc_reference_and_trace_compare",
            "pass",
            Duration::from_millis(1500),
        );
        report.mark_success();
        report
    }

    #[test]
    fn parity_timing_markdown_contains_core_stages_counts_and_workers() {
        let markdown = render_msl_parity_timing_markdown(&timing_report_for_render_test());
        assert!(markdown.contains("| core pipeline | pass | 2.000 |"));
        assert!(markdown.contains("| omc_reference_and_trace_compare | pass | 1.500 |"));
        assert!(markdown.contains("- sim_ok: 156"));
        assert!(markdown.contains("- rumoca_workers: 14"));
        assert!(markdown.contains("- hottest_compile_model: Modelica.X (24.000s)"));
    }

    #[test]
    fn timed_parity_stage_records_failure_before_returning_error() {
        let mut report = timing_report_for_render_test();
        let error = run_timed_parity_stage(&mut report, "quality_gate_eval", || {
            Err(io::Error::other("gate failed"))
        })
        .expect_err("stage should fail");
        assert_eq!(error.to_string(), "gate failed");
        let stage = report
            .stages
            .last()
            .expect("failed stage should be recorded");
        assert_eq!(stage.label, "quality_gate_eval");
        assert_eq!(stage.status, "fail");
    }
}
