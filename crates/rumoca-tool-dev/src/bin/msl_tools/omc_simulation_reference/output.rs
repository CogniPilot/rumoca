use super::*;
use rumoca::sim_trace_compare::count_agreement_bands_default;

pub(super) fn compute_trace_output_summary(
    trace_report: &TraceQuantification,
) -> TraceOutputSummary {
    let agreement =
        count_agreement_bands_default(trace_report.models.values().map(|item| &item.metric));
    let model_count = trace_report.models.len();
    let model_count_f64 = model_count.max(1) as f64;
    let agreement_high_percent = agreement.high_agreement as f64 * 100.0 / model_count_f64;
    let agreement_minor_percent = agreement.minor_agreement as f64 * 100.0 / model_count_f64;
    let agreement_deviation_percent = agreement.deviation as f64 * 100.0 / model_count_f64;
    let models_with_any_channel_deviation = trace_report
        .models
        .values()
        .filter(|item| item.metric.channel_deviation_count > 0)
        .count();
    let models_with_any_channel_deviation_percent =
        models_with_any_channel_deviation as f64 * 100.0 / model_count_f64;
    let max_model_channel_deviation_percent = trace_report
        .models
        .values()
        .map(|item| item.metric.channel_deviation_percent * 100.0)
        .fold(0.0_f64, f64::max);
    let total_channels_compared = trace_report
        .models
        .values()
        .map(|item| item.metric.compared_variables)
        .sum::<usize>();
    let bad_channels_total = trace_report
        .models
        .values()
        .map(|item| item.metric.channel_deviation_count)
        .sum::<usize>();
    let severe_channels_total = trace_report
        .models
        .values()
        .map(|item| item.metric.channel_severe_count)
        .sum::<usize>();
    let total_channels_compared_f64 = total_channels_compared.max(1) as f64;
    let bad_channels_percent = bad_channels_total as f64 * 100.0 / total_channels_compared_f64;
    let severe_channels_percent =
        severe_channels_total as f64 * 100.0 / total_channels_compared_f64;
    let violation_mass_total = trace_report
        .models
        .values()
        .map(|item| item.metric.channel_violation_mass)
        .filter(|value| value.is_finite())
        .sum::<f64>();
    let violation_mass_mean_per_model = violation_mass_total / model_count_f64;
    let violation_mass_mean_per_channel = violation_mass_total / total_channels_compared_f64;
    let (min_l1, median_l1, mean_l1, max_l1) = metric_distribution(
        trace_report
            .models
            .values()
            .map(|item| item.metric.bounded_normalized_l1_score),
    )
    .unwrap_or((0.0, 0.0, 0.0, 0.0));
    let (_, _, mean_model_mean, _) = metric_distribution(
        trace_report
            .models
            .values()
            .map(|item| item.metric.mean_channel_bounded_normalized_l1),
    )
    .unwrap_or((0.0, 0.0, 0.0, 0.0));
    let (_, _, _, max_model_max) = metric_distribution(
        trace_report
            .models
            .values()
            .map(|item| item.metric.max_channel_bounded_normalized_l1),
    )
    .unwrap_or((0.0, 0.0, 0.0, 0.0));

    TraceOutputSummary {
        models_compared: model_count,
        missing_trace_models: trace_report.missing_trace.len(),
        skipped_models: trace_report.skipped.len(),
        agreement_high: agreement.high_agreement,
        agreement_minor: agreement.minor_agreement,
        agreement_deviation: agreement.deviation,
        agreement_high_percent,
        agreement_minor_percent,
        agreement_deviation_percent,
        total_channels_compared,
        bad_channels_total,
        severe_channels_total,
        bad_channels_percent,
        severe_channels_percent,
        violation_mass_total,
        violation_mass_mean_per_model,
        violation_mass_mean_per_channel,
        models_with_any_channel_deviation,
        models_with_any_channel_deviation_percent,
        max_model_channel_deviation_percent,
        min_model_bounded_normalized_l1: min_l1,
        median_model_bounded_normalized_l1: median_l1,
        mean_model_bounded_normalized_l1: mean_l1,
        max_model_bounded_normalized_l1: max_l1,
        mean_model_mean_channel_bounded_normalized_l1: mean_model_mean,
        max_model_max_channel_bounded_normalized_l1: max_model_max,
    }
}

fn sorted_trace_metrics(quantification: &TraceQuantification) -> Vec<TraceModelMetric> {
    let mut metrics = quantification.models.values().cloned().collect::<Vec<_>>();
    metrics.sort_by(|a, b| {
        b.metric
            .max_channel_bounded_normalized_l1
            .partial_cmp(&a.metric.max_channel_bounded_normalized_l1)
            .unwrap_or(Ordering::Equal)
    });
    metrics
}

fn candidate_model_count(all_results: &BTreeMap<String, SimModelResult>) -> usize {
    all_results
        .values()
        .filter(|result| {
            result.status == "success" && result.rumoca_status.as_deref() == Some("sim_ok")
        })
        .count()
}

fn trace_runtime_totals(metrics: &[TraceModelMetric]) -> (f64, f64, f64, Option<f64>) {
    let total_rumoca_wall = metrics
        .iter()
        .filter_map(|metric| metric.rumoca_sim_wall_seconds)
        .sum::<f64>();
    let total_rumoca_sim = metrics
        .iter()
        .filter_map(|metric| metric.rumoca_sim_seconds)
        .sum::<f64>();
    let total_omc_sim = metrics
        .iter()
        .filter_map(|metric| metric.omc_sim_system_seconds)
        .sum::<f64>();
    let speedup_ratio = if total_rumoca_sim > 0.0 {
        Some(total_omc_sim / total_rumoca_sim)
    } else {
        None
    };
    (
        total_rumoca_wall,
        total_rumoca_sim,
        total_omc_sim,
        speedup_ratio,
    )
}

fn build_trace_report_payload(
    quantification: &TraceQuantification,
    trace_summary: &TraceOutputSummary,
    metrics: &[TraceModelMetric],
    candidate: usize,
) -> Value {
    let (total_rumoca_wall, total_rumoca_sim, total_omc_sim, speedup_ratio) =
        trace_runtime_totals(metrics);
    json!({
        "generated_at_unix_seconds": unix_timestamp_seconds(),
        "models_candidate": candidate,
        "models_compared": trace_summary.models_compared,
        "missing_trace_models": trace_summary.missing_trace_models,
        "skipped_models": trace_summary.skipped_models,
        "agreement_bands": {
            "high_agreement": trace_summary.agreement_high,
            "minor_agreement": trace_summary.agreement_minor,
            "deviation": trace_summary.agreement_deviation,
        },
        "agreement_bands_percent": {
            "high_agreement": trace_summary.agreement_high_percent,
            "minor_agreement": trace_summary.agreement_minor_percent,
            "deviation": trace_summary.agreement_deviation_percent,
        },
        "summary": {
            "min_model_bounded_normalized_l1": trace_summary.min_model_bounded_normalized_l1,
            "mean_model_bounded_normalized_l1": trace_summary.mean_model_bounded_normalized_l1,
            "median_model_bounded_normalized_l1": trace_summary.median_model_bounded_normalized_l1,
            "max_model_bounded_normalized_l1": trace_summary.max_model_bounded_normalized_l1,
            "worst_model_bounded_normalized_l1": trace_summary.max_model_bounded_normalized_l1,
            "mean_model_mean_channel_bounded_normalized_l1": trace_summary.mean_model_mean_channel_bounded_normalized_l1,
            "max_model_max_channel_bounded_normalized_l1": trace_summary.max_model_max_channel_bounded_normalized_l1,
            "global_max_channel_bounded_normalized_l1": trace_summary.max_model_max_channel_bounded_normalized_l1,
            "total_channels_compared": trace_summary.total_channels_compared,
            "bad_channels_total": trace_summary.bad_channels_total,
            "bad_channels_percent": trace_summary.bad_channels_percent,
            "severe_channels_total": trace_summary.severe_channels_total,
            "severe_channels_percent": trace_summary.severe_channels_percent,
            "models_with_any_channel_deviation": trace_summary.models_with_any_channel_deviation,
            "models_with_any_channel_deviation_percent": trace_summary.models_with_any_channel_deviation_percent,
            "violation_mass_total": trace_summary.violation_mass_total,
            "violation_mass_mean_per_model": trace_summary.violation_mass_mean_per_model,
            "violation_mass_mean_per_channel": trace_summary.violation_mass_mean_per_channel,
            "total_rumoca_sim_wall_seconds": total_rumoca_wall,
            "total_rumoca_sim_seconds": total_rumoca_sim,
            "total_omc_sim_system_seconds": total_omc_sim,
            "omc_sim_over_rumoca_sim_speedup_ratio": speedup_ratio,
        },
        "worst_models": metrics.iter().take(20).collect::<Vec<_>>(),
        "missing_trace": quantification.missing_trace,
        "skipped": quantification.skipped,
        "models": quantification.models,
    })
}

pub(super) fn write_trace_report(
    paths: &MslPaths,
    all_results: &BTreeMap<String, SimModelResult>,
    quantification: &TraceQuantification,
) -> Result<()> {
    let metrics = sorted_trace_metrics(quantification);
    let trace_summary = compute_trace_output_summary(quantification);
    let candidate = candidate_model_count(all_results);
    let payload = build_trace_report_payload(quantification, &trace_summary, &metrics, candidate);
    let trace_file = paths.results_dir.join("sim_trace_comparison.json");
    write_pretty_json(&trace_file, &payload)
}

pub(super) fn build_sim_output_payload(
    args: &Args,
    paths: &MslPaths,
    selection: &ModelSelection,
    context: &FinalizeContext,
    metrics: &RunMetrics,
    trace_summary: &TraceOutputSummary,
    state: &SimRunState,
) -> Value {
    let target_selection = json!({
        "source_file": selection.source_file.display().to_string(),
        "rule": selection.rule,
    });
    let timing = json!({
        "selection_seconds": round3(selection.selection_seconds),
        "batch_size_requested": args.batch_size,
        "batch_size_effective": context.effective_batch_size,
        "batch_timeout_seconds": args.batch_timeout_seconds,
        "workers_requested": args.workers,
        "workers_used": context.workers,
        "omc_threads": args.omc_threads,
        "batches_total": context.n_batches,
        "batches_ran": metrics.ran_batches,
        "batches_skipped": metrics.skipped_batches,
        "batch_elapsed_stats": metrics.batch_stats,
        "batch_details": state.batch_timings,
    });
    let runtime_ratio_stats = json!({
        "system_ratio_all_positive": metrics.system_ratio_all_positive,
        "system_ratio_both_success": metrics.system_ratio_both_success,
        "wall_ratio_all_positive": metrics.wall_ratio_all_positive,
        "wall_ratio_both_success": metrics.wall_ratio_both_success,
    });
    let runtime_comparison = json!({
        "ratio_definition": "omc_over_rumoca_higher_is_better",
        "ratio_metric_system": "omc_timeSimulation_over_rumoca_sim_seconds",
        "ratio_metric_wall": "omc_external_wall_over_rumoca_external_wall",
        "total_omc_sim_system_seconds": round3(metrics.total_omc_sim_system_seconds),
        "total_omc_total_system_seconds": round3(metrics.total_omc_total_system_seconds),
        "total_omc_wall_seconds": round3(metrics.total_omc_wall_seconds),
        "total_rumoca_sim_seconds": round3(metrics.total_rumoca_sim_seconds),
        "total_rumoca_sim_wall_seconds": round3(metrics.total_rumoca_sim_wall_seconds),
        "ratio_stats": runtime_ratio_stats,
    });
    let trace_comparison = json!({
        "report_file": paths.results_dir.join("sim_trace_comparison.json").display().to_string(),
        "models_compared": trace_summary.models_compared,
        "missing_trace_models": trace_summary.missing_trace_models,
        "skipped_models": trace_summary.skipped_models,
        "agreement_high": trace_summary.agreement_high,
        "agreement_high_percent": trace_summary.agreement_high_percent,
        "agreement_near": trace_summary.agreement_minor,
        "agreement_minor": trace_summary.agreement_minor,
        "agreement_near_percent": trace_summary.agreement_minor_percent,
        "agreement_minor_percent": trace_summary.agreement_minor_percent,
        "agreement_deviation": trace_summary.agreement_deviation,
        "agreement_deviation_percent": trace_summary.agreement_deviation_percent,
        "high_plus_near_models": trace_summary.agreement_high + trace_summary.agreement_minor,
        "high_plus_near_percent": trace_summary.agreement_high_percent + trace_summary.agreement_minor_percent,
        "total_channels_compared": trace_summary.total_channels_compared,
        "bad_channels_total": trace_summary.bad_channels_total,
        "bad_channels_percent": trace_summary.bad_channels_percent,
        "severe_channels_total": trace_summary.severe_channels_total,
        "severe_channels_percent": trace_summary.severe_channels_percent,
        "violation_mass_total": trace_summary.violation_mass_total,
        "violation_mass_mean_per_model": trace_summary.violation_mass_mean_per_model,
        "violation_mass_mean_per_channel": trace_summary.violation_mass_mean_per_channel,
        "models_with_any_channel_deviation": trace_summary.models_with_any_channel_deviation,
        "models_with_any_channel_deviation_percent": trace_summary.models_with_any_channel_deviation_percent,
        "max_model_channel_deviation_percent": trace_summary.max_model_channel_deviation_percent,
        "min_model_bounded_normalized_l1": trace_summary.min_model_bounded_normalized_l1,
        "median_model_bounded_normalized_l1": trace_summary.median_model_bounded_normalized_l1,
        "mean_model_bounded_normalized_l1": trace_summary.mean_model_bounded_normalized_l1,
        "max_model_bounded_normalized_l1": trace_summary.max_model_bounded_normalized_l1,
        "worst_model_bounded_normalized_l1": trace_summary.max_model_bounded_normalized_l1,
        "min_model_score_bounded_normalized_l1": trace_summary.min_model_bounded_normalized_l1,
        "median_model_score_bounded_normalized_l1": trace_summary.median_model_bounded_normalized_l1,
        "mean_model_score_bounded_normalized_l1": trace_summary.mean_model_bounded_normalized_l1,
        "max_model_score_bounded_normalized_l1": trace_summary.max_model_bounded_normalized_l1,
        "mean_model_mean_channel_bounded_normalized_l1": trace_summary.mean_model_mean_channel_bounded_normalized_l1,
        "max_model_max_channel_bounded_normalized_l1": trace_summary.max_model_max_channel_bounded_normalized_l1,
        "global_max_channel_bounded_normalized_l1": trace_summary.max_model_max_channel_bounded_normalized_l1,
    });
    json!({
        "msl_version": MSL_VERSION,
        "omc_version": context.omc_version,
        "git_commit": context.git_commit,
        "target_selection": target_selection,
        "stop_time": args.stop_time,
        "use_experiment_stop_time": args.use_experiment_stop_time,
        "total_models": context.total,
        "processed": state.all_results.len(),
        "sim_successful": metrics.sim_successful,
        "sim_failed": metrics.sim_failed,
        "sim_timed_out": metrics.sim_timed_out,
        "simulation_success_rate_percent": round3(metrics.success_rate),
        "elapsed_seconds": round3(context.elapsed_seconds),
        "timing": timing,
        "runtime_comparison": runtime_comparison,
        "trace_comparison": trace_comparison,
        "models": state.all_results,
    })
}

fn print_speed_snapshot(metrics: &RunMetrics) {
    if let (Some(system), Some(wall)) = (
        &metrics.system_ratio_both_success,
        &metrics.wall_ratio_both_success,
    ) {
        println!(
            "  Speed snapshot (informational only; omc/rumoca, >1 means Rumoca faster; both success, n={}): system_median={:.3e}, wall_median={:.3e}",
            system.sample_count, system.median_ratio, wall.median_ratio
        );
    }
}

fn print_trace_snapshot(trace_summary: &TraceOutputSummary) {
    println!(
        "  Trace gate snapshot ({} models): high={:.2}%, near={:.2}%, deviation={:.2}%",
        trace_summary.models_compared,
        trace_summary.agreement_high_percent,
        trace_summary.agreement_minor_percent,
        trace_summary.agreement_deviation_percent
    );
    println!(
        "    models_with_any_bad_channel={:.2}%, bad_channels={}, severe_channels={}, violation_mass_total={:.6e}",
        trace_summary.models_with_any_channel_deviation_percent,
        trace_summary.bad_channels_total,
        trace_summary.severe_channels_total,
        trace_summary.violation_mass_total
    );
}

fn print_scaling_snapshot(context: &FinalizeContext) {
    let elapsed = context.elapsed_seconds.max(f64::EPSILON);
    let workers = context.workers.max(1);
    let throughput_models_per_sec = context.total as f64 / elapsed;
    let throughput_per_worker = throughput_models_per_sec / workers as f64;
    println!(
        "  Scaling snapshot: workers={}, throughput={:.3} models/s, throughput_per_worker={:.3} models/s",
        workers, throughput_models_per_sec, throughput_per_worker
    );
}

pub(super) fn print_summary(
    output_file: &Path,
    context: &FinalizeContext,
    metrics: &RunMetrics,
    trace_summary: &TraceOutputSummary,
) {
    println!();
    println!("Results saved to {}", output_file.display());
    println!(
        "  Simulation: total={}, ok={}, failed={}, timed_out={}, success_rate={:.1}% ({}/{})",
        context.total,
        metrics.sim_successful,
        metrics.sim_failed,
        metrics.sim_timed_out,
        metrics.success_rate,
        metrics.sim_successful,
        context.total
    );
    print_scaling_snapshot(context);
    print_speed_snapshot(metrics);
    print_trace_snapshot(trace_summary);
    println!(
        "  Elapsed: {:.1}s ({:.2}s/model)",
        context.elapsed_seconds,
        context.elapsed_seconds / context.total.max(1) as f64
    );
}
