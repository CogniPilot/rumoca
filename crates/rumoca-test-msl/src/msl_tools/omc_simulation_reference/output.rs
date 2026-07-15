use super::*;
use rumoca_sim::sim_trace_compare::count_agreement_bands_default;

struct TraceChannelSummary {
    models_with_any_channel_deviation: usize,
    models_with_severe_channel: usize,
    models_with_no_severe_channel: usize,
    models_with_any_channel_deviation_percent: f64,
    models_with_no_severe_channel_percent: f64,
    max_model_channel_deviation_percent: f64,
    total_channels_compared: usize,
    bad_channels_total: usize,
    severe_channels_total: usize,
    bad_channels_percent: f64,
    severe_channels_percent: f64,
    violation_mass_total: f64,
    violation_mass_mean_per_model: f64,
    violation_mass_mean_per_channel: f64,
}

struct TraceChannelTotals {
    models_with_any_channel_deviation: usize,
    models_with_severe_channel: usize,
    total_channels_compared: usize,
    bad_channels_total: usize,
    severe_channels_total: usize,
    violation_mass_total: f64,
    model_count_f64: f64,
}

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
    let channels = trace_channel_summary(trace_report);
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
    let initial_condition = initial_condition_summary(trace_report);
    let state_selection = state_selection::state_selection_summary(trace_report);

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
        total_channels_compared: channels.total_channels_compared,
        bad_channels_total: channels.bad_channels_total,
        severe_channels_total: channels.severe_channels_total,
        bad_channels_percent: channels.bad_channels_percent,
        severe_channels_percent: channels.severe_channels_percent,
        violation_mass_total: channels.violation_mass_total,
        violation_mass_mean_per_model: channels.violation_mass_mean_per_model,
        violation_mass_mean_per_channel: channels.violation_mass_mean_per_channel,
        models_with_bad_channel: channels.models_with_any_channel_deviation,
        models_with_severe_channel: channels.models_with_severe_channel,
        models_with_any_channel_deviation: channels.models_with_any_channel_deviation,
        models_with_any_channel_deviation_percent: channels
            .models_with_any_channel_deviation_percent,
        models_with_no_severe_channel: channels.models_with_no_severe_channel,
        models_with_no_severe_channel_percent: channels.models_with_no_severe_channel_percent,
        max_model_channel_deviation_percent: channels.max_model_channel_deviation_percent,
        min_model_bounded_normalized_l1: min_l1,
        median_model_bounded_normalized_l1: median_l1,
        mean_model_bounded_normalized_l1: mean_l1,
        max_model_bounded_normalized_l1: max_l1,
        mean_model_mean_channel_bounded_normalized_l1: mean_model_mean,
        max_model_max_channel_bounded_normalized_l1: max_model_max,
        initial_condition,
        state_selection,
    }
}

fn trace_channel_summary(trace_report: &TraceQuantification) -> TraceChannelSummary {
    let model_count = trace_report.models.len();
    let model_count_f64 = model_count.max(1) as f64;
    let models_with_any_channel_deviation = trace_report
        .models
        .values()
        .filter(|item| item.metric.channel_deviation_count > 0)
        .count();
    let models_with_severe_channel = trace_report
        .models
        .values()
        .filter(|item| item.metric.channel_severe_count > 0)
        .count();
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
    let violation_mass_total = trace_report
        .models
        .values()
        .map(|item| item.metric.channel_violation_mass)
        .filter(|value| value.is_finite())
        .sum::<f64>();
    let totals = TraceChannelTotals {
        models_with_any_channel_deviation,
        models_with_severe_channel,
        total_channels_compared,
        bad_channels_total,
        severe_channels_total,
        violation_mass_total,
        model_count_f64,
    };
    trace_channel_summary_from_totals(trace_report, totals)
}

fn trace_channel_summary_from_totals(
    trace_report: &TraceQuantification,
    totals: TraceChannelTotals,
) -> TraceChannelSummary {
    let total_channels_compared_f64 = totals.total_channels_compared.max(1) as f64;
    let models_with_no_severe_channel = trace_report
        .models
        .len()
        .saturating_sub(totals.models_with_severe_channel);
    TraceChannelSummary {
        models_with_any_channel_deviation: totals.models_with_any_channel_deviation,
        models_with_severe_channel: totals.models_with_severe_channel,
        models_with_no_severe_channel,
        models_with_any_channel_deviation_percent: totals.models_with_any_channel_deviation as f64
            * 100.0
            / totals.model_count_f64,
        models_with_no_severe_channel_percent: models_with_no_severe_channel as f64 * 100.0
            / totals.model_count_f64,
        max_model_channel_deviation_percent: trace_report
            .models
            .values()
            .map(|item| item.metric.channel_deviation_percent * 100.0)
            .fold(0.0_f64, f64::max),
        total_channels_compared: totals.total_channels_compared,
        bad_channels_total: totals.bad_channels_total,
        severe_channels_total: totals.severe_channels_total,
        bad_channels_percent: totals.bad_channels_total as f64 * 100.0
            / total_channels_compared_f64,
        severe_channels_percent: totals.severe_channels_total as f64 * 100.0
            / total_channels_compared_f64,
        violation_mass_total: totals.violation_mass_total,
        violation_mass_mean_per_model: totals.violation_mass_total / totals.model_count_f64,
        violation_mass_mean_per_channel: totals.violation_mass_total / total_channels_compared_f64,
    }
}

fn initial_condition_summary(trace_report: &TraceQuantification) -> InitialConditionSummary {
    let model_count = trace_report.models.len();
    let model_count_f64 = model_count.max(1) as f64;
    let models_with_initial_condition_deviation = trace_report
        .models
        .values()
        .filter(|item| item.metric.initial_condition.deviation_count > 0)
        .count();
    let channels = trace_report
        .models
        .values()
        .map(|item| &item.metric.initial_condition);
    let mut summary = InitialConditionSummary {
        models_compared: model_count,
        models_with_accurate_initial_conditions: model_count
            .saturating_sub(models_with_initial_condition_deviation),
        models_with_initial_condition_deviation,
        ..InitialConditionSummary::default()
    };
    for stats in channels {
        summary.total_channels_compared += stats.channels_compared;
        summary.high_channels_total += stats.high_count;
        summary.near_channels_total += stats.minor_count;
        summary.deviation_channels_total += stats.deviation_count;
        summary.severe_channels_total += stats.severe_count;
        summary.violation_mass_total += stats.violation_mass_total;
        summary.mean_channel_bounded_normalized_error +=
            stats.mean_channel_bounded_normalized_error * stats.channels_compared as f64;
        summary.max_channel_bounded_normalized_error = summary
            .max_channel_bounded_normalized_error
            .max(stats.max_channel_bounded_normalized_error);
    }
    let channel_count_f64 = summary.total_channels_compared.max(1) as f64;
    summary.accurate_initial_conditions_percent =
        summary.models_with_accurate_initial_conditions as f64 * 100.0 / model_count_f64;
    summary.models_with_initial_condition_deviation_percent =
        summary.models_with_initial_condition_deviation as f64 * 100.0 / model_count_f64;
    summary.high_channels_percent = summary.high_channels_total as f64 * 100.0 / channel_count_f64;
    summary.near_channels_percent = summary.near_channels_total as f64 * 100.0 / channel_count_f64;
    summary.deviation_channels_percent =
        summary.deviation_channels_total as f64 * 100.0 / channel_count_f64;
    summary.severe_channels_percent =
        summary.severe_channels_total as f64 * 100.0 / channel_count_f64;
    summary.violation_mass_mean_per_model = summary.violation_mass_total / model_count_f64;
    summary.violation_mass_mean_per_channel = summary.violation_mass_total / channel_count_f64;
    summary.mean_channel_bounded_normalized_error /= channel_count_f64;
    summary
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
        .filter(|result| omc_model_is_trace_candidate(result))
        .count()
}

fn trace_runtime_totals(metrics: &[TraceModelMetric]) -> (f64, f64, f64, f64, Option<f64>) {
    let total_rumoca_wall = metrics
        .iter()
        .filter_map(|metric| metric.rumoca_sim_wall_seconds)
        .sum::<f64>();
    let total_rumoca_build = metrics
        .iter()
        .filter_map(|metric| metric.rumoca_sim_build_seconds)
        .sum::<f64>();
    let total_rumoca_sim = metrics
        .iter()
        .filter_map(|metric| metric.rumoca_sim_run_seconds.or(metric.rumoca_sim_seconds))
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
        total_rumoca_build,
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
    let (total_rumoca_wall, total_rumoca_build, total_rumoca_sim, total_omc_sim, speedup_ratio) =
        trace_runtime_totals(metrics);
    let shape_counts = trace_shape_counts(metrics);
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
            "models_with_bad_channel": trace_summary.models_with_bad_channel,
            "models_with_severe_channel": trace_summary.models_with_severe_channel,
            "models_with_any_channel_deviation": trace_summary.models_with_any_channel_deviation,
            "models_with_any_channel_deviation_percent": trace_summary.models_with_any_channel_deviation_percent,
            "models_with_no_severe_channel": trace_summary.models_with_no_severe_channel,
            "models_with_no_severe_channel_percent": trace_summary.models_with_no_severe_channel_percent,
            "violation_mass_total": trace_summary.violation_mass_total,
            "violation_mass_mean_per_model": trace_summary.violation_mass_mean_per_model,
            "violation_mass_mean_per_channel": trace_summary.violation_mass_mean_per_channel,
            "initial_condition": &trace_summary.initial_condition,
            "state_selection": &trace_summary.state_selection,
            "total_rumoca_sim_wall_seconds": total_rumoca_wall,
            "total_rumoca_sim_build_seconds": total_rumoca_build,
            "total_rumoca_sim_seconds": total_rumoca_sim,
            "total_omc_sim_system_seconds": total_omc_sim,
            "omc_sim_over_rumoca_sim_speedup_ratio": speedup_ratio,
        },
        "worst_models": metrics.iter().take(25).collect::<Vec<_>>(),
        "shape_counts": shape_counts,
        "missing_trace": quantification.missing_trace,
        "skipped": quantification.skipped,
        "models": quantification.models,
    })
}

fn trace_shape_counts(metrics: &[TraceModelMetric]) -> BTreeMap<String, usize> {
    let mut counts = BTreeMap::new();
    for channel in metrics
        .iter()
        .flat_map(|metric| metric.metric.worst_variables.iter())
    {
        *counts
            .entry(channel.shape.as_str().to_string())
            .or_default() += 1;
    }
    counts
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

fn build_timing_payload(
    args: &Args,
    context: &FinalizeContext,
    metrics: &RunMetrics,
    state: &SimRunState,
) -> Value {
    json!({
        "selection_seconds": round3(0.0),
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
    })
}

fn load_wall_time_provenance(
    args: &Args,
    paths: &MslPaths,
    context: &FinalizeContext,
    state: &SimRunState,
) -> crate::runtime_measurement::WallTimeMeasurementProvenance {
    let rumoca_payload = std::fs::read_to_string(path_for_rumoca_results(paths))
        .ok()
        .and_then(|text| serde_json::from_str::<Value>(&text).ok());
    let scheduler = rumoca_payload
        .as_ref()
        .and_then(|payload| payload.pointer("/timings/scheduler"));
    let load_before = rumoca_payload
        .as_ref()
        .and_then(|payload| payload.pointer("/timings/host_load_before"))
        .and_then(|value| {
            serde_json::from_value::<crate::runtime_measurement::HostLoadSnapshot>(value.clone())
                .ok()
        });
    let comparable_models = state.all_results.iter().filter(|(_, result)| {
        result.status == "success"
            && result.rumoca_status.as_deref() == Some("sim_ok")
            && runtime_pair(result.rumoca_sim_wall_seconds, result.omc_wall_seconds).is_some()
    });
    let (omc_cached_sample_count, omc_fresh_sample_count) =
        comparable_models.fold((0, 0), |(cached, fresh), (name, _)| {
            if state.cached_omc_models.contains(name) {
                (cached + 1, fresh)
            } else {
                (cached, fresh + 1)
            }
        });
    let scheduler_count = |field: &str| {
        scheduler
            .and_then(|value| value.get(field))
            .and_then(Value::as_u64)
            .and_then(|value| usize::try_from(value).ok())
            .unwrap_or(0)
    };
    crate::runtime_measurement::WallTimeMeasurementProvenance {
        omc_fresh_sample_count,
        omc_cached_sample_count,
        affinity_requested_worker_count: scheduler_count("affinity_requested_worker_count"),
        affinity_applied_worker_count: scheduler_count("affinity_applied_worker_count"),
        affinity_failed_worker_count: scheduler_count("affinity_failed_worker_count"),
        normalized_load_before: load_before.map(|snapshot| snapshot.normalized()),
        normalized_load_after: state.host_load_after.map(|snapshot| snapshot.normalized()),
        workers_used: context.workers,
        omc_threads: args.omc_threads,
    }
}

fn build_runtime_comparison_payload(
    args: &Args,
    paths: &MslPaths,
    context: &FinalizeContext,
    metrics: &RunMetrics,
    state: &SimRunState,
) -> Value {
    let runtime_ratio_stats = json!({
        "system_ratio_all_positive": metrics.system_ratio_all_positive,
        "system_ratio_both_success": metrics.system_ratio_both_success,
        "wall_ratio_all_positive": metrics.wall_ratio_all_positive,
        "wall_ratio_both_success": metrics.wall_ratio_both_success,
    });
    let diagnostics = build_runtime_comparison_diagnostics(metrics);
    let wall_time_provenance = load_wall_time_provenance(args, paths, context, state);
    json!({
        "ratio_definition": "omc_over_rumoca_higher_is_better (simulation/wall runtime; this is the SIM-time comparison, distinct from the compile-speed `speedup` in msl_speed_comparison.json)",
        "ratio_metric_system": "omc_timeSimulation_over_rumoca_sim_seconds",
        "ratio_metric_wall": "omc_external_wall_over_rumoca_external_wall",
        "omc_wall_metric_note": "omc_wall_seconds is the direct per-model wall time measured around the simulate() request in a warm session (one-time MSL load excluded)",
        "total_omc_sim_system_seconds": round3(metrics.total_omc_sim_system_seconds),
        "total_omc_total_system_seconds": round3(metrics.total_omc_total_system_seconds),
        "total_omc_wall_seconds": round3(metrics.total_omc_wall_seconds),
        "total_rumoca_sim_seconds": round3(metrics.total_rumoca_sim_seconds),
        "total_rumoca_sim_build_seconds": round3(metrics.total_rumoca_sim_build_seconds),
        "total_rumoca_sim_run_seconds": round3(metrics.total_rumoca_sim_run_seconds),
        "total_rumoca_sim_wall_seconds": round3(metrics.total_rumoca_sim_wall_seconds),
        "ratio_stats": runtime_ratio_stats,
        "diagnostics": diagnostics,
        "wall_time_provenance": wall_time_provenance,
    })
}

fn build_runtime_comparison_diagnostics(metrics: &RunMetrics) -> Value {
    let both_success_samples = metrics
        .system_ratio_both_success
        .as_ref()
        .map(|stats| stats.sample_count)
        .or_else(|| {
            metrics
                .wall_ratio_both_success
                .as_ref()
                .map(|stats| stats.sample_count)
        })
        .unwrap_or(0);
    let mut missing_ratio_stats = Vec::new();
    if metrics.system_ratio_both_success.is_none() {
        missing_ratio_stats.push("system_ratio_both_success");
    }
    if metrics.wall_ratio_both_success.is_none() {
        missing_ratio_stats.push("wall_ratio_both_success");
    }
    let reason = if missing_ratio_stats.is_empty() {
        None
    } else if both_success_samples == 0 {
        Some("no_omc_rumoca_both_success_models")
    } else {
        Some("missing_runtime_ratio_stats")
    };
    json!({
        "both_success_model_count": both_success_samples,
        "runtime_ratio_available": missing_ratio_stats.is_empty(),
        "missing_ratio_stats": missing_ratio_stats,
        "unavailable_reason": reason,
    })
}

fn build_trace_comparison_payload(paths: &MslPaths, trace_summary: &TraceOutputSummary) -> Value {
    let mut payload = json!({
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
        "initial_condition": &trace_summary.initial_condition,
        "state_selection": &trace_summary.state_selection,
    });
    let root = payload
        .as_object_mut()
        .expect("trace comparison payload is an object");
    root.insert(
        "models_with_bad_channel".to_string(),
        json!(trace_summary.models_with_bad_channel),
    );
    root.insert(
        "models_with_severe_channel".to_string(),
        json!(trace_summary.models_with_severe_channel),
    );
    root.insert(
        "models_with_no_severe_channel".to_string(),
        json!(trace_summary.models_with_no_severe_channel),
    );
    root.insert(
        "models_with_no_severe_channel_percent".to_string(),
        json!(trace_summary.models_with_no_severe_channel_percent),
    );
    payload
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
    let mut timing = build_timing_payload(args, context, metrics, state);
    timing["selection_seconds"] = json!(round3(selection.selection_seconds));
    let runtime_comparison = build_runtime_comparison_payload(args, paths, context, metrics, state);
    let trace_comparison = build_trace_comparison_payload(paths, trace_summary);
    let pipeline_progress = build_pipeline_progress_payload(context, metrics, trace_summary, state);
    let omc_assertion_failures = build_omc_assertion_failure_payload(state);
    json!({
        "msl_version": MSL_VERSION,
        "omc_version": context.omc_version,
        "git_commit": context.git_commit,
        // OMC version + MSL source fingerprint; a later run re-runs OMC when this
        // changes (see `omc_reference_cache_key`).
        "cache_key": context.cache_key,
        "target_selection": target_selection,
        "stop_time": args.stop_time,
        "use_experiment_stop_time": args.use_experiment_stop_time,
        "total_models": context.total,
        "processed": state.all_results.len(),
        "sim_successful": metrics.sim_successful,
        "sim_failed": metrics.sim_failed,
        "sim_timed_out": metrics.sim_timed_out,
        "simulation_success_rate_percent": round3(metrics.success_rate),
        "pipeline_progress": pipeline_progress,
        "elapsed_seconds": round3(context.elapsed_seconds),
        "timing": timing,
        "runtime_comparison": runtime_comparison,
        "trace_comparison": trace_comparison,
        "omc_assertion_failures": omc_assertion_failures,
        "models": state.all_results,
    })
}

fn build_pipeline_progress_payload(
    context: &FinalizeContext,
    metrics: &RunMetrics,
    trace_summary: &TraceOutputSummary,
    state: &SimRunState,
) -> Value {
    let ic_attempted = state
        .all_results
        .values()
        .filter(|result| result.rumoca_ic_status.is_some())
        .count();
    let ic_ok = state
        .all_results
        .values()
        .filter(|result| result.rumoca_ic_status.as_deref() == Some("ic_ok"))
        .count();
    let ic_solver_fail = state
        .all_results
        .values()
        .filter(|result| result.rumoca_ic_status.as_deref() == Some("ic_solver_fail"))
        .count();
    json!({
        "selected_models": context.total,
        "omc_successful": metrics.sim_successful,
        "omc_assertion_failure_models": omc_assertion_failure_model_count(state),
        "state_selection": &trace_summary.state_selection,
        "rumoca_ic_attempted": ic_attempted,
        "rumoca_ic_ok": ic_ok,
        "rumoca_ic_solver_fail": ic_solver_fail,
        "rumoca_sim_attempted": state.all_results.values().filter(|result| result.rumoca_status.is_some()).count(),
        "rumoca_sim_ok": state.all_results.values().filter(|result| result.rumoca_status.as_deref() == Some("sim_ok")).count(),
        "rumoca_sim_nan": state.all_results.values().filter(|result| result.rumoca_status.as_deref() == Some("sim_nan")).count(),
        "rumoca_sim_solver_fail": state.all_results.values().filter(|result| result.rumoca_status.as_deref() == Some("sim_solver_fail")).count(),
        "rumoca_sim_timeout": state.all_results.values().filter(|result| result.rumoca_status.as_deref() == Some("sim_timeout")).count(),
    })
}

fn build_omc_assertion_failure_payload(state: &SimRunState) -> Value {
    let mut records = Vec::new();
    let mut total_assertions = 0usize;
    for (model_name, result) in &state.all_results {
        let Some(error) = result.error.as_deref() else {
            continue;
        };
        let assertions = omc_assertion_failure_lines(error);
        if assertions.is_empty() {
            continue;
        }
        total_assertions += assertions.len();
        records.push(json!({
            "model_name": model_name,
            "status": result.status,
            "assertions": assertions,
        }));
    }
    json!({
        "model_count": records.len(),
        "assertion_count": total_assertions,
        "examples": records.iter().take(10).collect::<Vec<_>>(),
        "models": records,
    })
}

fn omc_assertion_failure_model_count(state: &SimRunState) -> usize {
    state
        .all_results
        .values()
        .filter(|result| {
            result
                .error
                .as_deref()
                .is_some_and(|error| !omc_assertion_failure_lines(error).is_empty())
        })
        .count()
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
    println!(
        "    initial_conditions: accurate_models={:.2}%, deviation_channels={:.2}%, violation_mass_total={:.6e}",
        trace_summary
            .initial_condition
            .accurate_initial_conditions_percent,
        trace_summary.initial_condition.deviation_channels_percent,
        trace_summary.initial_condition.violation_mass_total
    );
    println!(
        "    state_selection: exact_models={:.2}%, count_match={:.2}%, rumoca_only={}, omc_only={}",
        trace_summary.state_selection.exact_state_set_match_percent,
        trace_summary.state_selection.state_count_match_percent,
        trace_summary.state_selection.total_rumoca_only_states,
        trace_summary.state_selection.total_omc_only_states
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
    state: &SimRunState,
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
    print_omc_assertion_snapshot(state);
    println!(
        "  Elapsed: {:.1}s ({:.2}s/model)",
        context.elapsed_seconds,
        context.elapsed_seconds / context.total.max(1) as f64
    );
}

fn print_omc_assertion_snapshot(state: &SimRunState) {
    let assertion_model_count = omc_assertion_failure_model_count(state);
    if assertion_model_count == 0 {
        return;
    }
    println!("  OMC assertion failures: {assertion_model_count} model(s)");
    for (model_name, result) in &state.all_results {
        let Some(error) = result.error.as_deref() else {
            continue;
        };
        let assertions = omc_assertion_failure_lines(error);
        if assertions.is_empty() {
            continue;
        }
        println!("    - {model_name}: {}", assertions.join(" | "));
    }
}
